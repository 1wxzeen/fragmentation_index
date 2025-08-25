#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
})

.has_arctools <- requireNamespace("arctools", quietly = TRUE)

# -------- arg parser: expects --key value pairs --------
.parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  out <- list(); i <- 1
  while (i <= length(a)) {
    if (startsWith(a[i], "--")) {
      key <- sub("^--", "", a[i])
      val <- if (i + 1 <= length(a)) a[i + 1] else NA_character_
      out[[key]] <- val
      i <- i + 2
    } else i <- i + 1
  }
  out
}

args <- .parse_args()
deriveddir          <- args[["deriveddir"]]
outdir              <- args[["outdir"]]
id                  <- args[["id"]]
active_cut_vm       <- as.numeric(args[["active_cut_vm"]]);       if (is.na(active_cut_vm)) active_cut_vm <- 1500
active_cut_axis1    <- as.numeric(args[["active_cut_axis1"]]);    if (is.na(active_cut_axis1)) active_cut_axis1 <- 10
tz_local            <- args[["tz_local"]];                        if (is.null(tz_local)) tz_local <- "America/Chicago"
min_wear_hours      <- as.numeric(args[["min_wear_hours"]]);      if (is.na(min_wear_hours)) min_wear_hours <- 10
nonwear_minutes     <- as.numeric(args[["nonwear_minutes"]]);     if (is.na(nonwear_minutes)) nonwear_minutes <- 60
nonwear_tolerance   <- as.numeric(args[["nonwear_tolerance"]]);   if (is.na(nonwear_tolerance)) nonwear_tolerance <- 2 # minutes

if (is.null(deriveddir) || is.null(outdir) || is.null(id))
  stop("--deriveddir, --outdir, --id are required")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

.message <- function(...) cat(sprintf("%s %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))

# -------- load minute chunks --------
files <- list.files(deriveddir, pattern = "^min_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No min_*.csv files found in --deriveddir")

# Only filter by id if filenames actually contain it; otherwise, use all files (no warning)
keep <- grepl(id, basename(files))
if (any(keep)) files <- files[keep]

.message("Binding %d files", length(files))
DT <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)

# standardize names
if (!"Epoch" %in% names(DT)) {
  epoch_col <- intersect(names(DT), c("Timestamp","time","TimeStamp","Epoch_utc"))[1]
  if (is.na(epoch_col)) stop("Could not find Epoch column in per-minute files")
  setnames(DT, epoch_col, "Epoch")
}
if (!"VM" %in% names(DT))    stop("Missing VM column in per-minute files")
if (!"Axis1" %in% names(DT)) stop("Missing Axis1 column in per-minute files")

DT[, Epoch := as.POSIXct(Epoch, tz = "UTC")]
setorder(DT, Epoch)

n_before <- nrow(DT)

# collapse duplicates by SUM (defensible when merging partial minutes)
DT <- DT[, .(VM = sum(VM, na.rm = TRUE),
             Axis1 = sum(Axis1, na.rm = TRUE)), by = .(Epoch)]

# strict 1-min grid; gaps become NA
full <- data.table(Epoch = seq(min(DT$Epoch), max(DT$Epoch), by = "1 min"))
DT <- DT[full, on = "Epoch"] # previously used version of  DT <- full[DT, on = "Epoch"]
                             # means the 1-min grid isnt actually filled with "NA;" 
                             #  goal is to build a 1-min grid and fill gaps with NA
setorder(DT, Epoch)

# local time and date
DT[, Epoch_local := with_tz(Epoch, tz_local)]
DT[, date_local  := as.Date(Epoch_local)]

# -------- tolerant non-wear detection (bridges 1-min gaps; tolerates short blips) --------
# Treat NA as zero for DETECTION ONLY. NA minutes are still excluded from FI later.
zero_or_gap <- is.na(DT$Axis1) | DT$Axis1 == 0

# Collapse short non-zero islands (<= nonwear_tolerance) flanked by zeros back to zero
r <- rle(zero_or_gap)
if (nonwear_tolerance > 0) {
  idx <- which(r$values == FALSE & r$lengths <= nonwear_tolerance)
  if (length(idx)) r$values[idx] <- TRUE
}
zero_det <- inverse.rle(r)

# Run length per position (consecutive TRUE or FALSE blocks)
brk    <- c(TRUE, diff(zero_det) != 0)
grp    <- cumsum(brk)
runlen <- ave(zero_det, grp, FUN = length)

# Non-wear if zero_det TRUE for >= nonwear_minutes (e.g., 60 or 90)
DT[, nonwear := as.logical(zero_det) & (runlen >= nonwear_minutes)]
# Wear must be not non-wear and not missing Axis1 (so NA minutes remain excluded)
DT[, wear := !nonwear & !is.na(Axis1)]

# valid day (>= min_wear_hours of wear in local day)
byday <- DT[, .(wear_mins = sum(wear %in% TRUE)), by = date_local]
valid_days <- byday[wear_mins >= min_wear_hours * 60, date_local]
DT[, valid_day := date_local %in% valid_days]

# minutes used for FI
DT[, use_for_fi := (wear %in% TRUE) & (valid_day %in% TRUE)]

# -------- FI helpers --------
fi_from_series <- function(counts, active_cut, mask) {
  ok <- which(mask & !is.na(counts))
  if (length(ok) < 2) {
    return(list(astp = NA_real_, satp = NA_real_, A = 0L, T = 0L,
                mean_active_bout = NA_real_, mean_sedentary_bout = NA_real_))
  }
  flag <- counts[ok] >= active_cut
  d    <- diff(as.integer(flag))
  A    <- sum(flag)
  T    <- sum(d == -1L)
  S    <- sum(d ==  1L)
  rl   <- rle(as.integer(flag))
  meanA <- if (any(rl$values == 1L)) mean(rl$lengths[rl$values == 1L]) else NA_real_
  meanS <- if (any(rl$values == 0L)) mean(rl$lengths[rl$values == 0L]) else NA_real_
  list(astp = ifelse(A > 0, T / A, NA_real_),
       satp = ifelse((length(flag) - A) > 0, S / (length(flag) - A), NA_real_),
       A = as.integer(A), T = as.integer(T),
       mean_active_bout = meanA, mean_sedentary_bout = meanS)
}

calc_with_arctools <- function(counts, time, mask) {
  if (!.has_arctools) return(NA_real_)
  tryCatch({
    arctools::activity_stats(activity = counts[mask], time = time[mask])$astp
  }, error = function(e) NA_real_)
}

# -------- compute metrics --------
res_vm  <- fi_from_series(DT$VM,    active_cut_vm,    DT$use_for_fi)
res_ax1 <- fi_from_series(DT$Axis1, active_cut_axis1, DT$use_for_fi)

# logging
n_after <- nrow(DT)
minutes_nonwear <- sum(DT$nonwear %in% TRUE)
minutes_gap_na  <- sum(is.na(DT$Axis1))  # pure NA gaps
minutes_used    <- sum(DT$use_for_fi %in% TRUE)

.message("Rows before dedupe: %d, strict grid: %d", n_before, n_after)
.message("Non-wear minutes: %d (rule: %d-min, tolerance: %d)", minutes_nonwear, nonwear_minutes, nonwear_tolerance)
.message("Gap NA minutes (regardless of wear): %d", minutes_gap_na)
.message("Minutes used for FI: %d across %d valid day(s)", minutes_used, length(valid_days))

# -------- outputs --------
counts_out <- file.path(outdir, sprintf("counts_%s.csv", id))
DT_out <- DT[, .(Epoch_utc = Epoch, Epoch_local, VM, Axis1, wear, valid_day)]
fwrite(DT_out, counts_out)
.message("Wrote %s (%d rows)", counts_out, nrow(DT_out))

sum_out <- file.path(outdir, sprintf("fi_summary_%s.csv", id))
sum_dt <- data.table(
  id = id,
  tz_local = tz_local,
  valid_days = length(valid_days),
  minutes_used = minutes_used,
  nonwear_minutes = nonwear_minutes,
  nonwear_tolerance = nonwear_tolerance,
  active_cut_vm = active_cut_vm,
  active_cut_axis1 = active_cut_axis1,
  astp_vm = res_vm$astp,
  satp_vm = res_vm$satp,
  A_vm = res_vm$A,
  T_vm = res_vm$T,
  mean_active_bout_vm = res_vm$mean_active_bout,
  mean_sedentary_bout_vm = res_vm$mean_sedentary_bout,
  astp_axis1 = res_ax1$astp,
  satp_axis1 = res_ax1$satp,
  A_axis1 = res_ax1$A,
  T_axis1 = res_ax1$T,
  mean_active_bout_axis1 = res_ax1$mean_active_bout,
  mean_sedentary_bout_axis1 = res_ax1$mean_sedentary_bout,
  astp_arctools_vm = if (.has_arctools) calc_with_arctools(DT$VM, DT$Epoch, DT$use_for_fi) else NA_real_
)
fwrite(sum_dt, sum_out)
.message("Wrote %s", sum_out)

fi_txt <- file.path(outdir, sprintf("frag_index_%s.txt", id))
writeLines(sprintf("ASTP_VM=%.6f\nASTP_Axis1=%.6f", sum_dt$astp_vm, sum_dt$astp_axis1), fi_txt)
.message("Wrote %s", fi_txt)

# -------- QC plot: first 48h of VM Active flag (light theme for readability) --------
DT_plot <- DT[use_for_fi == TRUE]
if (nrow(DT_plot) > 0) {
  cutoff <- min(DT_plot$Epoch_local) + hours(48)
  P <- DT_plot[Epoch_local <= cutoff]
  P[, Active := as.integer(VM >= active_cut_vm)]

  g <- ggplot(P, aes(x = Epoch_local, y = Active)) +
    geom_step(linewidth = 0.7, color = "black") +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(
      title = sprintf("Active (VM) over time — ID %s", id),
      x = sprintf("Local time (%s)", tz_local),
      y = "Active = 1"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      text             = element_text(color = "black"),
      axis.text        = element_text(color = "black"),
      axis.title       = element_text(color = "black"),
      plot.title       = element_text(color = "black", face = "bold")
    )

  qc_png <- file.path(outdir, sprintf("qc_%s.png", id))
  ggsave(qc_png, g, width = 12, height = 4, dpi = 150, bg = "white")
  .message("Saved QC to %s", qc_png)
} else {
  .message("No minutes eligible for FI; QC plot skipped.")
}

.message("Done.")
