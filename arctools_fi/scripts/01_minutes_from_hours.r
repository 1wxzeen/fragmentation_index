#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(activityCounts)
})

# ---- arg parser ----
.parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  out <- list(); i <- 1
  while (i <= length(a)) {
    if (startsWith(a[i], "--")) { out[[sub("^--","",a[i])]] <- a[i+1]; i <- i + 2 } else i <- i + 1
  }
  out
}
args <- .parse_args()
indir    <- args[["indir"]]
outdir   <- args[["outdir"]]
tz_local <- args[["tz_local"]]; if (is.null(tz_local)) tz_local <- "America/Chicago"

if (is.null(indir) || is.null(outdir)) stop("--indir and --outdir are required")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
.message <- function(...) cat(sprintf("%s %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))

# ---- robust time parser (handles '2000-01-04-16-30-00-000', ISO, etc.) ----
.parse_time_utc <- function(x) {
  cand <- list(
    function(s) as.POSIXct(s, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    function(s) as.POSIXct(s, format = "%Y-%m-%d-%H-%M-%OS", tz = "UTC"),
    function(s) as.POSIXct(gsub("-", " ", s), format = "%Y %m %d %H %M %OS", tz = "UTC"),
    function(s) suppressWarnings(lubridate::ymd_hms(s, tz = "UTC", quiet = TRUE))
  )
  for (f in cand) {
    t <- suppressWarnings(f(x))
    if (!all(is.na(t))) return(t)
  }
  as.POSIXct(NA, tz = "UTC")
}

# ---- read one hour file ----
.read_hour <- function(f) {
  dt <- suppressWarnings(fread(f, fill = TRUE))
  tcol <- intersect(names(dt),
                    c("HEADER_TIMESTAMP","HEADER_TIME_STAMP","Timestamp","TimeStamp",
                      "time","datetime","DateTime","Epoch"))[1]
  if (is.na(tcol)) stop(sprintf("No time column in %s", basename(f)))
  setnames(dt, tcol, "time", skip_absent = TRUE)

  # Accept common X/Y/Z synonyms
  xc <- intersect(names(dt), c("X","x","AccX","AccelerometerX"))[1]
  yc <- intersect(names(dt), c("Y","y","AccY","AccelerometerY"))[1]
  zc <- intersect(names(dt), c("Z","z","AccZ","AccelerometerZ"))[1]
  if (any(is.na(c(xc,yc,zc))))
    stop(sprintf("Missing X/Y/Z in %s (cols: %s)", basename(f), paste(names(dt), collapse=",")))
  setnames(dt, c(xc,yc,zc), c("X","Y","Z"), skip_absent = TRUE)

  dt[, time := .parse_time_utc(time)]
  if (all(is.na(dt$time))) stop("Could not parse time values")
  dt[order(time)]
}

# keep only fully covered minutes (drop partial first/last)
.boundary_filter <- function(mins, file_start, file_end) {
  start_guard <- ceiling_date(file_start, unit = "minute")
  end_guard   <- floor_date(file_end,   unit = "minute") - minutes(1)
  if (nrow(mins) == 0) return(mins)
  if (is.na(start_guard) || is.na(end_guard) || start_guard > end_guard) return(mins[0])
  mins[Epoch >= start_guard & Epoch <= end_guard]
}

# infer sampling rate from timestamps (Hz)
.infer_hz <- function(tt) {
  d <- as.numeric(median(diff(tt), na.rm = TRUE))  # seconds
  hz <- as.integer(round(1 / d))
  hz
}

# convert one hour of raw XYZ to minute counts (Axis1 + VM) using activityCounts
.hour_to_minutes <- function(dt) {
  hz <- .infer_hz(dt$time)
  if (is.na(hz) || hz < 1) stop("Could not infer sampling rate (Hz).")
  # activityCounts::counts needs a table with time + x + y + z
  base <- data.table(Time = dt$time, x = dt$X, y = dt$Y, z = dt$Z)

  # per-second counts for each axis; Time column is used as starting anchor
  sec <- activityCounts::counts(
    data = base, hertz = hz,
    x_axis = 2, y_axis = 3, z_axis = 4,
    time_column = 1
  )
  setDT(sec)
  # standardize names (package returns x,y,z)
  setnames(sec, c("Time","x","y","z"), c("Time","x","y","z"), skip_absent = TRUE)
  sec[, Time := as.POSIXct(Time, tz = "UTC")]

  # compute VM at per-second level, then roll up to 1-min by SUM
  sec[, VM_sec := sqrt(x^2 + y^2 + z^2)]
  mins <- sec[, .(
    Axis1 = sum(x, na.rm = TRUE),
    VM    = sum(VM_sec, na.rm = TRUE)
  ), by = .(Epoch = floor_date(Time, unit = "minute"))]
  mins[order(Epoch)]
}

files <- list.files(indir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No .csv files found in --indir")
.message("Found %d files", length(files))

ok <- 0L; fail <- 0L
for (f in files) {
  .message("Processing %s", basename(f))
  tryCatch({
    hr <- .read_hour(f)
    if (nrow(hr) == 0) stop("Hour file empty after read")
    mins <- .hour_to_minutes(hr)
    #  keep boundary minutes so zero-runs can cross hours; duplicates will be summed later
    # mins <- .boundary_filter(mins, min(hr$time, na.rm = TRUE), max(hr$time, na.rm = TRUE)) commented out
    out <- file.path(outdir, sprintf("min_%s.csv", tools::file_path_sans_ext(basename(f))))
    fwrite(mins, out)
    .message("Wrote %s (%d rows)", out, nrow(mins))
    ok <- ok + 1L
  }, error = function(e) {
    .message("ERROR in %s: %s", basename(f), conditionMessage(e))
    fail <<- fail + 1L
  })
}
.message("Done. Succeeded: %d  Failed: %d", ok, fail)
