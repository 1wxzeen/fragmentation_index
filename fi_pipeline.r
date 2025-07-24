#Full week
#reads in every hourly CSV file for the subject (ie a full 7day wear period)
#converts all of it into counts, computing the fragmentation index, and then making a QC plot
#---The QC plot functionality for some reason doesn't work despite my debugging attempts
#This gives the true, week-long FI
#I haven't been able to run this script entirely, because whenever I do i get the message "Error: vector memory limit of 16.0 Gb reached, see mem.maxVSize()"

# The fi_pipeline_mini.r file is a mirror of this full "main" file, wherein the only difference is that the mini file only reads the first N files (roughly the first 24 hours)



#Dependencies should auto install
pkgs <- c("data.table","ggplot2","agcounts")
new  <- setdiff(pkgs, installed.packages()[,1])
if(length(new)) install.packages(new)
lapply(pkgs, library, character.only=TRUE)

#User settings (only spot where changes need to be made to account for file names and directories)
cycle     <- "NNYFS"  #NNYFS = National Youth Fitness Survey, a special NHANES "cycle" focused on children's fitness
id        <- "73557"  #participant folder name under nhanes_raw/<cycle>/[5 integer ID]

#configuring the paths
base_dir  <- "nhanes_raw"
hour_dir  <- file.path(base_dir, cycle, id)


csvs <- list.files(hour_dir, "\\.sensor\\.csv$", full.names=TRUE)
stopifnot(length(csvs)>0)

raw <- rbindlist(lapply(csvs, fread,
                        select=c("HEADER_TIMESTAMP","X","Y","Z")))
setnames(raw, "HEADER_TIMESTAMP", "time")
raw[, time := as.POSIXct(time, tz="UTC")]


cnt_out <- agcounts::calculate_counts(as.data.frame(raw), epoch=60)
cnt60   <- as.data.table(cnt_out)


tm <- grep("^(Time|Epoch)$", names(cnt60), ignore.case=TRUE, value=TRUE)[1]
setnames(cnt60, tm, "Time")
vm <- grep("^(VM|vm|Vector\\.Magnitude)$", names(cnt60),
           ignore.case=TRUE, value=TRUE)[1]
setnames(cnt60, vm, "VM")

#FI
cnt60[, Active := VM >= 10]   # 10‑count threshold
calc_fi <- function(f) {
  r <- rle(f)
  N <- sum(r$values)
  A <- sum(r$lengths[r$values])
  if(A==0) return(NA_real_)
  N/A
}
FI <- calc_fi(cnt60$Active)
cat("Fragmentation Index (FI):", round(FI,4), "\n")

#saving outputs innto nhanes_raw/<cycle>/<id>/
out_counts <- file.path(hour_dir, paste0("counts_", id, ".csv"))
out_fi     <- file.path(hour_dir, paste0("frag_index_", id, ".txt"))
out_qc     <- file.path(hour_dir, paste0("qc_", id, ".png"))

fwrite(cnt60, out_counts)
writeLines(as.character(FI), out_fi)

png(out_qc, width=1200, height=400, res=150)
ggplot(cnt60, aes(Time, as.numeric(Active))) +
  geom_step() +
  labs(title=paste("QC", id),
       y="Active 1/0", x="Minute")
dev.off()

cat("Written to", hour_dir, "\n",
    basename(out_counts), "\n",
    basename(out_fi), "\n",
    basename(out_qc), "\n")
