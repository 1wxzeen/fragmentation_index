#mini file with modular time resolution
#This file produces a FI output without hitting my 16 GB memory limit (only tested in N_demo = 24 and 48)
#Mirror of the main file (fi_pipeline.r); only differnece is inclusion of N_demo

#Dependencies should auto install
pkgs <- c("data.table","ggplot2","agcounts")
for(p in pkgs) if(!p %in% installed.packages()[,1]) install.packages(p)
lapply(pkgs, library, character.only=TRUE)

#User settings (only spot where changes need to be made to account for file names and directories)
cycle     <- "NNYFS"  #NNYFS = National Youth Fitness Survey, a special NHANES "cycle" focused on children's fitness
id        <- "73557"  #participant folder name under nhanes_raw/<cycle>/[5 integer ID]
N_demo    <- 24 #tells how many hourly files we want to load, where 24 ≈ 1 day, 48 ≈ 2 days, 168 ≈ 7 days

#configuring the paths
base_dir  <- "nhanes_raw"
hour_dir  <- file.path(base_dir, cycle, id)

#Load first N_demo files
csvs <- sort(list.files(hour_dir, "\\.sensor\\.csv$", full.names=TRUE))
csvs <- head(csvs, N_demo)
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
cnt60[, Active := VM >= 10]
calc_fi <- function(f) {
  r <- rle(f); N <- sum(r$values); A <- sum(r$lengths[r$values])
  if(A==0) return(NA_real_); N/A
}
FI <- calc_fi(cnt60$Active)
cat("Fragmentation Index (FI):", round(FI,4), "\n")

#saving outputs innto nhanes_raw/<cycle>/<id>/
out_counts <- file.path(hour_dir, paste0("counts_", id, "_mini.csv"))
out_fi     <- file.path(hour_dir, paste0("frag_index_", id, "_mini.txt"))
out_qc     <- file.path(hour_dir, paste0("qc_", id, "_mini.png"))

fwrite(cnt60, out_counts)
writeLines(as.character(FI), out_fi)

png(out_qc, width=1200, height=400, res=150)
ggplot(cnt60, aes(Time, as.numeric(Active))) +
  geom_step() +
  labs(title=paste("QC", id, "mini"),
       y="Active 1/0", x="Minute")
dev.off()

cat("Written to", hour_dir, "\n",
    basename(out_counts), "\n",
    basename(out_fi), "\n",
    basename(out_qc), "\n")
