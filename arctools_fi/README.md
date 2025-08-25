# Fragmentation Index from wrist accelerometer minutes

This mini-pipeline converts hourly wrist **.sensor.csv** files (eg NHANES, X/Y/Z acceleration) into minute-epoch counts and computes the fragmentation metrics (ASTP/SATP and bout metrics) for both Vector Magnitude (VM) and Axis1.

It is memory safe by processin one hour at a time, handles messy timestamps, forces a strict 1-min UTC grid, should algorithmically determines and marks the correct non-wear and valid days/timeframes, and saves a quick QC plot. 

## Quickstart guide

**Example CLI wiht arctools_fi/ as pwd**
```
#Hour to minute
Rscript scripts/01_minutes_from_hours.R \
  --indir raw/73557_raw \
  --outdir derived \
  --tz_local America/Chicago

#FI with 90-min non-wear
Rscript scripts/02_arctools_FI.R \
  --deriveddir derived \
  --outdir out \
  --id 73557 \
  --active_cut_axis1 10 \
  --active_cut_vm 1500 \
  --tz_local America/Chicago \
  --min_wear_hours 10 \
  --nonwear_minutes 90 \
  --nonwear_tolerance 2
```

**(In the arctools_fi/ as the pwd)**

0. One time installs
install.packages(c("data.table","lubridate","ggplot2","activityCounts","arctools"))

1. Move/place your raw hourly .sensor.csv files here (and set any ID variabel) after creating the directory
id <- "73557"
dir.create(file.path("raw", paste0(id, "_raw")), recursive = TRUE, showWarnings = FALSE)
- Ex: I moved root/nhanes_raw/NNYFS/73557 .sensor.csvs to this new root/arctools_fi/raw/73557_raw/ directory for my testing (these .sensor.csvs in .../raw/ are .gitignored)

2. Convert hour to min (Axis1 + VM) in UTC time
Console: 
```
rscript <- Sys.which("Rscript"); if (rscript == "") rscript <- file.path(R.home("bin"), "Rscript")
system2(rscript, c("scripts/01_minutes_from_hours.R",
                   "--indir",   file.path("raw", paste0(id, "_raw")),
                   "--outdir",  "derived",
                   "--tz_local","America/Chicago"),   # your study timezone (for plots/valid-day)
        stdout = "", stderr = "")
```

3. Combine the per-hour files into a single, minite by minute table, organized by time, with gaps left (just creating a continuous timeline with missing minutes filled)
```
system2(rscript, c("scripts/02_arctools_FI.R",
                   "--deriveddir","derived",
                   "--outdir","out",
                   "--id", id,
                   "--active_cut_axis1","10",       # Active if Axis1 ≥ 10 counts/min
                   "--active_cut_vm","1500",        # Active if VM ≥ 1500 counts/min
                   "--tz_local","America/Chicago",  # local time for plots/days
                   "--min_wear_hours","10",         # valid day threshold
                   "--nonwear_minutes","60",        # Non-wear rule (strict): flag any stretch of ≥60 consecutive minutes of zero counts as a non-wear; can change to 90 to match Choi et al. 
                   "--nonwear_tolerance","2"),      # allows for tiny blips inside non-wear runs
        stdout = "", stderr = "")
```

4. Outputs (located in root/acrtools_fi/out/)
list.files("out", full.names = TRUE)

## Folder layout

```
arctools_fi/
├─ raw/                        # put hourly .sensor.csv files here, for example for me: raw/73557_raw/*.sensor.csv
│  └─ 73557_raw/               # NHANES NNYFS ID that I used when testing
├─ derived/                    # auto: per-hour, per-minute chunks (Axis1 + VM)
├─ out/                        # auto: stitched minutes and FI outputs; kept these in the commit just to give an example of my outputs
└─ scripts/
   ├─ 01_minutes_from_hours.R  # hour→minute converter using activityCounts
   └─ 02_arctools_FI.R         # stitch, wear/valid-day, FI metrics, QC
```

**Filenames in derived/ won’t include the subject ID; Script 02 uses all minute files unless you explicitly filter.**

## Expected Inputs

Hourly CSV files with: 
- a timestamp column (autodetection among HEADER_TIMESTAMP, Timestamp, TimeStamp, time, etc.)
- X, Y, Z columns (synonyms like AccX, AccelerometerX, etc. are also accepted)

Timestamps can be like NHANES format (2000-01-04-16-30-00-000), ISO (T...Z), or YYYY-MM-DD HH:MM:SS.sss
    - **UTC is used for parsing to avoid timeframe issues**

## What the scripts do

### 01_minutes_from_hours.r

- reads one hour a time to givve per-second counts with **activityCounts::counts()** -> rolls to per-minute:
    - Axis1 = sum of X axis counts over the minute
    - VM = sum of √(x² + y² + z²) counts over the minunte
- keeps boundary minutes (collapses duplicates later when hours join)
- writes derived/min_<original-hour>.csv with columsn Epoch (UTC minute) Axis1, VM

### 02_arctools_FI.r

- Combines all the hour files into one timeline with one row per minute in UTC. If two files cover the same minute, their counts are added and only one row is kept. If a minute is missing, we still keep a blank row for it so the gaps are visible for potential downstream QC

- Finding non-wear time
    - Looks for long stretches of zero movement
    If there exist at least N minutes (choosable parameter like 60 or 90) in a row with zero counts, that is called a "non-wear"
    - Tiny blips of movements (like up to 2 minutes; also a choosable parameter) inside that stretch are **still counted** as non wear
    Any minute marked as non-wear is left out when computing FI metrics
- Add local clock time and pick valid days
    - adds a local time column for plots and day boundaries. **A valid day** is defined as any day with ≥10 hours of wear (changeable). Only minutes from valid days are used.
- Labeling each minute as Active or Sedantary
    - A minute is **Active** if counts are at/above a chosen cutpoint; else **Sedantary**. This is done in two ways:
        - Axis1 (defaulted cutpoint of 10)
        - VM (defaulted cutpoint of 1500)
- Computing fragmentation
    - For Axis1 and VM seperately the following are reported: **ASTP** (how lively Active state transitions to Sedantary), **SATP** (How likely Sedantary state transitions to Active), and average bout lengths for Active and Sedantary
- Results
    - Writes:
        - out/counts_<ID>.csv (minute timeline with UTC/local, Axis1, VM, wear, valid_day)
        - out/fi_summary_<ID>.csv (ASTP/SATP, bout lengths, A/T counts, settings)
        - out/frag_index_<ID>.txt (headline ASTPs)
        - out/qc_<ID>.png (48-hour Active strip plot in local time)
- Prints a quick run log:
    - Shows total minutes, non-wear minutes, gaps, minutes used, and number of valid days.

### Key CLI arguments
```
--nonwear_minutes (60 or 90; minimum zero-run length to call a non-wear on segment of time)
--nonwear_tolerance (2; allowed non-zero "blips" inside a non-wear, units: minutes)
--min_wear_hours (10; wear threshold for a valid day, units: hours),
--active_cut_axis1 (10; Axis1 cutpoint, units: counts/min)
--active_cut_vm (1500; VM cutpoint, units: counts/min)
--tz_local (ex: America/Chicago; local time zone for plots and valid day)
```

## Outputs

1. out/counts_<ID>.csv
Minute-level table (strict grid):
- **Epoch_utc** (POSIXct UTC)
- **Epoch_local** (POSIXct in --tz_local)
- **VM**, Axis1 (counts/min)
- **wear** (TRUE if not non-wear and Axis1 present)
- **valid_day** (TRUE if the local date has ≥ --min_wear_hours wear)
No duplicates minutes and gaps are rows with NA values for counts

2. out/fi_summary_<ID>.csv
One row with:
- Settings: time zone, cutpoints, non-wear rule, tolerance
- Usage stats: **valid_days**, **minutes_used**
- VM metrics: **astp_vm**, **satp_vm**, **A_vm**, **T_vm**, **mean_active_bout_vm**, **mean_sedentary_bout_vm**
- Axis1 metrics: same fields with **_axis1**
- **astp_arctools_vm** (if available)
quick check: **astp_** shuold be around equal to **T_*/A_** (only small minor differences only if you compute via a different mask)

3. out/frag_index_<ID>.txt
Two lines with ASTP for VM and Axis1

4. out/qc_<ID>.png
A 48-hour strip of Active (VM) over time in local time

## Interpreting Results

- ASTP (Active to Sendatary transition Probablity): **higher values means more fragmented** activity ie shorter active bouts relative 
- SATP (Sedantary to Active transition Probability): how often the sedantary bouts are broken
- Bout means give the expected length (in minutes) of active vs sedantary timeframes
- Axis choices and cutpoints:
    - Axis1 (cut=10): lables most movement as "active" and so there is inherently fewer Active to Sedantary transitions, resulting in a lower ASTP
    - VM (cut=1500): is strict and results in shorter active bouts, resulitng in a higher ATSP

## Glossary
- **Axis1**: Counts from the X-axis of the watch (one direction of movement)
- **Counts/Counts per minute** (cpm): A unit ActiGraph uses to summarize movement. Higher = more movement. We total counts every minute.
- **Cutpoint threshold**: The line that sepeartes Active from Sedantary minutes
    - A cutpoint is a threshold for the minute's count. If the count is at/above the cutpoint, then the minute is labeled as **Active**, if below the cutpoint threshold, then it's labeled as **Sedentary**
    - **Axis1 = 10** is a very lenient threshold and allows for practically any movement to count as Active 
        - ***Axis1*** asks if any movement happened
        - ***VM*** asks if the movement was strong enough to be clearly active
    - **VM = 1500** is stricter and requires the participant to have stronger movement, ie 3-axis movement
    - If the Active minutes look unrealistically high (or the QC plot looks to be "always on"), consider raising the cutpoint threshold
- **Transition**: a change from one label to the other between adjacent minutes; 
    - A->S transition: active minute follow by a sedantary minute
    - S->A transition: sedantary minute follow by an active minute
- **ASTP** (Active to Sedentary transition probability): "Out of all the active minutes, how often did i swtich to sedendary the next minute?"
    - Formula: ASTP = (# of A→S transitions) / (A)
    - Higher ASTP = more fragmented activity (shorter active bouts)
- **SATP** (Sedentary to Active transition probability): “Out of all the sedentary minutes, how often did I switch to active next minute?”
    - Formula: SATP = (# of S→A transitions) ÷ (S)
    - Higher SATP = more broken-up sedentary time (shorter sedentary bouts; more sit to moving transitions)
- **Bout length**: a continuous run of the same label (ie all Active or all Sedentary)
    - mean bout length is the average number of minutes in those runs
- **Epoch**: the time slice that is being considered (defaulted to 60s or 1min)
    - Epoch_utc column: minute in UTC
    - Epoch_local: the local clock time used for plots
- **Minute grid**: the minute by minute grid. If two files cover the same minute, the duplicate-minut counts are summed and kept in one row. If a minute is missing, a blank row is populated with NA so the gaps are visible
- **Valid day**: a calendar day (by local time) with at least 10 hours of wear
- **Non-wear**: a block of time in which the watch sensor likely wasn't worn. These are flagged as long runs of zero counts for 60 or 90 minutes, allowing for small blips like ≤2 minutes of activity without breaking the non-wear run. Non-wear minutes/blocks are excluded from the metrics