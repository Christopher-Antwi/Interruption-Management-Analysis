library(tidyverse)
library(lubridate)
library(readxl)

setwd(file.path(Sys.getenv("USERPROFILE"), "Dropbox", "Interruption Management"))
base <- "Cleaned Data& Code/"

# ── Load Data ──────────────────────────────────────────────────────────────────
feeders            <- read_rds(paste0(base, "liw_feeders.rds"))
tablet_press       <- read_rds(paste0(base, "tablet_press.rds"))
tablet_quality     <- read_rds(paste0(base, "tablet_quality.rds"))
content_uniformity <- read_rds(paste0(base, "content_uniformity.rds"))
temperature        <- read_rds(paste0(base, "temperature.rds"))
humidity           <- read_rds(paste0(base, "humidity.rds"))

# ── Load clean logbook ─────────────────────────────────────────────────────────
logbook_raw <- read_excel(
  paste0(base, "Contextual quality data/LogBook_Data_Analysis.xlsx"),
  sheet = "Failure Log"
) %>%
  rename(
    time       = `Observations:`,
    time_index = `Time Index`,
    day        = Date,
    operations = Operation,
    group      = Group
  ) %>%
  filter(!is.na(time), !is.na(day), !is.na(operations))

# ── 1. Speed ───────────────────────────────────────────────────────────────────
speed_30 <- tablet_press %>%
  mutate(slot = floor_date(timestamp, "30 seconds")) %>%
  group_by(slot) %>%
  summarise(Speed = mean(filling_shoe_m20m13_speed, na.rm = TRUE), .groups = "drop")

# ── 2. All-zero machine stop slots (filling shoe speed == 0) ──────────────────
machine_stops <- tablet_press %>%
  mutate(slot = floor_date(timestamp, "30 seconds")) %>%
  group_by(slot) %>%
  summarise(
    speed_zero = mean(filling_shoe_m20m13_speed, na.rm = TRUE) == 0,
    .groups = "drop"
  ) %>%
  filter(speed_zero) %>%
  arrange(slot)

# ── 3. Label contiguous runs ───────────────────────────────────────────────────
machine_stops_runs <- machine_stops %>%
  mutate(
    gap     = as.numeric(difftime(slot, lag(slot, default = slot[1]), units = "secs")),
    new_run = gap > 30 | row_number() == 1,
    run_id  = cumsum(new_run)
  ) %>%
  group_by(run_id) %>%
  summarise(
    run_start = min(slot),
    run_end   = max(slot),
    n_slots   = n(),
    .groups   = "drop"
  )

# ── 4. Exogenous logbook entries ──────────────────────────────────────────────
exogenous_groups <- c("Feeder/Feeding", "Electronic Communication", "Compression")

log_exog <- logbook_raw %>%
  filter(group %in% exogenous_groups) %>%
  mutate(
    log_timestamp = as.POSIXct(
      paste0("2018-01-", sprintf("%02d", as.integer(day)), " ", time),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )
  ) %>%
  filter(!is.na(log_timestamp)) %>%
  arrange(log_timestamp)

# ── 5. Match runs: ±30 min window + max 20 slots ──────────────────────────────
window_secs <- 30 * 60
log_times   <- sort(unique(log_exog$log_timestamp))

matched_runs <- machine_stops_runs %>%
  filter(n_slots <= 20) %>%
  filter(map_lgl(run_start, function(s) {
    any(abs(as.numeric(difftime(log_times, s, units = "secs"))) <= window_secs)
  }))

cat("Matched runs:", nrow(matched_runs), "\n")
cat("Total matched slots:", sum(matched_runs$n_slots), "\n")

# ── 6. Flag all slots in matched runs ─────────────────────────────────────────
matched_slot_ids <- machine_stops %>%
  mutate(
    gap     = as.numeric(difftime(slot, lag(slot, default = slot[1]), units = "secs")),
    new_run = gap > 30 | row_number() == 1,
    run_id  = cumsum(new_run)
  ) %>%
  filter(run_id %in% matched_runs$run_id) %>%
  pull(slot)

interruption_30 <- tibble(
  slot                 = sort(unique(machine_stops$slot)),
  Interruption         = as.integer(slot %in% matched_slot_ids),
  Interruption_Feeding = as.integer(slot %in% matched_slot_ids)
)

# ── 7. Environmental controls: rebuild from raw RDS ───────────────────────────
press_temp <- temperature %>%
  filter(machine == "press") %>%
  mutate(
    timestamp        = as.POSIXct(timestamp, tz = "UTC"),
    Press_Temperature = as.numeric(humidity)  # col is named humidity but contains temp values
  ) %>%
  filter(!is.na(timestamp), !is.na(Press_Temperature)) %>%
  mutate(slot = floor_date(timestamp, "30 seconds")) %>%
  group_by(slot) %>%
  summarise(Press_Temperature = mean(Press_Temperature, na.rm = TRUE), .groups = "drop")

press_hum <- humidity %>%
  filter(machine == "press") %>%
  mutate(
    timestamp    = as.POSIXct(timestamp, tz = "UTC"),
    Press_Humidity = as.numeric(humidity)
  ) %>%
  filter(!is.na(timestamp), !is.na(Press_Humidity)) %>%
  mutate(slot = floor_date(timestamp, "30 seconds")) %>%
  group_by(slot) %>%
  summarise(Press_Humidity = mean(Press_Humidity, na.rm = TRUE), .groups = "drop")

cat("Press temp rows:", nrow(press_temp), "\n")
cat("Press hum rows:", nrow(press_hum), "\n")

# ── 8. Tablet weight ───────────────────────────────────────────────────────────
tablet_q <- tablet_quality %>%
  mutate(
    timestamp   = as.POSIXct(timestamp, tz = "UTC"),
    Weight      = avg_weight_mg,
    Weight_Pass = as.integer(abs(avg_weight_mg - 400) / 400 <= 0.01)
  ) %>%
  select(timestamp, Weight, Weight_Pass) %>%
  drop_na(timestamp) %>%
  arrange(timestamp)

# ── 9. Drug content ────────────────────────────────────────────────────────────
drug_q <- content_uniformity %>%
  rename(Drug_Content = `Average Drug content (%)`) %>%
  select(timestamp, Drug_Content) %>%
  drop_na(timestamp) %>%
  arrange(timestamp)

# ── 10. Lot change ─────────────────────────────────────────────────────────────
lot_start <- as.POSIXct("2018-01-15 10:42:00", tz = "UTC")
lot_end   <- as.POSIXct("2018-01-15 16:20:00", tz = "UTC")

# ── 11. Build 30-sec panel ────────────────────────────────────────────────────
time_grid <- tibble(
  slot = seq(
    from = as.POSIXct("2018-01-12 08:42:30", tz = "UTC"),
    to   = as.POSIXct("2018-01-17 03:40:00", tz = "UTC"),
    by   = "30 secs"
  )
)

panel <- time_grid %>%
  left_join(speed_30,        by = "slot") %>%
  left_join(interruption_30, by = "slot") %>%
  left_join(press_temp,      by = "slot") %>%
  left_join(press_hum,       by = "slot") %>%
  replace_na(list(Interruption = 0, Interruption_Feeding = 0))

# ── 12. Forward-fill weight, drug content, temp, humidity ─────────────────────
# Weight and drug content forward-filled (sampled every 15-30 min)
panel <- panel %>%
  mutate(
    w_idx        = findInterval(slot, tablet_q$timestamp),
    Weight       = tablet_q$Weight[pmax(w_idx, 1)],
    Weight_Pass  = tablet_q$Weight_Pass[pmax(w_idx, 1)],
    d_idx        = findInterval(slot, drug_q$timestamp),
    Drug_Content = drug_q$Drug_Content[pmax(d_idx, 1)]
  ) %>%
  select(-w_idx, -d_idx)

# Forward-fill temp and humidity (recorded every 15 min)
panel <- panel %>%
  arrange(slot) %>%
  fill(Press_Temperature, Press_Humidity, .direction = "down")

# ── 13. Lot change, date FE, lag ──────────────────────────────────────────────
panel <- panel %>%
  mutate(
    Lot_Change = as.integer(slot >= lot_start & slot <= lot_end),
    date       = as.Date(slot),
    Date_FE    = as.factor(date)
  ) %>%
  arrange(slot) %>%
  mutate(
    Interruption_lag         = lag(Interruption, 1),
    Interruption_Feeding_lag = lag(Interruption_Feeding, 1)
  )

# ── 14. Save ──────────────────────────────────────────────────────────────────
saveRDS(panel, paste0(base, "analysis_ready.rds"))

cat("Done. Rows:", nrow(panel), "\n")
cat("Interruptions (lagged):", sum(panel$Interruption_lag, na.rm = TRUE), "\n")
cat("Non-interrupted:", sum(panel$Interruption_lag == 0, na.rm = TRUE), "\n")
cat("Speed non-NA:", sum(!is.na(panel$Speed)), "\n")
cat("Weight non-NA:", sum(!is.na(panel$Weight)), "\n")
cat("Drug_Content non-NA:", sum(!is.na(panel$Drug_Content)), "\n")
cat("Temp non-NA:", sum(!is.na(panel$Press_Temperature)), "\n")
cat("Humidity non-NA:", sum(!is.na(panel$Press_Humidity)), "\n")

# ── 15. Summary stats check vs Table 1 ────────────────────────────────────────
panel %>%
  summarise(
    mean_interruption = mean(Interruption_lag, na.rm = TRUE),
    mean_speed        = mean(Speed, na.rm = TRUE),
    mean_weight       = mean(Weight, na.rm = TRUE),
    mean_drug         = mean(Drug_Content, na.rm = TRUE),
    mean_lot          = mean(Lot_Change, na.rm = TRUE),
    mean_temp         = mean(Press_Temperature, na.rm = TRUE),
    mean_hum          = mean(Press_Humidity, na.rm = TRUE)
  )












