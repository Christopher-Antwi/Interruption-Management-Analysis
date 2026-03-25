# Clean Data in the Work space ----

## Attaching Required Packages 

source(paste(getwd(),"/Cleaned Data& Code/","ipak.R",sep = ""))

# Reading Data sets ----


blender = read_rds(paste(getwd(),"/Cleaned Data& Code/", "blenders.rds", sep = ""))

feeders = read_rds(paste(getwd(),"/Cleaned Data& Code/", "liw_feeders.rds", sep = ""))

tablet_press = read_rds(paste(getwd(),"/Cleaned Data& Code/", "tablet_press.rds", sep = ""))

#temperature = read_rds(paste(getwd(),"/Cleaned Data& Code/", "temperature.rds", sep = ""))

#humidity = read_rds(paste(getwd(),"//Data/processed/rds/", "humidity.rds", sep = ""))


# Blenders Data ---- 
blenderz_long = blender %>% pivot_longer(cols = c(massflow_blender_1:oos_concentration_at_blender_2_inlet), names_to = "measure", values_to = "value") %>%
  arrange(measure, timestamp) %>% mutate(status = if_else(value == 0, "interruption", "running")) %>% 
  mutate(slot_30sec = if_else(sec <= 29, 0, 30), slot_30min = if_else(min <= 29, 0, 30)) %>%
  group_by(measure) %>% mutate(status_chg = if_else(status == lag(status,1), 0, 1)) %>% 
  replace_na(list(status_chg = 0)) %>% mutate(seq = cumsum(status_chg)) %>% ungroup()

### Blenders Full Summary ----
blenders_full_summary = blenderz_long%>%
  group_by(measure, status) %>%
  summarise(
    n_obs        = n(),
    avg_value    = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value     = sd(value, na.rm = TRUE),
    min_value    = min(value, na.rm = TRUE),
    max_value    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(measure, status)
saveRDS(blenders_full_summary, "Cleaned Data& Code/Summary Data/blenders_full_summary.rds")



blendz_2 = blenderz_long %>% group_by(date, hour, slot_30min, measure, status) %>% 
  summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(start, end, measure, dur_secs)

blenders_summary = blenderz_long %>% group_by(measure, date, hour, min, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n(), avg_value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  arrange(start, end)

blenders_summary_seconds = blenderz_long %>% group_by(measure, value, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(start, end)



saveRDS(blendz_2, paste(getwd(),"/Cleaned Data& Code/", "blender_interruptions.rds", sep = ""))

saveRDS(blenderz_summary_seconds, paste(getwd(),"/Cleaned Data& Code/", "blenders_sec_summary.rds", sep = ""))

# Loss-In-Weight Feeders Data ----

feeders = feeders %>% separate_wider_delim(timestamp, delim = " ", names = c("day", "time"), too_few = "debug")
feeders = feeders %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))


feederz_long = feeders %>% select(timestamp,date, hour, min, sec, c(feed_factor_pd1:totalizer_pd7)) %>%
  pivot_longer(cols = c(feed_factor_pd1:totalizer_pd7), names_to = "measure", values_to = "value") %>%
  arrange(measure, timestamp) %>% mutate(status = if_else(value == 0, "interruption", "running")) %>% 
  mutate(slot_30sec = if_else(sec <= 29, 0, 30), slot_30min = if_else(min <= 29, 0, 30)) %>%
  group_by(measure) %>% mutate(status_chg = if_else(status == lag(status,1), 0, 1)) %>% 
  replace_na(list(status_chg = 0)) %>% mutate(seq = cumsum(status_chg)) %>% ungroup()


feedz_2 = feederz_long %>% group_by(date, hour, slot_30min, measure, status) %>% 
  summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(start, end, measure, dur_secs)



feeders_summary = feederz_long %>% group_by(measure, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n(), avg_value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  arrange(start, end)

### LiW Feeders Summary ----
feeders_full_summary <- feederz_long %>%
  group_by(measure, status) %>%
  summarise(
    n_obs        = n(),
    avg_value    = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value     = sd(value, na.rm = TRUE),
    min_value    = min(value, na.rm = TRUE),
    max_value    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(measure, status)

saveRDS(feeders_full_summary, "Cleaned Data& Code/Summary Data/feeders_full_summary.rds")


feeders_summary_seconds = feederz_long %>% group_by(measure, value, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(start, end)



hist(feedz_2 %>% filter(status == "interruption") %>% pull(dur_secs), breaks = 1000)

write_rds(feedz_2, paste(getwd(),"/Cleaned Data& Code/", "loss_in_weight_feeders_interruptions.rds", sep = ""))

write_rds(feeders_summary_seconds, paste(getwd(),"/Cleaned Data& Code/", "loss_in_weight_feeders_sec_summary.rds", sep = ""))
# Tablet Press ----
tablet_press_long = tablet_press %>% pivot_longer(cols = c(pre_compression_height_bottom:ejection_force_tablet), names_to = "measure", values_to = "value") %>%
  arrange(measure, timestamp) %>% mutate(status = if_else(value == 0, "interruption", "running")) %>% 
  mutate(slot_30sec = if_else(sec <= 29, 0, 30), slot_30min = if_else(min <= 29, 0, 30)) %>%
  group_by(measure) %>% mutate(status_chg = if_else(status == lag(status,1), 0, 1)) %>% 
  replace_na(list(status_chg = 0)) %>% mutate(seq = cumsum(status_chg)) %>% ungroup()

### Tablet Press Summary ----
press_full_summary <- tablet_press_long %>%
  group_by(measure, status) %>%
  summarise(
    n_obs        = n(),
    avg_value    = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value     = sd(value, na.rm = TRUE),
    min_value    = min(value, na.rm = TRUE),
    max_value    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(measure, status)

saveRDS(press_full_summary, "Cleaned Data& Code/Summary Data/press_full_summary.rds")

press_2 = tablet_press_long %>% group_by(date, hour, slot_30min, measure, status) %>% 
  summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(start, end, measure, dur_secs)


press_summary = tablet_press_long %>% group_by(measure, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n(), avg_value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  arrange(measure, seq, start)

press_summary_seconds = tablet_press_long %>% group_by(measure, value, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(measure, value, seq, start)

summary(tablet_press_long %>% filter(status == "interruption"))

hist(press_2 %>% filter(status == "interruption") %>% pull(dur_secs), breaks = 1000)

write_rds(press_2, paste(getwd(),"/Cleaned Data& Code/", "tablet_press_interruptions.rds", sep = ""))

write_rds(press_summary_seconds, paste(getwd(),"/Cleaned Data& Code/", "tablet_press_interruption_sec_summary.rds", sep = ""))


# Selected Data for Further Analysis ----
## MTTBI ----
feedz_2 = feedz_2 %>% select(-c(date))
blendz_2 = blendz_2 %>% select(-c(date))
press_2 = press_2 %>% select (-c(date))

mttbi = bind_rows(
  feedz_2,
  blendz_2,
  press_2
) %>%
  arrange(start, end)

## All Interruptions Summary ----
###  Second Level ----
all_interruptions_full = bind_rows(
  feeders_summary_seconds,
  blenders_summary_seconds,
  press_summary_seconds
) %>%
  arrange(start, end)
all_interruptions_full = all_interruptions_full %>% filter(!grepl("^index", measure))

### 30 Minute Interval ----
all_interruptions = bind_rows(
  feeders_summary,
  blenders_summary,
  press_summary
) %>%
  arrange(start, end)
all_interruptions = all_interruptions %>% filter(!grepl("^index", measure))

# Analysis ----
### Refill Activities ----
refills = feeders_summary %>%
  filter(grepl("^ref_act", measure), avg_value == 1)
refills_summary <- make_summary(refills, measure)
make_plots(refills, measure, "Refill Events")


## Overall number of interruptions ----


### LiW Feeders Interruptions Count ----
feeder_interruptions = feeders_summary %>% filter(status == "interruption")
feeder_interruptions_measure_count = feeder_interruptions %>% 
  group_by(start, end, ) %>% 
  summarise(n_measures =paste(measure, collapse = " ,"), n = n(), .groups = "drop") %>% 
  arrange(start)

feeder_interruptions_summary <- make_summary(feeder_interruptions, measure)
make_plots(feeder_interruptions, measure, "Feeder Interruptions")


saveRDS(feeder_interruptions_summary, "Cleaned Data& Code/Summary Data/feeder_interruptions_summary.rds")

### Blenders Interruptions  Instances Count ----
blender_interruptions = blenders_summary %>% filter(status == "interruption")

blender_interruptions_measure_count = blendz_2 %>% 
  group_by(start, end, ) %>% 
  summarise(n_measures =paste(measure, collapse = " ,"), n = n(), .groups = "drop") %>% 
  arrange(start)

blenders_interruptions_summary <- make_summary(blender_interruptions, measure)
saveRDS(blenders_interruptions_summary, "Cleaned Data& Code/Summary Data/blenders_interruptions_summary.rds")
make_plots(blender_interruptions, measure, "Blenders Interruption Events")
make_plots(blenders_summary, measure, "Blenders Events", by_status = TRUE)
### Tablet Press Interruptions Instances Count ----
press_interruptions <- press_summary %>% filter(status == "interruption")

press_interruptions_measure_count = press_interruptions %>% 
  group_by(start, end, ) %>% 
  summarise(n_measures =paste(measure, collapse = " ,"), n = n(), .groups = "drop") %>% 
  arrange(start)

press_interruptions_summary = make_summary(press_interruptions, measure)
saveRDS(press_interruptions_summary, "Cleaned Data& Code/Summary Data/press_interruptions_summary.rds")
make_plots(press_interruptions, measure, "Tablet Press Interruption Events")
make_plots(press_summary, measure , "Tablet Press Events", by_status = TRUE)

cat("Total Number of Interruption Instances:", nrow(feeder_interruptions_measure_count) + nrow(blender_interruptions_measure_count) + nrow(press_interruptions_measure_count))

### All Interruptions Instances Count ----
all_interruptions = bind_rows(
  feeder_interruptions,
  blender_interruptions,
  press_interruptions
) %>%
  arrange(start, end)

all_interruptions_summary = make_summary(all_interruptions, measure)
make_plots(all_interruptions, measure, "All Interruption Events")


# Would it make sense to drop all interruptions under 5 seconds?
all_interruptions %>% 
 filter(dur_secs <= 15) %>%
 ggplot(aes(x = dur_secs)) +
 geom_histogram(binwidth = 1, fill = "steelblue")

# 2. Empirical distribution of interruption duration
ggplot(all_interruptions, aes(x = dur_secs)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  geom_density(aes(y = after_stat(count)), color = "red") +
  labs(title = "Empirical Distribution of Interruption Duration",
       x = "Duration (seconds)", y = "Count")

## Warmup/Cool Down Event Study ----
### LiW Feeder Event Study ----

feeder_interruption_starts <- feederz_long %>%
  arrange(measure, timestamp) %>%
  group_by(measure) %>%
  mutate(
  prev_status = lag(status),
                                                                                                                is_start = status == "interruption" & (is.na(prev_status) | prev_status == "running")
                                                                                                                ) %>%
  filter(is_start) %>%
  select(measure, int_start = timestamp) %>%
  ungroup()

# Get all measures available
all_feeder_measures <- feeder_interruption_starts %>% 
  distinct(measure) %>% 
  pull(measure)

# Remove sample_events — use ALL interruption starts per measure
plot_feeder_measure_events <- function(m, window_sec = 120) {
  
  all_feeder_events <- feeder_interruption_starts %>% 
    filter(measure == m)
  
  feeder_event_data <- map_dfr(1:nrow(all_feeder_events), function(i) {
    t0 <- all_feeder_events$int_start[i]
    
    feederz_long %>%
      filter(
        measure   == m,
        timestamp >= t0 - window_sec,
        timestamp <= t0 + window_sec
      ) %>%
      mutate(
        t_rel   = as.numeric(difftime(timestamp, t0, units = "secs")),
        episode = i
      )
  })
  
  ggplot(feeder_event_data, aes(x = t_rel, y = value)) +
    geom_line(aes(group = episode),
              color = "steelblue", alpha = 0.15, linewidth = 0.3) +
    stat_summary(fun = mean, geom = "line",
                 color = "#154733", linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = window_sec,
             ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.05) +
    annotate("text", x = 5, y = Inf, label = "Interruption",
             color = "red", size = 3, vjust = 2, hjust = 0) +
    scale_x_continuous(breaks = seq(-window_sec, window_sec, by = 30)) +
    labs(
      title    = paste0(m, ": Signal Around Interruption Start"),
      subtitle = paste0("n = ", nrow(all_feeder_events), " episodes | Bold = average"),
      x        = "Seconds Relative to Interruption Start",
      y        = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8)
    )
}

# Generate one plot per measure
feeder_measure_plots <- map(all_feeder_measures, ~ plot_feeder_measure_events(.x, window_sec = 120))
names(feeder_measure_plots) <- all_feeder_measures

# View one at a time
walk(all_feeder_measures, ~ {
  print(feeder_measure_plots[[.x]])
  readline(prompt = paste("Showing", .x, "— press Enter for next..."))
})

valid_feeder_plots <- keep(feeder_measure_plots, ~ inherits(.x, "gg"))

pdf("Feeder_Event_Studies.pdf", width = 12, height = 6)
walk(valid_feeder_plots, print)
dev.off()



### Continuous Blender Event Study ----
blender_interruption_starts <- blenderz_long %>%
  arrange(measure, timestamp) %>%
  group_by(measure) %>%
  mutate(
    prev_status = lag(status),
    is_start = status == "interruption" & (is.na(prev_status) | prev_status == "running")
  ) %>%
  filter(is_start) %>%
  select(measure, int_start = timestamp) %>%
  ungroup()

# Get all measures available
all_blender_measures <- blender_interruption_starts %>% 
  distinct(measure) %>% 
  pull(measure)

# Remove sample_events — use ALL interruption starts per measure
plot_blender_measure_events <- function(m, window_sec = 15) {
  
  all_blender_events <- blender_interruption_starts %>% 
    filter(measure == m)
  
  blender_event_data <- map_dfr(1:nrow(all_blender_events), function(i) {
    t0 <- all_blender_events$int_start[i]
    
    blenderz_long %>%
      filter(
        measure   == m,
        timestamp >= t0 - window_sec,
        timestamp <= t0 + window_sec
      ) %>%
      mutate(
        t_rel   = as.numeric(difftime(timestamp, t0, units = "secs")),
        episode = i
      )
  })
  
  ggplot(blender_event_data, aes(x = t_rel, y = value)) +
    geom_line(aes(group = episode),
              color = "steelblue", alpha = 0.15, linewidth = 0.3) +
    stat_summary(fun = mean, geom = "line",
                 color = "#154733", linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = window_sec,
             ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.05) +
    annotate("text", x = 5, y = Inf, label = "Interruption",
             color = "red", size = 3, vjust = 2, hjust = 0) +
    scale_x_continuous(breaks = seq(-window_sec, window_sec, by = 5)) +
    labs(
      title    = paste0(m, ": Signal Around Interruption Start"),
      subtitle = paste0("n = ", nrow(all_blender_events), " episodes | Bold = average"),
      x        = "Seconds Relative to Interruption Start",
      y        = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8)
    )
}

# Generate one plot per measure
blender_measure_plots <- map(all_blender_measures, ~ plot_blender_measure_events(.x, window_sec = 15))
names(blender_measure_plots) <- all_blender_measures

# View one at a time
walk(all_blender_measures, ~ {
  print(blender_measure_plots[[.x]])
  readline(prompt = paste("Showing", .x, "— press Enter for next..."))
})

valid_blender_plots <- keep(blender_measure_plots, ~ inherits(.x, "gg"))

pdf("Blender_Event_Studies(15 Secs).pdf", width = 12, height = 6)
walk(valid_blender_plots, print)
dev.off()


### Tablet Press Event Study ----

tablet_press_interruption_starts <- tablet_press_long %>%
  arrange(measure, timestamp) %>%
  group_by(measure) %>%
  mutate(
    prev_status = lag(status),
    is_start = status == "interruption" & (is.na(prev_status) | prev_status == "running")
  ) %>%
  filter(is_start) %>%
  select(measure, int_start = timestamp) %>%
  ungroup()

# Get all measures available
all_tablet_press_measures <- tablet_press_interruption_starts %>% 
  distinct(measure) %>% 
  pull(measure)

# Remove sample_events — use ALL interruption starts per measure
plot_tablet_press_measure_events <- function(m, window_sec = 120) {
  
  all_tablet_press_events <- tablet_press_interruption_starts %>% 
    filter(measure == m)
  
  tablet_press_event_data <- map_dfr(1:nrow(all_tablet_press_events), function(i) {
    t0 <- all_tablet_press_events$int_start[i]
    
    tablet_press_long %>%
      filter(
        measure   == m,
        timestamp >= t0 - window_sec,
        timestamp <= t0 + window_sec
      ) %>%
      mutate(
        t_rel   = as.numeric(difftime(timestamp, t0, units = "secs")),
        episode = i
      )
  })
  
  ggplot(tablet_press_event_data, aes(x = t_rel, y = value)) +
    geom_line(aes(group = episode),
              color = "steelblue", alpha = 0.15, linewidth = 0.3) +
    stat_summary(fun = mean, geom = "line",
                 color = "#154733", linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = window_sec,
             ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.05) +
    annotate("text", x = 5, y = Inf, label = "Interruption",
             color = "red", size = 3, vjust = 2, hjust = 0) +
    scale_x_continuous(breaks = seq(-window_sec, window_sec, by = 30)) +
    labs(
      title    = paste0(m, ": Signal Around Interruption Start"),
      subtitle = paste0("n = ", nrow(all_tablet_press_events), " episodes | Bold = average"),
      x        = "Seconds Relative to Interruption Start",
      y        = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8)
    )
}

# Generate one plot per measure
tablet_press_measure_plots <- map(all_tablet_press_measures, ~ plot_tablet_press_measure_events(.x, window_sec = 120))
names(tablet_press_measure_plots) <- all_tablet_press_measures

# View one at a time
walk(all_tablet_press_measures, ~ {
  print(tablet_press_measure_plots[[.x]])
  readline(prompt = paste("Showing", .x, "— press Enter for next..."))
})

valid_tablet_press_plots <- keep(tablet_press_measure_plots, ~ inherits(.x, "gg"))

pdf("Tablet_Press_Event_Studies(2 Min).pdf", width = 12, height = 6)
walk(valid_tablet_press_plots, print)
dev.off()

### Environmental Conditions

env_conditions_summary = env_conditions %>%
  group_by(measure, machine) %>%
  summarise(
    n_obs        = n(),
    avg_value    = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value     = sd(value, na.rm = TRUE),
    min_value    = min(value, na.rm = TRUE),
    max_value    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(measure, machine)

### Environmental Conditions ----
# Separating timestamp into date and time

temp = liw_interruptions %>% separate_wider_delim(start, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-start_ok, -start_pieces, -start_remainder)





liw_interruptions = temp



feeder_conditions = env_conditions %>% filter(machine == "feeder")


env_hourly <- feeder_conditions %>%
  group_by(date, time, measure, machine) %>%
  summarise(avg_value = mean(value, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from  = measure,
              values_from = avg_value) %>%
  rename(avg_temperature = temperature,
         avg_humidity    = humidity)


## Contextual Quality Data ----
## Tablet Properties ----

tablet_quality_long = tablet_quality %>% pivot_longer(cols = c(avg_weight_mg:sd_hardness), names_to = "measure", values_to = "value") %>%
  arrange(timestamp, measure, value)
 
tablet_quality_summary = tablet_quality_long%>%
  group_by(measure) %>%
  summarise(
    n_obs        = n(),
    avg_value    = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value     = sd(value, na.rm = TRUE),
    min_value    = min(value, na.rm = TRUE),
    max_value    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(measure)

saveRDS(tablet_quality_summary, "tablet_quality_summary.rds")

tab_quality_measures <- unique(tablet_quality_long$measure)

plots <- map(tab_quality_measures, ~{
  tablet_quality_long %>%
    filter(measure == .x) %>%
    ggplot(aes(x = timestamp, y = value)) +
    geom_line(color = "#154733", linewidth = 0.6) +
    labs(title = .x, x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

tablet_plots = wrap_plots(plots, ncol = 2)

ggsave("tablet_quality_plots.png", plot = tablet_plots, width = 14, height = 18, dpi = 300)


## Content Uniformity ----
xbar_data <- content_uniformity %>%
  mutate(across(starts_with("Tablet"), as.numeric)) %>%
  rename(sample_mean = `Average Drug content (%)`) %>%
  rowwise() %>%
  mutate(sample_sd = sd(c_across(starts_with("Tablet")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sample_id = row_number())


grand_mean <- mean(xbar_data$sample_mean)
mean_sd    <- mean(xbar_data$sample_sd)
n          <- 10
ucl_x <- grand_mean + 3 * (mean_sd / sqrt(n))
lcl_x <- grand_mean - 3 * (mean_sd / sqrt(n))
ucl_s <- mean_sd * 1.716
lcl_s <- max(0, mean_sd * 0.284)

p1 <- ggplot(xbar_data, aes(x = sample_id, y = sample_mean)) +
  geom_line(color = "#154733", linewidth = 0.6) +
  geom_point(aes(color = sample_mean > ucl_x | sample_mean < lcl_x), size = 1.5) +
  scale_color_manual(values = c("FALSE" = "#154733", "TRUE" = "red")) +
  geom_hline(yintercept = grand_mean, color = "#FEE123", linetype = "dashed") +
  geom_hline(yintercept = ucl_x, color = "red", linetype = "dotted") +
  geom_hline(yintercept = lcl_x, color = "red", linetype = "dotted") +
  labs(title = "Xbar Chart — Average Drug Content", x = NULL, y = "Mean (%)") +
  theme_minimal() + theme(legend.position = "none")

p2 <- ggplot(xbar_data, aes(x = sample_id, y = sample_sd)) +
  geom_line(color = "#154733", linewidth = 0.6) +
  geom_point(aes(color = sample_sd > ucl_s), size = 1.5) +
  scale_color_manual(values = c("FALSE" = "#154733", "TRUE" = "red")) +
  geom_hline(yintercept = mean_sd, color = "#FEE123", linetype = "dashed") +
  geom_hline(yintercept = ucl_s, color = "red", linetype = "dotted") +
  geom_hline(yintercept = lcl_s, color = "red", linetype = "dotted") +
  labs(title = "S Chart — Within-Sample SD", x = "Sample (every 30 min)", y = "SD (%)") +
  theme_minimal() + theme(legend.position = "none")

p1 / p2

ggsave("Images/xbar_s_chart.png", plot = p1/p2, width = 14, height = 8, dpi = 300)

