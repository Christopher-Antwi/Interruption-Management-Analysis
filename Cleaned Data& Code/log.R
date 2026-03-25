
# Clean Data in the Work space

rm(list = ls())

# Attaching Required Packages

source(paste(getwd(),"/Code/","ipak.R",sep = ""))

#### Log Data ####

file = "Log.xlsx"

loc = paste(getwd(),"/Data/", file, sep = "")

log = read_excel(loc, sheet = "Failure Log", col_names = TRUE) %>% as_tibble()

log = log %>% select(index, day, time, operations:word_count)

rm(file, loc)

# Separating time into hour, min and sec

temp = log %>% separate_wider_delim(time, delim = ":", names = c("hour", "mins", "sec"), too_few = "debug") %>%
  select(-sec, -time_ok, -time_pieces, -time_remainder) %>% mutate(across(.cols = c(hour, mins), .fns = ~as.numeric(.x))) %>% 
  mutate(id = 1:n(), mins_l = hour * 60 + mins)

intrvl_mins = 60

temp = temp %>% mutate(counter = intrvl_mins) %>% uncount(weights = counter)

temp = temp %>% group_by(id) %>% mutate(mins_l = mins_l + seq(from = -intrvl_mins/2, to = (intrvl_mins/2) - 1, by = 1)) %>% ungroup()

temp = temp %>% mutate(hour = floor(mins_l/intrvl_mins), mins = mins_l %% intrvl_mins) %>% select(id, index, day, time, hour, mins, operations:word_count)

log = temp

rm(temp, intrvl_mins)

write_rds(log, paste(getwd(),"/Data/", "log.rds", sep = ""))