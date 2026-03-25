
# Clean Data in the Work space

rm(list = ls())

# Setting Working Directory for everyone
setwd(file.path(Sys.getenv("USERPROFILE"), "Dropbox", "Interruption Management"))

# Attaching Required Packages

source(paste(getwd(),"/Cleaned Data& Code/","ipak.R",sep = ""))

#### Blenders Data ####

file = "Blenders.csv"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

blndrs = read_csv(loc) %>% as_tibble()

rm(file, loc)

blndrs = blndrs %>% clean_names()

# Separating timestamp into date and time

temp = blndrs %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(day,hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, massflow_blender_1:oos_concentration_at_blender_2_inlet) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(massflow_blender_1:oos_concentration_at_blender_2_inlet), .fns = ~round(.x, 5)))

blndrsc = temp

write_rds(blndrsc, paste(getwd(),"/Cleaned Data& Code/", "blenders.rds", sep = ""))

rm(temp, temp1, temp2, blndrs, blndrsc)


#### LIW Feeder 1 Data ####

file = "LiW Feeders 1.csv"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

fdr1 = read_csv(loc) %>% as_tibble()

rm(file, loc)

fdr1 = fdr1 %>% clean_names()
#### LIW Feeder 2 Data ####

file = "LiW Feeders 2.csv"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

fdr2 = read_csv(loc) %>% as_tibble()
fdr2 = fdr2 %>% clean_names()

rm(file, loc)

liw_feeders = bind_cols ( fdr1, fdr2)
# Separating timestamp into date and time

temp = liw_feeders %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, feed_factor_pd1:totalizer_pd7) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(feed_factor_pd1:totalizer_pd7), .fns = ~round(.x, 5)))

liw_feeders = temp
write_rds(liw_feeders, paste(getwd(),"/Cleaned Data& Code/", "liw_feeders.rds", sep = ""))

rm(temp, temp1, temp2)


#### LIW Feeder 2 Data ####

#file = "LiW Feeders 2.csv"
#
#loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")
#
#fdr2 = read_csv(loc) %>% as_tibble()
#
#rm(file, loc)
# Cleaning the Names
#fdr2 = fdr2 %>% clean_names()
# Abnormal Records on row 73014 and 353384 : Dropping these two records
#fdr2 <- fdr2 %>% 
#  filter(!row_number() %in% c(73014, 353384))

# Saving FDR2
#write_rds(fdr2, paste(getwd(),"/Cleaned Data& Code/", "fdr2.rds", sep = ""))

# Left Join instead since there is no timestamp in FDR2
#liw_feeders = bind_cols(fdr1, fdr2)

# Separating timestamp into date and time
#temp = fdr2 %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")



#temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

#temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  #mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
#temp %>% count(var1)
#temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
#temp %>% count(day)
#
# Correcting month column
#temp %>% count(var2)
#temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
#temp %>% count(month)
#
# Renaming var3 to year
#temp = temp %>% rename(year = var3)

# Creating cleaned date column
#temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

#temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  #mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

#temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

#temp1 = temp %>% filter(is.na(sec))
#temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
#temp1 = temp1 %>% filter(sec <= 59)

#temp2 = temp %>% drop_na(sec)

#temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
#temp = temp %>% select(timestamp, date, hour, min, sec, percent_pd4:totalizer_pd7) %>% arrange(timestamp)

#temp = temp %>% mutate(across(.cols = c(percent_pd4:totalizer_pd7), .fns = ~round(.x, 5)))

#fdr2 = temp

#rm(fdr1, fdr2)


#### Tablet Press Data ####

file = "Tablet Press.csv"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

tablet_press = read_csv(loc) %>% as_tibble()

rm(file, loc)

tablet_press = tablet_press %>% clean_names()

# Separating timestamp into date and time

temp = tablet_press %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, pre_compression_height_bottom:ejection_force_tablet) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(pre_compression_height_bottom:ejection_force_tablet), .fns = ~round(.x, 5)))

tablet_press = temp

write_rds(tablet_press, paste(getwd(),"/Cleaned Data& Code/", "tablet_press.rds", sep = ""))

rm(temp, temp1, temp2, tablet_press)



##### Temperature Data ####

file = "Temperature.csv"
file = "temperature.rds"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

temperature = read_csv(loc) %>% as_tibble()

rm(file, loc)

temperature = temperature %>% clean_names()

# Fix in the humidity bind_rows — handle mixed formats
temperature <- bind_rows(
  temperature %>%
    rename(timestamp_feeder = feeder, feeder_temp = x3) %>%
    select(timestamp = timestamp_feeder, humidity = feeder_temp) %>%
    mutate(
      # Try mdy first, if NA try dmy
      timestamp = case_when(
        !is.na(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")) ~
          as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC"),
        TRUE ~
          as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")
      ),
      machine = "feeder"
    ),
  
  temperature %>%
    rename(timestamp_press = tablet_press, press_temp = x6) %>%
    select(timestamp = timestamp_press, humidity = press_temp) %>%
    mutate(
      timestamp = case_when(
        !is.na(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")) ~
          as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC"),
        TRUE ~
          as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")
      ),
      machine = "press"
    )
) %>%
  arrange(timestamp)






# Separating timestamp into date and time

temp = temperature %>% separate_wider_delim(timestamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-timestamp_ok, -timestamp_pieces, -timestamp_remainder)





temperature = temp

saveRDS(temperature, paste(getwd(),"/Cleaned Data& Code/", "temperature.rds", sep = ""))

rm(temp, temperature)


##### Humidity Data ####

file = "Humidity.csv"
file = "humidity.rds"

loc = paste(getwd(),"/Cleaned Data& Code/Machine data/", file, sep = "")

humidity = read_csv(loc) %>% as_tibble()

humidity = humidity %>% clean_names()

# Fix in the humidity bind_rows — handle mixed formats
humidity <- bind_rows(
  humidity %>%
    rename(timestamp_feeder = feeder, feeder_hum = x3) %>%
    select(timestamp = timestamp_feeder, humidity = feeder_hum) %>%
    mutate(
      # Try mdy first, if NA try dmy
      timestamp = case_when(
        !is.na(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")) ~
          as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC"),
        TRUE ~
          as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")
      ),
      machine = "feeder"
    ),
  
  humidity %>%
    rename(timestamp_press = tablet_press, press_hum = x6) %>%
    select(timestamp = timestamp_press, humidity = press_hum) %>%
    mutate(
      timestamp = case_when(
        !is.na(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")) ~
          as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC"),
        TRUE ~
          as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")
      ),
      machine = "press"
    )
) %>%
  arrange(timestamp)


rm(file, loc)


## Separating timestamp into date and time

temp = humidity %>% separate_wider_delim(timestamp, delim = " ", names = c("date", "time"), too_few = "debug")

## Removing irrelevant columns

temp = temp %>% select(-timestamp_ok, -timestamp_pieces, -timestamp_remainder)


humidity = temp

saveRDS(humidity, paste(getwd(),"/Cleaned Data& Code/", "humidity.rds", sep = ""))

rm(temp, humidity)



### Combined Weather Data ----
# Dropping NA's created from 0000 hours or midnight

humidity = humidity %>% filter(!is.na(timestamp))
temperature = temperature %>% filter(!is.na(timestamp))

env_conditions <- bind_rows(
  temperature %>%
    select(timestamp, value = temperature, machine) %>%
    mutate(measure = "temperature",
           value   = as.numeric(value)),
  
  humidity %>%
    select(timestamp, value = humidity, machine) %>%
    mutate(measure = "humidity",
           value   = as.numeric(value))
) %>%
  filter(!is.na(timestamp), !is.na(value)) %>%
  arrange(timestamp)

nrow(env_conditions)
unique(as.Date(env_conditions$timestamp))


# Separating timestamp into date and time

temp = env_conditions %>% separate_wider_delim(timestamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-timestamp_ok, -timestamp_pieces, -timestamp_remainder)





env_conditions = temp

env_conditions = env_conditions %>% 
  mutate(
    hour = lubridate::hour(timestamp)
  )

saveRDS(env_conditions, paste(getwd(),"/Cleaned Data& Code/", "environment_conditions.rds", sep = ""))





# Contextual Quality Data ----

## Tablet Quality Properties ----
file = "RM Tablet Properties and Drum Change.xlsx"


loc = paste(getwd(),"/Cleaned Data& Code/Contextual quality data/", file, sep = "")



tablet_quality <- read_excel(loc, sheet = "Tablet properties", skip = 1,
                             col_types = c("date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric")) %>%
  as_tibble() %>%
  rename(timestamp = 1)

# Separating timestamp into date and time

temp = tablet_quality %>% separate_wider_delim(timestamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-timestamp_ok, -timestamp_pieces, -timestamp_remainder)

tablet_quality = temp
tablet_quality = tablet_quality %>% drop_na(timestamp)
tablet_quality = tablet_quality %>% clean_names()
saveRDS(tablet_quality, paste(getwd(),"/Cleaned Data& Code/", "tablet_quality.rds", sep = ""))



## Content Uniformity ----
file = "RM Content Uniformity.xlsx"


loc = paste(getwd(),"/Cleaned Data& Code/Contextual quality data/", file, sep = "")



content_uniformity <- read_excel(loc, sheet = "Content Uniformity", col_types = c("date","numeric", rep("numeric", 11))) %>% as_tibble() %>% rename(timesamp = 1)

content_uniformity = content_uniformity %>%
  rename(timestamp = timesamp)
# Separating timestamp into date and time

temp = content_uniformity%>% separate_wider_delim(timestamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-timestamp_ok, -timestamp_pieces, -timestamp_remainder)

content_uniformity = temp

saveRDS(content_uniformity, paste(getwd(),"/Cleaned Data& Code/", "content_uniformity.rds", sep = ""))
