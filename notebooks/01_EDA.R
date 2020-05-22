# install.packages(c("vroom", "tidyverse"))
# installed.packages("ggthemes")
library(vroom)
library(dplyr)
library(tidyverse)
library(ggplot2)

path = "/Users/kkalyan/github/airQo/data/"
col_list_train <- list(location=col_factor(), target=col_double(), ID=col_factor())
col_list_test <- list(location=col_factor(), ID=col_factor())

raw_train <- vroom(paste0(path, "Train.csv"), delim = ",", col_types=col_list_train)
raw_tests <- vroom(paste0(path, "Test.csv"), delim = ",", col_types=col_list_test)

# precip
precip <- unlist(str_split(raw_tests$precip[10], ","))
plot(precip)

# rel_humidity
rel_humidity <- unlist(str_split(raw_tests$rel_humidity[10], ","))
plot(rel_humidity)

# wind_dir
wind_dir <- unlist(str_split(raw_tests$wind_dir[10], ","))
plot(wind_dir)

# wind_spd
wind_spd <- unlist(str_split(raw_tests$wind_spd[10], ","))
plot(wind_spd)

# atmos_press
atmos_press <- unlist(str_split(raw_tests$atmos_press[10], ","))
plot(atmos_press)

# Average by location
library(tibble)
options(pillar.sigfig = 10)
avg_by_location <- raw_train %>% group_by(location) %>% summarise(avg=mean(target))
avg_by_location

# Plot (Location, Target)
p <- ggplot(raw_train, aes(location, target)) 
p + geom_boxplot()

# Plot (Location)
p1 <- ggplot(raw_train) + geom_bar(aes(location))
p1

# Plot (Location)
p2 <- ggplot(raw_train, aes(target, x= ""))
p2 + geom_boxplot()


# Submit mean for different locaitons
submit <- raw_tests %>% select(c("ID", "location")) %>% 
  mutate(target = case_when(location == "A"  ~ 57.65,
                            location == "B"  ~ 49.86,
                            location == "C"  ~ 97.77,
                            location == "D"  ~ 58.15,
                            location == "E"  ~ 37.80,
                            TRUE ~ 0)) %>% 
  select(c("ID", "target"))

write_path = "/Users/kkalyan/github/airQo/submissions/"
write.csv(submit, paste0(write_path,"r-baseline-mean-all.csv"), row.names = FALSE)
# Remove NAs
# Create Featuers
# Plot
