
#--Task 1------------------------------------------
# Importing libraries needed for the analysis
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# ------Read GeoHealth Data and Addis ababa 2018 Excel files-----------

geohealth_data <- read_csv("GeoHealth_Data.csv")
addis_ababa_2018 <- read_csv("addis_ababa_2018_pm2.csv")

#---Task 2----Data Cleaning----------
#---------Hourly Agrigate_-------------
# Convert the 'Date (LT)' column to a proper date-time format
addis_ababa_2018 <- addis_ababa_2018 %>%
  mutate(`Date (LT)` = ymd_h(paste(Year, Month, Day, Hour, sep = "-")))

# Perform the hourly aggregation
addis_hourly_clean <- addis_ababa_2018 %>%
  mutate(hour = floor_date(`Date (LT)`, "Hour")) %>%
  group_by(hour) %>%
  summarise(ConcHR = mean(`Raw Conc.`, na.rm = TRUE))

# Convert the 'Time' column in geohealth_data to a proper date-time format with the year 2018
geohealth_data <- geohealth_data %>%
  mutate(Time = ymd_hms(paste("2018", Month, DD, hr, "00", "00", sep = "-")))

# Perform the hourly aggregation for geohealth_data
geohealth_hourly_clean <- geohealth_data %>%
  mutate(hour = floor_date(Time, "hour")) %>%
  group_by(hour) %>%
  summarise(ConcHR = mean(`ConcHR(ug/m3)`, na.rm = TRUE))

# --- Task 3: Daily Aggregation ---
# GeoHealth Data: Aggregating by day using geohealth_hourly_clean
geohealth_daily_clean <- geohealth_hourly_clean %>%
  mutate(date = as.Date(hour)) %>%
  filter(ConcHR <= 1000) %>%  # Remove extreme values
  group_by(date) %>%
  summarise(ConcHR = mean(ConcHR, na.rm = TRUE))  # Average hourly values for daily data

# Addis Ababa 2018 Data: Aggregating by day using addis_hourly_clean with proper filtering
addis_daily_clean <- addis_hourly_clean %>%
  mutate(date = as.Date(hour)) %>%
  filter(ConcHR >= 0 & ConcHR <= 1000) %>%  # Remove negative and extreme values
  group_by(date) %>%
  summarise(ConcHR = mean(ConcHR, na.rm = TRUE))  # Average hourly values for daily data

# --- Task 4: Plot Daily Line Charts Separately ---
# GeoHealth Daily Line Chart
ggplot(geohealth_daily_clean, aes(x = date, y = ConcHR)) +
  geom_line(color = "blue") +
  labs(title = "Daily PM2.5 Concentration - GeoHealth Data", 
       x = "Date", 
       y = "PM2.5 Concentration (ug/m3)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal()

# Addis Ababa Daily Line Chart
ggplot(addis_daily_clean, aes(x = date, y = ConcHR)) +
  geom_line(color = "red") +
  labs(title = "Daily PM2.5 Concentration - Addis Ababa 2018 Data", 
       x = "Date", 
       y = "PM2.5 Concentration (ug/m3)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal()

# --- Task 5: Combine GeoHealth and Addis Ababa Daily Data for Comparison ---
# Combine the datasets using the corrected daily averages
combined_daily_data <- bind_rows(
  mutate(geohealth_daily_clean, Source = "GeoHealth"),
  mutate(addis_daily_clean, Source = "Addis Ababa")
)

# --- Plot Both Datasets on the Same Graph ---
ggplot(combined_daily_data, aes(x = date, y = ConcHR, color = Source)) +
  geom_line(size = 1) +
  labs(title = "Daily PM2.5 Concentration: GeoHealth vs Addis Ababa", 
       x = "Date", 
       y = "PM2.5 Concentration (ug/m3)",
       color = "Data Source") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  theme(legend.position = "top")

# --- Task 6: Classify PM2.5 Levels Based on WHO Air Quality Guidelines ---

# Define a function to classify air quality based on PM2.5 levels
classify_pm25 <- function(conc) {
  if (conc <= 12) {
    return("Good")
  } else if (conc <= 35.4) {
    return("Moderate")
  } else if (conc <= 55.4) {
    return("Unhealthy for Sensitive Groups")
  } else if (conc <= 150.4) {
    return("Unhealthy")
  } else {
    return("Very Unhealthy")
  }
}

# Apply the classification to both datasets
geohealth_daily_clean <- geohealth_daily_clean %>%
  mutate(AirQuality = sapply(ConcHR, classify_pm25))

addis_daily_clean <- addis_daily_clean %>%
  mutate(AirQuality = sapply(ConcHR, classify_pm25))

# --- Task 7: Plot with PM2.5 Categories ---
# GeoHealth PM2.5 Levels with Classification
ggplot(geohealth_daily_clean, aes(x = date, y = ConcHR, color = AirQuality)) +
  geom_line(size = 1) +
  labs(title = "Daily PM2.5 Concentration - GeoHealth Data", 
       x = "Date", 
       y = "PM2.5 Concentration (ug/m3)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  scale_color_manual(values = c("Good" = "green", 
                                "Moderate" = "yellow", 
                                "Unhealthy for Sensitive Groups" = "orange", 
                                "Unhealthy" = "red", 
                                "Very Unhealthy" = "purple")) +
  theme(legend.position = "top")

# Addis Ababa PM2.5 Levels with Classification
ggplot(addis_daily_clean, aes(x = date, y = ConcHR, color = AirQuality)) +
  geom_line(size = 1) +
  labs(title = "Daily PM2.5 Concentration - Addis Ababa Data", 
       x = "Date", 
       y = "PM2.5 Concentration (ug/m3)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  scale_color_manual(values = c("Good" = "green", 
                                "Moderate" = "yellow", 
                                "Unhealthy for Sensitive Groups" = "orange", 
                                "Unhealthy" = "red", 
                                "Very Unhealthy" = "purple")) +
  theme(legend.position = "top")

#-------------PM2.5 Trend with Rolling Average-------------------------

library(zoo)
geohealth_daily_clean <- geohealth_daily_clean %>%
  mutate(Rolling_Avg = rollmean(ConcHR, k = 7, fill = NA, align = "right"))

addis_daily_clean <- addis_daily_clean %>%
  mutate(Rolling_Avg = rollmean(ConcHR, k = 7, fill = NA, align = "right"))

# Plot with rolling average
ggplot(geohealth_daily_clean, aes(x = date)) +
  geom_line(aes(y = ConcHR), color = "blue", alpha = 0.5) +
  geom_line(aes(y = Rolling_Avg), color = "darkblue") +
  labs(title = "PM2.5 Trend with Rolling Average - GeoHealth Data")

ggplot(addis_daily_clean, aes(x = date)) +
  geom_line(aes(y = ConcHR), color = "red", alpha = 0.5) +
  geom_line(aes(y = Rolling_Avg), color = "darkred") +
  labs(title = "PM2.5 Trend with Rolling Average - Addis Ababa Data")

#-------------------------------------------------------##################----------

t_test_result <- t.test(geohealth_daily_clean$ConcHR, addis_daily_clean$ConcHR)
print(t_test_result)

