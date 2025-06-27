install.packages("dplyr")
library(dplyr)

# ================================ BEGIN PREPARATION OF hourlyCalories_merged.csv FILE ==================================

df1 <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")

df1$ActivityHour <- as.POSIXct(df1$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

df3 <- df1 %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, Calories)

write.csv(df3, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF hourlyCalories_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF hourlyIntensities_merged.csv FILE ==================================

Intensities <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
Intensities$ActivityHour <- as.POSIXct(Intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

Intensities <- Intensities %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, TotalIntensity, AverageIntensity)

write.csv(Intensities, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF hourlyIntensities_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF hourlySteps_merged.csv FILE ==================================
Steps <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

Steps$ActivityHour <- as.POSIXct(Steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

Steps <- Steps %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, StepTotal)

write.csv(Steps, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF hourlySteps_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF heartrate_seconds_merged.csv FILE ==================================

HeartRate <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

HeartRate$Time <- as.POSIXct(HeartRate$Time, format = "%m/%d/%Y %I:%M:%S %p")

HeartRate <- HeartRate %>%
  mutate(
    Date = as.Date(Time),
    Time = format(Time, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, Value)

write.csv(HeartRate, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF heartrate_seconds_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF minuteCaloriesNarrow_merged.csv FILE ==================================

Calories_Min <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")

Calories_Min$ActivityMinute <- as.POSIXct(Calories_Min$ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p")

Calories_Min <- Calories_Min %>%
  mutate(
    Date = as.Date(ActivityMinute),
    Time = format(ActivityMinute, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, Calories)

write.csv(Calories_Min, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF minuteCaloriesNarrow_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF minuteCaloriesWide_merged.csv FILE ==================================

Calories_Hour2 <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged.csv")

Calories_Hour2$ActivityHour <- as.POSIXct(Calories_Hour2$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

Calories_Hour2 <- Calories_Hour2 %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, matches("Calories\\d{2}"))

write.csv(Calories_Hour2, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged_Ready.csv", row.names = FALSE)

## The dataest here is named minuteCaloriesWide_merged.csv but the data of time is in hours.

# ================================= END PREPARATION OF minuteCaloriesWide_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF minuteIntensitiesNarrow_merged.csv FILE ==================================

Intensities_Min <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")

Intensities_Min$ActivityMinute <- as.POSIXct(Intensities_Min$ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p")

Intensities_Min <- Intensities_Min %>%
  mutate(
    Date = as.Date(ActivityMinute),
    Time = format(ActivityMinute, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, Intensity)

write.csv(Intensities_Min, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged_Ready.csv", row.names = FALSE)

# ================================= END PREPARATION OF minuteIntensitiesNarrow_merged.csv FILE ===================================

# ================================ BEGIN PREPARATION OF minuteIntensitiesWide_merged.csv FILE ==================================

Intensities_Hour <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv")

Intensities_Hour$ActivityHour <- as.POSIXct(Intensities_Hour$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

Intensities_Hour <- Intensities_Hour %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M")
  ) %>%
  select(Id, Date, Time, matches("Intensity\\d{2}"))

write.csv(Intensities_Hour, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged_Ready.csv", row.names = FALSE)

## The dataset here is named minuteIntensityWide_merged.csv but the data of time is in hours.

# ================================ END PREPARATION OF minuteIntensitiesWide_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF minuteMETsNarrow_merged.csv FILE ==================================

MET <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")

MET$ActivityMinute <- as.POSIXct(MET$ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p")

MET <- MET %>%
  mutate(
    Date = as.Date(ActivityMinute),
    Time = format(ActivityMinute, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, METs)

write.csv(MET, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF minuteMETsNarrow_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF minuteSleep_merged.csv FILE ==================================

Sleep_Min <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")

Sleep_Min$date <- as.POSIXct(Sleep_Min$date, format = "%m/%d/%Y %I:%M:%S %p")

Sleep_Min <- Sleep_Min %>%
  mutate(
    Date = as.Date(date),
    Time = format(date, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, value, logId)

write.csv(Sleep_Min, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF minuteSleep_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF minuteStepsNarrow_merged.csv FILE ==================================

Steps_Min <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")

Steps_Min$ActivityMinute <- as.POSIXct(Steps_Min$ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p")

Steps_Min <- Steps_Min %>%
  mutate(
    Date = as.Date(ActivityMinute),
    Time = format(ActivityMinute, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, Steps)

write.csv(Steps_Min, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF minuteStepsNarrow_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF minuteStepsWide_merged.csv FILE ==================================

Steps_Hour <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteStepsWide_merged.csv")

Steps_Hour$ActivityHour <- as.POSIXct(Steps_Hour$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

Steps_Hour <- Steps_Hour %>%
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, matches("Steps\\d{2}"))

write.csv(Steps_Hour, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/minuteStepsWide_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF minuteStepsWide_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF sleepDay_merged.csv FILE ==================================

Sleep_Day <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

Sleep_Day$SleepDay <- as.POSIXct(Sleep_Day$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")

Sleep_Day <- Sleep_Day %>%
  mutate(
    Date = as.Date(SleepDay),
    Time = format(SleepDay, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed)

write.csv(Sleep_Day, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/sleepDay_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF sleepDay_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF weightLogInfo_merged.csv FILE ==================================

Weight <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

Weight$Date <- as.POSIXct(Weight$Date, format = "%m/%d/%Y %I:%M:%S %p")

Weight <- Weight %>%
  mutate(
    Date = as.Date(Date),
    Time = format(Date, format = "%H:%M:%S")
  ) %>%
  select(Id, Date, Time, WeightKg, WeightPounds, Fat, BMI, IsManualReport, LogId)

write.csv(Weight, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF weightLogInfo_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF dailyActivity_merged.csv FILE ==================================

Activity <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

Activity$ActivityDate <- as.POSIXct(Activity$ActivityDate, format = "%m/%d/%Y")

write.csv(Activity, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF dailyActivity_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF dailyCalories_merged.csv FILE ==================================

Calories <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

Calories$ActivityDay <- as.POSIXct(Calories$ActivityDay, format = "%m/%d/%Y")

write.csv(Calories, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF dailyCalories_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF dailyIntensities_merged.csv FILE ==================================

Intensities_Day <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

Intensities_Day$ActivityDay <- as.POSIXct(Intensities_Day$ActivityDay, format = "%m/%d/%Y")

write.csv(Intensities_Day, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF dailyIntensities_merged.csv FILE ==================================

# ================================ BEGIN PREPARATION OF dailySteps_merged.csv FILE ==================================

Steps_Day <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

Steps_Day$ActivityDay <- as.POSIXct(Steps_Day$ActivityDay, format = "%m/%d/%Y")

write.csv(Steps_Day, "/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/dailySteps_merged_Ready.csv", row.names = FALSE)

# ================================ END PREPARATION OF dailySteps_merged.csv FILE ==================================