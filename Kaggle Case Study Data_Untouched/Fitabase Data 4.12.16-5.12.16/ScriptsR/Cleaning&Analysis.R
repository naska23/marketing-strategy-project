install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)

# Everytime I import a file from my PC into R with read.csv the date datatype gets converted to String automatically, so I have to add a datatype convert line to get the date in the correct datatype each time. 
DailyActivity <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/dailyActivity_merged_Ready.csv")
DailyActivity$ActivityDate <- as.Date(DailyActivity$ActivityDate)
str(DailyActivity)
is.na(DailyActivity)
sum(is.na(DailyActivity))
colnames(DailyActivity)[2] ="Date"
summary(DailyActivity)
mean(DailyActivity$Calories)
sd(DailyActivity$Calories)
user_ids <- c(1503960366, 1844505072, 2347167796, 3977333714, 4319703577, 5553957443, 6290855005, 6962181067, 7007744171, 8253242879, 8583815059, 8792009665)
PotentialOutliersActivity <- DailyActivity[DailyActivity$Id %in% user_ids, ]
DailyActivity <- subset(DailyActivity, Calories != 0)
values_to_remove <- c(52, 57, 120, 257, 403, 4900)
DailyActivity <- subset(DailyActivity, !(Calories %in% values_to_remove))

DailyActivity %>%
  select(TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>%
  summary()

ggplot(data=DailyActivity, aes(x=ActivityDay, y=Calories)) + 
  geom_point()

qqnorm(DailyActivity$Calories)
qqline(DailyActivity$Calories, col = 15)

DailyCalories <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/dailyCalories_merged_Ready.csv")
DailyCalories$ActivityDay <- as.Date(DailyCalories$ActivityDay)
str(DailyCalories)
sum(is.na(DailyCalories))
summary(DailyCalories)
colSums(DailyCalories < 1000)
PotentialOutliersDailyCalories <- DailyCalories[DailyCalories$Calories < 1000, ]

# I checked the data and there appear to be potential outliers, since some values under calories are 0 which is not possible, even if having a sedentary lifestyle the BMR is still included in the measurement. BMR is calculated based on height, weight, age, gender. We do not have any info about those measurements, so we cannot say for sure which values are 

DailyIntensities <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/dailyIntensities_merged_Ready.csv")
DailyIntensities$ActivityDay <- as.Date(DailyIntensities$ActivityDay)
str(DailyIntensities)
sum(is.na(DailyIntensities))
summary(DailyIntensities)

DailySteps <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/dailySteps_merged_Ready.csv")
DailySteps$ActivityDay <- as.Date(DailySteps$ActivityDay)
str(DailySteps)
sum(is.na(DailySteps))
summary(DailySteps)

#=================================== END of importing/cleaning of data =====================================================

DailyActivity %>%
  select(TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()

# From a first glance at the data, we can see that the average day of steps per day are 7638. 
## On average 21.16 minutes in a day were spent doing Very active activity, 13.56 minutes of Fairly active activity and 192.8 minutes of lightly active minutes. Most of the active time was spent doing light activity.
### Participants were sedentary for the majority of their day (991.2 min = 16.52 h)

#================================= END summary of first part ===============================================================

Heartrate <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/heartrate_seconds_merged_Ready.csv")
Heartrate$Date <- as.Date(Heartrate$Date)
Heartrate$Time <- format(Heartrate$Time, format = "%H:%M:%S")
str(Heartrate)
sum(is.na(Heartrate))
summary(Heartrate)

# The average heartrate of all participants in one month was 77.33.

HourlyCalories <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/hourlyCalories_merged_Ready.csv")
HourlyCalories$Date <- as.Date(HourlyCalories$Date)
HourlyCalories$Time <- format(HourlyCalories$Time, format = "%H:%M:%S")
str(HourlyCalories)
sum(is.na(HourlyCalories))
summary(HourlyCalories)


# The average calories consumed per hour are 97.39 kcal ( I checked the calories a fitbit measures and they are in kcal)

MinuteSleep <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/minuteSleep_merged_Ready.csv")
MinuteSleep$Date <- as.Date(MinuteSleep$Date)
MinuteSleep$Time <- format(MinuteSleep$Time, format = "%H:%M:%S")
str(MinuteSleep)
sum(is.na(MinuteSleep))
glimpse(MinuteSleep)
MinuteSleep %>%
  count(value)
  

# FitBit measures the sleep which is divided into 3 sleep states: asleep (1), restless (2) and awake (3).
## In our participants: 1 = 172480; 2 = 14023; 3 = 2018. 
## Total entries: 188521 ; our participants spend 91.49% of their sleep sleeping, 7.44% of their sleep was restless, and 1.07% was spent awake. 
  
DailySleep <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/sleepDay_merged_Ready.csv")
DailySleep$Date <- as.Date(DailySleep$Date)
str(DailySleep)
sum(is.na(DailySleep))
glimpse(DailySleep)
summary(DailySleep)

qqnorm(DailySleep$TotalMinutesAsleep)
qqline(DailySleep$TotalMinutesAsleep, col = 5)

ggplot(data=DailySleep, aes(x=TotalMinutesAsleep)) + 
  geom_()

hist(DailySleep$TotalMinutesAsleep,
     xlab = "TotalMinutesAsleep", 
     main = "Histogram of Total Minutes Asleep", 
     breaks = sqrt(nrow(DailySleep)))

boxplot(DailySleep$TotalMinutesAsleep,
        ylab = "TotalMinutesAsleep"
        )

boxplot(DailyActivity$Calories,
        ylab = "Calories", col="#20C4BC"
)
boxplot.stats(DailyActivity$Calories)$out


#=========================================================================================================

HourlyIntensities <- read.csv("/Users/oblj-nkvarantan/Documents/CASE STUDY /Kaggle Case Study Data_Untouched/Fitabase Data 4.12.16-5.12.16/Ready Data Frames/hourlyIntensities_merged_Ready.csv")
HourlyIntensities$Date <- as.Date(HourlyIntensities$Date)
str(HourlyIntensities)
sum(is.na(HourlyIntensities))
glimpse(HourlyIntensities)
summary(HourlyIntensities)

HourlyIntensities %>%
  select(TotalIntensity, AverageIntensity) %>%
  summary()

Average_Intensity_Hour <- HourlyIntensities %>%
  group_by(Time) %>%
  summarise(AverageIntensity = mean(AverageIntensity))

ggplot(data=Average_Intensity_Hour) +
  geom_col(mapping = aes(x=Time, y=AverageIntensity, fill=AverageIntensity)) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Average intensity per hour")

# We can see that between 17:00 and 19:00 there is a spike in average activity intensity.
##Average intensity state exhibited during that hour (TotalIntensity for that ActivityHour divided by 60). 
#===============================================================================================================

ggplot(data=DailySleep) +
  geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) 

ActivityxSleep <- merge(DailyActivity, DailySleep, by=c('Id', 'Date'))
head(ActivityxSleep)

#Scatter plot SedentaryMinutes vs TotalMinutes Asleep and linear regression line fitted to show trend.The graph shows that on average that on the day the participants were more sedentary, their sleep was shorter.  
ggplot(data=ActivityxSleep, aes(x=TotalMinutesAsleep , y=SedentaryMinutes))+
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title="Sedentary lifestyle vs. lenght of sleep")

ggplot(data=ActivityxSleep, aes(x=TotalMinutesAsleep , y=VeryActiveMinutes))+
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title="Steps vs. lenght of sleep")

install.packages("tidyr")
library(tidyr)

ActivityType <- DailyActivity[c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes")]

column_averages <- colMeans(DailyActivity[c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes")], na.rm = TRUE)

averages_df <- data.frame(ActivityType = names(column_averages), AverageMinutes = column_averages)

ggplot(data=averages_df) + 
  geom_bar(mapping= aes(x=ActivityType, weight=AverageMinutes))

#=============================================================================================================================

DailyActivity2 <- DailyActivity %>%
  mutate(
  ActivityLevelSummary = factor (case_when(
   SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
   SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
   SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
   SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active",
   SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "ERROR",
 ),levels=c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")), Calories, Id) %>%
  drop_na()

sum_sedentary <- sum(DailyActivity$SedentaryMinutes)
sum_lightly <- sum(DailyActivity$LightlyActiveMinutes)
sum_fairly <- sum(DailyActivity$FairlyActiveMinutes)
sum_very <- sum(DailyActivity$VeryActiveMinutes)
total <- sum(DailyActivity$SedentaryMinutes)+sum(DailyActivity$LightlyActiveMinutes)+sum(DailyActivity$FairlyActiveMinutes)+sum(DailyActivity$VeryActiveMinutes)

col1 <- c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")
col2 <- c(sum_sedentary/total*100, sum_lightly/total*100, sum_fairly/total*100, sum_very/total*100)

ActivityLevel <- data.frame(Activity_Level = col1, Percentage = col2)
ActivityLevel$Activity_Level <- factor(ActivityLevel$Activity_Level, levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"))

ggplot(data=ActivityLevel, aes(x= Activity_Level, y = Percentage, fill = Activity_Level))+
  geom_col()+
  geom_text(aes(label =sprintf("%.1f%%", Percentage)), vjust = -0.5, color = "black")+
  scale_fill_manual(values = user_type_colors) +
  theme(legend.position = "none")+
  labs(title= "Activity Level", x = "", y= "Percentage")

mean_sedentary <- mean(DailyActivity$SedentaryMinutes)
mean_lightly <- mean(DailyActivity$LightlyActiveMinutes)
mean_fairly <- mean(DailyActivity$FairlyActiveMinutes)
mean_very <- mean(DailyActivity$VeryActiveMinutes)
DailyActivity$ActivityLevelSummary = NA
for(i in 1:nrow(DailyActivity)) {       # for-loop over rows

  if(DailyActivity[i,11] > mean_very) {
    DailyActivity[i,16 ] <- "Very Active"
  }
  else if(DailyActivity[i,12] > mean_fairly ) {
    DailyActivity[i,16 ] <- "Fairly Active"
  }
  else if(DailyActivity[i,13] > mean_lightly ) {
    DailyActivity[i,16 ] <- "Lightly Active"
  }
  else if(DailyActivity[i,14]> mean_sedentary) {
    DailyActivity[i,16 ] <- "Sedentary"
  } 
  else {
    DailyActivity[i,16 ] <- "ERROR"
    }
}



11 - very
12 - fairly
13 - light
14- sed

user_type_colors <- c("Sedentary" = "#B32832", "Lightly Active" = "#F59932", "Fairly Active" = "#34D6C2", "Very Active" = "#63DE77")

data_by_activity_level %>%
  group_by(Activity_Level) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(Activity_Level) %>%
  summarise(total_percent = total/totals) %>%
  ggplot(aes(Activity_Level, y=total_percent, fill=Activity_Level))+
  geom_col()+
  geom_text(aes(label = scales :: percent(total_percent)), position = position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = user_type_colors)+
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")+
  labs(title="Distribution by Level of Activity", x= "", y = "Percentage")

ggplot(data=data_by_activity_level, aes(x= Activity_Level, y = Calories, fill = Activity_Level))+
  geom_boxplot()+
  scale_fill_manual(values = user_type_colors) +
  labs(title= "Activity Level vs. Calories Burned", x = "", y= "Calories Burned")

activity_counts <- table(data_by_activity_level$Activity_Level)

# Print the counts
print(activity_counts)
#================================================================================================================================
install.packages("Matrix")

# Load the updated Matrix package
library(Matrix)

# Now, try loading ggpmisc again
install.packages("ggpmisc")
library(ggpmisc)

HourlyIntensitiesxHourlyCalories <- merge(HourlyIntensities, HourlyCalories, by=c('Id', 'Date', 'Time'))
head(HourlyIntensitiesxHourlyCalories)

summary(HourlyIntensitiesxHourlyCalories)

ggplot(data=HourlyIntensitiesxHourlyCalories, aes(x= AverageIntensity, y = Calories))+
  geom_point(color="#4a7384")+
  geom_smooth(method = "lm")+
  labs(title= "Average Hourly Intensity vs. Calories Burned", x = "Average Intensity", y= "Calories Burned")+
  annotate("text", x=2,y=875,label="r = 0.90", color="#4a7384", fontface="bold", size=4.5)

correlation <- cor(HourlyIntensitiesxHourlyCalories$AverageIntensity, HourlyIntensitiesxHourlyCalories$Calories)
print(correlation)
#================================================================================================================================

DailyActivity %>%
  select(TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>%
  summary()

#Divide in 3 categories  
StepCategory <- DailyActivity %>%
  mutate(
  Step_Category = case_when(
    TotalSteps < 5000 ~ "< 5k Steps", 
    TotalSteps >= 5000 & TotalSteps < 10000 ~ "5k - 10k Steps", 
    TotalSteps >= 10000 ~ "> 10k Steps")
  ) %>%
  select(Id, Step_Category) %>%
  count(Step_Category) %>%
  group_by(Step_Category) %>%
  arrange(desc(n))
print(StepCategory)

colors_step_cat_3 <- c("< 5k Steps" = "#ecaa27", "5k - 10k Steps" = "#abefe3", "> 10k Steps" = "#c5d3a9")


ggplot(data = StepCategory, aes(x = Step_Category, y = n, fill = Step_Category)) +
  geom_col() +
  scale_fill_manual(values = colors_step_cat_3) +
  labs(title = "Step Count by Category", y = "", x = "")

StepCategory %>%
  group_by(Step_Category) %>%
  summarise(total = n) %>%
  mutate(totals = sum(total)) %>%
  group_by(Step_Category) %>%
  summarise(total_percent = total/totals) %>%
  ggplot(aes(Step_Category, y=total_percent, fill=Step_Category))+
  geom_col()+
  geom_text(aes(label = scales :: percent(total_percent)), position = position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = colors_step_cat_3)+
  scale_y_continuous(labels = scales::percent) +
  labs(title="Daily % of steps per category", x="", y = "Percentage") + 
  theme_minimal()

## Divide in 5 categories 
StepCategory_4 <- DailyActivity %>%
  mutate(
    DailyActivityLevel = case_when(
      TotalSteps < 4000 ~ "Sedentary", 
      TotalSteps >= 4000 & TotalSteps < 8000 ~ "Lightly Active", 
      TotalSteps >= 8000 & TotalSteps < 12000 ~ "Fairly Active", 
      TotalSteps >= 12000 ~ "Very Active")
  ) %>%
  select(Id, DailyActivityLevel) %>%
  count(DailyActivityLevel) %>%
  group_by(DailyActivityLevel) %>%
  arrange(desc(n))
print(StepCategory_4)


StepCategory %>%
  count(Step_Category) %>%
  group_by(Step_Category)

colors_step_cat_5 <- c("< 2k Steps" = "#00204A", "2k - 5k Steps" = "#00599A", "5k - 10k Steps" = "#006CB5", "10k - 15k Steps" = "#004580", "> 15k Steps" = "#003366")

colors_step_cat_5 <- c("< 2k Steps" = "#235676", "2k - 5k Steps" = "#c3dac0", "5k - 10k Steps" = "#ead8d2", "10k - 15k Steps" = "#9bbda2", "> 15k Steps" = "#4a7384")

ggplot(data=StepCategory_5) +
  geom_col(mapping = aes(x=Step_Category_5, y = n)) +
  scale_fill_manual(values = colors_step_cat_5) +
  labs(title = "Step Count by Category", y = "", x = "")

#GPT code

ggplot(data = StepCategory_5, aes(x = Step_Category_5, y = n, fill = Step_Category_5)) +
  geom_col() +
  scale_fill_manual(values = colors_step_cat_5) +
  labs(title = "Step Count by Category", y = "", x = "")

StepCategory_4$DailyActivityLevel <- factor(StepCategory_4$DailyActivityLevel, levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"))


StepCategory_4 %>%
  group_by(DailyActivityLevel) %>%
  summarise(total = n) %>%
  mutate(totals = sum(total)) %>%
  group_by(DailyActivityLevel) %>%
  summarise(total_percent = total/totals) %>%
  ggplot(aes(DailyActivityLevel, y=total_percent, fill=DailyActivityLevel))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = scales :: percent(total_percent)), position = position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = colors_step_cat_4)+
  scale_y_continuous(labels = scales::percent) +
  labs(title="Daily Percentage of Steps per Category", x="", y= "Percentage")

color1 <- "#e55d87"
color2 <- "#5fc3e4"

gradient_palette <- colorRampPalette(c(color1, color2))

colors <- gradient_palette(10)

print(colors)

colors_step_cat_4 <- c("Sedentary" = "#BE93C5", "Lightly Active" = "#A7A3C7", "Fairly Active" = "#91B4C9", "Very Active" = "#7BC6CC")

"#E55D87" "#D66891" "#C7739B" "#B87EA6" "#A98AB0" "#9A95BA" "#8BA1C5" "#7CACCF" "#6DB7D9" "#5FC3E4"


ggplot(data=StepCategory_5, aes(x= Activity_Level, y = Calories, fill = Activity_Level))+
  geom_boxplot()+
  scale_fill_manual(values = user_type_colors) +
  labs(title= "Activity Level vs. Calories Burned", x = "", y= "Calories Burned")

#======================================== STEP COUNT BASED ON PERCENTILES ======================================================

q1 <- quantile(DailyActivity$TotalSteps, 0.25) # 25th percentile
q2 <- quantile(DailyActivity$TotalSteps, 0.5) # 50th percentile (median)
q3 <- quantile(DailyActivity$TotalSteps, 0.75) # 75th percentile

DailyActivity$StepCategory <- cut(
  DailyActivity$TotalSteps, 
  breaks = c(-Inf, q1, q2, q3, Inf), 
  labels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"), 
  include.lowest = TRUE
)

STEPS <- data.frame(DailyActivity$StepCategory)

Step_category_counts <- table(STEPS$DailyActivity.StepCategory)

category_percentages <- prop.table(Step_category_counts) * 100

Steps_categories <- data.frame(
  StepCategory = names(Step_category_counts), 
  Count = as.numeric(Step_category_counts), 
  Percentage = category_percentages
  )

print(Steps_categories)

ggplot(Steps_categories, aes(x=StepCategory, y=Percentage.Freq, fill=Steps_categories))+
  geom_col()+
  geom_text(aes(label = scales :: percent(Percentage.Freq)), position = position_stack(vjust = 0.5), color = "black")+
  #scale_fill_manual(values = colors_step_cat_5)+
  scale_y_continuous(labels = scales::percent) +
  labs(title="Daily % of steps per category", x="", y= "Percentage")

ggplot(Steps_categories, aes(x = StepCategory, y = Percentage.Freq, fill = StepCategory)) + 
  geom_col() 

#===============================================================================================================================

ggplot(ActivityxSleep, aes(x=TotalMinutesAsleep, y=Calories,)) +
  geom_point(color="#79A8CB") +
  geom_smooth(method = "lm")
  
  
  stat_smooth(method = "lm", formula = x ~ y, se = FALSE)+
  geom_text(aes(label = paste("R² =", round(summary(lm(Calories ~ TotalMinutesAsleep, data = ActivityxSleep))$r.squared, 2)), x = max(TotalMinutesAsleep), y = max(Calories), hjust = 1, vjust = 1)) +
  labs(title="Calories vs. Sleep")

  ##
  boxplot(ActivityxSleep$Calories,
        ylab = "Calories", col="#20C4BC"
)
boxplot.stats(ActivityxSleep$Calories)$out

##===============================================================================================================================

data_by_user_type_sleep <- ActivityxSleep %>%
  reframe(
    user_type = factor (case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active",
    ),levels=c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")), TotalMinutesAsleep, .group=Id) %>%
  drop_na()

install.packages("colorspace")
library(colorspace)

color1 <- "#be93c5"
color2 <- "#7bc6cc"

gradient_palette <- colorRampPalette(c(color1, color2))

colors <- gradient_palette(4)

print(colors)

user_type_colors <- c("Sedentary" = "#BE93C5", "Lightly Active" = "#A7A3C7", "Fairly Active" = "#91B4C9", "Very Active" = "#7BC6CC")


ggplot(data_by_user_type_sleep, aes(x = user_type , y = TotalMinutesAsleep, fill=user_type)) +
  geom_boxplot() +
  scale_fill_manual(values = user_type_colors) +
  labs(title = "Activity Type vs. Sleep Quality",
       x = "Activity Type",
       y = "Sleep Quality")

##=============================================================================================================================

color1 <- "#be93c5"
color2 <- "#7bc6cc"

gradient_palette <- colorRampPalette(c(color1, color2))

colors <- gradient_palette(7)

print(colors)

#===============================================================================================================================

DailyIntensities%>%
  colnames(DailyIntensities)[2] ="Date" 

DailyIntensities <- mutate(DailyIntensities, DayOfWeek = weekdays(Date))

weekly_summary_intensities <- DailyIntensities %>%
  group_by(DayOfWeek) %>%
  summarise(AvgSedentaryMinutes = mean(SedentaryMinutes), AvgLightlyActiveMinutes = mean(LightlyActiveMinutes), AvgFairlyActiveMinutes = mean(FairlyActiveMinutes), AvgVeryActiveMinutes = mean(VeryActiveMinutes))
 

weekly_summary$SleepComparison <- ifelse(weekly_summary$AvgMinutesAsleep > mean(DailySleep$TotalMinutesAsleep), "More Sleep", "Less Sleep")

weekly_summary_intensities$DayOfWeek <- factor(weekly_summary$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(weekly_summary_intensities, aes(x=DayOfWeek, y=AvgSedentaryMinutes, fill= DayOfWeek)) +
  geom_histogram()

ggplot(weekly_summary_intensities, aes(x=DayOfWeek, y=AvgVeryActiveMinutes, fill= DayOfWeek)) +
  geom_col()


 
plot1 <- ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgSedentaryMinutes, fill = AvgSedentaryMinutes)) +
  geom_col(fill="#BE93C5") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Sedentary Minutes", x="", y="Minutes")

plot2 <- ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgLightlyActiveMinutes, fill = AvgLightlyActiveMinutes)) +
  geom_col(fill = "#A7A3C7") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Lightly Active Minutes", x="", y="Minutes")

plot3 <- ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgFairlyActiveMinutes, fill = AvgFairlyActiveMinutes)) +
  geom_col(fill="#91B4C9") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Fairly Active Minutes",  x="", y="Minutes")

plot4 <- ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgVeryActiveMinutes, fill = AvgVeryActiveMinutes)) +
  geom_col(fill="#7BC6CC") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Very Active Minutes",  x="", y="Minutes")

# Arrange plots side by side
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

#=========================================== HOURLY INTENSITIES PER WEEK =======================================================

HourlyIntensities <- mutate(HourlyIntensities, DayOfWeek = weekdays(Date))

HourlyIntensities%>%
  glimpse()

weekly_summary_intensities <- HourlyIntensities %>%
  group_by(DayOfWeek) %>%
  summarise(AvgTotalIntensity = mean(TotalIntensity), AvgIntensity = mean(AverageIntensity)) %>%
  arrange(desc(AvgIntensity))

weekly_summary_intensities$DayOfWeek <- factor(weekly_summary$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

week_day_colors_7 <- c("Monday" = "#42047E", "Tuesday" = "#382C83", "Wednesday" = "#2E5488", "Thursday" = "#247C8E", "Friday" = "#1AA393", "Saturday" ="#10CB98" , "Sunday" = "#07F49E")

ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgTotalIntensity, fill = DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Total Intensity per week" ,  x="", y="Total Intensity")


ggplot(weekly_summary_intensities, aes(x = DayOfWeek, y = AvgIntensity, fill = DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  labs(title = "Average Intensity Per Week" ,  x="", y="Average Intensity")
 



color1 <- "#42047e" 
color2 <- "#07f49e"

gradient_palette <- colorRampPalette(c(color1, color2))

colors <- gradient_palette(7)

print(colors)

#================================================== AVERAGE INTENSITY BETWEEN 17-19 per week =============================================

HourlyIntensities$Time <- as.POSIXct(HourlyIntensities$Time, format = "%H:%M:%S")

AvgIntensity17to19 <- HourlyIntensities %>%
  filter(hour(Time) >= 17, hour(Time) <= 19) %>%
  group_by(DayOfWeek) %>%
  summarise(AverageIntensityBetween17to19 = mean(AverageIntensity, na.rm = TRUE))

AvgIntensity17to19$DayOfWeek <- factor(AvgIntensity17to19$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(AvgIntensity17to19, aes(x = DayOfWeek, y = AverageIntensityBetween17to19, fill = DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Total Intensity per week" ,  x="", y="Total Intensity")

# =========================================== TOTAL INTENSITY BETWEEN 17 - 19 per week ============================================

HourlyIntensities$Time <- as.POSIXct(HourlyIntensities$Time, format = "%H:%M:%S")

TotalIntensity17to19 <- HourlyIntensities %>%
  filter(hour(Time) >= 17, hour(Time) <= 19) %>%
  group_by(DayOfWeek) %>%
  summarise(AverageIntensityBetween17to19 = mean(TotalIntensity, na.rm = TRUE))

TotalIntensity17to19$DayOfWeek <- factor(TotalIntensity17to19$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(TotalIntensity17to19, aes(x = DayOfWeek, y = AverageIntensityBetween17to19, fill = DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Total Intensity per week" ,  x="", y="Total Intensity")

# ======================================== HOURLY INTENSITY PER DAY OF WEEK ====================================================
summary(HourlyIntensities)
HourlyIntensities$DayOfWeek <- factor(HourlyIntensities$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

plots <- HourlyIntensities %>%
  ggplot(aes(x = hour(Time), y = AverageIntensity, group = DayOfWeek, fill= DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  facet_wrap(~ DayOfWeek, scales = 'free_y') +
  theme(legend.position = "none" ) +
  labs(title = "Hourly Average Intensity for Each Day of the Week",
       x = "Hour of the Day",
       y = "Average Intensity") +
  theme_minimal()

plots <- plots +
  scale_y_continuous(limits = c(0, 60))

print(plots)

plots <- HourlyIntensities %>%
  ggplot(aes(x = hour(Time), y = TotalIntensity, group = DayOfWeek, fill= factor(DayOfWeek))) +
  geom_col() +
  scale_fill_manual(values = week_day_colors_7) +
  facet_wrap(~ DayOfWeek, scales = 'free_y') +
  theme(legend.position = "none") +
  labs(title = "Hourly Average Intensity for Each Day of the Week",
       x = "Hour of the Day",
       y = "Average Intensity") +
  theme_minimal()

print(plots)

#========================================================SLEEP ===================================================================
DailySleep <- mutate(DailySleep, DayOfWeek = weekdays(Date))

sleep_weekly_summary <- DailySleep %>%
  group_by(DayOfWeek) %>%
  summarise(AvgMinutesAsleep = mean(TotalMinutesAsleep), AvgTimeInBed = mean(TotalTimeInBed)) %>%
  arrange(desc(AvgMinutesAsleep))

sleep_weekly_summary$SleepComparison <- ifelse(weekly_summary$AvgMinutesAsleep > mean(DailySleep$TotalMinutesAsleep), "More Sleep", "Less Sleep")

sleep_weekly_summary$DayOfWeek <- factor(sleep_weekly_summary$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

week_day_colors <- c("Monday" = "#BE93C5", "Tuesday" = "#B29BC6", "Wednesday" = "#A7A3C7", "Thursday" = "#9CACC8", "Friday" = "#91B4C9", "Saturday" ="#86BDCA" , "Sunday" = "#7BC6CC")

ggplot(data = sleep_weekly_summary, aes(x = DayOfWeek , y = AvgMinutesAsleep, fill = DayOfWeek)) +
  geom_col() +
  scale_fill_manual(values = week_day_colors) +
  labs(title = "Average sleep based on day of the week", y = "", x = "")

ActivityxSleep_NZV_lightly <- ActivityxSleep %>% # NZV = no zero values for the lightly active minutes
  filter(LightlyActiveMinutes != 0)

ActivityxSleep_NZV_fairly <- ActivityxSleep %>% # NZV = no zero values for the fairly active minutes
  filter(FairlyActiveMinutes != 0)

ActivityxSleep_NZV_very <- ActivityxSleep %>% # NZV = no zero values for the very active minutes
  filter(VeryActiveMinutes != 0)

Sedentary_plot <- ggplot(ActivityxSleep, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point(color ="#42047E") +
  scale_fill_manual(values = "#42047E") +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(
    x = "Total Minutes Asleep",
    y = "Sedentary Minutes"
  ) +
  theme_minimal()

Lightly_active_plot <- ggplot(ActivityxSleep_NZV_lightly, aes(x = TotalMinutesAsleep, y = LightlyActiveMinutes)) +
  geom_point(color = "#07F49E") +
  scale_fill_manual(values= "#07F49E") +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(
    x = "Total Minutes Asleep",
    y = "Lightly Active Minutes"
  ) +
  theme_minimal()

Fairly_active_plot <- ggplot(ActivityxSleep_NZV_fairly, aes(x = TotalMinutesAsleep, y = FairlyActiveMinutes)) +
  geom_point(color = "#247C8E") +
  scale_fill_manual(values = "#247C8E" ) +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(
    x = "Total Minutes Asleep",
    y = "Fairly Active Minutes"
  ) +
  theme_minimal()

Very_active_plot <- ggplot(ActivityxSleep_NZV_very, aes(x = TotalMinutesAsleep, y = VeryActiveMinutes)) +
  geom_point(color = "#10CB98") +
  scale_fill_manual(values = "#10CB98") +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(
    x = "Total Minutes Asleep",
    y = "Very Active Minutes"
  ) +
  theme_minimal()

grid.arrange(Sedentary_plot, Lightly_active_plot, Fairly_active_plot, Very_active_plot, ncol = 2)

"#42047E" "#382C83" "#2E5488" "#247C8E" "#1AA393" "#10CB98" "#07F49E"
#================================================================================================================================

DailyActivity2 <- DailyActivity %>%
  mutate(
  DailyActivityLevel = case_when(
    TotalSteps < 4000 ~ "Sedentary", 
    TotalSteps >= 4000 & TotalSteps < 8000 ~ "Lightly Active", 
    TotalSteps >= 8000 & TotalSteps < 12000 ~ "Fairly Active", 
    TotalSteps >= 12000 ~ "Very Active"
    )
  )

DailyActivity2$DailyActivityLevel <- factor(DailyActivity2$DailyActivityLevel, levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"))

ggplot(data=DailyActivity2, aes(x = DailyActivityLevel, y = Calories, fill = DailyActivityLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = daily_activity_colors) +
  theme(legend.position = "none") +
  labs(title = "Daily Activity Level vs. Calories Burned", x = "Daily Activity Level", y= "Calories Burned")

daily_activity_colors <- c("Sedentary" = "#BE93C5", "Lightly Active" = "#A7A3C7", "Fairly Active" = "#91B4C9", "Very Active" = "#7BC6CC")

#=======================================================HEARTRATE=================================================================

Heartrate <- mutate(Heartrate, Time = as.POSIXct(Time, format = "%H:%M:%S"))

Heartrate$DateTime <- as.POSIXct(paste(Heartrate$Date, Heartrate$Time), format = "%Y-%m-%d %H:%M:%S")

Heartrate$Time <- as.ITime(Heartrate$Time)

str(Heartrate)

HourlyHeartrate <- Heartrate %>%
  mutate(Time = round_date(Time, "hour")) %>%
  group_by(Id, Time) %>%
  summarize(Heartrate = mean(Value)) %>%
  ungroup()

Heartrate <- mutate(Heartrate, DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))

# Create a heatmap
heatmap_plot <- Heartrate %>%
  mutate(Hour = format(DateTime, "%H"), Minute = format(DateTime, "%M")) %>%
  group_by(Hour, Minute) %>%
  summarize(AverageHeartRate = mean(Value)) %>%
  ggplot(aes(x = Hour, y = Minute, fill = AverageHeartRate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heart Rate", x = "Hour", y = "Minute", fill = "Average Heart Rate") +
  theme_minimal()

# Print the heatmap
print(heatmap_plot)

Heartrate %>%
  mutate((DateTime), 
         Hour = format(DateTime, "%H")) %>%
  group_by(Hour) %>%
  summarize(AverageHeartRate = mean(Value))


