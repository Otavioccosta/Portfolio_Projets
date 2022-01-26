## Openning Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
install.packages("gridExtra")
library(gridExtra)

## Openning Datasets to work with
daily_act <- read_csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/dailyActivity_merged.csv")
daily_sleep <- read.csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/sleepDay_merged.csv")
daily_calories <- read_csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/dailyCalories_merged.csv")
daily_intensities <- read_csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/dailyIntensities_merged.csv")
daily_steps <- read_csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/dailySteps_merged.csv")
weight_info <- read_csv(
  "Case Study - BellaBeat/Fitabase-Data-csv/weightLogInfo_merged.csv")

##Checking if daily_act has the same Calories data than Calories_df
daily_act_1 <- daily_act %>% 
  select(Id, ActivityDate, Calories)
head(daily_act_1)

daily_calories_1 <- daily_calories %>% 
  select(Id, ActivityDay, Calories)
head(daily_calories_1)

##Checked. They are the same, we can use daily_activity for exploration.

##Checking if daily_act has the same intensities data than Intensities_df
daily_act_2 <- daily_act %>% 
  select(Id, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes)
head(daily_act_2)

daily_intensities_1 <- daily_intensities %>% 
  select(Id, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes)
head(daily_intensities_1)

##Checked. They are the same, we can use daily_activity for exploration.

##Checking if daily_act has the same steps data than Steps_df
daily_act_3 <- daily_act %>% 
  select(Id, ActivityDate, TotalSteps)
head(daily_act_3)

daily_steps_1 <- daily_steps %>% 
  select(Id, ActivityDay, StepTotal)
head(daily_steps_1)

##Checked. They are the same, we can use daily_activity for exploration.

## Using summary to understanding burned calories distribution
daily_act %>% 
  select(TotalSteps, SedentaryMinutes, Calories) %>% 
  summary()

## Using summary to understanding activity distribution
daily_act %>% 
  select(VeryActiveMinutes, FairlyActiveMinutes,
         LightlyActiveMinutes) %>% 
  summary()

## Using summary to understanding sleep time distribution
daily_sleep %>% 
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

## Using summary to understanding weight distribution
weight_info %>% 
  select(WeightKg, BMI) %>% 
  summary()

## Plotting burned calories vs activity level
plot1 <- ggplot(data=daily_act, aes(x=LightlyActiveMinutes, y=Calories)) + 
  geom_point() + geom_smooth() +
  labs(title="Min. Atividade Leve vs. Calorias")+
  xlim(0, 150)
plot2 <- ggplot(data=daily_act, aes(x=VeryActiveMinutes, y=Calories)) + 
  geom_point() + geom_smooth() +
  labs(title="Min. Atividade Intensa vs. Calorias")+
  xlim(0, 150)
grid.arrange(plot1, plot2, ncol=2)

## Plotting burned calories vs total steps
ggplot(data=daily_act, aes(x=TotalSteps, y=Calories))+
  geom_point() + geom_smooth(color="red",fill="green")+
  labs(title="Quantidade de passos vs. Calorias",
       subtitle="A influência da quantidade da passos, no gasto calórico")
  xlim(0, 30000)

## Merging data to use 2 columns from differents dataframes
daily_df <- merge(daily_sleep, daily_act, by = c("Id", "Date"))
  
## Plotting Sedentary Minutes vs Minutes Asleep
ggplot(data=daily_df, aes(x=SedentaryMinutes, y=TotalMinutesAsleep)) +
  geom_point() + geom_smooth(fill="red")+
  xlim(400, 1100)+
  labs(title = "Tempo Dormindo vs. Sedentarismo",
       subtitle = "Tendência de tempo dormindo em relação ao tempo sedentário")
