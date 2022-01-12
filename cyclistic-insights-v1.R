#Openning libraries that we'll work with

install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("readxl")
library(readxl)
install.packages("janitor")
library(janitor)

## Uploading monthly datasets

dec20 <- read_excel("Cyclistic-xls-files/202012-Cyclistic-tripdata.xlsx")
jan21 <- read_excel("Cyclistic-xls-files/202101-Cyclistic-tripdata.xlsx")
feb21 <- read_excel("Cyclistic-xls-files/202102-Cyclistic-tripdata.xlsx")
mar21 <- read_excel("Cyclistic-xls-files/202103-Cyclistic-tripdata.xlsx")
apr21 <- read_excel("Cyclistic-xls-files/202104-Cyclistic-tripdata.xlsx")
may21 <- read_excel("Cyclistic-xls-files/202105-Cyclistic-tripdata.xlsx")
jun21 <- read_excel("Cyclistic-xls-files/202106-Cyclistic-tripdata.xlsx")
jul21 <- read_excel("Cyclistic-xls-files/202107-Cyclistic-tripdata.xlsx")
ago21 <- read_excel("Cyclistic-xls-files/202108-Cyclistic-tripdata.xlsx")
sep21 <- read_excel("Cyclistic-xls-files/202109-Cyclistic-tripdata.xlsx")
oct21 <- read_excel("Cyclistic-xls-files/202110-Cyclistic-tripdata.xlsx")
nov21 <- read_excel("Cyclistic-xls-files/202111-Cyclistic-tripdata.xlsx")

##Putting all datasets in the same dataframe

bikeride <- rbind(dec20, jan21, feb21, mar21, apr21, may21, jun21, jul21,
                  ago21, sep21, oct21, nov21)

## Getting the columns that we need to make the analysis

bike_ride_time <- bikeride %>% 
  select(started_at, ended_at, rideable_type, member_casual)

## Cleaning completely empty rows and columns

bike_ride_time <- janitor::remove_empty(bike_ride_time, which = c("cols"))
bike_ride_time <- janitor::remove_empty(bike_ride_time, which = c("rows"))

## Converting a string to format date and time

bike_ride_time$started_at <- ymd_hms(bike_ride_time$started_at)
bike_ride_time$ended_at <- ymd_hms(bike_ride_time$ended_at)

## Creating a "minutes" column

bike_ride_time$minutes <- difftime(bike_ride_time$ended_at, 
                                   bike_ride_time$started_at,
                                   units = "mins")

## Creating "weekday" column

bike_ride_time$weekday <- weekdays(bike_ride_time$started_at)

## Filttering data only showning >0 minutes

bike_ride_p0 <- bike_ride_time %>% 
  filter(minutes>0 & minutes<200)

## Calculating minutes mean and median of member_casual groups

bike_ride_p0 %>% 
  group_by(member_casual) %>% 
  summarize(média=round(mean(minutes), 0),
            mediana=round(median(minutes),0))

## Plotting duration of rented bikes

ggplot(data=bike_ride_p0)+
  geom_histogram(bins=60, mapping=aes(x=minutes), color="darkblue", 
                 fill="lightblue")+
  xlim(0, 100)+
  facet_wrap(~member_casual)+
  labs(title="Duração de Uso", 
       subtitle="Frequência de duração de uso das bikes, em minutos")

## Plotting weekday distribution

ggplot(data=bike_ride_p0)+
  geom_bar(mapping=aes(x=weekday, fill=member_casual), 
           stat = "count", position = "dodge")+
  xlab("Dias da Semana")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday", "Thursday", "Friday",
                            "Saturday", "Sunday"))
labs(title="Distribuição do uso durante a semana",
     subtitle= "Frequência de uso durante os dias da semana, por tipo de usuário")

## Plotting bikeable type distribution

ggplot(data=bike_ride_p0)+
  geom_bar(mapping = aes(x=rideable_type, fill=member_casual))+
  labs(title = "Uso por tipo de bicicleta",
       subtitle = "O tipo de bicicleta usada, por cada tipo de usuário")+
  xlab("Tipo de bicicleta")
