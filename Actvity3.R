install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(ggplot2)
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
datCC <- read.csv("/cloud/project/activity03/climate-change.csv")

colnames(datCO2)[4] <- "CO2"
colnames(datCO2)[1] <- "Country"

plot(datCO2$Year, datCO2$CO2, type = "l", xlab = "Year", ylab = "CO2 Emissions")

#Prompt 1: Plot total all time emissions for the US, Mex, Can

colnames(datCC)[1] <- "Hemisphere"

datCC$bettertime<-ymd(datCC$Day, tz="EST")

NorthAnomaly <- datCC %>%
  filter(Hemisphere == "Northern Hemisphere")

SouthAnomaly <- datCC %>%
  filter(Hemisphere == "Southern Hemisphere")

#Base R

plot(NorthAnomaly$bettertime, NorthAnomaly$temperature_anomaly, type = "l", xlab = "Date", 
     ylab = "Temperature anomaly (°C)")

points(SouthAnomaly$bettertime, SouthAnomaly$temperature_anomaly, type = "l", col = 'red')

dat_hemispheres <- datCC %>%
  filter(Hemisphere == "Northern Hemisphere" | Hemisphere == "Southern Hemisphere")

#ggplot

ggplot(dat_hemispheres, aes(x=bettertime, y=temperature_anomaly, col=Hemisphere))+
  geom_line()+
  labs(x="Year", y= "Temperature anomaly (°C)")+
  theme_classic()

#Prompt 2: Plot total all time emissions for the US, Mex, Can

totalemissions <- NA_CO2 %>%
  group_by(Country) %>%
  summarize(TotalEmish = sum(CO2))

ggplot(totalemissions)+
  aes(x=Country, y=TotalEmish, col=Country)+
  geom_col()

NA_CO2 <- datCO2 %>%
  filter(Country== "United States" | Country == "Mexico" | Country == "Canada")

ggplot(NA_CO2, aes(x=Year, y=CO2, col=Country))+
  geom_line()+
  labs(x="Year", y= "CO2 emissions")+
  theme_classic()

