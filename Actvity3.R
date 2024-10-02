install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(ggplot2)
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
datCC <- read.csv("/cloud/project/activity03/climate-change.csv")

colnames(datCO2)[4] <- "CO2"
colnames(datCO2)[1] <- "Country"

plot(datCO2$Year, datCO2$CO2, type = "l", xlab = "Year", ylab = "CO2 Emissions")

#Prompt 1: Plot air temp anomalies by hemisphere

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

NA_CO2 <- datCO2 %>%
  filter(Country== "United States" | 
           Country == "Mexico" | Country == "Canada")

totalemissions <- NA_CO2 %>%
  group_by(Country) %>%
  summarize(TotalEmish = sum(CO2))

ggplot(totalemissions)+
  aes(x=Country, y=TotalEmish, fill=Country)+
  geom_col()+
  labs(x="Country", y="Total All Time CO₂ Emissions (tons)")

#Question 1: Make a graph that communicates about emissions of any countries

Scandinavia_CO2 <- datCO2 %>%
  filter(Country== "Norway" | 
           Country == "Sweden" | Country == "Finland" | 
           Country == "Denmark")

ggplot(Scandinavia_CO2, aes(x=Year, y=CO2, col=Country))+
  geom_line()+
  labs(x="Year", y= "CO₂ emissions (tons)")+
  theme_classic()

#Question 2: Make one graph for world air temp and one for CO2 emissions

#Air Temp

dat_world <- datCC %>%
  filter(Hemisphere == "World")

ggplot(dat_world, aes(x=bettertime, y=temperature_anomaly))+
  geom_line()+
  labs(x="Year", y= "Temperature anomaly (°C)")+
  theme_classic()

#CO2

World_CO2 <- datCO2 %>%
  filter(Country == "World")

ggplot(World_CO2, aes(x=Year, y=CO2))+
  geom_line()+
  labs(x="Year", y= "CO₂ emissions (tons)")+
  theme_classic()

#Question 3: Download your own environmental data and create a visualization

Brazil_Burned <- annual_burned_area_by_landcover %>%
  filter(Entity == "Brazil")

Brazil_Burned$Annual_Total <- 
  rowSums(Brazil_Burned[, c("Yearly burned area across other land categories", 
                            "Yearly burned area across croplands", 
                            "Yearly burned area across forests",
                            "Yearly burned area across savannas",
                            "Yearly burned area across shrublands and grasslands") ])

ggplot(Brazil_Burned, aes(x=Year, y=Annual_Total, col='red'))+
  geom_line()+
  labs(x="Year", y= "Annual Area Burned by Wildfires (ha)")+
  theme_classic()+
  theme(legend.position="none")

