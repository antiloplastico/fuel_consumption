library(tidyverse)
library(ggplot2)
library(corrplot)
library(modelr)
rethink = read_csv("rethink.csv") #read the database
speed = read_csv("19842021_guzzled.csv") #read the database

speed = speed %>% filter(hpv > 0)

View (speed)

rethink = rethink %>% filter(Displ < 70 & MPG <100) #let's tide it up

View (rethink)

ggplot(rethink %>% filter(Fuel == "Tier 2 Cert Gasoline"), aes(x = 1/MPG, y = CO2)) +
  geom_point(alpha = 0.1) +
  geom_smooth()


cor(1/(rethink %>% filter (MPG > 0 & CO2 > 0 & Fuel == "Tier 2 Cert Gasoline"))$MPG, (rethink %>% filter (MPG > 0 & CO2 > 0 & Fuel == "Tier 2 Cert Gasoline"))$CO2)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline" )), aes(x = HP, y = MPG)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #Good approx: HP = Displacement (Liters)* Cylinders * 10


ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ < 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #No turbo, function is apparently logarithmic

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ > 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #Turbo only, function is initially logarithmic and after exponential (but just one)


######################TESTS#####################



ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ < 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)


ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1)), aes(x = HP, y = Displ)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 )), aes(x = HP, y = MPG)) +
  geom_point(alpha = 0.15) +
  geom_smooth(se=FALSE)

cor((rethink %>% filter(Displ < 70 & Displ > 0.1 & HP < 200 & HP > 80 & Cylinders > 0))$Displ/
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & HP < 200 & HP > 80 & Cylinders > 0))$Cylinders,
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & HP < 200 & HP > 80 & Cylinders > 0))$HP/
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & HP < 200 & HP > 80 & Cylinders > 0))$Displ
)

ggplot((rethink %>% filter(Test == "FTP")), aes(x = CO, y = CO2)) +
  geom_point(alpha = 0.2) +
  xlim(0, 1) +
  ylim(0, 1000) +
  geom_smooth(se=FALSE)

#ggplot ((rethink %>% group_by(Fuel)

ggplot((rethink), aes(x = Fuel=="Electricity", y = CO2)) +
  geom_boxplot()

cor((rethink %>% filter(Displ < 70 & Displ > 0.1))$Displ/Cylinders, (rethink %>% filter(Displ < 70 & Displ > 0.1))$HP/Displ)

cor((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0))$HP/
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0))$Displ,
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0))$Displ/
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0))$Cylinders)


cor((rethink %>% filter (MPG > 0 & CO2 > 0))$MPG, (rethink %>% filter (MPG > 0 & CO2 > 0))$CO2)
cor((rethink %>% filter (MPG > 0 & NOx > 0))$MPG, (rethink %>% filter (MPG > 0 & NOx > 0))$NOx)

cor((rethink %>% filter (NOx > 0 & CO > 0))$NOx, (rethink %>% filter (NOx > 0 & CO > 0))$CO)

cor(rethink$Cylinders, rethink$HP)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Cylinders,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$MPG)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Displ,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$MPG)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$HP,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$MPG)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Weight,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$MPG)

ggplot(data = rethink, mapping = aes(x = Displ, y = HP, color = Fuel)) +
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ, y = Weight)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ, y = MPG)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = (Weight), y = MPG)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)
