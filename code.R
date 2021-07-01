library(tidyverse)
library(ggplot2)
library(corrplot)
library(modelr)
rethink = read_csv("rethink.csv") #read the database

rethink = rethink %>% filter(Displ < 70 & MPG <100) #let's tide it up

rethink = rethink %>% mutate(KML = MPG*0.425144, CO2bK = CO2*0.621371, Lb100 = 100/KML) # add CO2 grams per kilometer and liters used by 100km

View (rethink)

ggplot(rethink %>% filter(Fuel == "Tier 2 Cert Gasoline"), aes(x = Lb100, y = CO2bK)) +
  geom_point(alpha = 0.1) +
  geom_smooth()


cor((rethink %>% filter (MPG > 0 & CO2 > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100,
    (rethink %>% filter (MPG > 0 & CO2 > 0 & Fuel == "Tier 2 Cert Gasoline"))$CO2bK
    )

ggplot((rethink %>% filter(Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline" )), aes(x = HP, y = Lb100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #Good approx: HP = Displacement (Liters)* Cylinders * 10


ggplot((rethink %>% filter(Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ < 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #No turbo, function is apparently logarithmic

ggplot((rethink %>% filter(Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ > 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) #Turbo only, function is initially logarithmic and after exponential (but just one)

ggplot((rethink %>% filter (Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = HP, y = Lb100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)+
  ylim(0, 0.1)

ggplot((rethink %>% filter(Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ, y = Lb100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Weight, y = Lb100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

ggplot((rethink %>% filter(Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Gears, y = Lb100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Cylinders,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Displ,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$HP,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Weight,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100)

cor((rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Gears,
    (rethink %>% filter(Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline"))$Lb100)

######################TESTS#####################



ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & Fuel == "Tier 2 Cert Gasoline" & HP/Displ < 120)), aes(x = Displ*Cylinders, y = HP)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)


ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1)), aes(x = HP, y = Displ)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)

##important
ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 )), aes(x = HP, y = 1/MPG)) +
  geom_point(alpha = 0.15) +
  geom_smooth(se=FALSE)+
  ylim(0, 0.1)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 )), aes(y = HP/Displ, x = Displ/Cylinders)) +
  geom_point(alpha = 0.15) +
  geom_smooth(se=FALSE)

cor((rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & HP > 0 & CO2 > 0))$HP,
    (rethink %>% filter(Displ < 70 & Displ > 0.1 & Cylinders > 0 & HP > 0 & CO2 > 0))$CO2^2
)


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

ggplot((rethink), aes(x = Fuel, y = CO2)) +
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

ggplot(data = rethink, mapping = aes(x = Displ, y = HP, color = Fuel)) +
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot((rethink %>% filter(Displ < 70 & Displ > 0.1 & Fuel == "Tier 2 Cert Gasoline")), aes(x = Displ, y = Weight)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE)
