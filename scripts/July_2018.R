#water potentials and diurnal measurements in July 2018 in the learning block at RMI 
library(tidyverse)
library(ggplot2)
library(dplyr)

water_ <- read.csv("data/viniferaTraitMeans/WaterPotentialsRMI18July2018.csv")
water <- water_[c(1:11)]
colnames(water)[1] <- "ID" 

#need to plot all columns on the same plot
ggplot(water, aes(Variety, y = value, color = variable)) + 
  geom_line(aes(y = LeafPotential1, col = "LeafPotential1")) + 
  geom_line(aes(y = LeafPotential2, col = "LeafPotential2")) +
  geom_line(aes(y = LeafPotential3, col = "LeafPotential3"))+
  geom_line(aes(y = LeafPotential4, col = "LeafPotential4"))+
  geom_line(aes(y = LeafPotential5, col = "LeafPotential5"))+
  geom_line(aes(y = LeafPotential6, col = "LeafPotential6"))+
  geom_line(aes(y = LeafPotential7, col = "LeafPotential7"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#still not right, I want to have the x axis be each of the time point and the y be the value
ggplot(melt_water, aes(x = value, y = Region, color = variable)) + 
  geom_point(aes(x = LeafPotential1, col = "LeafPotential1")) + 
  geom_point(aes(x = LeafPotential2, col = "LeafPotential2")) +
  geom_point(aes(x = LeafPotential3, col = "LeafPotential3"))+
  geom_point(aes(x = LeafPotential4, col = "LeafPotential4"))+
  geom_point(aes(x = LeafPotential5, col = "LeafPotential5"))+
  geom_point(aes(x = LeafPotential6, col = "LeafPotential6"))+
  geom_point(aes(x = LeafPotential7, col = "LeafPotential7"))

library(reshape2)

melt_water <- melt(water, id.vars = c("Variety", "Region"), 
     measure.vars = c("LeafPotential1", "LeafPotential2", "LeafPotential3", 
                      "LeafPotential4", "LeafPotential5", "LeafPotential6", "LeafPotential7"))

ggplot(melt_water, aes(x = variable, y = value, color =Variety, group = ID)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust =1))
head(melt_water)
#take the mean of the two reps per variety, or group by ID 
#make an error bar 
