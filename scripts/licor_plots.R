#PCA with licor and water use efficiency data

library(tidyverse)
library(ggplot2)
library(dplyr)

april_licor <- read_csv("data/april_licor.csv")
june_licor <-read_csv("data/june_licor.csv")
august_licor <- read_csv("data/august_licor.csv")
wue_raw <- read.csv("~/beth_data/WUE_monitor_vars_RMI_April2015.csv")
water_use_efficiency <- read_csv("data/water_use_efficiency.csv")


# merging data ------------------------------------------------------------
#rename columns lower case
colnames(august_licor)[1:5]<- tolower(colnames(august_licor)[1:5])
colnames(april_licor)
colnames(august_licor)
colnames(june_licor)

#change column names before merge

april_licor <- april_licor[which(april_licor$Obs%in%3:16),]

august_licor<- august_licor %>% 
  select(-id) %>% 
  filter(Obs %in% 3:16)

june_licor<- june_licor %>% 
  #rename(id = `ï..ID`) %>% 
  select(-ID) %>% 
  filter(Obs %in% 3:16)


june_licor$row_plant <- gsub(pattern = "-", replacement = ".",june_licor$row_plant)
august_licor$row_plant <- as.character(august_licor$row_plant)

licor_2015 <- bind_rows(april_licor, june_licor, august_licor) %>% 
  as.data.frame()



# plotting data with ggplot2 ----------------------------------------------

ggplot() + geom_point(data = licor_2015, aes(x=variety, y=Photo, color=month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#simple plots of photosynthesis by variety

ggplot() + geom_point (data = april_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("April Photosynthesis by Variety")

ggplot() + geom_point (data = june_licor, aes(x=variety, y=(Photo), color=variety)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("June Photosynthesis by Variety")

ggplot() + geom_point (data = august_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("August Photosynthesis by Variety")

licor_2015 %>% 
  ggplot() +
  geom_point(aes(x=variety, y=Photo, color=factor(month)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("Figure 1. Test")

ggplot() + geom_point (data = wue, aes(x=VARIETY, y=(Leaf_potential), color=VARIETY))

#water use efficiency plots 

wateruse <- water_use_efficiency %>% 
  filter(!leaf_potential_avg == "NA", !stem_potential_avg == "NA") %>% 
  filter(!is.na(photo_avg)) %>% 
  mutate(sla = leaf_weight_1_avg/Leaf_area_1_avg) %>% 
  filter(!is.na(sla))

ggplot(wateruse, aes(x = wue_avg , y = photo_avg, color = variety)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(licor_2015, aes(x = variety, y = Photo, color = variety))+
  geom_point()+
  facet_wrap("month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
ggplot(wateruse, aes(x = variety, y = sla, color = sla))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# PCA ---------------------------------------------------------------------

pca.1 <- prcomp(wateruse[2:4,8:9])
biplot(pca.1)
summary(pca.1)

plot(pca.1)


