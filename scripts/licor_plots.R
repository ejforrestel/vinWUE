#PCA with licor and water use efficiency data
###EJF notes to do: add in stomtal traits to data and check for April data; look at relationships between leaf potential and stomatal conductance -- look for isohydric and anisohydric symdromes; double check some of trait values -- SLA is weird sometimes, not always issue with numeric conversion


library(tidyverse)
library(ggplot2)
library(dplyr)

april_licor <- read_csv("data/april_licor.csv")
june_licor <-read_csv("data/june_licor.csv")
august_licor <- read_csv("data/august_licor.csv")
water_use_april <- read_csv("data/WUE_monitor_vars_RMI_April2015.csv")
#water_use_efficiency <- read_csv("data/water_use_efficiency.csv")
water_use_june <- read_csv("data/WUE_monitor_vars_RMI_June2015.csv")
water_use_august <- read.csv("data/August2015_RMI_data_wue.csv")

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
  
licor_2015_wue <- licor_2015 %>% 
  mutate(wue = (Photo/Cond))

head(licor_2015_wue)

variety_traits <- licor_2015_wue %>%
  group_by(variety, month) %>%
  summarize(mean_photo = mean(Photo, na.rm = TRUE), mean_wue = mean(wue, na.rm = TRUE))
  

# plotting data with ggplot2 ----------------------------------------------
ggplot()+geom_point(data=variety_traits, aes(x=month, y = mean_wue, color = variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = licor_2015, aes(x=variety, y=Photo, color=month))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#simple plots of photosynthesis by variety

ggplot() + geom_point (data = april_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("April Photosynthesis by Variety")

ggplot() + geom_boxplot(data = april_licor, aes(x=variety, y=(Photo), color=variety))+
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

#ggplot() + geom_point (data = wue, aes(x=VARIETY, y=(Leaf_potential), color=VARIETY))

###EJF - double check numeric conversions, weird things happening with data - check why it is reading in as non-numeric
#MEC I think it reads in as character because some of the data is num,num and num/note because there were 2 observations in one box
#I can average these boxes and re run to see if it comes in as numeric, because otherwise it shows up as NA later and we lose these points
#averaging the boxes worked, data comes in as numeric 

#water use efficiency plots 


#add varieties to August  
water_use_variety <- water_use_april %>% 
  mutate(row_plant = (paste(ROW, PLANT, sep="_"))) %>% 
  select(VARIETY, row_plant) %>% 
  filter(!VARIETY == "")

water_use_august_mutate <- water_use_august %>% 
  select(ROW, PLANT, Leaf_potential, Stem_potential) %>% 
  mutate(row_plant = (paste(ROW, PLANT, sep= "_"))) 

wue_august <- full_join(water_use_august_mutate, water_use_variety, by = "row_plant")

#..... June

water_use_june_mutate <- water_use_june %>% 
  select(ROW, PLANT, Leaf_potential, Stem_potential) %>% 
  mutate(row_plant = (paste(ROW, PLANT, sep= "_"))) 

wue_june <- full_join(water_use_june_mutate, water_use_variety, by = "row_plant")


#water use for April is fine does not need a join with variety

water_use_apr <- water_use_april %>%
  filter(!Leaf_potential=="1") %>% 
  filter(!Leaf_area1=="1") %>% 
  filter(!leaf_weight1_mg =="1") %>% 
  mutate(sla = leaf_weight1_mg/Leaf_area1) %>% 
  mutate(intrinsic_wue = A/g) %>% 
  select(VARIETY, Leaf_potential, Stem_potential, intrinsic_wue, sla) %>% 
  filter(!VARIETY == "")
  
  #summarize(mean_sla = mean(sla), mean_leaf_potential = mean(Leaf_potential),
            #mean_stem_potential = mean(Stem_potential), mean_wue = mean(intrinsic_wue))


  




# PCA ---------------------------------------------------------------------
#April 
pca.1 <- prcomp(water_use_apr[,2:5], scale = T)
biplot(pca.1)
summary(pca.1)

plot(pca.1)

#June has weight but no area, only stem and leaf potential


#August only has stem and leaf potential, still do PCA?

pca.2 <- prcomp(wue_august[,3:4], scale = T)
biplot(pca.2)
summary(pca.2)

plot(pca.2)

 


# preliminary plots

ggplot(water_use_apr, aes(x = intrinsic_wue, y = sla, color = VARIETY)) +
  geom_point()

ggplot(licor_2015, aes(x = variety, y = Photo, color = variety ))+
  geom_point()+
  facet_wrap("month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(wue_august, aes(x = Leaf_potential, y = Stem_potential, color = VARIETY))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# climate data ------------------------------------------------------------

#will use other climate data later on 

climate <- read.csv("data/2013_15TminTmax_RMI.csv")
library(lubridate)
paste(climate$year, climate$month, climate$day, sep = "-")


