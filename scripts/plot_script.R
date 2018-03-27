#analysis of licor data from Beth Forrestel 

# read data in ------------------------------------------------------------


library(tidyverse)
april_licor <- read_csv("data/april_licor.csv")
june_licor <-read_csv("data/june_licor.csv")
august_licor <- read_csv("data/august_licor.csv")
wue <- read.csv("~/beth_data/WUE_monitor_vars_RMI_April2015.csv")

#rename columns lower case
colnames(august_licor)[1:5]<- tolower(colnames(august_licor)[1:5])
colnames(april_licor)
colnames(august_licor)
colnames(june_licor)

#change column names before merge

august_licor<- august_licor %>% 
  select(-id)

june_licor<- june_licor %>% 
  #rename(id = `ï..ID`) %>% 
  select(-ID)

colnames(april_licor)
colnames(august_licor)
colnames(june_licor)

june_licor$row_plant <- gsub(pattern = "-", replacement = ".",june_licor$row_plant)
august_licor$row_plant <- as.character(august_licor$row_plant)

licor_2015 <- bind_rows(april_licor, june_licor, august_licor)
  

# plotting ----------------------------------------------------------------
install.packages("ggplot")
library(ggplot)
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

#can we look at same plant or variety over the 3 months? Does this matter?

ggplot() + geom_point (data = wue, aes(x=VARIETY, y=(Leaf_potential), color=VARIETY))




# Plantecophys ------------------------------------------------------------

#light curves for each plant 
install.packages("plantecophys")
library(plantecophys)

#filter for one plants
test_plant <- licor_2015 %>% 
  filter(row_plant == "18.7", month == "4", day == "24")

#works to make curves for one plant at a time

fit3 <- fitaci(test_plant, fitmethod = "bilinear")
fit3
plot(fit3)


#group by test, filtering out unique ids that may be breaking the code
#making data.frame to be consistent with cran.r example
#added day to unique identifier to separate replicates 
head(licor_2015)
licor_clean <- licor_2015 %>% 
  mutate(unique = (paste(row_plant, month, day, sep="_"))) %>% 
  #filter(!unique == "44.2_8") %>% 
  #mutate(unique = as.factor(unique)) %>% 
  #select(month, Photo, Ci, Tleaf, PARi) %>% 
  as.data.frame()


#glimpse(licor_unique)
#class(licor_unique)

#need to group by the row_plant id to get separate aic curves for each plant

multiple <- fitacis(data = licor_unique, group = "unique", fitmethod = "bilinear")
with(coef(multiple), plot(Vcmax, Jmax))

april_plot <- fitacis(data = april, group = "unique", fitmethod = "bilinear")
#     error showing up "error: $ operator is invalid for atomic vectors"

#different ways to plot, would ideally be plotted by variety with replicates of each plant
plot(multiple, how="oneplot")
#try to plot just the rows that work, for some reason only works through 12, 
# and the plot looks strange at 12
plot(april_plot, how = "manyplots")

april <- licor_unique %>% 
  filter(month == "4") %>% 
  droplevels()


head(manyacidat) # check demo data structure
str(manyacidat)
fit
plot(fit)

#trying broom to clean up data set 
install.packages("broom")
library(broom)
tidy(f)




#making multiple curves from one data set, trouble with grouping by row_plant

#plot(data(licor_2015), what = c("licor_2015", "model", "none"), xlim = NULL,
#     ylim = NULL, whichA = c("Ac", "Aj", "Amin", "Ap"),
#     pch = 19, addzeroline = TRUE, legendbty = "o",
#     transitionpoint = TRUE, linecols = c("black", "blue", "red"), lwd = c(1))



# Water use efficiency data -----------------------------------------------

#create a new data table with WUE data from licor_2015 and wue data sets
#create new column with WUE then remove first three observations (or somehow keep the last three)
#then average
library(tidyverse)
library(dplyr)

water_use <- licor_clean %>% 
  transform(licor_clean, Obs = as.numeric(Obs)) %>% 
  inner_join(licor_2015, wue, by = "VARIETY")

  
  #filter(Obs == "1","2","3")

#glimpse(licor_clean)

# PCA with leaf potential and stem potential

# PCA with SLA -> dry weight/area


# Leaf area by variety (PCA with function prcomp)

pca1 = prcomp(data = water_use, subset(Photo), scale. = TRUE)


library(ggplot2)
leaf_area = as.data.frame()

# stomatal density

# stem size 

# color by origin to see interesting clusters by geography?

# phenology data ----------------------------------------------------------

# by growing degree days show flowering time, veraison, Brix as maturity 

# growing degree days = (Tmin+Tmax/2)-Tbase=10

#looking at GDD to 50% flowering 



#climate and water use efficiency data 



                      