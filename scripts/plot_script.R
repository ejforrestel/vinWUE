#analysis of licor data from Beth Forrestel 

# read data in ------------------------------------------------------------


library(tidyverse)
april_licor <- read_csv("~/beth_data/april_licor.csv")
june_licor <-read_csv("~/beth_data/june_licor.csv")
august_licor <- read_csv("~/beth_data/august_licor.csv")
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

ggplot() + geom_point(data = licor_2015, aes(x=variety, y=Photo, color=month))

licor_2015 %>% 
  ggplot() +
  geom_point(aes(x=variety, y=Photo, color=factor(month)))


# plotting ----------------------------------------------------------------


#simple plots of photosynthesis by variety

ggplot() + geom_point (data = april_licor, aes(x=variety, y=(Photo), color=variety))
ggplot() + geom_point (data = june_licor, aes(x=variety, y=(Photo), color=variety))    
ggplot() + geom_point (data = august_licor, aes(x=variety, y=(Photo), color=variety))

#can we look at same plant or variety over the 3 months? Does this matter?

ggplot() + geom_point (data = wue, aes(x=VARIETY, y=(Leaf_potential), color=VARIETY))

#light curves for each plant?
install.packages("plantecophys")
library(plantecophys)
fitaci(licor_2015, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi"))
      
plot(, what = c("licor_2015", "model", "none")) 

#climate and water use efficiency data 


                      