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
  

ggplot() + geom_point(data = licor_2015, aes(x=variety, y=Photo, color=month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))


# plotting ----------------------------------------------------------------


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
  filter(row_plant == "42.7", month == "4")

#group by test, filtering out unique ids that may be breaking the code
#making data.frame to be consistent with cran.r example
head(licor_2015)
licor_unique <- licor_2015 %>% 
  mutate(unique = (paste(row_plant, month, sep="_"))) %>% 
  filter(!unique == "44.2_8") %>% 
  mutate(unique = as.factor(unique)) %>% 
  select(unique, Photo, Ci, Tleaf, PARi) %>% 
  as.data.frame()
glimpse(licor_unique)
class(licor_unique)

#need to group by the row_plant id to get separate aic curves for each plant

f <-fitacis(data = licor_unique, group = "unique", fitmethod= "bilinear")
with(coef(f), plot(Vcmax, Jmax))
#error showing up "rror: $ operator is invalid for atomic vectors"
plot(f, how="oneplot")
#try to plot just the rows that work
plot(f[[163]])

head(manyacidat) # check demo data structure
str(manyacidat)
fit
plot(fit)

#trying broom to clean up data set 
install.packages("broom")
library(broom)
tidy(f)

#works to make curves for one plant at a time
#still need to get fitacis to work with entire data set 

fit2 <- fitaci(test_plant, fitmethod = "bilinear")
fit2
plot(fit2)


#making multiple curves from one data set, trouble with grouping by row_plant

#plot(data(licor_2015), what = c("licor_2015", "model", "none"), xlim = NULL,
#     ylim = NULL, whichA = c("Ac", "Aj", "Amin", "Ap"),
#     pch = 19, addzeroline = TRUE, legendbty = "o",
#     transitionpoint = TRUE, linecols = c("black", "blue", "red"), lwd = c(1))



# Water use efficiency data -----------------------------------------------


#climate and water use efficiency data 



                      