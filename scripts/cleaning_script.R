#cleaning script
#first, pulling in the monthly licor data to make one licor_2015 dataset
library(tidyverse)
library(ggplot2)
library(dplyr)

april_licor <- read_csv("data/april_licor.csv")
june_licor <-read_csv("data/june_licor.csv")
august_licor <- read_csv("data/august_licor.csv")

colnames(august_licor)[1:5]<- tolower(colnames(august_licor)[1:5])

#change column names before merge

april_licor <- april_licor[which(april_licor$Obs%in%3:16),]

august_licor<- august_licor %>% 
  filter(Obs %in% 3:16)

#filtering by the observations after 3 so that we remove the stabilization obs
june_licor$row_plant <- gsub(pattern = "-", replacement = ".",june_licor$row_plant)

june_licor<- june_licor %>% 
  select(-ID) %>% 
  filter(Obs %in% 3:16)

august_licor$row_plant <- as.character(august_licor$row_plant)

licor_2015 <- bind_rows(april_licor, june_licor, august_licor) %>% 
  as.data.frame()

licor_wue <- licor_2015 %>% 
  mutate(wue = (Photo/Cond)) %>% 
  select(month, day, row_plant, variety, Photo, Cond, wue)

#We need to clean the traits data before it can be used for the plots

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv") # just variety with event and DOY
#PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv") # variety with region and RMI dates and traits
#vitis_ind_mean <- read_csv("data/viniferaTraitMeans/Vitis_ind_mean.csv") # traits by individual plant
#vitis_trait <- read_csv("data/viniferaTraitMeans/Vitis_trait_data.csv") # stomata traits by individual
vitis_var_means <- read_csv("data/viniferaTraitMeans/Vitis_var_means.csv") #important traits by month

#variety instead of varietal for merging
vitis_var_means <- vitis_var_means %>% 
  rename(variety = Varietal) 
  
vitis_var_means$variety <- gsub(pattern = "/", replacement = "_",vitis_var_means$variety)

#creating a subset dataframe that averages photosynthesis and
#water use efficiency by variety for each month
v_wue <- licor_wue %>%
  group_by(variety, month) %>%
  summarize(mean_photo = mean(Photo, na.rm = TRUE), 
            mean_wue = mean(wue, na.rm = TRUE))

#vitis_var_means has 49 varieties with their traits 

vitis_var_means <- subset(vitis_var_means, 
                          select=c("variety", "C13", "C", "N", "C.N", "mean_stom", "stom_den", "SPI")) 
#vitis_var_means <- full_join(vitis_var_means, region, by = "variety")

#reshaping Phenology data
Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)
Pheno2015$variety <- gsub(pattern = "/", replacement = "_",Pheno2015$variety)

#need a dataset that has the region for each of the varieties to add to dataframes later

region <- read_csv("data/Phenodata/region.csv") 

#vitis_region <- full_join(vitis_var_means, region, by = "variety") %>% 
#  na.omit(T)

#creating datasets for each pheno event for plots, this time filtering event first before
#merge to avoid the earlier problems with missing data 

Pheno2015 <- full_join(Pheno2015, region, by = "variety") %>% 
  na.omit(Pheno2015, TRUE)

vitis_flo <- Pheno2015 %>% 
  filter(event == "flo") #28 varieties

vitis_ver <- Pheno2015 %>% 
  filter(event == "ver") #28 varieties

vitis_bb <- Pheno2015 %>% 
  filter(event =="bb") #28 varieties

vitis_mat <- Pheno2015 %>% 
  filter(event=="brix22") #28 varieties

#preparing water use data for merge
wue_variety <- licor_wue %>% 
  group_by(variety) %>% 
  summarize(mean_wue = mean(wue))

#adding water use averages to the flowering summary by variety, only left with 11 when na.omit is T

v_flo <- full_join(vitis_flo, wue_variety, by = "variety") 

#same for budburst
v_bb <- full_join(vitis_bb, wue_variety, by = "variety") 

#same for veraison
v_ver <- full_join(vitis_ver, wue_variety, by = "variety") 

#same for maturity
v_mat <- full_join(vitis_mat, wue_variety, by = "variety") 





