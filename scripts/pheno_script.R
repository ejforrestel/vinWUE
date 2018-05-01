#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)
#library(wesanderson)

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")
vitis_mean <- read_csv("data/viniferaTraitMeans/Vitis_mean.csv") #not there?
vitis_trait <- read_csv("data/viniferaTraitMeans/Vitis_trait_data.csv")
vitis_var_means <- read_csv("data/viniferaTraitMeans/Vitis_var_means.csv")


# all data sets need to be summarized by variety before graphs are made 
#PCA all traits
vitis_var_means_select <- vitis_var_means %>% 
  select(Varietal, C13, C.N, mean_stom, stom_den, SPI)

pca.5 <- prcomp(vitis_var_means_select[,(2:6)], scale = T)
biplot(pca.5)
summary(pca.5)
#add aesthetics

#rank order the traits by month 


# Combining
vitis_traits_clean <- vitis_var_means_select %>% 
  rename(variety = Varietal)

Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)

vitis_traits_combo <- full_join(vitis_traits_clean, Pheno2015, by = "variety")
vitis_traits_combo <- full_join(vitis_traits_combo, region, by = "variety")

vitis_traits_flo <- vitis_traits_combo %>% 
  filter(event == "flo")

vitis_traits_ver <- vitis_traits_combo %>% 
  filter(event == "ver")

#C13 by flowering
ggplot(data = vitis_traits_flo, aes(x = doy.2015, y = C13, color = region))+geom_point()

#C13 by veraison 
ggplot(data = vitis_traits_ver, aes(x = doy.2015, y = C13, color = region))+geom_point()

#WUE mean by variety by month 
month.wue <- aggregate(x = licor_2015_wue, by = variety, FUN = mean)

  

#WUE by flowering 


#WUE by veraison 




#plot phenology with water use traits
pheno_wue_combined <- licor_2015_wue %>% 
  select(month, variety, Photo, Cond, wue) %>% 
  group_by(variety) %>%
  summarize(mean_photo = mean(Photo, na.rm = TRUE))

Pheno_bb <- full_join(pheno_wue_combined, Pheno2015, by = "variety") %>% 
  filter(event == "bb")

region <- PhenoVarRMI %>% 
  select(variety, region)

Pheno_bb_region <- full_join(region, Pheno_bb, by = "variety")

#wue at bb by variety 
ggplot()+ geom_point(data = Pheno_bb_region, aes(x=doy.2015, y = wue, color = region))+
  xlab("Budburst 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Budburst")

#wue at flowering
Pheno_flo <- full_join(pheno_wue_combined, Pheno2015, by = "variety") %>% 
  filter(event == "flo")

Pheno_flo_region <- full_join(region, Pheno_flo, by = "variety") %>% 
  filter(!wue == "NA") %>% 
  filter(!region == "NA")

ggplot()+ geom_point(data = Pheno_flo_region, aes(x=doy.2015, y = wue, color = region))+
  xlab("Flowering 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Flowering")

#wue at verasion 

Pheno_ver <- full_join(pheno_wue_combined, Pheno2015, by = "variety") %>% 
  filter(event == "ver")

Pheno_ver_region <- full_join(region, Pheno_ver, by = "variety") %>% 
  filter(!wue == "NA") %>% 
  filter(!region == "NA")

ggplot()+ geom_point(data = Pheno_ver_region, aes(x=doy.2015, y = wue, color = region))+
  xlab("Verasion 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Verasion")

#wue at maturity using brix at 22
Pheno_brix <- full_join(pheno_wue_combined, Pheno2015, by = "variety") %>% 
  filter(event == "brix22")

Pheno_brix_region <- full_join(region, Pheno_brix, by = "variety") %>% 
  filter(!wue == "NA") %>% 
  filter(!region == "NA")

ggplot()+ geom_point(data = Pheno_brix_region, aes(x=doy.2015, y = wue, color = region))+
  xlab("Maturity 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Maturity")


#variety_traits comes from the licor_plots script, combining the original licor data

Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)

pheno_joined <- full_join(Pheno2015, PhenoVarRMI, by = "variety")
pheno_joined2 <- full_join(pheno_joined, variety_traits, by = "variety")
pheno_joined3 <- pheno_joined2 %>% 
  filter(!is.na(mean_wue)) %>% 
  filter(!is.na(mean_photo)) %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(event))


ggplot() + geom_point(data = pheno_joined3, aes(x = variety, y = mean_wue, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = region, y = mean_wue, color = variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = RMI_bb, y = mean_photo, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#getting closer to the figure I think you want
ggplot() + geom_point(data = pheno_joined3, aes(x = event, y = mean_wue, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Phenological Event") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use")


#getting closer
ggplot() + geom_line(data = pheno_joined3, aes(x = doy.2015, y = mean_wue, color = region))+
  #scale_color_manual(values = pal)+
  xlab("Phenological Event") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use")

#aggregate before line plot


ggplot() + geom_boxplot(data = pheno_joined3, aes(x=variety, y= mean_photo, color=region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("Photosynthesis by Variety")

ggplot(data = pheno_joined3, aes(x=region, y= mean_photo)) + geom_boxplot(alpha = 0)+
  geom_jitter(alpha = 0.3, color = region)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~month)+
  xlab("Region") +
  ylab("Photosynthesis") +
  ggtitle("Photosynthesis by Region")


#this one is interesting, starting to cluster by region
ggplot() + geom_point(data = pheno_joined3, aes(x = RMI_bb, y = mean_wue, color = region))


#looking at photo continuously
ggplot(data = pheno_joined3, aes(x = RMI_bb, y = mean_wue, group = region, color = variety)) +
  geom_point()

ggplot(data = pheno_joined3, aes(x = RMI_bb, y = mean_wue, color = variety)) +
  geom_point()+
  facet_wrap(~region)

ggplot(data = variety_traits, aes(x = month, y = mean_wue, group = variety, color = variety)) +
  geom_line()

ggplot(data = pheno_joined3, aes(x = month, y = mean_wue, group = region, color = variety)) +
  geom_point()

ggplot(data = pheno_joined3, aes(x = RMI_flow, y = mean_wue, group = region, color = region)) +
  geom_point()

#PCA with traits 
mean_traits <- vitis_trait %>% 
  mutate(mean_stom_length = (stom_length_1.1+stom_length_1.2+stom_length_1.3+stom_length_1.4+stom_length_1.5)/5) %>% 
  mutate(mean_stom_number = (stom_number_1+stom_number_2)/2) %>% 
  select(Variety_name, C13, C:N, mean_stom_length, mean_stom_number) %>% 
  rename(variety = Variety_name)

variety_traits <- variety_traits %>% 
  select(variety, mean_photo, mean_wue)

traits <- vitis_var_means %>% 
  rename(variety = Varietal) %>% 
  select(variety, C13,C, N, C.N, mean_stom, stom_den, SPI)

traits_combined <- full_join(traits, variety_traits, by = "variety") %>% 
  filter(!C13=="NA") %>% 
  filter(!C=="NA") %>% 
  filter(!N=="NA") %>% 
  filter(!C.N=="NA") %>% 
  filter(!mean_stom=="NA") %>% 
  filter(!stom_den=="NA")

  
traits_combined2 <- aggregate(traits_combined[,2:10], list(traits_combined$variety), mean)
  
  #how to summarize by variety? group_by not working 

pca.4 <- prcomp(traits_combined2[,(4:8)], scale = T)
biplot(pca.4)
summary(pca.4)

#adding in mean_wue and mean_photo means losing some of the other data because of NAs so 
#I did this in two steps

traits_combined_b <- traits_combined %>% 
  filter(!mean_photo == "NA") %>% 
  filter(!mean_wue == "NA")

pca.5 <- prcomp(traits_combined_b[,2:10], scale = T)
biplot(pca.5)
summary(pca.5)
