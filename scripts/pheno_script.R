#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")
vitis_mean <- read_csv("data/viniferaTraitMeans/Vitis_mean.csv")
vitis_trait <- read_csv("data/viniferaTraitMeans/Vitis_trait_data.csv")
vitis_var_means <- read_csv("data/viniferaTraitMeans/Vitis_var_means.csv")

#plot phenology with water use traits
pheno_wue_combined <- licor_2015_wue %>% 
  select(month, variety, Photo, Cond, wue) 

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

ggplot(data = licor_2015_wue, aes(x = variety, y = wue)) +
  geom_point()

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


