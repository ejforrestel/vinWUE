#phenology with water use traits 
library(tidyverse)
library(ggplot2)

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")

#plot phenology with water use traits
Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)

pheno_joined <- full_join(Pheno2015, PhenoVarRMI, by = "variety")

pheno_joined2 <- full_join(pheno_joined, variety_traits, by = "variety")

pheno_joined3 <- pheno_joined2 %>% 
  filter(!is.na(mean_wue))

ggplot() + geom_point(data = pheno_joined3, aes(x = variety, y = mean_wue, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = region, y = mean_wue, color = variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = RMI_bb, y = mean_photo, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#this one is interesting, starting to cluster by region
ggplot() + geom_point(data = pheno_joined3, aes(x = RMI_bb, y = mean_wue, color = region))

#looking at photo continuously
ggplot(data = pheno_joined3, aes(x = RMI_bb, y = mean_wue, group = region, color = variety)) +
  geom_point()

ggplot(data = variety_traits, aes(x = month, y = mean_wue, group = variety, color = variety)) +
  geom_line()

ggplot(data = pheno_joined3, aes(x = month, y = mean_wue, group = region, color = variety)) +
  geom_point()


