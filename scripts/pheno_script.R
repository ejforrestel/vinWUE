#phenology with water use traits 
library(tidyverse)
library(ggplot2)

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")

#plot phenology with water use traits
#variety_traits comes from the licor_plots script, combining the original licor data

Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)

pheno_joined <- full_join(Pheno2015, PhenoVarRMI, by = "variety")
pheno_joined2 <- full_join(pheno_joined, variety_traits, by = "variety")
pheno_joined3 <- pheno_joined2 %>% 
  filter(!is.na(mean_wue)) %>% 
  filter(!is.na(mean_photo)) %>% 
  filter(!is.na(region))


ggplot() + geom_point(data = pheno_joined3, aes(x = variety, y = mean_wue, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = region, y = mean_wue, color = variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + geom_point(data = pheno_joined3, aes(x = RMI_bb, y = mean_photo, color = region))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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


