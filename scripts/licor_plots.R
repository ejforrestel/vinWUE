#PCA with licor and water use efficiency data
###EJF notes to do: add in stomtal traits to data and check for April data; 
#look at relationships between leaf potential and stomatal conductance -- 
#look for isohydric and anisohydric symdromes; double check some of trait values -- SLA is weird sometimes, 
# not always issue with numeric conversion

library(tidyverse)
library(ggplot2)
library(dplyr)

# plotting data with ggplot2 ----------------------------------------------

#photosynthesis by variety by month

#as a scatterplot
ggplot() + geom_point (data = april_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("April Photosynthesis by Variety")

#as a boxplot
ggplot() + geom_boxplot(data = april_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("April Photosynthesis by Variety")

#june 
ggplot() + geom_point (data = june_licor, aes(x=variety, y=(Photo), color=variety)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("June Photosynthesis by Variety")

#august
ggplot() + geom_point (data = august_licor, aes(x=variety, y=(Photo), color=variety))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("August Photosynthesis by Variety")

#adding the color as month to start seeing patterns
licor_2015 %>% 
  ggplot() +
  geom_point(aes(x=variety, y=Photo, color=factor(month)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Variety") +
  ylab("Photosynthesis") +
  ggtitle("Figure 1. Test")

ggplot(licor_2015, aes(x = variety, y = Photo, color = variety ))+
  geom_point()+
  facet_wrap("month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



