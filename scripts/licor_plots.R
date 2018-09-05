#PCA with licor and water use efficiency data
###EJF notes to do: add in stomtal traits to data and check for April data; 
#look at relationships between leaf potential and stomatal conductance -- 
#look for isohydric and anisohydric symdromes; double check some of trait values -- SLA is weird sometimes, 
# not always issue with numeric conversion

#need to add licor data that pairs with water potentials from july 2018

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


#water potential data July 2018______________________________________________________
# mdata is the melted data from the cleaning script, getting mean and sd for the line plot
datam<- aggregate(data=mdata,value ~ Region*variable,FUN = mean)
datasd<- aggregate(data=mdata,value ~ Region*variable,FUN = sd)

#make the values negative to reflect water potential
datam$value <- datam$value*-1
#remove stem potential to add back to the graph later
datam <- datam %>% !(datam$variable=="StemPotential4")
datam2 = datam[datam$variable != 'StemPotential4',]

#plot the mean data without the stem potentials, need to add sd to points
ggplot(datam2, aes(x= as.numeric(variable), y = value, color = Region)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd))+
  geom_path(size = 1.5)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Water Potential Mpa") +
  xlab("Leaf Potentials") +
  ggtitle("Diurnal Leaf Potential, Mid-Day Stem Potential July 2018") +
  scale_color_brewer(palette = "Paired")

#create new data set with just the stem potentials, as means
datastem <- datam[datam$variable == 'StemPotential4',]

#plot the stem onto the line plot
p + geom_point(data = datastem,mapping= aes(x=rep(4,11),y = value, color = Region))
 

#if you don't have this color palatte, the default for ggplot is not very distinct
install.packages("RColorBrewer")           
library(RColorBrewer)
display.brewer.all()
