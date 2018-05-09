#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)


#PCA all traits

pca.5 <- prcomp(vitis_var_means[,(2:6)], scale = T)
biplot(pca.5)
summary(pca.5)


#C13 by flowering - needto combine data first with traits and event before plotting
ggplot(data = , aes(x = doy.2015, y = C13, color = region))+geom_point()

#C13 by veraison 
ggplot(data = , aes(x = doy.2015, y = C13, color = region))+geom_point()


#wue at bb by variety 
ggplot()+ geom_point(data = , aes(x=doy.2015, y = wue, color = region))+
  xlab("Budburst 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Budburst")

#wue at flowering

ggplot()+ geom_point(data = __________, aes(x=doy.2015, y = wue, color = region))+
  xlab("Flowering 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Flowering")

#wue at verasion 

ggplot()+ geom_point(data = ___________, aes(x=doy.2015, y = wue, color = region))+
  xlab("Verasion 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Verasion")

#wue at maturity using brix at 22

ggplot()+ geom_point(data = _________, aes(x=doy.2015, y = wue, color = region))+
  xlab("Maturity 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Maturity")


