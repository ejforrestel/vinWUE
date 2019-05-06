#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)


#C13 by flowering - looking for a way to color by region but keep all the variety points, shape = also does not work
ggplot(data = flo_c13, aes(x = doy.2015, y = C13, color = region, shape = variety)) + geom_point() 

#C13 by veraison 
ggplot(data = ver_c13, aes(x = doy.2015, y = C13, color = region))+geom_point()

#WUE mean by variety by month 

#variety and water use efficiency
ggplot(data = v_wue, aes(x = variety, y = mean_wue, color = variety))+facet_wrap(~month)+
       geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#variety and photosynthesis
ggplot(data = v_wue, aes(x = variety, y = mean_photo, color = variety))+facet_wrap(~month)+ geom_point()


#WUE by event
#flowering
ggplot()+ geom_point(data = v_flo, aes(x=doy.2015, y = mean_wue, color = variety))+
  xlab("Flowering 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Flowering")

#wue at bb  
ggplot()+ geom_point(data = v_bb, aes(x=doy.2015, y = mean_wue, color = variety))+
  xlab("Budburst 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Budburst")

#wue at verasion 
ggplot()+ geom_point(data = v_ver, aes(x=doy.2015, y = mean_wue, color = variety))+
  xlab("Veraison 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Veraison")

#wue at maturity using brix at 22
ggplot()+ geom_point(data = v_ver, aes(x=doy.2015, y = mean_wue, color = variety))+
  xlab("Veraison 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Veraison")


ggplot()+ geom_point(data =v_mat, aes(x=doy.2015, y = mean_wue, color = variety))+
  xlab("Maturity 2015") +
  ylab("Intrinsic Water Use Efficiency") +
  ggtitle("Regional Water Use at Maturity")





