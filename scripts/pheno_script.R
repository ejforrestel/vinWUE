#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)

#PCA all traits 

pca.5 <- prcomp(vitis_var_means[,(2:8)], scale = T)
biplot(pca.5)
summary(pca.5)

#add aesthetics

library(ggfortify)
df <- vitis_var_means[c(2,5,6,7,8)]
autoplot(prcomp(df))
autoplot(prcomp(df), data = vitis_var_means, colour = 'region',
         frame = TRUE, frame.type = 'norm')

#interested in regional PCA but can't get the points from variety with ellipses or coloring by region
df.2 <- vitis_var_means[c(2,5,6,7,8)]
autoplot(prcomp(df.2))
autoplot(prcomp(df.2), data = vitis_var_means, colour = 'variety', 
         frame = TRUE, frame.type = 'norm')

#there must be a way to do the PCA with the variety points and then use region 
#to cluster
library(cluster)
autoplot(clara(vitis_var_means[c(2,5,6,7,8)], 8), 
         frame.type = 'norm', frame = TRUE)

#clusters look a little distinct, but how do we dictate what the clusters are?
autoplot(clara(vitis_var_means[c(2,5,6,7,8)], 8), 
         frame.type = 'norm', frame = TRUE, frame.color = "variety")

#rank order the traits by month 

cor(v_wue$mean_photo, v_wue$mean_wue, method = c("spearman"))


#C13 by flowering
ggplot(data = _______, aes(x = doy.2015, y = C13, color = region))+geom_point()

#C13 by veraison 
ggplot(data = ________, aes(x = doy.2015, y = C13, color = region))+geom_point()

#WUE mean by variety by month 
ggplot(data = v_wue, aes(x = variety, y = mean_wue, color = variety))+facet_wrap(~month)+
       geom_point()

ggplot(data = v_wue, 
       aes(x = variety, y = mean_photo, color = variety))+facet_wrap(~month)+ geom_point()


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


#aggregate before line plot




#PCA with traits 

