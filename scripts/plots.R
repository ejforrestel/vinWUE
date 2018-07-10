#phenology with water use traits 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cluster)

#PCA all traits 

pca.5 <- prcomp(vitis_var_means[,(2:8)], scale = T)
biplot(pca.5)
summary(pca.5)

#add aesthetics

df <- vitis_var_means[c(2,5,6,7,8)]
autoplot(prcomp(df))
#autoplot(prcomp(df), data = vitis_var_means, colour = 'region',  #error in graphics
#         frame = TRUE, frame.type = 'norm')

#interested in regional PCA but can't get the points from variety with ellipses or coloring by region
df.2 <- vitis_var_means[c(2,5,6,7,8)]
autoplot(prcomp(df.2))
#too few points to calculate an ellipse
#autoplot(prcomp(df.2), data = vitis_var_means, colour = 'variety', 
#         frame = TRUE, frame.type = 'norm')

#there must be a way to do the PCA with the variety points and then use region 
#to cluster

autoplot(clara(vitis_var_means[c(2,5,6,7,8)], 8), 
         frame.type = 'norm', frame = TRUE) #gives a graphic but "too few points to calculate an ellipse"

#clusters look a little distinct, but how do we dictate what the clusters are?
autoplot(clara(vitis_var_means[c(2,5,6,7,8)], 8), 
         frame.type = 'norm', frame = TRUE, frame.color = "variety") #gives a graphic but "too few points to calculate an ellipse"

#rank order the traits by month 

cor(v_wue$mean_photo, v_wue$mean_wue, method = c("spearman"))
# gives -0.4169321

