#script to try models
library(tidyverse)
library(ggplot2)
library(MASS)

vitis_var_means <- read_csv("data/viniferaTraitMeans/Vitis_var_means.csv")
#vitis_mean <- read_csv("data/viniferaTraitMeans/Vitis_mean.csv")
vitis_trait <- read_csv("data/viniferaTraitMeans/Vitis_trait_data.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")

vitis_var_means <- vitis_var_means %>% 
  rename(variety = Varietal) 

vitis_var_means <- subset(vitis_var_means, select=c("variety", "C13", "C", "N", "C.N", "mean_stom", "stom_den", "SPI"))


PhenoVarRMI <- PhenoVarRMI %>% 
  na.omit(PhenoVarRMI)

#vitis <- full_join(vitis_var_means, PhenoVarRMI, by = "variety") #this is not very helpful because they end 
#up mostly NAs 

#this is called full.model just to make it easy for substituting betas, but I get a 0 slope when
#I use variety in the model, so I need to figure out how to add flowering time with the traits and 
#maybe this data does not fit in a linear model
full.model = lm(SPI ~ stom_den , data = vitis_var_means)

vitis_var_means$ei = full.model$residuals
vitis_var_means$yhat = full.model$fitted.values

qqnorm(full.model$residuals)
qqline(full.model$residuals)

ei = full.model$residuals
the.SWtest = shapiro.test(ei)
the.SWtest

qplot(yhat, ei, data = vitis_var_means) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

Group = rep("Lower",nrow(vitis_var_means))


