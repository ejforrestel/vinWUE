#phenology with water use traits 
library(tidyverse)
library(ggplot2)

Pheno2015 <- read_csv("data/Phenodata/Pheno2015.csv")
PhenoVarRMI <- read_csv("data/Phenodata/PhenoVarRMI.csv")

#plot phenology with water use traits
Pheno2015$variety <- gsub(pattern = " ", replacement = "_",Pheno2015$variety)

