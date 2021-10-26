# setwd("C:/Users/01429358/Documents/R")
library(emmeans)
library(nlme)
library(lme4)
library(MASS) # to call quasi families
library(effects) # for plotting mixed effect models

# model selection:
library(MuMIn) # for model ranking using AIC scores
library(AICcmodavg) # for model averaging (what you might not need for now)l
library(dplyr)
library(Rcpp) # the glm below seems to need to added in to work for some reason

sabap12all <- read.csv("merged_SABAP_onlySA_WORKS.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL, strip.white=TRUE)
str(sabap12all)
sabap12all$periodcat <- as.factor(sabap12all$period)


#speciesnames=unique(sabap12all$common.name)   #make a list of all the species name you need to loop through.
#modelresults=NULL  #this is an empty object where your results will be saved



#here you subset the different species
  VEdata=subset(sabap12all, common.name == "Verreaux'sEagle") #subset species data
  MEdata=subset(sabap12all, common.name == "MartialEagle")
  # MartialEagle
  
  
  
  M1=glm(cbind(sightings, (cards-sightings))~periodcat, family=quasibinomial, data=VEdata) 
  summary(M1)
  plot(allEffects(M1)) # this plots out the effects
  
  # the emmeans gives you the back transformed reporting rates for each period category
  emm <- emmeans(M1, "periodcat", type="response")
  emm
  eff_size(emm, sigma = sigma(M1), edf = df.residual(M1))
  
  M2=glm(cbind(sightings, (cards-sightings))~periodcat, family=quasibinomial, data=MEdata) 
  summary(M2)
  plot(allEffects(M2))
  
  
  emm <- emmeans(M2, "periodcat", type="response")
  emm
  eff_size(emm, sigma = sigma(M2), edf = df.residual(M2))
  