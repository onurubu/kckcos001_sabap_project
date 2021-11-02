####  COŞKUN KÜÇÜKKARAGÖZ SABAP 1 vs 2 COMPARISON ANALYSIS

####  INITIAL SETUP ####
## Installing required packages, this only needs to be run once, afterwards these lines can be commented out
# {install.packages("emmeans")
# install.packages("Rcpp")
# install.packages("nlme")
# install.packages("lme4")
# install.packages("effects")
# install.packages("MuMIn")
# install.packages("AICcmodavg")
# install.packages("dplyr")
# install.packages("beepr") #this package is only required if you want your PC to play a sound when your model creation is finished
# install.packages("car")}

## Calling required packages
{library(nlme)
  library(lme4)
  library(MASS) # to call quasi families
  library(effects) # for plotting mixed effect models
  library(beepr) #this package is only required if you want your PC to play a sound when your model creation is finished
  
  # model selection:
  library(MuMIn) # for model ranking using AIC scores
  library(AICcmodavg) # for model averaging (what you might not need for now)
  library(dplyr)
  library(Rcpp)
  library(emmeans)
  library(car)} # the glm below seems to need to added in to work for some reason

## Read the data in
{sabap12all <- read.csv("merged_SABAP_onlySA_WORKS.csv", header=TRUE, sep=",", fill=TRUE, row.names=NULL, strip.white=TRUE)
sabap12all$periodcat <- as.factor(sabap12all$period)}


#speciesnames=unique(sabap12all$common.name)   #make a list of all the species name you need to loop through.
#modelresults=NULL  #this is an empty object where your results will be saved

## Setting up a data frame of the species being analysed, one column holding their SABAP 2 ID, and another holding their given name according to the SABAP 1 vs 2 data
{ num <- c(151,142,147,134,167,169,131,370,150,106,110,108,107,109) # currently missing Taita Falcon
  name <- c("BateleurBateleur","MartialEagle","Southern BandedSnake-eagle","TawnyEagle","AfricanMarsh-harrier","BlackHarrier","BatHawk","Pel'sFishing-owl","BeardedVulture","CapeVulture","HoodedVulture","Lappet-facedVulture","White-backedVulture","White-headedVulture") # Species naming must be identical to the naming used in the SABAP 1 vs 2 data
  spp <- cbind(num,name)
  spp <- data.frame(spp)
  spp$num<- as.character(spp$num)
  rm(name,num) }

#### MODEL CREATION ####

# here you subset the different species, create their models, get the emm and eff, and get the plots
for (l in 1:nrow(spp)){
  assign(paste0(spp[l,2],"_data"),subset(sabap12all, common.name == paste0(spp[l,2])))
  assign(paste0("S1v2_",spp[l,2]),glm(cbind(sightings, (cards-sightings))~periodcat, family=quasibinomial, data=get(paste0(spp[l,2],"_data"))))
  
  # the emmeans gives you the ****back transformed**** reporting rates for each period category, eff size gives the difference between the means and associated confidence interval
  assign(paste0(spp[l,2],"_emm"), emmeans(object = get(paste0("S1v2_",spp[l,2])), "periodcat", type="response"))
  # assign(paste0(spp[l,2],"_eff"),eff_size(get(paste0(spp[l,2],"_emm")), sigma = sigma(get(paste0("S1v2_",spp[1,2]))), edf = df.residual(get(paste0("S1v2_",spp[1,2]))))) # unused effect estimator, will use own function
  plot(allEffects(get(paste0("S1v2_",spp[l,2]))), main = paste0(spp[l,2]),ylab="Reporting Rate",xlab="SABAP Period")
  {if (l==nrow(spp)){beep(3)}}
}

#### MODEL ANALYSIS ####

## showing percentage change of reporting rate for all species
for (k in 1:nrow(spp)){
  zz <- summary(get(paste0(spp[k,2],"_emm")))$response[2]/summary(get(paste0(spp[k,2],"_emm")))$response[1]
  
  {if (zz<1) {print(paste0(spp[k,2],": ",round(((1-zz)*100),4),"% decrease in reporting rate between SABAP 1 and 2"))}}
  {if (zz>1) {print(paste0(spp[k,2],": ",round(((zz-1)*100),4),"% increase in reporting rate between SABAP 1 and 2"))}}
  {if (zz==1) {print(paste0(spp[k,2],": There is no change in reporting rate between SABAP 1 and 2"))}}
  cat("\n")
}

## summary of all fitted models along with Anova for test of association of reporting rate and year
  
  # BateleurBateleur
  summary(S1v2_BateleurBateleur)
  # plot(allEffects(S1v2_BateleurBateleur),main=paste0(spp[1,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_BateleurBateleur)
    
  # MartialEagle
  summary(S1v2_MartialEagle)
  # plot(allEffects(S1v2_MartialEagle),main=paste0(spp[2,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_MartialEagle)
  
  # Southern BandedSnake-eagle
  summary(`S1v2_Southern BandedSnake-eagle`)
  # plot(allEffects(`S1v2_Southern BandedSnake-eagle`),main=paste0(spp[3,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_Southern BandedSnake-eagle`)
  
  # TawnyEagle
  summary(S1v2_TawnyEagle)
  # plot(allEffects(S1v2_TawnyEagle),main=paste0(spp[4,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_TawnyEagle)
  
  # AfricanMarsh-harrier
  summary(`S1v2_AfricanMarsh-harrier`)
  # plot(allEffects(`S1v2_AfricanMarsh-harrier`),main=paste0(spp[5,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_AfricanMarsh-harrier`)
  
  # BlackHarrier
  summary(S1v2_BlackHarrier)
  # plot(allEffects(S1v2_BlackHarrier),main=paste0(spp[6,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_BlackHarrier)
  
  # BatHawk
  summary(S1v2_BatHawk)
  # plot(allEffects(S1v2_BatHawk),main=paste0(spp[7,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_BatHawk)
  
  # Pel'sFishing-owl
  summary(`S1v2_Pel'sFishing-owl`)
  # plot(allEffects(`S1v2_Pel'sFishing-owl`),main=paste0(spp[8,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_Pel'sFishing-owl`)
  
  # BeardedVulture
  summary(S1v2_BeardedVulture)
  # plot(allEffects(S1v2_BeardedVulture),main=paste0(spp[9,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_BeardedVulture)
  
  # CapeVulture
  summary(S1v2_CapeVulture)
  # plot(allEffects(S1v2_CapeVulture),main=paste0(spp[10,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_CapeVulture)
  
  # HoodedVulture
  summary(S1v2_HoodedVulture)
  # plot(allEffects(S1v2_HoodedVulture),main=paste0(spp[11,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(S1v2_HoodedVulture)
  
  # Lappet-facedVulture
  summary(`S1v2_Lappet-facedVulture`)
  # plot(allEffects(`S1v2_Lappet-facedVulture`),main=paste0(spp[12,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_Lappet-facedVulture`)
  
  # White-backedVulture
  summary(`S1v2_White-backedVulture`)
  # plot(allEffects(`S1v2_White-backedVulture`),main=paste0(spp[13,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_White-backedVulture`)
  
  # White-headedVulture
  summary(`S1v2_White-headedVulture`)
  # plot(allEffects(`S1v2_White-headedVulture`),main=paste0(spp[14,2]),ylab="Reporting Rate",xlab="SABAP Edition") # this plots out the effects
  Anova(`S1v2_White-headedVulture`)
  
#### UNUSED CODE ####
#   # BateleurBateleur
#   BateleurBateleur_emm #back transformed reporting rate for the category
#   BateleurBateleur_eff #difference between the means and associated confidence interval
#   
#   # MartialEagle
#   MartialEagle_emm #back transformed reporting rate for the category
#   MartialEagle_eff #difference between the means and associated confidence interval
#   
#   # Southern BandedSnake-eagle
#   `Southern BandedSnake-eagle_emm` #back transformed reporting rate for the category
#   `Southern BandedSnake-eagle_eff` #difference between the means and associated confidence interval
#   
#   # TawnyEagle
#   TawnyEagle_emm #back transformed reporting rate for the category
#   TawnyEagle_eff #difference between the means and associated confidence interval
#   
#   # AfricanMarsh-harrier
#   `AfricanMarsh-harrier_emm` #back transformed reporting rate for the category
#   `AfricanMarsh-harrier_eff` #difference between the means and associated confidence interval
#   
#   # BlackHarrier
#   BlackHarrier_emm #back transformed reporting rate for the category
#   BlackHarrier_eff #difference between the means and associated confidence interval
#   
#   # BatHawk
#   BatHawk_emm #back transformed reporting rate for the category
#   BatHawk_eff #difference between the means and associated confidence interval
#   
#   # Pel'sFishing-owl
#   `Pel'sFishing-owl_emm` #back transformed reporting rate for the category
#   `Pel'sFishing-owl_eff` #difference between the means and associated confidence interval
#   
#   # BeardedVulture
#   BeardedVulture_emm #back transformed reporting rate for the category
#   BeardedVulture_eff #difference between the means and associated confidence interval
#   
#   # CapeVulture
#   CapeVulture_emm #back transformed reporting rate for the category
#   CapeVulture_eff #difference between the means and associated confidence interval
#   
#   # HoodedVulture
#   HoodedVulture_emm #back transformed reporting rate for the category
#   HoodedVulture_eff #difference between the means and associated confidence interval
#   
#   # Lappet-facedVulture
#   `Lappet-facedVulture_emm` #back transformed reporting rate for the category
#   `Lappet-facedVulture_eff` #difference between the means and associated confidence interval
#   
#   # White-backedVulture
#   `White-backedVulture_emm` #back transformed reporting rate for the category
#   `White-backedVulture_eff` #difference between the means and associated confidence interval
#   
#   # White-headedVulture
#   `White-headedVulture_emm` #back transformed reporting rate for the category
#   `White-headedVulture_eff` #difference between the means and associated confidence interval
#### END ####