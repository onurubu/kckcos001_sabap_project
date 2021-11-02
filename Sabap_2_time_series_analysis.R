####  COŞKUN KÜÇÜKKARAGÖZ SABAP 2 TIME SERIES ANALYSIS

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
  # install.packages("car")
  # install.packages("beepr")} #this package is only required if you want your PC to play a sound when your model creation is finished

## calling required packages
  {library(nlme)
  library(lme4)
  library(MASS) # to call quasi families
  library(effects) # for plotting mixed effect models
  library(beepr) #this package is only required if you want your PC to play a sound when your model creation is finished
  library(car)
  # model selection:
  library(MuMIn) # for model ranking using AIC scores
  library(AICcmodavg) # for model averaging (what you might not need for now)
  library(dplyr)
  library(Rcpp)} # the glm below seems to need to added in to work for some reason

## Setting up a data frame of the species being analysed, one column holding their SABAP 2 ID, and another holding their given name according to the SABAP 1 vs 2 data
  { num <- c(151,142,147,134,167,169,131,370,150,106,110,108,107,109) # currently missing Taita Falcon
    name <- c("BateleurBateleur","MartialEagle","Southern BandedSnake-eagle","TawnyEagle","AfricanMarsh-harrier","BlackHarrier","BatHawk","Pel'sFishing-owl","BeardedVulture","CapeVulture","HoodedVulture","Lappet-facedVulture","White-backedVulture","White-headedVulture") # Species naming must be identical to the naming used in the SABAP 1 vs 2 data
    spp <- cbind(num,name)
    spp <- data.frame(spp)
    spp$num<- as.character(spp$num)
    rm(name,num) }

#### BINARY COLUMN SETUP ####

### This does section not need to be run if the output files have already been created ###
    
## Adds the function that will be used to add a binary column to the end of SABAP2 .csv data.
# bincol <- function(infile){
#   D <- read.csv(infile, header = T)
#   bin<-rep(NA,nrow(D))
#   for (i in 1:nrow(D)){
#     if ((D[i,"Spp"]==spp[j,1])==TRUE){ bin[i]=1 }
#     else { bin[i]=0 }
#   }
#   
#   return(cbind(D,bin))
# }

##  Specifies the directories for the input and output files, runs the binary column addition function, and writes the output
# for (j in 1:nrow(spp)){
#   infile = paste0("/home/onurubu/Desktop/UNI/2021/BIO3019S/Project - Arjun Amar/kckcos001_sabap_project/data_raw/",spp[j,2],".csv") # Needs to be changed to match your personal directories where the raw data is found
#   bin_data <- bincol(infile)
#   write.csv(bin_data, file = paste0("/home/onurubu/Desktop/UNI/2021/BIO3019S/Project - Arjun Amar/kckcos001_sabap_project/data_processed/",
#                                   spp[j,1], "_", spp[j,2], ".csv")) # Should be changed to where you would like the output to be placed
# }

#### READING IN THE DATA, PROPERLY FORMATTING IT, AND CREATING THE MODELS ####
  
## This loop readies all species data given in the spp data frame from their respective .csv files, formats it according to our specifications, and then creates our models. WARNING: This will take a long long time to run
for (k in 1:nrow(spp)){
  xx <-  read.csv(file = paste0("/home/onurubu/Desktop/UNI/2021/BIO3019S/Project - Arjun Amar/kckcos001_sabap_project/data_processed/",spp[k,1], "_", spp[k,2], ".csv"))
  xx$year <- substring(xx$StartDate, 1, 4) # this just pulls out the year variable from the date column
  xx$month <- substring(xx$StartDate, 6, 7)  # this just pulls out the month variable from the date column; we might want to explore controlling for this variable
  xx$year <- as.numeric(xx$year) # this makes sure year is treated as a continuous term
  mess <- subset(xx, Pentad %in% unique(Pentad[bin == 1])) # this selects only those pentads where the species has ever been present I included this because the data was too big otherwise
  
  M2 <- mess %>% group_by(Pentad) %>% mutate(duplicate.flag = n() > 100)   # This creaates a new df (called ME2), which selects all the records for each pentad which has over a certain number of records - eg. 100 records - it adds a flag to those pentads which qualify based on the condition specified 

  # Then, you can use filter to subset each group of data, as needed:
  M3 <- M2 %>% filter(duplicate.flag) %>% filter(year>2006)# here, we select only those wih X repeat records, and also only those with data since sabap2 started
  #  this would be for selecting only unqiue values:  filter(!duplicate.flag)
  
  M3$year <- as.numeric(M3$year) # this makes sure year is treated as a continuous term
  dat <- assign(paste0("D_",spp[k,1],"_",spp[k,2]),M3)
  # assign(paste0("M_",spp[k,1],"_",spp[k,2]),glm(bin ~ year + Pentad , family = binomial, data = dat)) # this controls for pentad as a fixed effect
  # assign(paste0("Mo_",spp[k,1],"_",spp[k,2]),glm(bin ~ year + Pentad + month , family = binomial, data = dat))
  #m1 <- glmer(bin ~ year + (1|Pentad), family = binomial, data = ME3) - this is a mixed effects model - but takes too long to run
  
  {if (k==nrow(spp)){beep(3)}}  #this line is only required if you want your PC to play a sound when your model creation is finished, if you don't have beepr installed you will receive an error, so be careful
  }

#### ANALYSING THE MODELS ####
  
## These functions show you the model output and the plot of the model:
  # summary (m1)
  # plot(allEffects(m1))

## These lines should be copied, replacing "m1" with the name of the desired model
  
    ### Bateleur
      summary(M_151_BateleurBateleur)
      plot(allEffects(M_151_BateleurBateleur),selection = 1,main=paste0(spp[1,2]))
      Anova(M_151_BateleurBateleur)
      # controlling for month
      summary(Mo_151_BateleurBateleur)
      plot(allEffects(Mo_151_BateleurBateleur),selection = 1,main=paste0(spp[1,2]))
    
    ### MartialEagle
      summary(M_142_MartialEagle)
      plot(allEffects(M_142_MartialEagle),main=paste0(spp[2,2]),selection = 1)
      
      # controlling for month
      summary(Mo_142_MartialEagle)
      plot(allEffects(Mo_142_MartialEagle),main=paste0(spp[2,2]),selection = 1)
    
    ### Southern BandedSnake-eagle
      summary(`M_147_Southern BandedSnake-eagle`)
      plot(allEffects(`M_147_Southern BandedSnake-eagle`),main=paste0(spp[3,2]),selection = 1)
      
      # controlling for month
      summary(`Mo_147_Southern BandedSnake-eagle`)
      plot(allEffects(`Mo_147_Southern BandedSnake-eagle`),main=paste0(spp[3,2]), selection =1)
      
    ### TawnyEagle
      summary(M_134_TawnyEagle)
      plot(allEffects(M_134_TawnyEagle),main=paste0(spp[4,2]),selection = 1)
      
      # controlling for month
      summary(Mo_134_TawnyEagle)
      plot(allEffects(Mo_134_TawnyEagle),main=paste0(spp[4,2]),selection = 1)
    
    ### AfricanMarsh-harrier
      summary(`M_167_AfricanMarsh-harrier`)
      plot(allEffects(`M_167_AfricanMarsh-harrier`),main=paste0(spp[5,2]),selection = 1) #large decrease
      
      # controlling for month
      summary(`Mo_167_AfricanMarsh-harrier`)
      plot(allEffects(`Mo_167_AfricanMarsh-harrier`),main=paste0(spp[5,2]),selection = 1)
      
    ### BlackHarrier
      summary(M_169_BlackHarrier)
      plot(allEffects(M_169_BlackHarrier),main=paste0(spp[6,2]),selection = 1) #large decrease
      
      # controlling for month
      summary(Mo_169_BlackHarrier)
      plot(allEffects(Mo_169_BlackHarrier),main=paste0(spp[6,2]),selection = 1)
      
    ### BatHawk
      summary(M_131_BatHawk)
      plot(allEffects(M_131_BatHawk),main=paste0(spp[7,2]),selection = 1)
      
      #controlling for month
      summary(Mo_131_BatHawk)
      plot(allEffects(Mo_131_BatHawk),main=paste0(spp[7,2]),selection = 1)
    
    ### Pel'sFishing-owl
      summary(`M_370_Pel'sFishing-owl`)
      plot(allEffects(`M_370_Pel'sFishing-owl`),selection = 1,main=paste0(spp[8,2]))
      # controlling for month
      summary(`Mo_370_Pel'sFishing-owl`)
      plot(allEffects(`Mo_370_Pel'sFishing-owl`),selection = 1,main=paste0(spp[8,2]))
      
    ### BeardedVulture
      summary(M_150_BeardedVulture)
      plot(allEffects(M_150_BeardedVulture),selection = 1,main=paste0(spp[9,2]))
      
      #controlling for month
      summary(M_150_BeardedVulture)
      plot(allEffects(M_150_BeardedVulture),selection = 1,main=paste0(spp[9,2]))
      
    ### CapeVulture
      summary(M_106_CapeVulture)
      plot(allEffects(M_106_CapeVulture),selection = 1,main=paste0(spp[10,2]))
      
      # controlling for month
      summary(Mo_106_CapeVulture)
      plot(allEffects(Mo_106_CapeVulture),selection = 1,main=paste0(spp[10,2]))
      
    ### HoodedVulture
      summary(M_110_HoodedVulture)
      plot(allEffects(M_110_HoodedVulture),selection = 1,main=paste0(spp[11,2]))
      
      # controlling for month
      summary(Mo_110_HoodedVulture)
      plot(allEffects(Mo_110_HoodedVulture),selection = 1,main=paste0(spp[11,2]))
      
    ### Lappet-facedVulture
      summary(`M_108_Lappet-facedVulture`)
      plot(allEffects(`M_108_Lappet-facedVulture`),selection = 1,main=paste0(spp[12,2]))
      
      # controlling for month
      summary(`Mo_108_Lappet-facedVulture`)
      plot(allEffects(`Mo_108_Lappet-facedVulture`),selection = 1,main=paste0(spp[12,2]))
      
    ### White-backedVulture
      summary(`M_107_White-backedVulture`)
      plot(allEffects(`M_107_White-backedVulture`),selection = 1,main=paste0(spp[13,2]))
      
      # controlling for month
      summary(`Mo_107_White-backedVulture`)
      plot(allEffects(`Mo_107_White-backedVulture`),selection = 1,main=paste0(spp[13,2]))
      
    ### White-headedVulture
      summary(`M_109_White-headedVulture`)
      plot(allEffects(`M_109_White-headedVulture`),selection = 1,main=paste0(spp[14,2]))
      
      # controlling for month
      summary(`Mo_109_White-headedVulture`)
      plot(allEffects(`Mo_109_White-headedVulture`),selection = 1,main=paste0(spp[14,2]))
      