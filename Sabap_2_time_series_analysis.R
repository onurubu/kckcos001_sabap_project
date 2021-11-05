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
  library(effects)# for plotting mixed effect models
  library(emmeans)
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
  assign(paste0("M_",spp[k,1],"_",spp[k,2]),glm(bin ~ year + Pentad , family = binomial, data = dat)) # this controls for pentad as a fixed effect
  assign(paste0("Mo_",spp[k,1],"_",spp[k,2]),glm(bin ~ year + Pentad + month , family = binomial, data = dat))
  #m1 <- glmer(bin ~ year + (1|Pentad), family = binomial, data = ME3) - this is a mixed effects model - but takes too long to run
  
  {if (k==nrow(spp)){beep(3)}}  #this line is only required if you want your PC to play a sound when your model creation is finished, if you don't have beepr installed you will receive an error, so be careful
  }

#### ANALYSING THE MODELS ####
  
## These functions show you the model output and the plot of the model:
  # summary (m1)
  # plot(allEffects(m1))

## These lines should be copied, replacing "m1" with the name of the desired model
  
    ### Bateleur
      # summary(M_151_BateleurBateleur)
      # plot(allEffects(M_151_BateleurBateleur),selection = 1,main=paste0(spp[1,2]))
      # Anova(M_151_BateleurBateleur)
      # controlling for month
      summary(Mo_151_BateleurBateleur) # *** Significant
      plot(allEffects(Mo_151_BateleurBateleur),selection = 1,main=paste0(spp[1,2]))
      Anova(Mo_151_BateleurBateleur)
      
      {r <-  exp(predict(Mo_151_BateleurBateleur,newdata = list(year=2017,Pentad="1755_2550",month="05")))/exp(predict(Mo_151_BateleurBateleur,newdata = list(year=2016,Pentad="1755_2550",month="05"))) 
      {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
      {if(r==1){print(paste0("No Change over year"))}}
      {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
        
      # {r <- as.numeric(exp(Mo_151_BateleurBateleur$coefficients[2]))
      # {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
      # {if(r==1){print(paste0("No Change over year"))}}
      # {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}

    ### MartialEagle
      # summary(M_142_MartialEagle)
      # plot(allEffects(M_142_MartialEagle),main=paste0(spp[2,2]),selection = 1)
      # Anova(M_142_MartialEagle)
      # controlling for month
      summary(Mo_142_MartialEagle)
      plot(allEffects(Mo_142_MartialEagle),main=paste0(spp[2,2]),selection = 1, ylab = "log(Reporting Rate)")
      Anova(Mo_142_MartialEagle)
      
        {r <-  exp(predict(Mo_142_MartialEagle,newdata = list(year=2017,Pentad="2850_2955",month="05")))/exp(predict(Mo_142_MartialEagle,newdata = list(year=2016,Pentad="2850_2955",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### Southern BandedSnake-eagle
      # summary(`M_147_Southern BandedSnake-eagle`)
      # plot(allEffects(`M_147_Southern BandedSnake-eagle`),main=paste0(spp[3,2]),selection = 1)
      # Anova(`M_147_Southern BandedSnake-eagle`)
      # controlling for month
      summary(`Mo_147_Southern BandedSnake-eagle`)
      plot(allEffects(`Mo_147_Southern BandedSnake-eagle`),main=paste0(spp[3,2]), selection =1)
      Anova(`Mo_147_Southern BandedSnake-eagle`)
      
        {r <-  exp(predict(`Mo_147_Southern BandedSnake-eagle`,newdata = list(year=2017,Pentad="2735_3215",month="05")))/exp(predict(`Mo_147_Southern BandedSnake-eagle`,newdata = list(year=2016,Pentad="2735_3215",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
      
    ### TawnyEagle
      # summary(M_134_TawnyEagle)
      # plot(allEffects(M_134_TawnyEagle),main=paste0(spp[4,2]),selection = 1)
      # Anova(M_134_TawnyEagle)
      # controlling for month
      summary(Mo_134_TawnyEagle)
      plot(allEffects(Mo_134_TawnyEagle),main=paste0(spp[4,2]),selection = 1)
      Anova(Mo_134_TawnyEagle)
      
      {r <-  exp(predict(Mo_134_TawnyEagle,newdata = list(year=2017,Pentad="2925_3025",month="05")))/exp(predict(Mo_134_TawnyEagle,newdata = list(year=2016,Pentad="2925_3025",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
      
    ### AfricanMarsh-harrier
      # summary(`M_167_AfricanMarsh-harrier`)
      # plot(allEffects(`M_167_AfricanMarsh-harrier`),main=paste0(spp[5,2]),selection = 1) #large decrease
      # Anova(`M_167_AfricanMarsh-harrier`)
      # controlling for month
      summary(`Mo_167_AfricanMarsh-harrier`) # *** Significant
      plot(allEffects(`Mo_167_AfricanMarsh-harrier`),main=paste0(spp[5,2]),selection = 1, xlab="log(Reporting Rate)")
      Anova(`Mo_167_AfricanMarsh-harrier`)
      
      {r <-  exp(predict(`Mo_167_AfricanMarsh-harrier`,newdata = list(year=2017,Pentad="2925_3025",month="05")))/exp(predict(`Mo_167_AfricanMarsh-harrier`,newdata = list(year=2016,Pentad="2925_3025",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### BlackHarrier
      # summary(M_169_BlackHarrier)
      # plot(allEffects(M_169_BlackHarrier),main=paste0(spp[6,2]),selection = 1) #large decrease
      # Anova(M_169_BlackHarrier)
      # controlling for month
      summary(Mo_169_BlackHarrier) # *** Significant
      plot(allEffects(Mo_169_BlackHarrier),main=paste0(spp[6,2]),selection = 1)
      Anova(Mo_169_BlackHarrier)
      
      {r <-  exp(predict(Mo_169_BlackHarrier,newdata = list(year=2017,Pentad="3415_1825",month="05")))/exp(predict(Mo_169_BlackHarrier,newdata = list(year=2016,Pentad="3415_1825",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### BatHawk
      # summary(M_131_BatHawk)
      # plot(allEffects(M_131_BatHawk),main=paste0(spp[7,2]),selection = 1)
      # Anova(M_131_BatHawk)
      #controlling for month
      summary(Mo_131_BatHawk)
      plot(allEffects(Mo_131_BatHawk),main=paste0(spp[7,2]),selection = 1)
      Anova(Mo_131_BatHawk)
     
      {r <-  exp(predict(Mo_131_BatHawk,newdata = list(year=2017,Pentad="2350_2955",month="05")))/exp(predict(Mo_131_BatHawk,newdata = list(year=2016,Pentad="2350_2955",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### Pel'sFishing-owl
      summary(`M_370_Pel'sFishing-owl`)
      plot(allEffects(`M_370_Pel'sFishing-owl`),selection = 1,main=paste0(spp[8,2]))
      Anova(`M_370_Pel'sFishing-owl`)
      
      {r <-  exp(predict(`M_370_Pel'sFishing-owl`,newdata = list(year=2017,Pentad="1755_2550",month="05")))/exp(predict(`M_370_Pel'sFishing-owl`,newdata = list(year=2016,Pentad="1755_2550",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
      # controlling for month
      # summary(`Mo_370_Pel'sFishing-owl`)
      # plot(allEffects(`Mo_370_Pel'sFishing-owl`),selection = 1,main=paste0(spp[8,2]))
      # Anova(`Mo_370_Pel'sFishing-owl`) ### EFFECT OF MONTH NOT SIGNIFICANT
      
    ### BeardedVulture
      # summary(M_150_BeardedVulture)
      # plot(allEffects(M_150_BeardedVulture),selection = 1,main=paste0(spp[9,2]))
      # Anova(M_150_BeardedVulture)
      #controlling for month
      summary(Mo_150_BeardedVulture)
      plot(allEffects(Mo_150_BeardedVulture),selection = 1,main=paste0(spp[9,2]))
      Anova(Mo_150_BeardedVulture)
      
      {r <-  exp(predict(Mo_150_BeardedVulture,newdata = list(year=2017,Pentad="3040_2710",month="05")))/exp(predict(Mo_150_BeardedVulture,newdata = list(year=2016,Pentad="3040_2710",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### CapeVulture
      # summary(M_106_CapeVulture)
      # plot(allEffects(M_106_CapeVulture),selection = 1,main=paste0(spp[10,2]))
      # Anova(M_106_CapeVulture)
      # controlling for month
      summary(Mo_106_CapeVulture) # *** Significant
      plot(allEffects(Mo_106_CapeVulture),selection = 1,main=paste0(spp[10,2]), xlab="log(Reporting Rate)")
      Anova(Mo_106_CapeVulture)
      
      {r <-  exp(predict(Mo_106_CapeVulture,newdata = list(year=2017,Pentad="3040_2710",month="05")))/exp(predict(Mo_106_CapeVulture,newdata = list(year=2016,Pentad="3040_2710",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### HoodedVulture
      # summary(M_110_HoodedVulture)
      # plot(allEffects(M_110_HoodedVulture),selection = 1,main=paste0(spp[11,2]))
      # Anova(M_110_HoodedVulture)
      # controlling for month
      summary(Mo_110_HoodedVulture)
      plot(allEffects(Mo_110_HoodedVulture),selection = 1,main=paste0(spp[11,2]))
      Anova(Mo_110_HoodedVulture)
      
      {r <-  exp(predict(Mo_110_HoodedVulture,newdata = list(year=2017,Pentad="2505_3150",month="05")))/exp(predict(Mo_110_HoodedVulture,newdata = list(year=2016,Pentad="2505_3150",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### Lappet-facedVulture
      summary(`M_108_Lappet-facedVulture`) # * Significant
      plot(allEffects(`M_108_Lappet-facedVulture`),selection = 1,main=paste0(spp[12,2]))
      Anova(`M_108_Lappet-facedVulture`)
      
      {r <-  exp(predict(`M_108_Lappet-facedVulture`,newdata = list(year=2017,Pentad="2900_2425",month="05")))/exp(predict(`M_108_Lappet-facedVulture`,newdata = list(year=2016,Pentad="2900_2425",month="05"))) 
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
      # controlling for month
      # summary(`Mo_108_Lappet-facedVulture`)
      # plot(allEffects(`Mo_108_Lappet-facedVulture`),selection = 1,main=paste0(spp[12,2]))
      # Anova(`Mo_108_Lappet-facedVulture`) ### EFFECT OF MONTH NOT SIGNIFICANT
      
    ### White-backedVulture
      # summary(`M_107_White-backedVulture`)
      # plot(allEffects(`M_107_White-backedVulture`),selection = 1,main=paste0(spp[13,2]))
      # Anova(`M_107_White-backedVulture`)
      # controlling for month
      summary(`Mo_107_White-backedVulture`)
      plot(allEffects(`Mo_107_White-backedVulture`),selection = 1,main=paste0(spp[13,2]))
      Anova(`Mo_107_White-backedVulture`)
      
      {r <-  exp(predict(`Mo_107_White-backedVulture`,newdata = list(year=2017,Pentad="3355_2320",month="05")))/exp(predict(`Mo_107_White-backedVulture`,newdata = list(year=2016,Pentad="3355_2320",month="05")))
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
    ### White-headedVulture
      summary(`M_109_White-headedVulture`) # *** Significant
      plot(allEffects(`M_109_White-headedVulture`),selection = 1,main=paste0(spp[14,2]))
      Anova(`M_109_White-headedVulture`)
      
      {r <-  exp(predict(`M_109_White-headedVulture`,newdata = list(year=2017,Pentad="2520_3135",month="05")))/exp(predict(`M_109_White-headedVulture`,newdata = list(year=2016,Pentad="2520_3135",month="05")))
        {if(r>1) {print(paste0(round((r-1)*100,4),"% increase"))}}
        {if(r==1){print(paste0("No Change over year"))}}
        {if(r<1) {print(paste0(round((1-r)*100,4),"% decrease"))}}}
      
      # controlling for month
      # summary(`Mo_109_White-headedVulture`)
      # plot(allEffects(`Mo_109_White-headedVulture`),selection = 1,main=paste0(spp[14,2]))
      # Anova(`Mo_109_White-headedVulture`) ### EFFECT OF MONTH NOT SIGNIFICANT
      
#### COMPARING ANALYSES ####

for (f in 1:nrow(spp)){
  {if (f==1) {deltaS1v2 <- c()}}
  {if (f==1) {deltaS2 <- c()}}
  cc <- ((exp(summary(get(paste0(spp[f,2],"_emm")))$emmean[2])/exp(summary(get(paste0(spp[f,2],"_emm")))$emmean[1]))-1)*100
  dd <- (((as.numeric(exp(get(paste0("Mo_",spp[f,1],"_",spp[f,2]))$coefficients[2]))))-1)*100
  deltaS1v2 <-  append(deltaS1v2, cc)        
  deltaS2 <-  append(deltaS2, dd) 
}
      
  {M_comparison <- lm(deltaS2~deltaS1v2)
  summary(M_comparison)
  plot(deltaS2~deltaS1v2, xlab = "Reporting rate change between SABAP 1 and 2 (%)", ylab = "SABAP 2 annual reporting rate change %",pch=16)
  abline(M_comparison)
  g <- spp[1:nrow(spp),2]
  text(deltaS1v2,deltaS2,g,pos=1)
                      }
#### END ####