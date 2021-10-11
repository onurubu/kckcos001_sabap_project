

ME <- read.csv("~/Teaching/BIO2019S Quantitative Biology/BIO3019S projects/BIO3019S project Amar/142ME.csv")
 View(ME)



 #install.packages("Rcpp")
 #install.packages("nlme")
 #install.packages("lme4")
 #install.packages("effects")
 #install.packages("MuMIn")
 #install.packages("AICcmodavg")


library(nlme)
library(lme4)
library(MASS) # to call quasi families
library(effects) # for plotting mixed effect models

# model selection:
library(MuMIn) # for model ranking using AIC scores
library(AICcmodavg) # for model averaging (what you might not need for now)l
library(dplyr)
library(Rcpp) # the glm below seems to need to added in to work for some reason




ME$year<- substring(ME$StartDate ,7,10) # this just pulls out the year variable from the date column

ME$month<- substring(ME$StartDate ,4,5) # this just pulls out the month variable from the date column
# we might want to explore controlling for this variable

ME$year<- as.numeric(ME$year) # this makes sure year is treated as a continuous term
str(ME) # this looks at the structure of the df
summary(ME) # summary of the df

  


mess<- subset(ME, Pentad %in% unique(Pentad[bin == 1])) # this selects only those pentads where the species has ever been present
# I included this because the data was too big otherwise
summary(mess)
str(mess)

# This creaates a new df (called ME2), which selects all the records for each pentad which has over a certain number of 
# records - eg. 100 records - it adds a flag to those pentads which qualify based on the condition specified

ME2 = mess %>% 
  group_by(Pentad) %>% 
  mutate(duplicate.flag = n() > 100) 
  



# Then, you can use filter to subset each group of data, as needed:
# here, we select only those will X repeat records, and also only those with data since sabap2 started
  
ME3 = ME2 %>% filter(duplicate.flag) %>% filter(year>2006)
summary(ME3)
str(ME3)

#  this would be for selectig only unqiue values:  filter(!duplicate.flag)

ME3$year <- as.numeric(ME3$year)
  
  

m1 <- glm(bin ~ year + Pentad , family = binomial, data=ME3) # instead this controls for pentad as a fixed effect
#m1 <- glmer(bin ~ year + (1|Pentad), family = binomial, data=ME3) - this is a mixed effects model - but takes too long to run
summary (m1)
plot(allEffects(m1))


