# import library required
library(dplyr)
library(psych)
# library(olsrr)
library(car)
library(mgcv)



####################################
###Data Observation and Cleansing###
####################################

# loading dataset
load("pokemon.Rdata")
# observe data
head(pok,3)
# primary observe of data
summary(pok)
# checking number of missing value
sum(is.na(pok))
# checking unreasonable items in age
pok_age_s0 <- pok[pok$age <0,]
pok_age_s0
#@ This dataset is really good, without error or missing value

# However, according to the article, the records where the answer of 
# ATTENTION_filter1 is not "Disagree" should be filtered
pok_new <- pok[pok$ATTENTION_filter1=="Disagree",]
pok_new$ATTENTION_filter1 <- NULL
summary(pok_new)



#################################
###Preparation of Grouped data###
#################################

###convert variables into numeric values###
pok_new[,4:30] <- pok_new[,4:30] %>% mutate_if(is.factor,as.numeric)
summary(pok_new)

###Justify the grouping###

# Calculate Cronbach's Alpha
## Attitude & StepsAttitude
alpha(pok_new[7:18], check.keys=TRUE)
# Physical Behaviour
alpha(pok_new[19:24], check.keys=TRUE)
# Pokemon Behviour
alpha(pok_new[27:29], check.keys=TRUE)

#@ Since alpha of behaviour and Pokemon behaviour are "Acceptable"
#@ we group them together

# variable grouping
Attitude <- rowMeans(pok_new[7:18])
Behaviour <- rowMeans(pok_new[19:24])
PokemonBehaviour <- rowMeans(pok_new[27:29])

# create new dataset
Pok_Grouped <- pok_new[c(4:6)]
Pok_Grouped$Attitude <- Attitude
Pok_Grouped$PhysicalActivity <- Behaviour
Pok_Grouped$PokemonGo_AppUsage <- pok_new$app_usage_PokemonGoApp_pokemonusage1
Pok_Grouped$social_sharing <- pok_new$social_sharing
Pok_Grouped$PokemonGo_Relate.Behaviour <- PokemonBehaviour
head(Pok_Grouped)

summary(Pok_Grouped)



########################
###Data Visualisation###
########################

# libraries required
library(ggplot2)
library(GGally)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

###Relations visualisation###

ggpairs(Pok_Grouped)
dev.off()

par(mfrow = c(3, 3))
for(i in 1:8){
  if(i != 2)
    boxplot(Pok_Grouped[,i]~education, data=Pok_Grouped)
}
for(i in 1:8){
  if(i != 3)
    boxplot(Pok_Grouped[,i]~Gender, data=Pok_Grouped)
}
dev.off()


########################
###Model Constructing###
########################

###linear model###

# full model construction
Test_Pok.Linear <- gam(PhysicalActivity~s(age) + s(education) + Gender +
                         s(Attitude) + PokemonGo_AppUsage + social_sharing +
                         s(PokemonGo_Relate.Behaviour), data=Pok_Grouped)
summary(Test_Pok.Linear)
# assumption checking for full model
par(mfrow = c(2, 2))
gam.check(Test_Pok.Linear)
dev.off()


## variable selection
# use multiple for discovering best model
Test_Pok.Linear <- stepAIC(Test_Pok.Linear)
# model observation
summary(Test_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Test_Pok.Linear)
dev.off()





###Linear model###
Pok.Linear <- glm(PhysicalActivity ~ .^2 + I(age^2) + I(education^2) + 
                    + I(Gender^2) + I(Attitude^2) + I(PokemonGo_AppUsage^2) +
                    I(social_sharing^2) + I(PokemonGo_Relate.Behaviour^2), 
                  data=Pok_Grouped)
summary(Pok.Linear)

## variable selection
# use multiple for discovering best model
Select_Pok.Linear <- stepAIC(Pok.Linear)
# model observation
summary(Select_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Select_Pok.Linear)
dev.off()