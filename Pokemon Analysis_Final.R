# import library required
library(dplyr)
library(psych)
library(olsrr)
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

par(mfrow = c(3, 3))
for(i in 1:8){
  if(i != 2)
    boxplot(Pok_Grouped[,i]~education, data=Pok_Grouped)
}
for(i in 1:8){
  if(i != 3)
    boxplot(Pok_Grouped[,i]~Gender, data=Pok_Grouped)
}



########################
###Model Constructing###
########################

###Linear model###
Pok.Linear <- glm(PhysicalActivity ~ .^2, data=Pok_Grouped)
summary(Pok.Linear)
## variable selection
# use multiple for discovering best model
Selected_Pok.Linear <- stepAIC(Pok.Linear)
# model observation
summary(Selected_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Selected_Pok.Linear)
dev.off()



############
###Result###
############

# result observation by summary
summary(Selected_Pok.Gamma)

# Remove social sharing
New.Pok_Grouped <- Pok_Grouped
New.Pok_Grouped$social_sharing <- NULL
ggpairs(New.Pok_Grouped)

# O1: plot for observing relation between Number of app usage and amount of Physical Activity
summary(Selected_Pok.Gamma)
plot(PhyscialActivity~PokemonGo_AppUsage, data=Pok_Grouped)
#@ show nothing. But in summary, negative relation

# O2: plot relations between PokemonGo_AppUsage and PokemonGo_Relate.Behaviour
plot(PokemonGo_Relate.Behaviour~PokemonGo_AppUsage, data=Pok_Grouped)

# o3 
summary(Selected_Pok.Gamma)

# O4 relations between Gender and education
boxplot(education~Gender, data=Pok_Grouped, names=c("Female", "Male"))
#@ seems female have higher average education level
boxplot(Attitude~Gender, data=Pok_Grouped, names=c("Female", "Male"))
#@ female have more positive attitude, but not so obvious with overlapping
boxplot(Attitude~education, data=Pok_Grouped)
abline(h=5.33, col = "Red", lty = 5)
#@ higer education is usually higher than first three




# Relations visualization for education
par(mfrow = c(2, 3))
for(i in 1:7){
  if(i != 2)
    boxplot(Pok_Grouped[,i]~education, data=Pok_Grouped)
}
# Relations visualization for Gender
for(i in 1:7){
  if(i != 3)
    boxplot(Pok_Grouped[,i]~Gender, data=Pok_Grouped)
}
