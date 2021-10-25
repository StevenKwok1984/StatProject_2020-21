# import library required
library(dplyr)
library(psych)
library(olsrr)
library(car)



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
Pok_Grouped$PhyscialActivity <- Behaviour
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

par(mfrow = c(2, 3))
for(i in 1:7){
  if(i != 2)
    boxplot(Pok_Grouped[,i]~education, data=Pok_Grouped)
}
for(i in 1:7){
  if(i != 3)
    boxplot(Pok_Grouped[,i]~Gender, data=Pok_Grouped)
}



########################
###Model Constructing###
########################

###linear model###

# full model construction
Pok.Linear <- glm(Attitude ~ education*age, data = Pok_Grouped)
## variable selection
# use multiple for discovering best model
Selected_Pok.Linear <- stepAIC(Pok.Linear)
# model observation
summary(Selected_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Selected_Pok.Linear)
dev.off()


###log-linear model###
# full model construction
Pok.Log_Linear <- glm(log(PhyscialActivity) ~ education*age, data = Pok_Grouped)
## variable selection
# apply AIC
Selected_Pok.Log_Linear <- stepAIC(Pok.Log_Linear)

# model observation
summary(Selected_Pok.Log_Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Selected_Pok.Log_Linear)
dev.off()


###log10-linear model###
# full model construction
Pok.Log10_Linear <- glm(log10(PhyscialActivity) ~ education*age, data = Pok_Grouped)
## variable selection
# apply AIC
Selected_Pok.Log10_Linear <- stepAIC(Pok.Log10_Linear)
# model observation
summary(Selected_Pok.Log10_Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Selected_Pok.Log10_Linear)
dev.off()


###Gamma###

Pok.Gamma <- glm(PhyscialActivity ~ education*age, family = Gamma(link="identity"), 
                 data=Pok_Grouped)
## variable selection
# use multiple for discovering best model
final_ols <- ols_step_best_subset(Pok.Gamma)
Seleced_Pok.Gamma <- stepAIC(Pok.Gamma)
# model observation
summary(Seleced_Pok.Gamma)
# assumption checking
par(mfrow = c(2, 2))
plot(Seleced_Pok.Gamma)
dev.off()



# failed
###inverse-gaussian model###

# full model construction
Pok.invGaussian <- glm(PhyscialActivity ~ ., family=inverse.gaussian(link="identity")
                       , data = Pok_Grouped)
summary(Pok.invGaussian)

#assumption checking
par(mfrow = c(2, 2))
plot(Pok.invGaussian)
dev.off()

## variable selection
final_ols <- ols_step_best_subset(Pok.invGaussian)
# use multiple for discovering best model
Selected_Pok.invGaussian <- stepAIC(Pok.invGaussian)
# model observation
summary(Selected_Pok.invGaussian)
# assumption checking
par(mfrow = c(2, 2))
plot(Selected_Pok.invGaussian)

dev.off()


############
###Result###
############

summary(fit)