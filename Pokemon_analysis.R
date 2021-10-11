# import library required
library(dplyr)
library(psych)
library(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(GGally)
library(olsrr)



####################################
###Data Observation and Cleansing###
####################################

# The first observation
load("pokemon.Rdata")
head(pok,1)
# primary observe of data
summary(pok)
# checking unreasonable items in age
pok_age_s0 <- pok[pok$age <0,]
pok_age_s0
pok_age_l100 <- pok[pok$age >100,]
pok_age_l100
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
# Attitude
alpha(pok_new[7:12], check.keys=TRUE)
# steps Attitude
alpha(pok_new[13:18], check.keys=TRUE)
# Behaviour
alpha(pok_new[19:24], check.keys=TRUE)
# Pokemon Behviour
alpha(pok_new[27:29], check.keys=TRUE)

#@ Since alpha of behaviour and Pokemon behaviour are "Acceptable"
#@ we group them together

# variable grouping
Attitude <- rowMeans(pok_new[7:12])
StepAttitude <- rowMeans(pok_new[13:18])
Behqviour <- rowMeans(pok_new[19:24])
PokemonBehaviour <- rowMeans(pok_new[27:29])

# create new dataset
Pok_Grouped <- pok_new[c(4:6)]
Pok_Grouped$Attitude <- Attitude
Pok_Grouped$StepsAttitude <- StepAttitude
Pok_Grouped$PhyscialActivity <- Behaviour
Pok_Grouped$PokemonGo_AppUsage <- pok_new$app_usage_PokemonGoApp_pokemonusage1
Pok_Grouped$social_sharing <- pok_new$social_sharing
Pok_Grouped$PokemonRelate_Behaviour <- PokemonBehaviour
head(Pok_Grouped)

summary(Pok_Grouped)



########################
###Data Visualisation###
########################

ggpairs(Pok_Grouped)

boxplot(PhyscialActivity~education,
        data=Pok_Grouped,
        main="Different boxplots for gender",
        xlab="Month Number",
        ylab="Amount of Physical Activity"
)

boxplot(PhyscialActivity~Gender,
        data=Pok_Grouped,
        main="Different boxplots for Education Level",
        xlab="Education Level",
        ylab="Amount of Physical Activity"
)



########################
###Model Constructing###
########################

###Simple linear model###
# Relations between all variables and Physical Acticity
Pok_Linear <- lm(PhyscialActivity ~ . , data = Pok_Grouped)

ols_step_best_subset(Pok_Linear)


##Stepwise Selection
stepwise.Pok_Linear <- stepAIC(Pok_Linear, direction = "both", 
                               trace = TRUE)
stepwise.Pok_Linear
#@ According the AIC criteria, stewise.Pok_Linear model is the best model with 
#@ smallest AIC. However, "StepsAttutide" and "Attitude" having the same aspects,
#@ Thus, we should drop one. According to the result, Drop of "Attitude" only cause
#@ small rise, so we drop "Attitude" 
Pok_Linear_final <- lm(PhyscialActivity ~ age + education + StepsAttitude + 
                         PokemonGo_AppUsage + PokemonRelate_Behaviour,
                       data = Pok_Grouped)
summary(Pok_Linear_final)



#####################################
###Data Visualisation of new model###
#####################################

###assumption checking###

par(mfrow = c(2, 2))
plot(Pok_Linear)
dev.off()
par(mfrow = c(2, 2))
plot(Pok_Linear_final)

###Relations visualisation###

Pok.Grouped_New <- Pok_Grouped
Pok.Grouped_New$Attitude <- NULL
Pok.Grouped_New$social_sharing <- NULL

ggpairs(Pok.Grouped_New)

par(mfrow = c(2, 3))
for(i in 1:7){
        if(i != 2)
                boxplot(Pok.Grouped_New[,i]~education, data=Pok.Grouped_New)
}
for(i in 1:7){
        if(i != 3)
                boxplot(Pok.Grouped_New[,i]~Gender, data=Pok.Grouped_New)
}
