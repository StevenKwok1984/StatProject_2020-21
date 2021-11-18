# import necessary libraries
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

# Calculate Cronbach's Alpha for grouping justification
## Attitude
alpha(pok_new[7:18], check.keys=TRUE)
# Physical Behaviour
alpha(pok_new[19:24], check.keys=TRUE)
# Pokemon Go related behw=aviour Behviour
alpha(pok_new[27:29], check.keys=TRUE)
#@ Since all alpha scores of targeted series of variables are "Acceptable",
#@ we grouped them together
# variable grouping
Attitude <- rowMeans(pok_new[7:18])
Behaviour <- rowMeans(pok_new[19:24])
PokemonBehaviour <- rowMeans(pok_new[27:29])

# create new dataset with grouped data
Pok_Grouped <- pok_new[c(4:6)]
Pok_Grouped$Attitude <- Attitude
Pok_Grouped$PhysicalActivity <- Behaviour
Pok_Grouped$PokemonGo_AppUsage <- pok_new$app_usage_PokemonGoApp_pokemonusage1
Pok_Grouped$social_sharing <- pok_new$social_sharing
Pok_Grouped$PokemonGo_Relate.Behaviour <- PokemonBehaviour
# show the first few lines of the dataset
head(Pok_Grouped)
# count number of records
count(Pok_Grouped)



########################
###Data Visualisation###
########################

# libraries required for visualisation
library(ggplot2)
library(GGally)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

###Relations visualisation###
# scatter plot
ggpairs(Pok_Grouped)

# gender vs remaining
par(mfrow = c(2, 2))
boxplot(education~Gender, data=Pok_Grouped, names=c("Female","Male"))
boxplot(Attitude~Gender, data=Pok_Grouped, names=c("Female","Male"))
boxplot(PhysicalActivity~Gender, data=Pok_Grouped, names=c("Female","Male"))
boxplot(PokemonGo_Relate.Behaviour~Gender, data=Pok_Grouped, 
        names=c("Female","Male"))
dev.off()

# education level vs remaining
par(mfrow = c(2, 2))
boxplot(age~education, data=Pok_Grouped)
boxplot(Attitude~education, data=Pok_Grouped)
boxplot(PhysicalActivity~education, data=Pok_Grouped)
boxplot(PokemonGo_AppUsage~education, data=Pok_Grouped)
dev.off()



########################
###Model Constructing###
########################

###Linear model###
Pok.Linear <- glm(PhysicalActivity ~ .^2 + I(age^2) + I(education^2) + 
                     + I(Attitude^2) + I(PokemonGo_AppUsage^2) +
                    I(social_sharing^2) + I(PokemonGo_Relate.Behaviour^2), 
                  data=Pok_Grouped)
# full model summary
summary(Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Pok.Linear)


## variable selection
# use multiple for discovering best model
Final_Pok.Linear <- stepAIC(Pok.Linear)
# model observation
summary(Final_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Final_Pok.Linear)
dev.off()

# model comparison
Comp_Pok.Linear <- glm(formula = PhysicalActivity ~ age + education + Gender +
                         Attitude + PokemonGo_AppUsage + 
                         I(Attitude^2) + age:education + 
                         education:Attitude, data = Pok_Grouped)
summary(Comp_Pok.Linear)



############
###Result###
############


# O1: plot for observing relation between Number of app usage and
# the amount of Physical Activity
summary(Selected_Pok.Linear)
plot(PhyscialActivity~PokemonGo_AppUsage, data=Pok_Grouped)
#@ show nothing. But in summary, negative relation

# O2: plot relations between PokemonGo_AppUsage and PokemonGo_Relate.Behaviour
plot(PokemonGo_Relate.Behaviour~PokemonGo_AppUsage, data=Pok_Grouped)

# o3 
summary(Selected_Pok.Linear)

# O4 relations between Gender and education
boxplot(education~Gender, data=Pok_Grouped, names=c("Female", "Male"))
#@ seems female have higher average education level
boxplot(Attitude~Gender, data=Pok_Grouped, names=c("Female", "Male"),
        main="relations between Gender and Attitude ")
#@ female have more positive attitude, but not so obvious with overlapping
boxplot(Attitude~education, data=Pok_Grouped, 
        main="relations between education and Attitude") 
abline(h=5.33, col = "Red", lty = 5)
#@ higher education is usually higher than first three

