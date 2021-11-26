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
library(ggpubr)
a <- ggplot(aes(x = age, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm")
b <- ggplot(aes(x = education, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm")
c <- ggplot(aes(x = Attitude, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm")
d <- ggplot(aes(x = PokemonGo_AppUsage, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm")
e <- ggplot(aes(x = social_sharing, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm")
f <- ggplot(aes(x = PokemonGo_Relate.Behaviour, y = PhysicalActivity), data = Pok_Grouped) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))
f
ggarrange(a, b, c, d, e, f,
          ncol = 3, nrow = 2)
dev.off()



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
Pok.Linear <- lm(PhysicalActivity ~ .^2 + I(age^2) + 
                     + I(Attitude^2) +
                     I(PokemonGo_Relate.Behaviour^2), 
                  data=Pok_Grouped)
summary(Pok.Linear)
anova(Pok.Linear)
AIC(Pok.Linear)

#Performing step-wise regression on Pok.Linear model using ols_step_both_aic()
library(olsrr)
ols_step_both_aic(Pok.Linear)
#Creating a new model selected using ols_step_both_aic()
Select_Pok.Linear <- lm(PhysicalActivity ~  age + PokemonGo_AppUsage + 
                          PokemonGo_Relate.Behaviour + I(education*Gender) + 
                          I(age*Attitude) + I(Attitude*PokemonGo_Relate.Behaviour) +
                          I(social_sharing*PokemonGo_Relate.Behaviour), data=Pok_Grouped)
summary(Select_Pok.Linear)
AIC(Select_Pok.Linear)

###obtain final model###
library(moderndive)
get_regression_table(Select_Pok.Linear)
#Removing insignificant variable from Pok.Linear2 model
Final_Pok.Linear <- lm(PhysicalActivity ~ age + PokemonGo_AppUsage +
                         PokemonGo_Relate.Behaviour + I(education*Gender) + 
                         I(age*Attitude)+
                         I(social_sharing*PokemonGo_Relate.Behaviour)
                         , data=Pok_Grouped)
summary(Final_Pok.Linear)
AIC(Final_Pok.Linear)

# justification for interaction between PokemonGo_Relate.Behaviour and social_sharing
ggplot(Pok_Grouped, aes(x = PokemonGo_Relate.Behaviour, y = PhysicalActivity,
                        color = as.factor(social_sharing))) +
  geom_jitter() +
  labs(x = "Pokemon Go Related Behaviour", y = "Amount of Physical Activity",
       color = "social sharing") +
  geom_smooth(method = "lm", se = FALSE)

# model assumption checking
par(mfrow = c(2, 2))
plot(Final_Pok.Linear)



############
###Result###
############

# O1: plot for observing relation between Number of app usage and
# the amount of Physical Activity
summary(Selected_Pok.Linear)
plot(PhyscialActivity~PokemonGo_AppUsage, data=Pok_Grouped)
#@ show nothing. But in summary, negative relation

# O2: relations between PokemonGo_AppUsage and PokemonGo_Relate.Behaviour
# model for further comparison
Comp_Pok.Linear <- lm(PhysicalActivity ~ age + PokemonGo_AppUsage +
                          I(education*Gender) + 
                          I(age*Attitude), data=Pok_Grouped)
summary(Comp_Pok.Linear)
# plot
plot(PokemonGo_Relate.Behaviour~PokemonGo_AppUsage, data=Pok_Grouped)

# o3 
summary(Final_Pok.Linear)

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

#testing

###Linear model###
Test_Pok.Linear <- lm(PhysicalActivity ~ .^2, data=Pok_Grouped)
# full model summary
summary(Test_Pok.Linear)
# assumption checking
par(mfrow = c(2, 2))
plot(Pok.Linear)
