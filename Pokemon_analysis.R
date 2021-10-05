# import library required
library(dplyr)
library(psych)


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
Behaviour <- rowMeans(pok_new[19:24])
PokemonBehaviour <- rowMeans(pok_new[27:29])

# create new dataset
Pok_Grouped <- pok_new[c(1:6)]
Pok_Grouped$Attitude <- Attitude
Pok_Grouped$StepsAttitude <- StepAttitude
Pok_Grouped$Behaviour <- Behaviour
Pok_Grouped$PokemonGo_AppUsage <- pok_new$app_usage_PokemonGoApp_pokemonusage1
Pok_Grouped$social_sharing <- pok_new$social_sharing
Pok_Grouped$PokemonRelate_Behaviour <- PokemonBehaviour
head(Pok_Grouped)


########################
###Model Constructing###
########################

###Simple linear model###
Pokeon_Linear <- lm()
