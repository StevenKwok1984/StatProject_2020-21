# import library required
library(dplyr)

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
summary(pok_new)


################
###Regression###
################

###convert variables into numeric values###
pok_new[,4:31] <- pok_new[,4:31] %>% mutate_if(is.factor,as.numeric)
summary(pok_new)

