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
pok_cleaned <- pok[pok$ATTENTION_filter1=="Disagree",]
pok_cleaned$ATTENTION_filter1 <- NULL
summary(pok_cleaned)

