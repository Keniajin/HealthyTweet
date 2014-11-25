#check for and/or install dependencies
need<-c("shiny","ggplot2","tm","doBy","wordcloud") #packages of interest

for(i in 1:length(need)){
  if(require(need[i], character.only = TRUE)==FALSE){install.packages(need[i]);library(need[i], character.only = TRUE)} else { library(need[i],character.only = TRUE)}
}

library(shiny)
library(ggplot2) #make graphs in R
library(tm)
library(doBy)
library(wordcloud)

#load the datasets
hivData <- read.csv("data/Tweets_HIV.csv")
malariaData <- read.csv("data/Tweets_Malaria.csv")
pneumoniaData <- read.csv("data/Tweets_Pneumonia.csv")

# The list of datasesr
datas <<- list("Malaria Data" = "malaria",
               "HIV Data" = "hiv",
               "Pneumonia Data" = "pneumonia")

#cleaning the HIV
#people who tweeted HIV and Fund
hivData$hivFund[grepl("[Ff][Uu][Nn][Dd] | [Mm][Oo][Nn][Ee][Yy]" ,hivData$content)] <- 1 #mentioned funding
hivData$hivFund[grepl("[Cc][Oo][Nn][Dd][Oo][Mm][Ss]" ,hivData$content)] <- 2 # mentioned condoms

hivData$hivFund[grepl("[Vv][Aa][Cc][Cc][Ii][Nn][Ee]"   ,hivData$content)] <- 3 # mentioned vaccines

hivData$hivFund[grepl("[Aa][Bb][Ss][Tt][Aa][Ii][Nn]"   ,hivData$content)] <- 4 # mentioned abstain
hivData$hivFund[grepl("[Gg][Aa][Yy] | [Ll][Ee][Ss][Bb][Ii][Aa][Nn]"   ,hivData$content)] <- 5 # mentioned same sex
table(hivData$hivFund)
hivData$hivFund <- factor(hivData$hivFund,
                          levels = c(1,2,3 , 4,5),
                          labels = c("Fund", "Condoms", "Vaccines", "Abstain" , "Same sex")) 

#generate dates in R
hivData$tweetDate <- as.Date(hivData$firstpost_date ,"%d/%m/%y")
summary(hivData$tweetDate)
hivData$tweetMY <- format(hivData$tweetDate, "%b/%Y") 

tweetHIV <- hivData$content
tweetHIV <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetHIV)
tweetHIV <- gsub("#hiv|#aids|\\# |hiv|aids|#AIDS|#HIV|HIV|AIDS|HIV\\/AIDS|via|http", "", tweetHIV)
tweetHIV <- gsub("â€","", tweetHIV)
tweetHIV <- gsub("hiv","", tweetHIV)
tweetHIV <- toString(tweetHIV)


#cleaning the malaria
malariaData$malFund[grepl("[Ff][Uu][Nn][Dd] | [Mm][Oo][Nn][Ee][Yy]" ,malariaData$content)] <- 1 #mentioned funding
malariaData$malFund[grepl("[Nn][Ee][Tt] |[Nn][Ee][Tt][Ss] " ,malariaData$content)] <- 2 # mentioned net

malariaData$malFund[grepl("[Vv][Aa][Cc][Cc][Ii][Nn][Ee]"   ,malariaData$content)] <- 3 # mentioned vaccines

malariaData$malFund[grepl("[Mm][Oo][Ss][Qq][Uu][Ii][Tt][Oo]"   ,malariaData$content)] <- 4 # mentioned mosquito
malariaData$malFund[grepl("[Ee][Nn][Dd] | [Ff][Ii][Nn][Ii][Ss][Hh]"   ,malariaData$content)] <- 5 # mentioned same finish
table(malariaData$malFund)
malariaData$malFund <- factor(malariaData$malFund,
                          levels = c(1,2,3 , 4,5),
                          labels = c("Fund", "Net", "Vaccines", "Mosquito" , "End")) 

#generate dates in R
malariaData$tweetDate <- as.Date(malariaData$firstpost_date ,"%d/%m/%y")
summary(malariaData$tweetDate)
malariaData$tweetMY <- format(malariaData$tweetDate, "%b/%Y") 

tweetmalaria <- malariaData$content
tweetmalaria <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetmalaria)
tweetmalaria <- gsub("#malaria|\\# |malaria|#[mM][aA][lL][aA][rR][iI][aA]|[mM][aA][lL][aA][rR][iI][aA]|via|http", "", tweetmalaria)
tweetmalaria <- gsub("â€","", tweetmalaria)
tweetmalaria <- gsub("malaria","", tweetmalaria)
tweetmalaria <- toString(tweetmalaria)

#cleaning the pneumonia
pneumoniaData$pneumFund[grepl("[Ff][Uu][Nn][Dd] | [Mm][Oo][Nn][Ee][Yy]" ,pneumoniaData$content)] <- 1 #mentioned funding
pneumoniaData$pneumFund[grepl("[Tt][Rr][Ee][Aa][Tt] | [Cc][Uu][Rr][Ee]" ,pneumoniaData$content)] <- 2 # mentioned treat

pneumoniaData$pneumFund[grepl("[Vv][Aa][Cc][Cc][Ii][Nn][Ee]"   ,pneumoniaData$content)] <- 3 # mentioned vaccines

pneumoniaData$pneumFund[grepl("[Kk][Ii][Ll][Ll] | [Dd][Ee][Aa][Tt][Hh][Ss]"   ,pneumoniaData$content)] <- 4 # mentioned killer
pneumoniaData$pneumFund[grepl("[Cc][Oo][Uu][Gg][Hh] | [Ff][Ee][Vv][Ee][Rr]"   ,pneumoniaData$content)] <- 5 # mentioned fever
table(pneumoniaData$pneumFund)
pneumoniaData$pneumFund <- factor(pneumoniaData$pneumFund,
                              levels = c(1,2,3 , 4,5),
                              labels = c("Fund", "Cure", "Vaccines", "Killer" , "Fever")) 

#generate dates in R
pneumoniaData$tweetDate <- as.Date(pneumoniaData$firstpost_date ,"%d/%m/%y")
summary(pneumoniaData$tweetDate)
pneumoniaData$tweetMY <- format(pneumoniaData$tweetDate, "%b/%Y") 

tweetpneumonia <- pneumoniaData$content
tweetpneumonia <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetpneumonia)
tweetpneumonia <- gsub("#pneumonia|\\# |PNEUMONIA|#PNEUMONIA|pneumonia|via|http|#Pneumonia", "", tweetpneumonia)
tweetpneumonia <- gsub("â€","", tweetpneumonia)
tweetpneumonia <- gsub("pneumonia","", tweetpneumonia)
tweetpneumonia <- toString(tweetpneumonia)
#summaries
total.nHIV <- summaryBy(hivFund~tweetMY + hivFund , data=hivData[!is.na(hivData$hivFund) & !is.na(hivData$tweetMY) ,], FUN=function(x) c(count=sum(!is.na(x)))) 
total.nmalaria <- summaryBy(malFund~tweetMY + malFund , data=malariaData[!is.na(malariaData$malFund) & !is.na(malariaData$tweetMY) ,], FUN=function(x) c(count=sum(!is.na(x)))) 
total.npneumonia <- summaryBy(pneumFund~tweetMY + pneumFund , data=pneumoniaData[!is.na(pneumoniaData$pneumFund) & !is.na(pneumoniaData$tweetMY) ,], FUN=function(x) c(count=sum(!is.na(x)))) 

