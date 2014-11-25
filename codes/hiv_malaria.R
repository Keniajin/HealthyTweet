library(tm)
library(ggplot2)
library(doBy) #used to summarize

#load the data in R
hivData <- read.csv("data/Tweets_HIV.csv")
head(hivData)
names(hivData)

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

#

total.nfund <- summaryBy(hivFund~tweetMY + hivFund , data=hivData[!is.na(hivData$hivFund) & !is.na(hivData$tweetMY) ,], FUN=function(x) c(count=sum(!is.na(x)))) 
ggplot(total.nfund,aes(tweetMY,hivFund.count,fill=hivFund))+
  geom_bar(stat="identity",position="dodge") + ylab("Mentioned") + xlab("Month of Tweet") +  ggtitle("HIV Tweets") + 
  theme(plot.title = element_text(lineheight=2.8, face="bold") ) 

#use word cloud to get the famous word
library("wordcloud")
contentHIV <- hivData$content[1:3]
contentHIV <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", toString(contentHIV))
contentHIV <- gsub("#hiv | #aids | \\# | hiv | aids | #AIDS | #HIV |HIV | AIDS", "", contentHIV)
wordcloud(contentHIV)

tweetHIV <- hivData$content
tweetHIV <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetHIV)
tweetHIV <- gsub("#hiv|#aids|\\# |hiv|aids|#AIDS|#HIV|HIV|AIDS|HIV\\/AIDS|via|http", "", tweetHIV)
tweetHIV <- gsub("â€","", tweetHIV)
tweetHIV <- gsub("hiv","", tweetHIV)
tweetHIV <- toString(tweetHIV)

wordcloud(tweetHIV, scale=c(5,0.5), min.freq = 300 ,
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))





