args <- commandArgs(TRUE)
 
dsince <- args[1]
duntil <- args[2]
dhashtag <- args[3]

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library("wordcloud")
library("tm")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"

####Switch Between Keys When "Rate limited .... blocking for a minute and retrying up to 119 times ..." appears

# consumer_key = "hIigoH5u1alM986aA9zHojwMT"
# consumer_secret = "C5sROILDhdepOs4MILCo1MHFIhELBPt8GDaOA62d72MKp1MtIL"
# access_token = "3237786421-f5y2kLaeOcK3YJnB80CPyW1VQq9zHKEaTbjGpzz"
# access_secret = "FVbL7Nl8ElwDYGoilwBD7RCMkNhr0kQ6QTULnILBtSqZt"
consumer_key = "j2WBa4FY4ugV8A4QZRhD4Fxva"
consumer_secret = "1zgKsG4nkTvrVWw9pWDPS6mwxWezzuLkIMTyB5LfhOo5QkWhf1"
access_token = "3041165352-lbylH8f8LHmucbzjkPPzPDG3W55dHTpbmQatfMg"
access_secret = "mIQ1QzMFP9oK1Ecc7POmrYfAuKevEr9s8vzuUizci90Em"
setup_twitter_oauth(consumer_key,consumer_secret,access_token=access_token,access_secret=access_secret)
score.sentiment=function(sentences, pos.words,neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores=laply(sentences, function(sentence, pos.words,neg.words) 
  {
    sentence=gsub("[^[:alnum:]///' ]", "", sentence)
    sentence=gsub('[[:punct:]]','',sentence)
    sentence=gsub('[[:cntrl:]]','',sentence)
    sentence=gsub('\\d+','',sentence)
    sentence=tolower(sentence)
    word.list=str_split(sentence, '\\s+')
    words=unlist(word.list)
    pos.matches=match(words, pos.words)
    neg.matches=match(words, neg.words)
    pos.matches=!is.na(pos.matches)
    neg.matches=!is.na(neg.matches)
    score=sum(pos.matches)-sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df=data.frame(score=scores, text=sentences)
  return(scores.df)
}
hu.liu.pos=scan('positive-words.txt', what='character', comment.char=';')
hu.liu.neg=scan('negative-words.txt', what='character', comment.char=';')
pos.words=c(hu.liu.pos,'upgrade')
neg.words=c(hu.liu.neg,'wtf','wait','waiting','epicfail','mechanical')


	fname = "tweetrestweets.csv" #paste(hashtags[i],'.csv', sep='');
      hash = dhashtag;
      unemployment.list <- searchTwitter(hash, n=100, since=dsince, until=duntil);
      if(!(length(unemployment.list)>0)) next;
      unemployment.df = twListToDF(unemployment.list)
      write.table(unemployment.df,fname, row.names=F,append=T, sep=',', col.names=T)


      resfname = "tweetres.csv" #paste(hashtags[i],'res','.csv', sep='');
  DatasetUnemployment <- read.csv(fname)
  DatasetUnemployment$text <- as.factor(DatasetUnemployment$text) 
  Unemployment.scores=score.sentiment(DatasetUnemployment $text,pos.words,neg.words, .progress='text')
  write.csv(Unemployment.scores,file=resfname,row.names=TRUE)
  png(filename="tweetrestemp.png", width=500, height=500)
  hist(Unemployment.scores$score, xlab="ScoreofTweets",col="lightblue",main="Collective Sentiment Analysis");
  dev.off()
  
 
# png(filename="temp.png", width=500, height=500)
# hist(x, col="lightblue")
# dev.off()