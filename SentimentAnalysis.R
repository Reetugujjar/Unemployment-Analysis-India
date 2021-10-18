# install.packages(c("twitteR","plyr","stringr","ggplot2","ROAuth"), dependencies = T);
# install.packages("tm")

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library("wordcloud")
library("tm")
setwd('~/Desktop/Phoenix/')
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
hashtags <- c("unemployed", "canned", "downsized", "sacked", "pinkslip", "fired");
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
hu.liu.pos=scan('~/Desktop/positive-words.txt', what='character', comment.char=';')
hu.liu.neg=scan('~/Desktop/negative-words.txt', what='character', comment.char=';')
pos.words=c(hu.liu.pos,'upgrade')
neg.words=c(hu.liu.neg,'wtf','wait','waiting','epicfail','mechanical')

#number of iterations in the loop
iters<-6
#vector for appending output
ls<-vector('list',length=iters)

### mining Individual Hashtags :: ALL
for(i in 1:iters){
  fname = paste(hashtags[i],'.csv', sep='');
  hash = paste('#',hashtags[i],sep='');
  unemployment.list <- searchTwitter(hash, n=10000);
  unemployment.df = twListToDF(unemployment.list)
  write.table(unemployment.df,fname, row.names=F,append=T, sep=',', col.names=T)
}

###Collective mining :: ALL
for(i in 1:iters){
  fname = "Merged.csv" #paste(hashtags[i],'.csv', sep='');
  hash = paste('#',hashtags[i],sep='');
  unemployment.list <- searchTwitter(hash, n=10000);
  unemployment.df = twListToDF(unemployment.list)
  write.table(unemployment.df,fname, row.names=F,append=T, sep=',', col.names=T)
}

### mining Individual Hashtags :: DateWise
for(k in 4:1){
  begin=1;
  end=7;
  if (file.exists(paste(k,'',sep='')))
  {
    setwd(paste(k,'',sep=''))
  }else{
    dir.create(paste(k,'',sep=''), showWarnings = TRUE, recursive = FALSE, mode = "0755")
    setwd(paste(k,'',sep=''))
  }
    for(i in 1:iters){
      fname = "Merged.csv" #paste(hashtags[i],'.csv', sep='');
      hash = paste('#',hashtags[i],sep='');
      unemployment.list <- searchTwitter(hash, n=10000, since=paste('2015-06-',begin,sep=''), until=paste('2015-06-',end,sep=''));
      if(!(length(unemployment.list)>0)) next;
      unemployment.df = twListToDF(unemployment.list)
      write.table(unemployment.df,fname, row.names=F,append=T, sep=',', col.names=T)
    }
  setwd('..')
  begin = end+1;
  end = begin+7
}


###Collective Sentiment
library(RColorBrewer)
par(mfrow=c(1,1))

  fname = "Merged.csv" #paste(hashtags[i],'.csv', sep='');
  resfname = "score.csv" #paste(hashtags[i],'res','.csv', sep='');
  DatasetUnemployment <- read.csv(fname)
  DatasetUnemployment$text <- as.factor(DatasetUnemployment$text) 
  Unemployment.scores=score.sentiment(DatasetUnemployment $text,pos.words,neg.words, .progress='text')
  write.csv(Unemployment.scores,file=resfname,row.names=TRUE)

  hist(Unemployment.scores$score, xlab="ScoreofTweets",col=brewer.pal(9,"Set3"),main="Collective Sentiment Analysis");


##### Individual Sentiment
par(mfrow=c(2,3))
for(i in 1:iters){
fname = paste(hashtags[i],'.csv', sep='');
resfname = paste(hashtags[i],'res','.csv', sep='');
DatasetUnemployment <- read.csv(fname)
DatasetUnemployment$text <- as.factor(DatasetUnemployment$text) 
Unemployment.scores=score.sentiment(DatasetUnemployment $text,pos.words,neg.words, .progress='text')
write.csv(Unemployment.scores,file=resfname,row.names=TRUE)

hist(Unemployment.scores$score, xlab="ScoreofTweets",col=brewer.pal(9,"Set3"),main=paste('#',hashtags[i]));

##################################
################################## WORDCLOUD

library(RColorBrewer)

convention <- read.table("Merged.csv",
                          header = TRUE,
                          sep = ",", fill = TRUE)

r_stats_text <- as.factor(convention$text)
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
png("~/Desktop/Wordcloud.png", width=12,height=8, units='in', res=300)
wordcloud(r_stats_text_corpus, min.freq=5, max.words=100,colors = brewer.pal(8,"Dark2"))
dev.off()


########## FINAL ANALYSIS
a = c(1,2,3,4)
for(j in 1:4)
{
  setwd(paste(j,'',sep=''));
    fname = "Merged.csv";
    DatasetUnemployment <- read.csv(fname,row.names=NULL)
    a[j]=nrow(DatasetUnemployment)
  setwd('..')
}
lines(a, main="Unemployment Analysis", xlab="Weeks")
