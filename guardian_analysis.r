library(tm)
library(SnowballC)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)

#data source
library("readxl")
tbl<- read_excel("ArticleData.xlsx")
#head(tbl)

#Feature engineering
tbl1<-tbl
tbl1$year<-unlist(strsplit(tbl$Link[[5]],"/"))[5]
summary(tbl1$year)
for (i in 1:nrow(tbl)){
  if(unlist(strsplit(tbl$Link[[i]],"/"))[5] %in% c(1980:2016))
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[5]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[6] %in% c(1980:2016))
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[6]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[7] %in% c(1980:2016))   
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[7]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[8] %in% c(1980:2016))   
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[8]
  else 
    tbl1[i,"year"]<-NA
}

## list of categories
unique(tbl$Topic)  ## 2185 authors
library('plyr')
topic.freq<- count(tbl1$Topic)
topic.freq.sorted<-topic.freq[order(-topic.freq$freq),]

freqCorpus <- Corpus(VectorSource(tbl$Topic))
freqCorpus <- tm_map(freqCorpus, PlainTextDocument)
wordcloud(freqCorpus, max.words = 200, random.order = FALSE)

unique(tbl$Author) ## 190 authors
author.freq <- count(tbl$Author)
author.freq.sorted<-author.freq[order(-author.freq$freq),]


## count of list of articles written by each author across time
newtbl <- cbind(tbl1$Author, tbl1$Topic, tbl1$year)
aggr <- aggregate(tbl1$Topic~tbl1$Author+tbl1$year, FUN=length) 
colnames(aggr) <- c("Author","Year","Article_count")
sorted.author.article <- aggr[order(aggr$Author,aggr$Year),]


## Docs
poly <- tbl1[tbl1$Author == "Polly Toynbee",]
topic <- Corpus(VectorSource(poly$Topic))
topic <- tm_map(topic, removePunctuation)
topic <- tm_map(topic, removeNumbers)
topic <- tm_map(topic, tolower)
topic <- tm_map(topic, removeWords, stopwords("english"))
topic <- tm_map(topic, stemDocument)
topic <- tm_map(topic, stripWhitespace)
topic <- tm_map(topic, PlainTextDocument)
wordcloud(topic)

#Article
#poly <- tbl1[tbl1$Author == "Polly Toynbee",]
article <- Corpus(VectorSource(poly$Article))
article <- tm_map(article, removePunctuation)
article <- tm_map(article, removeNumbers)
article <- tm_map(article, tolower)
article <- tm_map(article, removeWords, stopwords("english"))
mystopwords <- c("says","take","like","make","will","one","can","every","many","may","just","much","last","still","get","yet","make","even","xcxa","less")
article <- tm_map(article, removeWords, mystopwords)
article <- tm_map(article, stemDocument)
article <- tm_map(article, stripWhitespace)
article <- tm_map(article, PlainTextDocument)
wordcloud(article,min.freq = 1500, max.words = 40)

#Creating Matrix
dtm <- DocumentTermMatrix(article)
dtm
tdm <- TermDocumentMatrix(article)
tdm

#Exploration
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
freq[head(ord,n=20)]
freq[tail(ord, n=20)] #top 20 frequent words throughtout his carrier

dtms <- removeSparseTerms(dtm, 0.3)
dtms
head(inspect(dtms))
freq1 <- colSums(as.matrix(dtms))
head(freq1)

findFreqTerms(dtm, lowfreq=1000)
wf <- data.frame(word=names(freq1), freq=freq1)   
head(wf)  


findAssocs(dtm, c("political" , "labour"), corlimit=0.30)
findAssocs(dtm, c("people" , "labour"), corlimit=0.30)

set.seed(1)
wordcloud(names(freq), freq, max.words = 40)

library(cluster)
d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit 
plot(fit, hang=-1)

##https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
library(topicmodels)

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
summary(ldaOut)
ldaOut.topics <- as.matrix(topics(ldaOut))

ldaOut.terms <- as.matrix(terms(ldaOut,6))
topicProbabilities <- as.data.frame(ldaOut@gamma)
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
