library(dplyr)
library(tm)
library(tokenizers)
library(SnowballC)
library(textstem)
library(gutenbergr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(textmineR)
library(topicmodels)
library(LDAvis)
library(servr)
library(janitor)
library(purrr)
library(ROAuth)
library(plyr)
library(stringr)
library(tidytext)
library(textdata)
library(jsonlite)
library(syuzhet)
library(magrittr)
library(tidyr)
library(textreg)
library(posterior)

set.seed(1)

df <- fromJSON("https://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")

set.seed(178)
test<-sample_n(df,5000)
summary(test)
str(test)
head(test)

#Removing emoticons
test$review_text <- sapply(test$review_text,function(row) iconv(row, "latin1", "ASCII", sub="")) 

corpus<-iconv((test$review_text))
corpus<-Corpus(VectorSource(corpus))

inspect(corpus[17])

lex1<-get_sentiments("afinn")
lex2<-get_sentiments("bing")
lex3<-get_sentiments("nrc")

#Cleaning corpus
corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removeWords, stopwords("english"))
corpus<-tm_map(corpus, stemDocument)

corpus<-tm_map(corpus,removeWords, c("redmi", "phones", "also","amazon","using","oneplus","one","mobile","charger",
                                     "pubg","network","scanner","box","network","nord")) 
corpus<-tm_map(corpus, stripWhitespace)

reviews<-corpus

#Create term document
tdm<-TermDocumentMatrix(reviews)
tdm<-as.matrix(tdm)
tdm[1:10, 1:5]

#Bar plot of words
w<-rowSums(tdm)
w<-subset(w,w>=25)
barplot(w,las=2,col="blue", main = "Words with Highest Frequency") 

#Creating word cloud 
w<-sort(rowSums(tdm),decreasing = T)
set.seed(201)
wordcloud(words = names(w),
          freq = w,
          max.words = 200,
          random.order = F,
          min.freq = 5,
          colors=brewer.pal(8,"Dark2"),
          scale = c(3,0.5))

#Obtain sentiment scores 

sentiment_data<-iconv(test$review_text)

##NRC
s<-get_nrc_sentiment(sentiment_data)

#Calculate review wise score 
s$score<-s$positive-s$negative
s[1:10,]
s%>%count('score')
positive<-1651+388+113+37+16+5+3
negative<-1+21+99+510
Neutral<-2156

##afinn
s_afinn<-sentiment_data%>%get_sentiment("afinn")

#Bing
s_bing<-sentiment_data%>%get_sentiment("bing")


#including all scores in final dataset
score<-test%>%mutate(score_nrc=s$score,score_afinn=s_afinn,score_bing=s_bing)

score%>%count('score_nrc') 
score%>%count('score_afinn') 
score%>%count('score_bing') 

#Evaluation
score1<-score%>% select("review_text","review_rating","score_nrc","score_afinn","score_bing")
score2<-score1%>%separate(review_rating,c("rating","Rest"))
score2<-score2[-3]

score2%>%count('rating')
class(score2$rating)
score2$rating<-as.numeric(score2$rating)
str(score2)

hist(score2$rating, main = "Ratings", col = "light blue")

#Accuracy 
nrc<-(((sum(score2$rating>3 & score2$score_nrc>0))+(sum(score2$rating<3 & score2$score_nrc<0)))/
        (sum(score2$rating>3 & score2$score_nrc>0)+sum(score2$rating<3 & score2$score_nrc<0)+
           sum(score2$rating>3 & score2$score_nrc<0)+sum(score2$rating<3 & score2$score_nrc>0)))

afinn<-(((sum(score2$rating>3 & score2$score_afinn>0))+(sum(score2$rating<3 & score2$score_afinn<0)))/
          (sum(score2$rating>3 & score2$score_afinn>0)+sum(score2$rating<3 & score2$score_afinn<0)+
             sum(score2$rating>3 & score2$score_afinn<0)+sum(score2$rating<3 & score2$score_afinn>0)))

bing<-(((sum(score2$rating>3 & score2$score_bing>0))+(sum(score2$rating<3 & score2$score_bing<0)))/
         (sum(score2$rating>3 & score2$score_bing>0)+sum(score2$rating<3 & score2$score_bing<0)+
            sum(score2$rating>3 & score2$score_bing<0)+sum(score2$rating<3 & score2$score_bing>0)))


#Final dataset 
sum(score2$score_afinn=0)
final1<-score2%>% select("review_text","score_afinn")
final1<-final1%>%mutate(sentiment='positive')

final1<-final1 %>%
  mutate(sentiment= case_when(
    (score_afinn>0) ~ "positive",
    (score_afinn<0) ~ "negative"))

sum(is.na(final1$sentiment))

final<-na.omit(final1, final1$sentiment)

final%>%count('sentiment')  

positive<-final[which(final$sentiment == "positive"),]
negative<-final[which(final$sentiment == "negative"),]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Positive Reviews Topic Modelling 
positiveTM<-positive[-2]
positiveTM<-positiveTM[-2]

#Cleaning the emojis
positiveTM$review_text <- sapply(positiveTM$review_text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Creating corpus
pos_corp<-stringr::str_conv(positiveTM$review_text, 'UTF-8')
pos_corp<-Corpus(VectorSource(pos_corp))
print(pos_corp[[17]]$content)


#Tokenization 
pos_token<-tokenize_words(pos_corp$content)
print((pos_token[17]))

#Pre-processing 
pos_corp<-tm_map(pos_corp, tolower)
pos_corp<-tm_map(pos_corp, removePunctuation)
pos_corp<-tm_map(pos_corp, removeNumbers)
pos_corp<-tm_map(pos_corp, removeWords, stopwords("english"))
pos_corp<-tm_map(pos_corp, stemDocument)
pos_corp<-tm_map(pos_corp, lemmatize_words)
pos_corp<-tm_map(pos_corp, stripWhitespace)

print(pos_corp[56]$content)

#DTM- Term Frequency
pos_dtm<-DocumentTermMatrix(pos_corp) 

raw.sum=apply(pos_dtm,1,FUN = sum)
pos_dtm=pos_dtm[raw.sum!=0,]

pos_dtm 
findFreqTerms(pos_dtm,200)
findAssocs(pos_dtm,"good",corlimit = 0.2)

#Removing sparse term
pos_dtm_s<-removeSparseTerms(pos_dtm,0.99)
pos_dtm_s 
findFreqTerms(pos_dtm_s)

raw.sum=apply(pos_dtm_s,1,FUN = sum)
pos_dtm_s=pos_dtm_s[raw.sum!=0,] 


#Frequency table
pos_new<-as.matrix(pos_dtm_s)
pos_frequency<-colSums(pos_new)
pos_frequency<-sort(pos_frequency, decreasing=TRUE)
pos_doc_length<-rowSums(pos_new)
pos_frequency[1:50]

#Word cloud
pos_words<-names(pos_frequency)
wordcloud(pos_words[1:100], pos_frequency[1:100],rot.per=0.15,
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))


#Topic modelling with LDA
iter<-1000
coherence<-c()

for(i in (3:20)){
  pos_ldaOut<-LDA(pos_dtm_s,i,method = "Gibbs",
                  control = list(iter=iter, seed=1000))
  phi<-posterior(pos_ldaOut)$terms %>% as.matrix
  theta<-posterior(pos_ldaOut)$topics %>% as.matrix
  coherence_one<-mean(textmineR::CalcProbCoherence(phi=phi, dtm=pos_new))
  coherence<-append(coherence, coherence_one)
}

terms(pos_ldaOut,10)
k<-c(3:20) [which.max(coherence)]
print(k) 

coherence_mat<-data.frame(k=c(3:20),
                          coherence=coherence,
                          stringsAsFactors = FALSE)

ggplot(coherence_mat,aes(x=k, y=coherence))+
  geom_point()+
  geom_line(group=1)+
  ggtitle("Best Topic by Coherence Score") +theme_minimal()+
  scale_x_continuous(breaks=seq(1,31,1))+ylab("Coherence")


#Implementing model with suggested k value 
pos_ldaOut<-LDA(pos_dtm_s,k=7,method = "Gibbs",
                control = list(iter=iter, seed=1000))

terms(pos_ldaOut,20)


pos_phi<-posterior(pos_ldaOut)$terms%>%as.matrix
pos_theta<-posterior(pos_ldaOut)$topics%>%as.matrix()

pos_ldaOut_terms<-as.matrix(terms(pos_ldaOut,10))

#Grouping document by topics
pos_ldaOut_topics<-data.frame(topics(pos_ldaOut))
pos_ldaOut_topics$index<-as.numeric(row.names(pos_ldaOut_topics))
positiveTM$index<-as.numeric(row.names(positiveTM))
pos_datawithtopic<-merge(positiveTM, pos_ldaOut_topics, by='index', all.x = TRUE)
pos_datawithtopic<-pos_datawithtopic[order(pos_datawithtopic$index),]

pos_datawithtopic[0:10,]

pos_topicprob<-as.data.frame(pos_ldaOut@gamma)
pos_topicprob[0:10,1:7] 

#Visualizing with LDAVis
pos_vocab<-colnames(pos_phi)

pos_json_lda<-createJSON(phi=pos_phi, theta=pos_theta,
                         vocab=pos_vocab, doc.length = pos_doc_length,
                         term.frequency = pos_frequency)

serVis(pos_json_lda, out.dir = "vis", open.browser = TRUE)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Negative Reviews Topic Modelling 
negativeTM<-negative[-2]
negativeTM<-negativeTM[-2]

#Cleaning the emojis
negativeTM$review_text <- sapply(negativeTM$review_text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Creating corpus
neg_corp<-stringr::str_conv(negativeTM$review_text, 'UTF-8')
neg_corp<-Corpus(VectorSource(neg_corp))
print(neg_corp[[17]]$content)


#Tokenization 
neg_token<-tokenize_words(neg_corp$content)
print((neg_token[17]))

#Pre-processing
neg_corp<-tm_map(neg_corp, tolower)
neg_corp<-tm_map(neg_corp, removePunctuation)
neg_corp<-tm_map(neg_corp, removeNumbers)
neg_corp<-tm_map(neg_corp, removeWords, stopwords("english"))
neg_corp<-tm_map(neg_corp, stemDocument)
neg_corp<-tm_map(neg_corp, lemmatize_words)
neg_corp<-tm_map(neg_corp, stripWhitespace)

print(neg_corp[56]$content)

#DTM- Term Frequency
neg_dtm<-DocumentTermMatrix(neg_corp) 

raw.sum=apply(neg_dtm,1,FUN = sum)
neg_dtm=neg_dtm[raw.sum!=0,]

neg_dtm 
findFreqTerms(neg_dtm,50)
findAssocs(neg_dtm,"camera",corlimit = 0.2)

#Removing sparse term
neg_dtm_s<-removeSparseTerms(neg_dtm,0.99)
neg_dtm_s 
findFreqTerms(neg_dtm_s)

raw.sum=apply(neg_dtm_s,1,FUN = sum)
neg_dtm_s=neg_dtm_s[raw.sum!=0,] 

#Frequency table
neg_new<-as.matrix(neg_dtm_s)
neg_frequency<-colSums(neg_new)
neg_frequency<-sort(neg_frequency, decreasing=TRUE)
neg_doc_length<-rowSums(neg_new)

neg_frequency[1:50]

#Word cloud
neg_words<-names(neg_frequency)
wordcloud(neg_words[1:100], neg_frequency[1:100],rot.per=0.15,
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))


#Topic modelling with LDA
iter<-1000
coherence<-c()

for(i in (3:20)){
  neg_ldaOut<-LDA(neg_dtm_s,i,method = "Gibbs",
                  control = list(iter=iter, seed=1000))
  phi<-posterior(neg_ldaOut)$terms %>% as.matrix
  theta<-posterior(neg_ldaOut)$topics %>% as.matrix
  coherence_one<-mean(textmineR::CalcProbCoherence(phi=phi, dtm=neg_new))
  coherence<-append(coherence, coherence_one)
}

terms(neg_ldaOut,10)
k<-c(3:20) [which.max(coherence)]
print(k) 

coherence_mat<-data.frame(k=c(3:20),
                          coherence=coherence,
                          stringsAsFactors = FALSE)

ggplot(coherence_mat,aes(x=k, y=coherence))+
  geom_point()+
  geom_line(group=1)+
  ggtitle("Best Topic by Coherence Score") +theme_minimal()+
  scale_x_continuous(breaks=seq(1,31,1))+ylab("Coherence")


#Implementing model with suggested k value 
neg_ldaOut<-LDA(neg_dtm_s,k=7,method = "Gibbs",
                control = list(iter=iter, seed=1000))

terms(neg_ldaOut,10)

neg_phi<-posterior(neg_ldaOut)$terms%>%as.matrix()
neg_theta<-posterior(neg_ldaOut)$topics%>%as.matrix()

neg_ldaOut_terms<-as.matrix(terms(neg_ldaOut,10))

#Grouping document by topics
neg_ldaOut_topics<-data.frame(topics(neg_ldaOut))
neg_ldaOut_topics$index<-as.numeric(row.names(neg_ldaOut_topics))
negativeTM$index<-as.numeric(row.names(negativeTM))
neg_datawithtopic<-merge(negativeTM, neg_ldaOut_topics, by='index', all.x = TRUE)
neg_datawithtopic<-neg_datawithtopic[order(neg_datawithtopic$index),]

neg_datawithtopic[0:10,]

neg_topicprob<-as.data.frame(neg_ldaOut@gamma)
neg_topicprob[0:10,1:7] 

#Visualizing with LDAVis
neg_vocab<-colnames(neg_phi)

neg_json_lda<-createJSON(phi=neg_phi, theta=neg_theta,
                         vocab=neg_vocab, doc.length = neg_doc_length,
                         term.frequency = neg_frequency)

serVis(neg_json_lda, out.dir = "vis", open.browser = TRUE)
