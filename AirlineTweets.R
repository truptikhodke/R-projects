#Loading the Airline tweets data and storing it in sms_raw variable
tweets_raw <- read.csv("Downloads/Tweets.csv")
str(tweets_raw$airline_sentiment)
str(tweets_raw$text)

#creating a table of number of positive,negative and neutral tweets
table(tweets_raw$airline_sentiment)

library(ggplot2)
plot(tweets_raw$airline_sentiment)

#installing and loading package tm
install.packages("tm")
library(tm)


#creating a corpus
tweets_corpus <- Corpus(VectorSource(tweets_raw$text))

#printing the corpus
print(tweets_corpus)

#viewing the contents of corpus with the inspect function
inspect(tweets_corpus[1:3])

library(SnowballC)
#using tm_map function to clean the corpus

corpus_clean <- tm_map(tweets_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, stripWhitespace)



#inspecting the claen corpus
inspect(corpus_clean[1:3])

#creating a sparse matrix with the help of DocumentTermMatrix function
tweets_dtm <- DocumentTermMatrix(corpus_clean)

#Dividing 75% data as train and 25% as test
nrow(tweets_raw)
tweets_train_labels <- tweets_raw[1:10980, ]
tweets_test_labels  <- tweets_raw[10981:14640, ]

#splitting dtm
tweets_data_train <- tweets_dtm[1:10980, ]
tweets_data_test  <- tweets_dtm[10981:14640, ]

#splitting the corpus
tweets_corpus_train <- corpus_clean[1:10980 ]
tweets_corpus_test  <- corpus_clean[10981:14640]

#comparing proportion of positive, negative and neutral in training and test dataset
prop.table(table(tweets_train_labels$airline_sentiment))
prop.table(table(tweets_test_labels$airline_sentiment))

#installing and loading wordcload package
install.packages("wordcloud")
library(wordcloud)
library(SnowballC)
require(wordcloud)
wordcloud(sms_corpus_train, min.freq = 50,random.order = FALSE)

positive <- subset(tweets_train_labels, airline_sentiment == "positive")
negative <- subset(tweets_train_labels, airline_sentiment == "negative")
neutral <- subset(tweets_train_labels, airline_sentiment == "neutral")

wordcloud(positive$text, max.words = 40, scale = c(2, 0.5))
wordcloud(negative$text, max.words = 40, scale = c(2, 0.5))
wordcloud(neutral$text, max.words = 40, scale = c(2, 0.5))


convert_counts <-function(x) {
  x <-ifelse(x > 0, "YES", "No")
}
tweets_freq_terms <- findFreqTerms(tweets_data_train, 5)
tweets_freq_terms

tweets_data_freq_train <- tweets_data_train[ , tweets_freq_terms]
tweets_data_freq_test <- tweets_data_test[ , tweets_freq_terms]
tweets_train <- apply(tweets_data_freq_train, MARGIN = 2, convert_counts)
tweets_test <- apply(tweets_data_freq_test, MARGIN = 2, convert_counts)


tweets_classifier_train <- naiveBayes(tweets_train,tweets_train_labels$airline_sentiment,laplace=0)


tweets_test_pred <-predict(tweets_classifier_train, tweets_test)
CrossTable(tweets_test_pred, tweets_test_labels$airline_sentiment, prop.chisq = FALSE, 
           prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


tweets_classifier_train2 <- naiveBayes(tweets_train,tweets_train_labels$airline_sentiment,laplace=1)
tweets_test_pred2 <-predict(tweets_classifier_train2, tweets_test)
CrossTable(tweets_test_pred2, tweets_test_labels$airline_sentiment, prop.chisq = FALSE, 
           prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

