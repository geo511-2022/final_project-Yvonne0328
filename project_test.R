library(twitteR) 
library(rtweet)
library(dplyr)
# Change consumer_key, consume_secret, access_token, and 
# access_secret based on your own keys


# read data
LA <- read.csv("data/disaster_LA.csv")
ALL <- read.csv("data/disaster_ALL.csv")

# word clouds
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

text_LA <- LA$text
docs_LA <- Corpus(VectorSource(text_LA))
docs_LA<- docs_LA %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_LA <- tm_map(docs_LA, content_transformer(tolower))
docs_LA <- tm_map(docs_LA, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs_LA) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_LA <- data.frame(word = names(words),freq=words)
png(filename = "LA.png")
wordcloud(words = df_LA$word, freq = df_LA$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()


text_ALL <- ALL$text
docs_ALL <- Corpus(VectorSource(text_ALL))
docs_ALL<- docs_ALL %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_ALL <- tm_map(docs_ALL, content_transformer(tolower))
docs_ALL <- tm_map(docs_ALL, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs_ALL) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_ALL <- data.frame(word = names(words),freq=words)
png(filename = "ALL.png")
wordcloud(words = df_ALL$word, freq = df_ALL$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

