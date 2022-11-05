library(twitteR) 
# Change consumer_key, consume_secret, access_token, and 
# access_secret based on your own keys
consumer_key <- "cTKgtvwRAmKQByf0iOFZKtkl4"
consumer_secret <-"TyuZTByVG40typ4JRE83SN4sE50AXdioLHhkC395FXxoRutLzi"
access_token <- "378502668-1KfCRRSUcMxRT5owoRFwHrBwuVUKeG5w3TJfmGcg"
access_secret <- "JYuXFPIMddsbn5rtf8wfVSuauDZKkGPOauzucLv5wB8Tq" 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# data accessing
loc_LA <- "34.04993,-118.24084,50mi"
loc_NY <- "40.71278, -74.0045,50mi"
loc_Boston <- "34.04993,-118.24084,50mi"

LA <- searchTwitter("wildfire", n = 1000, lang = "en", geocode = loc_LA)
LA_WS <- searchTwitter("wildfire OR smoke", n = 1000, lang = "en", geocode = loc_LA)

ALL <- searchTwitter("wildfire", n = 1000, lang = "en")
ALL_WS <- search_tweets("wildfire OR smoke", n = 1000, lang = "en")

wildfire_LA <- twListToDF(LA)
save_as_csv(wildfire_LA, "wildfire_LA.csv")

wildfire_smoke_LA <- twListToDF(LA)
save_as_csv(wildfire_smoke_LA, "wildfire_smoke_LA.csv")

wildfire_ALL <- twListToDF(ALL)
save_as_csv(wildfire_ALL, "wildfire_ALL.csv")

wildfire_smoke_ALL <- as.data.frame(ALL_WS)
save_as_csv(wildfire_smoke_ALL, "wildfire_smoke_ALL.csv")

# read data
LA_w <- read.csv("data/wildfire_LA.csv")
LA_ws <- read.csv("data/wildfire_smoke_LA.csv")
ALL_w <- read.csv("data/wildfire_ALL.csv")
AL_ws <- read.csv("data/wildfire_smoke_ALL.csv")

# word clouds
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

text_LA_w <- LA_w$text

docs_LA_w <- Corpus(VectorSource(text_LA_w))

docs_LA_w<- docs_LA_w %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_LA_w <- tm_map(docs_LA_w, content_transformer(tolower))
docs_LA_w <- tm_map(docs_LA_w, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs_LA_w) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_LA_w <- data.frame(word = names(words),freq=words)

wordcloud(words = df_LA_w$word, freq = df_LA_w$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))




text_LA_ws <- LA_ws$text
text_ALL_w <- ALL_w$text
text_ALL_ws <- ALL_WS$text