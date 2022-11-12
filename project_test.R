library(twitteR) 
library(rtweet)
library(dplyr)
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(spData)
library(sf)
library(tidyverse)
library(maps)

# access_secret based on your own keys


point <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
data(world.cities)
seoul <- world.cities %>%
  filter(name == 'Seoul')%>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)%>%
  st_buffer(dist = 120000)

data <- st_intersection(point, seoul)


# word clouds
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

steps <- c("before", "during", "after", "")

for (i in steps){
  data_temp <- data %>%
    filter(timeline == i)
  text_data <- data_temp$text
  docs_data <- Corpus(VectorSource(text_data))
  docs_data<- docs_data %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs_data <- tm_map(docs_data, content_transformer(tolower))
  docs_data <- tm_map(docs_data, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs_data) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df_data <- data.frame(word = names(words),freq=words) %>%
    filter(word != "korea" & word != "…" & word != "korea")
  
  
  png(filename = paste0("WC", i, ".png"), width = 1024, height = 1024, units = "px")
  wordcloud(words = df_data$word, freq = df_data$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  assign(paste0("df", i), df_data)
  
}

afinn = get_sentiments("afinn")
bing = get_sentiments("bing")
nrc = get_sentiments("nrc")
save_as_csv(afinn, "data/afinn.csv")
save_as_csv(nrc, "data/nrc.csv")


#sentiment resilience
#BING
png(filename = "bing_before.png",width = 1024, height = 1024, units = "px")
dfbefore %>%
  inner_join(bing) %>%
  group_by(sentiment) %>%
  slice_max(freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

png(filename = "bing_during.png",width = 1024, height = 1024, units = "px")
dfduring %>%
  inner_join(bing) %>%
  group_by(sentiment) %>%
  slice_max(freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

png(filename = "bing_after.png",width = 1024, height = 1024, units = "px")
dfafter %>%
  inner_join(bing) %>%
  group_by(sentiment) %>%
  slice_max(freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

#NRC
png(filename = "nrc_before.png", width = 1024, height = 1024, units = "px")
dfbefore %>%
  inner_join(nrc) %>%
  mutate(temp = freq) %>%
  mutate(temp2 = sentiment) %>%
  pivot_wider(names_from = temp2, values_from = temp, values_fill = 0) %>% 
  mutate(score = positive - negative) %>%
  group_by(sentiment) %>%
  slice_max(order_by = freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

png(filename = "nrc_during.png", width = 1024, height = 1024, units = "px")
dfduring %>%
  inner_join(nrc) %>%
  mutate(temp = freq) %>%
  mutate(temp2 = sentiment) %>%
  pivot_wider(names_from = temp2, values_from = temp, values_fill = 0) %>% 
  mutate(score = positive - negative) %>%
  group_by(sentiment) %>%
  slice_max(order_by = freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

png(filename = "nrc_after.png", width = 1024, height = 1024, units = "px")
dfafter %>%
  inner_join(nrc) %>%
  mutate(temp = freq) %>%
  mutate(temp2 = sentiment) %>%
  pivot_wider(names_from = temp2, values_from = temp, values_fill = 0) %>% 
  mutate(score = positive - negative) %>%
  group_by(sentiment) %>%
  slice_max(order_by = freq, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(freq, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
dev.off()

#afinn
SCORE <- c()
dfbefore <- dfbefore %>%
  inner_join(afinn) %>%
  mutate(score = freq*value)
dfduring <- dfduring %>%
  inner_join(afinn) %>%
  mutate(score = freq*value)
dfafter <- dfafter %>%
  inner_join(afinn) %>%
  mutate(score = freq*value)

SCORE  <- c(sum(dfbefore$score), sum(dfduring$score), sum(dfafter$score))
timeframe <- c("-1", "0", "1")
names <- c("before", "during", "after")
trend <- data.frame(names,timeframe, SCORE)

png(filename = "social_resilience.png", width = 1024, height = 1024, units = "px")
ggplot(trend, aes(x= factor(names,level = c("before", "during", "after")), SCORE, group = 1))+
  geom_line(arrow = arrow())+
  geom_point()+
  xlab("timeline")+
  ylab("emotion score")+
  ggtitle("social resilience")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

