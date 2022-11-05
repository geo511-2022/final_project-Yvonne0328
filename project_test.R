library(twitteR) 
# Change consumer_key, consume_secret, access_token, and 
# access_secret based on your own keys
consumer_key <- "cTKgtvwRAmKQByf0iOFZKtkl4"
consumer_secret <-"TyuZTByVG40typ4JRE83SN4sE50AXdioLHhkC395FXxoRutLzi"
access_token <- "378502668-1KfCRRSUcMxRT5owoRFwHrBwuVUKeG5w3TJfmGcg"
access_secret <- "JYuXFPIMddsbn5rtf8wfVSuauDZKkGPOauzucLv5wB8Tq" 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# CAMP: 2018/11/08-2018/11/25
loc_LA <- "34.04993,-118.24084,50mi"
loc_NY <- "40.71278, -73.00611, 50mi"

LA <- searchTwitter("wildfire", n = 100, lang = "en", geocode = loc_LA)
NY <- searchTwitter("wildfire", n = 100, lang = "en", geocode = loc_NY)

wildfire_LA <- twListToDF(LA)
wildfire_NY <- twListToDF(NY)





