library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(udpipe)
library(SnowballC)
library(ggthemes)

#Scraping data from the FAO Kenya Food Security Outlook Report (2020)

url <- "http://www.fao.org/giews/countrybrief/country.jsp?code=KEN" 
webpage<-read_html(url)
output_text<-webpage %>% html_nodes("#maincontent") %>%html_text()
view(output_text)

#Outputting scraped text to desktop
write_lines(output_text,"//Users/simbarasherunyowa/Desktop/coding_\ final_project\\.kenya_scraped_text.txt")

#Unnesting tokens 
food_security_text<-read_file("kenya_scraped_text.txt")
food_security_df<-tibble(text = food_security_text)
food_security_tokens<-unnest_tokens(food_security_df, word_tokens, text, token = "words")
view(food_security_tokens)

#Creating and running function to remove stopwords
stop_word_remover<-function(df) {
  cleaned_words<-anti_join(df, stop_words, by = c("word_tokens" ="word"))
  }

tokens_no_stop<-stop_word_remover(food_security_tokens)
view(tokens_no_stop)

#Stemming dataframe
getStemLanguages()
stemmed_df<- wordStem(tokens_no_stop$word_tokens, language = "porter")
stemmed_tibble<-(as_tibble(stemmed_df)) %>%
  rename(word = value)
view(stemmed_tibble)

#Performing sentiment analysis on Kenya 2020 Food Security Report with affin and bing 
nrc<-get_sentiments("nrc")
bing<-get_sentiments("bing")

#NRC analysis
nrc_data<-inner_join(nrc,stemmed_tibble)
nrc_data %>%
  ggplot(aes(x = sentiment))+
  geom_histogram(stat = "count", fill = "#ef476f" )+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  labs(title = "NRC Sentiment Analysis of Kenya 2020 Food Security Report", y = "Count", x = "Sentiment")+
  coord_flip()+
  theme_clean()

#Bing Analysis  
bing_data<-inner_join(bing, stemmed_tibble)
bing_data %>%
  ggplot(aes(x=sentiment))+
  geom_histogram(stat = "count", fill = "#ef476f" )+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  labs(title = "Bing Sentiment Analysis of Kenya 2020 Food Security Report", y = "Count", x = "Sentiment")+
  coord_flip()+
  theme_clean()







