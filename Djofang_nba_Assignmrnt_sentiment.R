
# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/NBA Fan Engagement/data")

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(ggplot2)
library(sentimentr)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')


tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'), 'nba', 'rt', 'hawks','lol', 'cavs',
           'bulls','â')


#read
oct20_text <- read.csv('M_Oct2020.csv')
sep20_text <- read.csv('L_Sep2020.csv')


#NBAFiles <- sample(nrows = 10000)

#sampling the data for 10000 observation
odx <- 1:nrow(oct20_text)
set.seed(123)
odx <- sample(odx, 10000)
sample_oct20 = oct20_text[odx,]

sdx <- 1:nrow(sep20_text)
set.seed(123)
sdx <- sample(sdx, 10000)
sample_sep20 = sep20_text[sdx,]

# Deleting non- AcSII in text
gsub("[^\x01-\x7F]", "", sample_oct20$text)
gsub("[^\x01-\x7F]", "", sample_sep20$text)

#saving the files to harddrive
write.csv(sample_oct20,'oct_2020_unclean.csv', row.names = F)
write.csv(sample_sep20,'sep_2020_unclean.csv', row.names = F)

oct20_tweet <- read.csv('oct_2020_unclean.csv', header = TRUE)
sep20_tweet <- read.csv('sep_2020_unclean.csv', header = TRUE)


#creating sentiment for the graph
oct20_tweet$text %>% 
  get_sentences() %>% 
  sentiment() -> oct20_senti

sep20_tweet$text %>% 
  get_sentences() %>% 
  sentiment() -> sep20_senti



#the graph explains to you that most of the comment are arround the neutral point

oct20_senti %>% 
  ggplot() + geom_density(aes(sentiment))


oct20_senti_bar <- oct20_senti %>% 
  mutate(polarity_level = ifelse(sentiment > 0, "positive","Negative" )) %>%
  count(polarity_level)  
  
ggplot(data = oct20_senti_bar, aes(x = polarity_level,fill = polarity_level)) +
  geom_bar()

# Assigning Sentiment for each  months in order to create word cloud

oct20_tweet %>% 
  get_sentences(text) %>% 
  sentiment() -> senti_oct20

sep20_tweet %>% 
  get_sentences(text) %>% 
  sentiment() -> senti_sep20


# Creating polarity level with the assigned Senti


oct20_sen <- senti_oct20 %>% 
  mutate(polarity_level = ifelse(sentiment > 0, "positive","Negative" ))
sep20_sen <- senti_sep20 %>% 
  mutate(polarity_level = ifelse(sentiment > 0, "positive","Negative" ))

#two data base on the polarity level
Oct_positive <- grepl("positive",oct20_sen$polarity_level)
oct_positive_tweet <- oct20_sen[Oct_positive,]
Oct_negative <- grepl("Negative",oct20_sen$polarity_level)
oct_negative_tweet <- oct20_sen[Oct_negative,]

# Vector Corpus; omit the meta data
Oct_positive_corpus <- VCorpus(VectorSource(oct_positive_tweet$text))
Oct_negative_corpus <- VCorpus(VectorSource(oct_negative_tweet$text))

# Clean up the data
Oct_positive_corpus <- cleanCorpus(Oct_positive_corpus, stops)
Oct_negative_corpus <- cleanCorpus(Oct_negative_corpus, stops)  

# Make a Term Document Matrix
oct20_positive_TDM  <- TermDocumentMatrix(Oct_positive_corpus)
oct20_negative_TDM  <- TermDocumentMatrix(Oct_negative_corpus)
oct20_positive_TDMm <- as.matrix(oct20_positive_TDM)
oct20_negative_TDMm <- as.matrix(oct20_negative_TDM)

# Frequency Data Frame
oct20_positive_TDmv <- sort(rowSums(oct20_positive_TDMm), decreasing = TRUE)
oct20_positiveDF   <- data.frame(word = names(oct20_positive_TDmv), freq = oct20_positive_TDmv)
oct20_negative_TDmv <- sort(rowSums(oct20_negative_TDMm), decreasing = TRUE)
oct20_negativeDF   <- data.frame(word = names(oct20_negative_TDmv), freq = oct20_negative_TDmv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Reds")
pal <- pal[-(1:2)]

pal1 <- brewer.pal(8, "Greens")
pal1 <- pal1[-(1:2)]

# Making a word cloud for october
# positive october wordcloud
set.seed(1234)
wordcloud(oct20_positiveDF$word,
          oct20_positiveDF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# negative october wordcloud
set.seed(1234)
wordcloud(oct20_negativeDF$word,
          oct20_negativeDF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))

