#' Title: NBA CASE
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 14 2021


# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/NBA Fan Engagement/data")

# Libs
library(tm)
library(qdap)
library(RColorBrewer)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(ggalt)


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

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
#read
oct20_text <- read.csv('M_Oct2020.csv')
sep20_text <- read.csv('L_Sep2020.csv')
aug20_text <- read.csv('K_Aug2020.csv')
jul20_text <- read.csv('J_July2020.csv')
#NBAFiles <- sample(nrows = 10000)

#sampling the data for 10000 observation
odx <- 1:nrow(oct20_text)
set.seed(123)
odx <- sample(odx, 6000)
sample_oct20 = oct20_text[odx,]

sdx <- 1:nrow(sep20_text)
set.seed(123)
sdx <- sample(sdx, 6000)
sample_sep20 = sep20_text[sdx,]

adx <- 1:nrow(aug20_text)
set.seed(123)
sdx <- sample(sdx, 6000)
sample_aug20 = aug20_text[sdx,]

jdx <- 1:nrow(jul20_text)
set.seed(123)
sdx <- sample(sdx, 6000)
sample_jul20 = jul20_text[sdx,]

# Deleting non- AcSII in text
gsub("[^\x01-\x7F]", "", sample_oct20$text)
gsub("[^\x01-\x7F]", "", sample_sep20$text)
gsub("[^\x01-\x7F]", "", sample_aug20$text)
gsub("[^\x01-\x7F]", "", sample_jul20$text)
#saving the files to harddrive
write.csv(sample_oct20,'oct_2020_unclean.csv', row.names = F)
write.csv(sample_sep20,'sep_2020_unclean.csv', row.names = F)
write.csv(sample_aug20,'aug_2020_unclean.csv', row.names = F)
write.csv(sample_jul20,'jul_2020_unclean.csv', row.names = F)


oct20_tweet <- read.csv('oct_2020_unclean.csv', header = TRUE)
sep20_tweet <- read.csv('sep_2020_unclean.csv', header = TRUE)
aug20_tweet <- read.csv('aug_2020_unclean.csv', header = TRUE)
jul20_tweet <- read.csv('jul_2020_unclean.csv', header = TRUE)


topteams_oct20_tweet = oct20_tweet %>% 
  filter(team %in% c('Toronto Raptors'))

# Vector Corpus; omit the meta data
oct20_text <- VCorpus(VectorSource(oct20_tweet$text))
sep20_text <- VCorpus(VectorSource(sep20_tweet$text))
aug20_text <- VCorpus(VectorSource(aug20_tweet$text))
jul20_text <- VCorpus(VectorSource(jul20_tweet$text))

# Clean up the data
oct20_text <- cleanCorpus(oct20_text, stops)
sep20_text <- cleanCorpus(sep20_text, stops)
aug20_text <- cleanCorpus(aug20_text, stops)
jul20_text <- cleanCorpus(jul20_text, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
octTDM  <- TermDocumentMatrix(oct20_text, 
                              control=list(tokenize=bigramTokens))
sepTDM  <- TermDocumentMatrix(sep20_text, 
                              control=list(tokenize=bigramTokens))
augTDM  <- TermDocumentMatrix(aug20_text, 
                              control=list(tokenize=bigramTokens))
julTDM  <- TermDocumentMatrix(jul20_text, 
                              control=list(tokenize=bigramTokens))

octTDMm <- as.matrix(octTDM)                                          
sepTDMm <- as.matrix(sepTDM)
augTDMm <- as.matrix(augTDM)
julTDMm <- as.matrix(julTDM)

# Get Row Sums & organize
oct_tweet_TDmv <- sort(rowSums(octTDMm), decreasing = TRUE)
topterm_oct20_DF <- data.frame(word=names(oct_tweet_TDmv),frequency=oct_tweet_TDmv)
sep_tweet_TDmv <- sort(rowSums(sepTDMm), decreasing = TRUE)
topterm_sep20_DF <- data.frame(word=names(sep_tweet_TDmv),frequency=sep_tweet_TDmv)
aug_tweet_TDmv <- sort(rowSums(augTDMm), decreasing = TRUE)
topterm_aug20_DF <- data.frame(word=names(aug_tweet_TDmv),frequency=aug_tweet_TDmv)
jul_tweet_TDmv <- sort(rowSums(julTDMm), decreasing = TRUE)
topterm_jul20_DF <- data.frame(word=names(jul_tweet_TDmv),frequency=jul_tweet_TDmv)



# Remove the row attributes meta family
rownames(topterm_oct20_DF) <- NULL
rownames(topterm_sep20_DF) <- NULL
rownames(topterm_aug20_DF) <- NULL
rownames(topterm_jul20_DF) <- NULL

# Review a section
topterm_oct20_DF[01:15,]
topterm_sep20_DF[01:15,]
topterm_aug20_DF[01:15,]
topterm_jul20_DF[01:15,]

# Simple barplot
topWords_oct  <- subset(topterm_oct20_DF, topterm_oct20_DF$frequency >= 125) 
topWords_oct  <- topWords_oct[order(topWords_oct$frequency, decreasing=F),]

topWords_sep  <- subset(topterm_sep20_DF, topterm_sep20_DF$frequency >= 125) 
topWords_sep  <- topWords_sep[order(topWords_sep$frequency, decreasing=F),]

topWords_aug  <- subset(topterm_aug20_DF, topterm_aug20_DF$frequency >= 125) 
topWords_aug  <- topWords_aug[order(topWords_aug$frequency, decreasing=F),]

topWords_jul  <- subset(topterm_jul20_DF, topterm_jul20_DF$frequency >= 125) 
topWords_jul  <- topWords_jul[order(topWords_jul$frequency, decreasing=F),]

#remove row attributes
rownames(topWords_oct) <- NULL
rownames(topWords_sep) <- NULL
rownames(topWords_aug) <- NULL
rownames(topWords_jul) <- NULL

# Chg to factor for ggplot
topWords_oct$word <- factor(topWords_oct$word, 
                            levels=unique(as.character(topWords_oct$word)))
topWords_sep$word <- factor(topWords_sep$word, 
                                levels=unique(as.character(topWords_sep$word)))
topWords_aug$word <- factor(topWords_aug$word, 
                                levels=unique(as.character(topWords_aug$word)))

topWords_jul$word <- factor(topWords_jul$word, 
                            levels=unique(as.character(topWords_jul$word)))



#top teams bar-chart


ggplot(topWords_oct, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on october")

ggplot(topWords_sep, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='blue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on september")

ggplot(topWords_aug, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on August")

ggplot(topWords_jul, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='blue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on July")
