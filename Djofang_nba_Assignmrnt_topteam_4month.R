#' Title: NBA CASE
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 14 2021


# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/NBA Fan Engagement/data")

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)
library(plotrix)


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
stops <- c(stopwords('SMART'), 'nba', 'rt', 'hawks','lol','detroit','pistons', 'cleveland','atlanta','houston','miami','rockets',
           'chicago','bulls','warriors','hornet','golden','â' ,'celtics','brooklyn', 'lakers','clippers','raptors','heat','knicks')

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


# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Spectral")
pal <- pal[-(1:2)]

pal1 <- brewer.pal(8, "Paired")
pal1 <- pal1[-(1:2)]

pal2 <- brewer.pal(8, "Reds")
pal2 <- pal2[-(1:2)]

# Making a word cloud for october
# october wordcloud
set.seed(1234)
wordcloud(topterm_oct20_DF$word,
          topterm_oct20_DF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# september wordcloud
set.seed(1234)
wordcloud(topterm_sep20_DF$word,
          topterm_sep20_DF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))

# August wordcloud
set.seed(1234)
wordcloud(topterm_aug20_DF$word,
          topterm_aug20_DF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))
# July wordcloud
set.seed(1234)
wordcloud(topterm_jul20_DF$word,
          topterm_jul20_DF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))


# Reduce TDM
reducedTDM <- removeSparseTerms(octTDM, sparse=0.975) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')


assocText <- rm_url(oct20_tweet$text)

# MORE QDAP!
word_associate(assocText, 
               match.string = 'lebron', 
               stopwords = stops,
               network.plot = T,
               cloud.colors = c('black','darkred'))

