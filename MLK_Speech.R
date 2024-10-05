
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("NLP")
install.packages("tidyr")
install.packages("tidytext")
install.packages("dplyr")
install.packages('ggthemes')

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(NLP)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(qdap)
library(ggthemes)



filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)


# Load the data as a corpus
docs <- Corpus(VectorSource(text))


inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

associations <- findAssocs(dtm, terms = "freedom", corlimit = 0.3)
associations
associations_df <- list_vect2df(associations, col2 = "word", col3 = "score")
# Plot the associations_df values
ggplot(associations_df, aes(score, word)) + 
  geom_point(size = 3) + 
  theme_gdocs() + 
  ggtitle("Word Associations to 'freedom'")

head(d, 10)


x <- barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col =rainbow(10), main ="Most frequent words",
        ylab = "Word frequencies",
        ylim=c(0,20))
y <- d[1:10,]$freq
text(x,y+2,labels=as.character(y))

# identify word as positive or negative sentiments and sort them according to their frequency

MLK_text <- data.frame(line = 1:length(text), text = text, stringsAsFactors = FALSE)

# filter blank lines (even rows)
MLK_text <- MLK_text %>% filter(line %% 2 == 1) %>%
  mutate(line = 1:nrow(.)) #renumber lines

# Make single word vector
MLK_text <- MLK_text %>%
  unnest_tokens(word, text)


bing <- get_sentiments("bing")
MLK_Wordcount <- MLK_text%>%
  inner_join(bing)%>%
  count(word, sentiment, sort = TRUE)
MLK_Wordcount

MLK_Wordcount %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  theme_minimal()+
  labs(y = "Contribution to sentiment")


