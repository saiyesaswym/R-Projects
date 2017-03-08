library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

reviews <- read.csv("textreviews.csv",stringsAsFactors = FALSE)
str(reviews)

#combining all reviews together
reviews_text <- paste(reviews,collapse = " ")

#setting up source and corpus
review_source <- VectorSource(reviews_text)
docs <- Corpus(review_source)

#cleaning
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
corpus <- tm_map(docs, toSpace, "\\|")

corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords("english"))

#building Document Term matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
