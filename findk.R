#this is help determine best # of topics for dataset (k) 
#read Charlotte review csv into dataframe
review.C <- read.csv("review_C.csv")

#split reviews marked useful and not useful
useful <- review.C[review.C$votes.useful > 0, ]
not.useful <- review.C[review.C$votes.useful == 0, ]

#sample useful reviews
set.seed(1111)
N <- 2000
usamp <- useful[sample(nrow(useful),N),]

#split sample into train and validate
library(caTools)
sample = sample.split(usamp$text, SplitRatio = .5)
train = subset(usamp, sample == TRUE)
validate = subset(usamp, sample == FALSE)

#create dtms for train and validate
library(tm)
library(topicmodels)

#transfer text data to a corpus
corpus1 <- Corpus(VectorSource(train$text))
corpus2 <- Corpus(VectorSource(validate$text))

#create a document term matrix for training sample
train_dtm <- DocumentTermMatrix(corpus1, control = 
                                        list(weighting = weightTf, stopwords = TRUE, 
                                             minWordLength = 3, removeNumbers = TRUE,
                                             removePunctuation = TRUE))

#mean term frequency-inverse document frequency (tf-idf) over documents 
#containing this term is used to select the vocabulary. This measure allows 
#to omit terms which have low frequency as well as those occurring in many 
#documents.

library(slam)
dim(train_dtm)
summary(col_sums(train_dtm))
term_tfidf <- tapply(train_dtm$v/row_sums(train_dtm)[train_dtm$i], train_dtm$j, mean) *
        log2(nDocs(train_dtm)/col_sums(train_dtm > 0))
summary(term_tfidf)
train_dtm <- train_dtm[,term_tfidf >= 0.1]
train_dtm <- train_dtm[row_sums(train_dtm) > 0,]
summary(col_sums(train_dtm))
dim(train_dtm)

#create a document term matrix for validation sample
val_dtm <- DocumentTermMatrix(corpus2, control = 
                                        list(weighting = weightTf, stopwords = TRUE, 
                                             minWordLength = 3, removeNumbers = TRUE,
                                             removePunctuation = TRUE))

#mean term frequency-inverse document frequency (tf-idf)
 
dim(val_dtm)
summary(col_sums(val_dtm))
term_tfidf <- tapply(val_dtm$v/row_sums(val_dtm)[val_dtm$i], val_dtm$j, mean) *
        log2(nDocs(val_dtm)/col_sums(val_dtm > 0))
summary(term_tfidf)
val_dtm <- val_dtm[,term_tfidf >= 0.1]
val_dtm <- val_dtm[row_sums(val_dtm) > 0,]
summary(col_sums(val_dtm))
dim(val_dtm)

#list of k values to try
k_try <- c(10, 20, 30, 40, 60, 80, 100)

#dataframe for results for each k
k_res <- data.frame()
SEED <- 5555
for (k in k_try) {
        time1 <- Sys.time()
        TM <- LDA(train_dtm, k = k, control = list(seed = SEED))
        time2 <- Sys.time()
        comp.time <- time2 -time1
        perp <- perplexity(TM, val_dtm)
        new_row <- data.frame(k = k, perp = perp, comp.time = comp.time)
        k_res <- rbind(k_res, new_row) }

#plot results
with(k_res, plot(k, perp, type = "l"))

