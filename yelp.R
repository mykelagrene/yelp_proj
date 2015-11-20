library(jsonlite)

fnames <- c('business','checkin','review','tip','user')
jfile <- paste0(getwd(),'/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_',fnames,'.json')

##get business file and clean it up
business <- stream_in(file(jfile[1])) #creates dataframe
business <- flatten(business) #flattens dataframe
names(business) <- make.names(names(business), unique = TRUE) #make syntactically valid names

#flatten categories lists into character strings
business$categories <- sapply(business$categories, toString)

#reduce business data frame to only include features needed
business <- business[, c("business_id", "name", "categories", "latitude", "longitude", "stars")]

#write business.csv
write.csv(business, "business.csv")

##get review file and clean it up
time1 <- Sys.time() #see how long this takes! (~12.5 min)
review <- stream_in(file(jfile[3])) #creates dataframe 
time2 <- Sys.time()
time2 - time1
review <- flatten(review)

#reduce review data frame to include text, business_id, stars, votes.useful
review <- review[,c("text", "votes.useful", "stars", "business_id")]

#add business info to review data frame
review$business_name <- business[match(review$business_id, business$business_id), "name"]
review$business_lat <- business[match(review$business_id, business$business_id), "latitude"]
review$business_long <- business[match(review$business_id, business$business_id), "longitude"]

##geo filter to get businesses in/near Charlotte
#create lat, long bounding box for Charlotte (35.2269° N, 80.8433° W)
maxlat <- 36
minlat <- 34
maxlong <- -80
minlong <- -82

#select reviews of Charlotte businesses
review.C <- review[(review$business_lat > minlat &
                            review$business_lat < maxlat &
                            review$business_long > minlong &
                            review$business_long < maxlong), ]

#remove review data frame from memory
rm(review)

#write review.C data frame out to csv file
write.csv(review.C, "review_C.csv")

#read Charlotte review csv into dataframe
review.C <- read.csv("review_C.csv")

#split reviews marked useful and not useful
useful <- review.C[review.C$votes.useful > 0, ]
not.useful <- review.C[review.C$votes.useful == 0, ]

#show distribution of stars
table(useful$stars)
hist(useful$stars, ylim = c(0,20000))
table(not.useful$stars)
hist(not.useful$stars, ylim = c(0,20000))

#explore business categories
business <- read.csv("business.csv")
useful$business_cat <- business[match(useful$business_id, business$business_id), "categories"]
useful$business_cat <- as.character(useful$business_cat)
cat.sum.useful <- as.data.frame(table(unlist(strsplit(useful$business_cat, ', '))))
names(cat.sum.useful) <- c("category", "count")
cat.sum.useful <- cat.sum.useful[order(cat.sum.useful$count, decreasing = TRUE),]
#top 20
cat.sum.useful <- cat.sum.useful[1:20,]

not.useful$business_cat <- business[match(not.useful$business_id, business$business_id), "categories"]
not.useful$business_cat <- as.character(not.useful$business_cat)
cat.sum.not.useful <- as.data.frame(table(unlist(strsplit(not.useful$business_cat, ', '))))
names(cat.sum.not.useful) <- c("category", "count")
cat.sum.not.useful <- cat.sum.not.useful[order(cat.sum.not.useful$count, decreasing = TRUE),]
#top 20
cat.sum.not.useful <- cat.sum.not.useful[1:20,]

#create a panel plot comparing 2
library(ggplot2)
library(gridExtra)
cat.sum.useful$category <-factor(cat.sum.useful$category,
                           levels=cat.sum.useful[order(cat.sum.useful$count), "category"])
x1 <- ggplot(data=cat.sum.useful, aes(x=category, y=count)) + 
        geom_bar(stat="identity") +
        ylim(0,40000) +
        coord_flip() +
        labs(x = "Business Category") +
        labs(y = "Count of Useful Reviews")

cat.sum.not.useful$category <-factor(cat.sum.not.useful$category,
                                 levels=cat.sum.not.useful[order(cat.sum.not.useful$count), "category"])
x2 <- ggplot(data=cat.sum.not.useful, aes(x=category, y=count)) + 
        geom_bar(stat="identity") +
        ylim(0,40000) +
        coord_flip() +
        labs(x = "Business Category") +
        labs(y = "Count of Not Useful Reviews")

grid.arrange(x1, x2, ncol = 1)

##topic modeling useful reviews
library(tm)
library(topicmodels)

#sample dataframe for modeling
set.seed(4321)
N <- 2000
usamp <- useful[sample(nrow(useful),N),]


#transfer text data to a corpus
corpus1 <- Corpus(VectorSource(usamp$text))

#create a document term matrix
usamp_dtm <- DocumentTermMatrix(corpus1, control = 
                                        list(weighting = weightTf, stopwords = TRUE,
                                             wordLengths = c(5,Inf), removeNumbers = TRUE,
                                             removePunctuation = TRUE))
#remove rows with no valid terms
library(slam)
usamp_dtm <- usamp_dtm[row_sums(usamp_dtm) > 0,]
dim(usamp_dtm)

#mean term frequency-inverse document frequency (tf-idf) over documents 
#containing this term is used to select the vocabulary. This measure allows 
#to omit terms which have low frequency as well as those occurring in many 
#documents.

#library(slam)
#dim(usamp_dtm)
#summary(col_sums(usamp_dtm))
#term_tfidf <- tapply(usamp_dtm$v/row_sums(usamp_dtm)[usamp_dtm$i], usamp_dtm$j, mean) *
#        log2(nDocs(usamp_dtm)/col_sums(usamp_dtm > 0))
#summary(term_tfidf)
#usamp_dtm <- usamp_dtm[,term_tfidf >= 0.1]
#usamp_dtm <- usamp_dtm[row_sums(usamp_dtm) > 0,]
#summary(col_sums(usamp_dtm))
#dim(usamp_dtm)


#make topic model
k <- 20
SEED <- 1234

#LDA model
time1 <- Sys.time()
usampTM1 <- LDA(usamp_dtm, k = k, control = list(seed = SEED))
time2 <- Sys.time()
time2 -time1
uTerms <- terms(usampTM1, 10) #get top 10 terms for each topic
uTerms

#CTM model
time1 <- Sys.time()
usampTM2 <- CTM(usamp_dtm, k = k, control = list(seed = SEED,
                var = list(tol = 10^-4), em = list(tol = 10^-3)))
time2 <- Sys.time()
time2 - time1
ucTerms <- terms(usampTM2, 10) #get top 10 terms for each topic
ucTerms

##topic modeling not useful reviews

#sample dataframe for testing
set.seed(1234)
nsamp <- not.useful[sample(nrow(not.useful),N),]


#transfer text data to a corpus
corpus2 <- Corpus(VectorSource(nsamp$text))

#create a document term matrix
nsamp_dtm <- DocumentTermMatrix(corpus2, control = 
                                        list(weighting = weightTf, stopwords = TRUE,
                                             wordLengths = c(5,Inf), removeNumbers = TRUE,
                                             removePunctuation = TRUE))
#remove rows with no vaild terms
nsamp_dtm <- nsamp_dtm[row_sums(nsamp_dtm) > 0,]
dim(nsamp_dtm)

#mean term frequency-inverse document frequency (tf-idf) over documents 
#containing this term is used to select the vocabulary. This measure allows 
#to omit terms which have low frequency as well as those occurring in many 
#documents.
 
#dim(nsamp_dtm)
#summary(col_sums(nsamp_dtm))
#term_tfidf <- tapply(nsamp_dtm$v/row_sums(nsamp_dtm)[nsamp_dtm$i], nsamp_dtm$j, mean) *
#        log2(nDocs(nsamp_dtm)/col_sums(nsamp_dtm > 0))
#summary(term_tfidf)
#nsamp_dtm <- nsamp_dtm[,term_tfidf >= 0.1]
#nsamp_dtm <- nsamp_dtm[row_sums(nsamp_dtm) > 0,]
#summary(col_sums(nsamp_dtm))
#dim(nsamp_dtm)

#LDA model for not useful reviews
time1 <- Sys.time()
nsampTM1 <- LDA(nsamp_dtm, k = k, control = list(seed = SEED))
time2 <- Sys.time()
time2 - time1
nTerms <- terms(nsampTM1, 10) #get top 10 terms for each topic
nTerms

write.csv(uTerms, "useful_topics.csv")
write.csv(nTerms, "not_useful_topics.csv")

#distribution of most likely topics
utopics <- topics(usampTM1, 1)
table(utopics)

ntopics <- topics(nsampTM1, 1)
table(ntopics)

#using term frequency - inverse frequency to reduce the word dictionary removes
#words like delicious and awesome and amazing from the useful set. To check:
sum(grepl("delicious", usamp$text))
sum(usamp_dtm$dimnames$Terms == "delicious")

#finding word counts in document term matrix:
usamp_dtm2 <- as.matrix(usamp_dtm)
ufrequency <- colSums(usamp_dtm2)
ufrequency <- sort(ufrequency, decreasing=TRUE)
head(ufrequency)
ufrequency["burger"]

nsamp_dtm2 <- as.matrix(nsamp_dtm)
nfrequency <- colSums(nsamp_dtm2)
nfrequency <- sort(nfrequency, decreasing=TRUE)
head(nfrequency)

#show top 20 words in each
barplot(ufrequency[1:20], las=2)
barplot(nfrequency[1:20], las=2)

#show stats for how many useful words
uwordcount <- rowSums(usamp_dtm2)
summary(uwordcount)
hist(uwordcount)
nwordcount <- rowSums(nsamp_dtm2)
summary(nwordcount)
hist(nwordcount)

old.par <- par(mfrow=c(1, 2))
barplot(ufrequency[1:20], las=2, main = "Top 20 Word Counts, Useful Reviews")
barplot(nfrequency[1:20], las=2, main = "Top 20 Word Counts, Not-Useful Reviews")
par(old.par)
old.par <- par(mfrow=c(1, 2))
hist(uwordcount, main = "Histogram of Words per Useful Review", xlab = "Word Count")
hist(nwordcount, main = "Histogram of Words per Not-Useful Review", xlab = "Word Count")
par(old.par)
