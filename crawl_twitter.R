library(twitteR)
library(tm)


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_bude = userTimeline("@BudeSumiyati",n=100)

df <- do.call("rbind", lapply(tweet_bude, as.data.frame))
dim(df)

corpus <- SimpleCorpus(VectorSource(df$text))
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"dari","sejak","dengan","lagi",
                                        "dan","juga","selalu","karena",
                                        "ada","yang","kamu","ini","itu","saja","bisa"))

DTM <- DocumentTermMatrix(corpus)
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
#head <- sums[1:75,]
wordcloud(words = sums$term, freq = sums$count, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

