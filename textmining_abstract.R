#library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(SentimentAnalysis)
library(syuzhet)
library(data.table)
library(rcrossref)
library(textstem)

#twitter_user="@FoxNews"

#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#tweet_data = userTimeline(twitter_user,n=500)

#df <- do.call("rbind", lapply(tweet_data, as.data.frame))
#dim(df)
abstract<-cr_abstract(doi = '10.1109/TASC.2010.2088091')
abstract<-"\n                    A 1.3 GHz test cavity has been designed to test wafer samples of superconducting materials. This mushroom shaped cavity, operating in TE\n                    01\n                    mode, creates a unique distribution of surface fields. The surface magnetic field on the sample wafer is 3.75 times greater than elsewhere on the Niobium cavity surface. This field design is made possible through dielectrically loading the cavity by locating a hemisphere of ultra-pure sapphire just above the sample wafer. The sapphire pulls the fields away from the walls so the maximum field the Nb surface sees is 25% of the surface field on the sample. In this manner, it should be possible to drive the sample wafer well beyond the BCS limit for Niobium while still maintaining a respectable Q. The sapphire's purity must be tested for its loss tangent and dielectric constant to finalize the design of the mushroom test cavity. A sapphire loaded CEBAF cavity has been constructed and tested. The results on the dielectric constant and loss tangent will be presented.\n                  "

abstract<-lemmatize_strings(abstract)

corpus <- SimpleCorpus(VectorSource(abstract))

# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
removeSymbols <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus <- tm_map(corpus, content_transformer(removeSymbols))
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

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

sent <- analyzeSentiment(DTM, language = "english")
# were going to just select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# Now lets take a look at what these sentiment values look like. 
head(sent)
summary(sent$SentimentGI)

sent2 <- get_nrc_sentiment(abstract)
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")

sent3$emotion <- factor(sent3$emotion, levels = sent3$emotion)

ggplot(sent3[1:8,1:2], 
       aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Emotion", y = "Total Count") + 
  ggtitle("Sentiment of abstract") + 
  theme(plot.title = element_text(hjust=0.5))

ggplot(sent3[9:10,1:2], 
       aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Positivity", y = "Total Count") + 
  ggtitle("Positivity of abstract") + 
  theme(plot.title = element_text(hjust=0.5))

