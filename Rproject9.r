#first load this all libaries to authentication of twitter or visualisation of the data 
library(ROAuth)
library(twitteR)
library(base64enc)
library(httpuv)
library(tm)
library(RCurl)
library(arules)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(httr)
library(syuzhet)
library("base64enc")
library("twitteR")
library("ROAuth")
library("devtools")
library("memoise")
library("whisker")
library("rstudioapi")
library("git2r")
library("withr")
library("rjson")
library("bit64")
library ("httr")
library ("httpuv")
library("NLP")
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
#put Your own keys here that you can get from twitter developer account from twitter api ok do that
consumerKey <- "qGA23jQZUjiU1RbnSXjgeyPEJ"
consumerSecret <- ""
accessToken <- "902905259231633408-NuPAgsM6UerXJUZYDPkhXqPQuELDLkX"
accessTokenSecret <- "LERsXQdoyAeZiWOStHSOA2PkFvhxRDbisPK9DngfaQ4Uc"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
Avengers=searchTwitter("Avengers",n=100,lang="en")
Avengers
Avengers=twListToDF(Avengers)
View(Avengers)
Avengers=sapply(Avengers,function(x) x$getText())
Avengers_text= sapply(Avengers, function(x) x$getText())
# create a corpus it is type that is a collection of all tweets for creating wordcloud
Avengers_corpus = Corpus(VectorSource(Avengers_text))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(Avengers_corpus,
                         control = list(removePunctuation = TRUE,
  stopwords = c(":", "learning", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("AvengersCloud.png", width=12, height=8, units="in", res=300)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

(freq.terms <- findFreqTerms(tdm, lowfreq = 50))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat="identity") +xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))
ggplot(Avengers, aes(created)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  
  ggtitle(Avengers)
# we can predict from this tweets of the  movie will be hit because i search in this all live tweets the most frequent tweets for the graphical representation and from that 
# i will tell most frequent words are the positive negative bad good awesome from that i will 
# response of audience is more and after that i tell the graph is high for good and low for bad 
# from that i tell movie is good its all about the spoilers and all its all .
#but here in all 100 tweets most common words is only Avengers from that we can conclude movie is good 
#because peoples not posting negative reviews to movie like bad AVERAGE etc. for the movie Thank you From krishna kakade _:-:
