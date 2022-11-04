library(readxl)
library(tidytext)
library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(emo)


# remover URL 
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}

# remover menção 
removeMention <- function(x){
  gsub("@\\w+", "", x)
}

# remover hashtag 
removeHashtag <- function(x){
  gsub("#\\S+", "", x)
}

# remover Carriage 
removeCarriage <- function(x){
  gsub("[\r\n]", "", x)
}

# remover emojis
removeEmoticon <- function(x){
  gsub("[^\x01-\x7F]", "", x)
}

# remover retweet 
removeRT <- function(x){
  gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
}

# remover invoice 
removeInvoice <- function(x){
  gsub("inv/[0-9]+/+[xvi]+/[xvi]+/[0-9]+", "", x, ignore.case = T)
}

# remover HTML 
unescapeHTML <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# Função para fazr espaço em branco
toSpace <- content_transformer(function(x, pattern){
  gsub(pattern, " ", x)
})

# Spell Normalization Function
spell.correction = content_transformer(function(x, dict){
  words = sapply(unlist(str_split(x, "\\s+")),function(x){
    if(is.na(spell.lex[match(x, dict$slang),"formal"])){
      x = x
    } else{
      x = spell.lex[match(x, dict$slang),"formal"]
    }
  })
  x = paste(words, collapse = " ")
})

# Stemming Words
stemming = function(x){
  paste(sapply(unlist(str_split(x,'\\s+')),katadasar),collapse = " ")
}
##############     TWEET 1  ####################################################
# Lendo os dados

tweet1 <- read_xlsx("tweet1.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 3  ####################################################
tweet3 <- read_xlsx("tweet3.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 10  ####################################################
tweet10 <- read_xlsx("tweet10.xlsx")

data_tweet = tweet10$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 11  ####################################################
tweet11 <- read_xlsx("tweet11.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 12  ####################################################
tweet12 <- read_xlsx("tweet12.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 13  ####################################################
tweet13 <- read_xlsx("tweet13.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 14  ####################################################
tweet14 <- read_xlsx("tweet14.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 15  ####################################################
tweet15 <- read_xlsx("tweet15.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 16  ####################################################
tweet16 <- read_xlsx("tweet16.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 17  ####################################################
tweet17 <- read_xlsx("tweet17.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 18  ####################################################
tweet18 <- read_xlsx("tweet18.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 19  ####################################################
tweet19 <- read_xlsx("tweet19.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 20  ###################################################
tweet20 <- read_xlsx("tweet20.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 21  ####################################################
tweet21 <- read_xlsx("tweet21.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 22  ####################################################
tweet22 <- read_xlsx("tweet22.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 23  ####################################################
tweet23 <- read_xlsx("tweet23.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 24  ####################################################
tweet24 <- read_xlsx("tweet24.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 25  ####################################################
tweet25 <- read_xlsx("tweet25.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 26  ####################################################
tweet26 <- read_xlsx("tweet26.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 27  ####################################################
tweet27 <- read_xlsx("tweet27.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 28  ####################################################
tweet28 <- read_xlsx("tweet28.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 3  ####################################################
tweet29 <- read_xlsx("tweet29.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 30  ####################################################
tweet30 <- read_xlsx("tweet30.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 31  ####################################################
tweet31 <- read_xlsx("tweet31.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 32  ####################################################
tweet32 <- read_xlsx("tweet32.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]

##############     TWEET 33  ####################################################
tweet33 <- read_xlsx("tweet33.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 34  ####################################################
tweet34 <- read_xlsx("tweet34.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]


##############     TWEET 35  ####################################################
tweet35 <- read_xlsx("tweet35.xlsx")

data_tweet = tweet1$text
data_tweet = unique(data_tweet)

# Removendo emojis
data_tweet <- ji_replace_all(data_tweet,"")
data_tweet <- str_remove(data_tweet, "[–]")
data_tweet <- str_remove(data_tweet, "[”]")
data_tweet <- str_remove(data_tweet, "[ª]")
data_tweet <- str_replace_all(data_tweet,"⃣" , " ")
data_tweet <- str_replace_all(data_tweet, "•", " ")


# Trabalhano com corpus
tweet_corpus = VCorpus(VectorSource(data_tweet))

# Case folding
tweet_corpus = tm_map(tweet_corpus,content_transformer(tolower))
# Retweet removal
tweet.corpus = tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeCarriage))
# Invoice removal
tweet_corpus = tm_map(tweet_corpus,content_transformer(removeInvoice))
# Removendo simbolos
# pontuação
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numeros
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminando espaços em brancos extras
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)

# Removendo stopwords 
df_tweet = data.frame(text = sapply(tweet_corpus,as.character),
                      stringsAsFactors = FALSE)

rm_stopword = VCorpus(VectorSource(df_tweet$text))
# Usando lista de stopword
stop = readLines('stopwords.txt')
rm_stopword = tm_map(rm_stopword,removeWords,stop)
# Salvando os dados
df_clean = data.frame(text = sapply(rm_stopword,as.character),
                      stringsAsFactors = FALSE)
View(df_clean)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm_stopword, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm, 
                           lowfreq = 30)
freq.terms[1:50]













