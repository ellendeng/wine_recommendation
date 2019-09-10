# Machine Learning Summar 2019 Final Project
#install.packages("tidyverse")
#install.packages('wordcloud')
#install.packages('tidytext')
#install.packages('SnowballC')
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(SnowballC)

# read in data and create a data frame - wine

wine = read.csv(file.choose())
glimpse(wine)

# create a new variable vintage by extracting from title
wine$title = as.character(wine$title)
vintage_patt = "([1|2]{1}[0|9]{1}[0-9]{2})"
y = str_extract_all(wine$title, pattern = vintage_patt, simplify = TRUE)
y = as.data.frame(apply(y, 1:2, as.numeric))
y[is.na(y)] = 0
m = as.matrix(apply(y, 1, max))
y = cbind(y, m)
colnames(y) = c("a","s","d")
y = y %>% mutate(f = ifelse(d>2019, a, d))
y$f[y$f == 0] = NA
wine$vintage = y$f

# create a new dataset removing NA from vintage and vintage before 2000
wine.new = wine %>% filter(!is.na(vintage), vintage > 2000)

# create new variables
wine.new$region = str_c(wine.new$province, wine.new$country, sep = "_")
wine.new$variety_region = str_c(wine.new$variety, wine.new$region, sep = "_")
wine.new$variety_country = str_c(wine.new$variety, wine.new$country, sep = "_")
wine.new$price_range = cut(wine.new$price, c(0,10,20,50,100,500,5000))
wine.new$point_range = cut(wine.new$points, c(80,84,89,94,100))

glimpse(wine.new)

# EDA

wine.new %>% na.omit(price) %>% ggplot(aes(x = price_range)) + geom_bar(color = "navyblue", fill = "royalblue")
wine.new %>% na.omit(points) %>% ggplot(aes(x = point_range)) + geom_bar(color = "navyblue", fill = "royalblue")
wine.new %>% ggplot(aes(x = vintage)) + geom_bar(color = "navyblue", fill = "royalblue")

top_region10 = wine.new %>% na.omit() %>% group_by(region) %>% filter(vintage >= 2000) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% top_n(10,n) %>% select(region)
top_variety = wine.new %>% count(variety, sort = TRUE) %>% top_n(10) %>% select(variety)
wine.new %>% filter(variety %in% unname(unlist(top_variety)), region %in% unname(unlist(top_region10))) %>% 
  group_by(variety, region) %>% count(sort = TRUE) %>%  
  ggplot(aes(x=reorder(variety,n), y=n, fill = region)) + geom_col() + coord_flip() + scale_fill_brewer(palette="Spectral") + labs(x="Variety", y="Count")

wine.new %>% na.omit() %>% group_by(region) %>% filter(vintage > 2000, region %in% unname(unlist(top_region))) %>%
  summarise(n=n(), avg_price = mean(price),avg_points = mean(points)) %>% 
  arrange(desc(n)) %>% mutate(ratio= avg_price/avg_points, Co = cor(avg_price, avg_points)) %>%  
  ggplot(aes(x=avg_points, y=avg_price)) + geom_point(aes(size = (n), color = region)) + xlim(c(86.5,90)) + ylim(c(20,75)) + geom_text(aes(label = region), size = 3, nudge_x = 0.0, nudge_y = 1) + scale_size_continuous(range=c(1, 20))

top_region = wine.new %>% na.omit() %>% group_by(region) %>% filter(vintage >= 2000) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% top_n(20,n) %>% select(region)

wine.new %>% na.omit() %>% group_by(region, vintage) %>% filter(vintage >= 2000, price < 100, region %in% unname(unlist(top_region))) %>%
  summarise(n=n(), avg_price = mean(price)) %>% arrange(desc(n)) %>% 
  ggplot(aes(x=vintage, y=avg_price)) + geom_line() + facet_wrap(~region)

# text processing 

wine.new$doc_id = c(1:nrow(wine.new))
wine.new$text = as.character(wine.new$description)
wine.new= wine.new[c(21,22,1:20)]

# clean and process description data
wine_stopwords = c("will","malbec","gris","made","–","vineyard","riesling","pinot", "noir","chardonnay","zinfandel","delivers","character","texture","well","cabernet","merlot","sauvignon","fruit","fruits","now","drink","palate","offers","shows","wine","winery", "vintage","where", "when","finish","and", "but","flavors","flavor","aroma","aromas","taste","tastes","nose","note","notes")
wine_stopwords.df = data.frame(wine_stopwords,"Custom")
colnames(wine_stopwords.df) = c("word","lexicon")
custom_stopwords = rbind(stop_words,wine_stopwords.df)
wine.new$text = tolower(wine.new$text)

tokenized.df = wine.new %>%
  select(doc_id, text, description, region, variety, vintage, title, variety_region, variety_country) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stopwords) %>% 
  filter(!str_detect(word, "[0-9]"))

bigram.df = wine.new %>% 
  select(doc_id, text, description, region, variety, vintage, title, variety_region, variety_country) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stopwords$word,
         !word2 %in% custom_stopwords$word,
         !str_detect(word1, "[0-9]"),
         !str_detect(word2, "[0-9]")) %>%
  unite(bigram, word1, word2, sep = " ")


# plot term freqnency and wordcloud
token_count = tokenized.df %>%
  count(word, sort = TRUE)

wordcloud(token_count$word, token_count$n,random.order = FALSE, rot.per = 0.1, max.words = 50, colors = brewer.pal(8, 'Dark2'))

bigram_count = bigram.df %>%
  count(bigram, sort = TRUE)

wordcloud(bigram_count$bigram, bigram_count$n,random.order = FALSE, rot.per = 0.1, max.words = 50, colors = brewer.pal(8, 'Dark2'))

bigram.df %>%
  count(bigram) %>%
  top_n(50,n) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", color = "navyblue", fill = "royalblue") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(y = "Frequency",
       x = "Top words",
       title = "Top words used",
       subtitle = "")

# term frequency by variety_region

top_variety_region = wine.new %>% na.omit() %>% group_by(variety_region) %>% filter(vintage >= 2000) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% top_n(12,n) %>% select(variety_region)

tokenized.df %>%
  filter(variety_region %in% unname(unlist(top_variety_region))) %>%
  group_by(variety_region, word) %>% 
  tally() %>%
  top_n(15,n) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = factor(variety_region))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  facet_wrap(~ variety_region, scales = "free") + 
  coord_flip() +
  labs(y = "Frequency",
       x = "Top words",
       title = "Top words used by variety_region",
       subtitle = "")

bigram.df %>%
  filter(variety_region %in% unname(unlist(top_variety_region))) %>%
  group_by(variety_region, bigram) %>% 
  tally() %>%
  top_n(15,n) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = factor(variety_region))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  facet_wrap(~ variety_region, scales = "free") + 
  coord_flip() +
  labs(y = "Frequency",
       x = "Top bigrams",
       title = "Top bigrams used by variety_region",
       subtitle = "")

# TFIDF

tf_idf_words <- tokenized_df %>% 
  group_by(variety_region,word) %>%
  tally() %>%
  bind_tf_idf(word, variety_region, n) %>%
  arrange(desc(tf_idf))
tf_idf_words

tf_idf_words %>% 
  filter(variety_region %in% unname(unlist(top_variety_region))) %>%
  top_n(15,n) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = variety_region)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~ variety_region, scales = "free")

tf_idf_bigrams <- bigram_df %>% 
  filter(variety_region %in% unname(unlist(top_variety_region))) %>%
  group_by(variety_region,bigram) %>%
  tally() %>%
  bind_tf_idf(bigram, variety_region, n) %>%
  arrange(desc(tf_idf))
tf_idf_bigrams

tf_idf_bigrams %>% 
  filter(!str_detect(bigram, "^9"),
         !str_detect(bigram, "^2")) %>%
  top_n(15,n) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x = reorder(bigram, tf_idf), y = tf_idf, fill = variety_region)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~ variety_region, scales = "free")

# modelling - classification

# create a sample dataset

top_variety_region_model = wine.new %>% na.omit() %>% group_by(variety_region) %>% filter(vintage >= 2000) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% top_n(10,n) %>% select(variety_region)

set.seed(1234)
wine.s = wine.new %>% filter(variety_region %in% unname(unlist(top_variety_region_model))) %>% sample_n(20000)

# create a 70/30% train/test dataset split 

#install.packages('caret')
library(caret)

set.seed(12)
indexes = createDataPartition(wine.s$variety_region, times = 1, p = 0.7, list = FALSE)
train = wine.s[indexes,] %>% arrange(doc_id)
test = wine.s[-indexes,] %>% arrange(doc_id)
train %>% count(variety_region, sort = T)

#install.packages('quanteda')
library(quanteda)

# tokenize and process text
train.tokens = tokens(train$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
train.tokens = tokens_tolower(train.tokens)
train.tokens = tokens_select(train.tokens, custom_stopwords, 
                              selection = "remove")
train.tokens = tokens_wordstem(train.tokens, language = "english")
train.tokens.dfm = dfm(train.tokens, tolower = FALSE)
train.tokens.dfm = dfm_trim(train.tokens.dfm, sparsity = 0.99)
train.tokens.matrix = as.matrix(train.tokens.dfm)
train.tokens.df = cbind(Label = train$variety_region, convert(train.tokens.dfm, to = 'data.frame'))

# tfidf
term.frequency <- function(row) {
  row / sum(row)
}

inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

tf.idf <- function(x, idf) {
  x * idf
}

train.tokens.df = apply(train.tokens.matrix, 1, term.frequency)
train.tokens.idf = apply(train.tokens.matrix, 2, inverse.doc.freq)
train.tokens.tfidf = apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
train.tokens.tfidf = t(train.tokens.tfidf)
incomplete.cases = which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases] = rep(0.0, ncol(train.tokens.tfidf))
train.tokens.tfidf.df = cbind(Label = train$variety_region, data.frame(train.tokens.tfidf))

# train model with linear SVM ane 10-fold Cross Validation

cv.cntrl = trainControl(method = "cv", number = 10)

token_model_svm = train(Label~., data = train.tokens.tfidf.df, method = 'svmLinear3', trControl = cv.cntrl)
plot(token_model_svm)
token_model_svm$finalModel

# process test data - test dataset needs to have the same structure as train dataset

test.tokens = tokens(test$text, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)
test.tokens = tokens_tolower(test.tokens)
test.tokens = tokens_select(test.tokens, custom_stopwords, 
                            selection = "remove")
test.tokens = tokens_wordstem(test.tokens, language = "english")
test.tokens.dfm = dfm(test.tokens, tolower = FALSE)
test.tokens.dfm = dfm_select(test.tokens.dfm, pattern = train.tokens.dfm,
                             selection = "keep")
test.tokens.matrix = as.matrix(test.tokens.dfm)

test.tokens.df <- apply(test.tokens.matrix, 1, term.frequency)
test.tokens.tfidf <-  apply(test.tokens.df, 2, tf.idf, idf = train.tokens.idf)

test.tokens.tfidf <- t(test.tokens.tfidf)
test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
test.tokens.tfidf.df = cbind(Label = test$variety_region, data.frame(test.tokens.tfidf))

result = predict(token_model_svm, newdata = test.tokens.tfidf.df)
confusionMatrix(result, test.tokens.tfidf.df$Label)

# wine recommendation - cosine similarity

token.sample = wine.s %>%
  select(doc_id, text, description, region, variety, vintage, title, variety_region, variety_country) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stopwords) %>% 
  filter(!str_detect(word, "[0-9]")) %>%
  mutate(word = SnowballC::wordStem(word))

tfidf.s = token.sample %>% 
  count(title,word, sort = TRUE) %>%
  bind_tf_idf(word, title, n)

sparse.matrix = tfidf.s%>%
  cast_sparse(title, word, tf_idf)

#install.packages('text2vec')
library(text2vec)

similarities = sim2(sparse.matrix, method = "cosine", norm = "l2") 

get_similar_wine = function(similarities, reference_wine, n_recommendations = 3){
  sort(similarities[reference_wine, ], decreasing = TRUE)[1:(2 + n_recommendations)]
}

get_similar_wine(similarities, 10000)