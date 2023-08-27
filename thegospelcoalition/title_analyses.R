
install.packages("reshape2")
library(broom)
library(tm)
library(tidytext)
library(topicmodels)
library(reshape2)


dsl <- read.csv('tgc_authors_articles_data_long.csv') %>%
  select(-X.1, -X)

#### tf-idf by gender ####
text_df <- data.frame(text = dsl$publication_titles,
                      line = seq(1, length(dsl$last_name), 1),
                      gender = dsl$gender)

text_words <- text_df %>%
  unnest_tokens(word, text)

#data(stop_words)

text_words <- text_words %>%
  anti_join(stop_words)


book_words <- text_words %>%
  count(gender, word, sort = TRUE)


total_words <- book_words %>% 
  group_by(gender) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

#look at the top ones
freq_by_rank <- book_words %>% 
  #anti_join(data.frame(word = c('ten', 
  #                             '10',
  #                              "we're"))) %>%
  group_by(gender) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank
freq_by_rank[freq_by_rank$gender == 'female',]


book_tf_idf <- book_words %>%
  bind_tf_idf(word, gender, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(gender) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gender, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_light()

#### Topic modeling ####

#get document term matrix
#old way, with tm
setup <- function (x) {
  output <- VCorpus(VectorSource(x)) %>% #make it a corpus object
    tm_map(content_transformer(tolower)) %>% #lowercase all letters
    tm_map(removeWords, stopwords("english")) %>%#remove stopwords in English
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stemDocument) 
  return(output)
}

#Make objects you need
corpus <- setup(text_df$text) 

#after first LDA, realized need to remove very common words that
#don't distinguish the topics
corpus <- tm_map(corpus, removeWords, c('god',
                                        'church',
                                        'gospel',
                                        'jesus',
                                        'book'))

#corpusx <- tidy(corpus)
corpusx <- DocumentTermMatrix(VCorpus(VectorSource(corpus))) #make it a document/term matrix. Need for associations



#then model, get 5-10 topics per gender
#make sure we don't have rows with all 0
row.sum <- apply(corpusx, 1, FUN = sum)
corpusx <- corpusx[row.sum != 0,]
ap_lda <- LDA(corpusx, k = 4, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#look at document/topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")


#### Other old things you can do ####

#ASSOCIATONS
findAssocs(corpusx, "gospel", .8) #associations with top word


findFreqTerms(corpusx, 50) #finds frequent terms in object that appear at least 50 times



#Ngrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(corpus, #cleaned corpus
                          control = list(tokenize = BigramTokenizer))


findFreqTerms(tdm, 10) #will find ngrams that appear N number of times

freqns(tdm) #will give frequency table of ngrams


#### LDA with tidy text ####
text_df <- data.frame(text = dsl$publication_titles,
                      document = seq(1, length(dsl$last_name), 1),
                      gender = dsl$gender)

text_words <- text_df %>%
  unnest_tokens(word, text)

#data(stop_words)

text_words <- text_words %>%
  anti_join(stop_words) %>%
  anti_join(data.frame(word = c('1',
                                '2',
                                '3',
                                '4',
                                '5',
                                '6',
                                '7',
                                '8',
                                '9',
                                '10')))


book_words <- text_words %>%
  count(document, word, sort = TRUE)

#make dtm
titles_dtm <- book_words %>%
  cast_dtm(document, word, n)

titles_lda <- LDA(titles_dtm, k = 4, control = list(seed = 1234))

titles_topics <- tidy(titles_lda, matrix = "beta")

top_terms <- titles_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


