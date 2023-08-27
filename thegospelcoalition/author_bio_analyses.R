
#author_bios

library(tidytext)



#get data, short form
ds <- read.csv('tgc_authors_articles_data4.csv')
ds <- ds %>%
  filter(is.na(publication_titles) == FALSE)

ds <- ds %>%
  select(-c(X.3, X.2, X.1, X, ajax_urls))

#### frequent words by gender ####
text_df <- data.frame(text = ds$author_bio,
                      line = seq(1, length(ds$author_bio), 1),
                      gender = ds$gender)

text_words <- text_df %>%
  unnest_tokens(word, text)

data(stop_words)
text_words <- text_words %>%
  anti_join(stop_words)

book_words <- text_words %>%
  count(gender, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(gender) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
  

#top male words
book_words %>%
  filter(gender == 'male') %>%
  arrange(-n) %>%
  head(30)

#top female words
book_words %>%
  filter(gender == 'female') %>%
  arrange(-n) %>%
  head(30)

#### Bigrams by gender ####
text_df <- data.frame(text = ds$author_bio,
                      line = seq(1, length(ds$author_bio), 1),
                      gender = ds$gender)

text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

#female bigrams
fem_bigrams <- data.frame(text = ds$author_bio,
                      line = seq(1, length(ds$author_bio), 1),
                      gender = ds$gender) %>%
  filter(gender == 'female') %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
colnames(fem_bigrams) <- c('bigram', 'female_n')

#male bigrams
male_bigrams <- data.frame(text = ds$author_bio,
                          line = seq(1, length(ds$author_bio), 1),
                          gender = ds$gender) %>%
  filter(gender == 'male') %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
colnames(male_bigrams) <- c('bigram', 'male_n')


text_bigrams <- text_bigrams %>%
  left_join(fem_bigrams, by = 'bigram') %>%
  left_join(male_bigrams, by = 'bigram')

female_bigram_total <- data.frame(text = ds$author_bio,
                                  line = seq(1, length(ds$author_bio), 1),
                                  gender = ds$gender) %>%
  filter(gender == 'female') %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(line) 
female_bigram_total <- length(female_bigram_total$line) 


male_bigram_total <- data.frame(text = ds$author_bio,
                                  line = seq(1, length(ds$author_bio), 1),
                                  gender = ds$gender) %>%
  filter(gender == 'male') %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(line) 
male_bigram_total <- length(male_bigram_total$line) 

text_bigrams <- text_bigrams %>%
  mutate(female_percent = female_n / female_bigram_total * 100,
         male_percent = male_n / male_bigram_total * 100)

text_bigrams <- text_bigrams %>%
  mutate(percent_diff = male_percent - female_percent)

head(text_bigrams %>%
       arrange(-percent_diff), 15)

head(text_bigrams %>%
       arrange(percent_diff), 15)

distinct_bigrams <- rbind(head(text_bigrams %>%
                                 arrange(-percent_diff), 15),
                          head(text_bigrams %>%
                                 arrange(percent_diff), 15)
                          )

write.csv(distinct_bigrams, file = 'distinct_bio_bigrams.csv')

biograms <- read.csv('bigram_bio_gender.csv')
biograms <- biograms %>%
  gather(key = gender, value = percent, female_percent:male_percent)

ggplot(biograms, aes(reorder(X, percent), percent, fill = gender)) +
  geom_bar(stat = 'identity') +
  coord_flip()




#### percent relational, educational words ####
words_relational <- c('mother',
                'mama',
                'mom',
                'mommy',
                'father',
                'dad',
                'daddy',
                'husband',
                'spouse',
                'wife',
                'married',
                'parent',
                'daughter',
                'daughters',
                'son',
                'sons',
                'children',
                'kids',
                'family')

#bios that have specific graduate degrees listed
words_educational <- c("mdiv",
                       "ma",
                       "thm",
                       "jd",
                       "masters",
                       "phd",
                       "doctorate")


#What percent of women's vs men's bios have relational words?
text_df <- data.frame(text = ds$author_bio,
                      line = seq(1, length(ds$author_bio), 1),
                      gender = ds$gender)

text_words <- text_df %>%
  unnest_tokens(word, text)

#data(stop_words)

text_words <- text_words %>%
  anti_join(stop_words)

#line is the bio number.

#so first, flag all relational words
text_words$relational <- 0
for(i in 1:length(text_words$word)) {
  text_words$relational[i] <- ifelse(text_words$word[i] %in% words_relational, 1, 0)
}

#now determine if a given bio has at least 1
relational_ds <- text_words %>%
  group_by(gender, line) %>%
  summarise(total_relational_words = sum(relational))
relational_ds$has_relational_words <- ifelse(relational_ds$total_relational_words > 0, 1, 0)

#density function
ggplot(relational_ds %>%
         filter(is.na(gender) == FALSE), aes(total_relational_words, group = gender, fill = gender)) +
  geom_density(alpha = .3)

#binary
relational_ds %>%
  group_by(gender) %>%
  summarise(percent_relational_bios = round(mean(has_relational_words)*100, 0))






#What percent of women's vs men's bios have educational words?

#so first, flag all educational words
text_words$educational <- 0
for(i in 1:length(text_words$word)) {
  text_words$educational[i] <- ifelse(text_words$word[i] %in% words_educational, 1, 0)
}

#now determine if a given bio has at least 1
educational_ds <- text_words %>%
  group_by(gender, line) %>%
  summarise(total_educational_words = sum(educational))
educational_ds$has_educational_words <- ifelse(educational_ds$total_educational_words > 0, 1, 0)

#density function
ggplot(educational_ds %>%
         filter(is.na(gender) == FALSE), aes(total_educational_words, group = gender, fill = gender)) +
  geom_density(alpha = .3)

#binary
educational_ds %>%
  group_by(gender) %>%
  summarise(percent_educational_bios = round(mean(has_educational_words)*100, 0))


#or do phd
words_educational <- c("phd",
                       "doctorate")

#so first, flag all educational words
text_words$educational <- 0
for(i in 1:length(text_words$word)) {
  text_words$educational[i] <- ifelse(text_words$word[i] %in% words_educational, 1, 0)
}

#now determine if a given bio has at least 1
educational_ds <- text_words %>%
  group_by(gender, line) %>%
  summarise(total_educational_words = sum(educational))
educational_ds$has_educational_words <- ifelse(educational_ds$total_educational_words > 0, 1, 0)

#density function
ggplot(educational_ds %>%
         filter(is.na(gender) == FALSE), aes(total_educational_words, group = gender, fill = gender)) +
  geom_density(alpha = .3)

#binary
educational_ds %>%
  group_by(gender) %>%
  summarise(percent_educational_bios = round(mean(has_educational_words)*100, 0),
            n = length(unique(line)))

#what females have phd?
phd_ladies <- educational_ds %>%
  filter(gender == 'female' &
           has_educational_words == 1) %>%
  data.frame() %>%
  select(line)

ds[phd_ladies$line, c('first_name',
                      'last_name')]

#### tf-idf by gender ####
text_df <- data.frame(text = ds$author_bio,
                         line = seq(1, length(ds$author_bio), 1),
                      gender = ds$gender)

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

