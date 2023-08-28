#### General instructions ####

#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)
library(lubridate)
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(reshape2)
library(textstem)



#### Setting up my data for ann arbor vs detroit vs columbus topics ####

#list subs you want
subs <- c("uofm",
          "OSU")

results <- data.frame(url = NA,
                      title = NA,
                      sub = NA,
                      posts = NA,
                      date = NA,
                      comments = NA)
#get sub posts
for(i in 1:length(subs)) {
  step1 <- find_thread_urls(subreddit = subs[i], sort_by = "new")
  output <- data.frame(url = step1$url,
                       title = step1$title,
                       sub = subs[i],
                       posts = step1$text,
                       date = step1$date_utc,
                       comments = step1$comments)
  
  results <- results %>%
    rbind(output)
}

results$month <- month(ymd(results$date), label = T)

#If you also want to get meta data and comments:
results_comments <- data.frame(url = NA,
                               date = NA,
                               comment = NA,
                               upvotes = NA,
                               downvotes = NA,
                               author = NA)

#get urls for posts with comments only
urls_filtered <- results %>%
  filter(comments > 0) %>%
  select(url)

#now run this to get all the actual comments
for(i in 1:length(urls_filtered$url)) {
  tryCatch({
    contents <- get_thread_content(urls_filtered$url[i])
    comments <- contents$comments %>%
      comments1 <- comments %>%
      select(url,
             date,
             comment,
             upvotes,
             downvotes,
             author)}, error=function(e){})
  
  results_comments <- results_comments %>%
    rbind(comments)
}

#filter out automod comments
thread1_contents <- thread1_contents %>%
  filter(author != "AutoModerator")



#### Basic overview ####

#Subs and counts of posts by month
results %>%
  group_by(sub, month) %>%
  summarise(posts = length(posts))

#Subs and mentions of keywords

results$chunky <- ifelse(grepl("chunky", results$title) == TRUE, 1, 0)

results %>%
  group_by(sub) %>%
  summarise(percent_chonky = mean(chunky, na.rm = T) * 100)

#top titles
ds_text <- data.frame(line = seq(1, length(results$title), 1),
                      text = results$title,
                      sub = results$sub)

#trigrams for more context
ds_text_bi <- ds_text %>%
  unnest_tokens(word, text, token = 'ngrams', n = 1) %>%
  filter(is.na(word) == F &
           is.na(sub) == F)

data("stop_words")

ds_text_bi <- ds_text_bi %>%
  anti_join(stop_words, by = "word")

top20 <- ds_text_bi %>%
  group_by(sub) %>%
  count(word) %>%
  slice_max(., order_by = n, n = 20)





top20$theme <- c("school/general",
                 "school/general",
                 "academics",
                 "school/general",
                 "academics",
                 "academics",
                 "sports",
                 "academics",
                 "sports",
                 "other",
                 "academics",
                 "sports",
                 "sports",
                 "other",
                 "academics",
                 "other",
                 "academics",
                 "sports",
                 "academics",
                 "academics", #20
                 "academics",
                 "school/general",
                 "academics",
                 "academics",
                 "academics",
                 "academics",
                 "academics",
                 "academics",
                 "other",
                 "other",
                 "school/general",
                 "academics",
                 "academics",
                 "academics",
                 "academics",
                 "school/general",
                 "academics",
                 "academics",
                 "academics",
                 "academics")


#make the words factors so that you can order them how you want
#first need an invisible filler because some words are repeated between subs and that ruins the factor strategy
top20$filler <- NA
top20$filler[top20$sub == "OSU"] <- " "
top20$filler[top20$sub == "uofm"] <- "  "

#order the words you need
top20 <- top20 %>%
  arrange(sub, n)

top20$wordfac <- paste(top20$word, top20$filler, sep = "")
top20$wordfac <- factor(top20$wordfac,
                        levels = top20$wordfac)

ggplot(top20, aes(x = wordfac, y = n, fill = theme)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("#355C7D", "#6C5B7B", "#F67280", "#F8B195")) +
  theme_classic(base_size = 14) +
  facet_wrap(~sub, ncol = 2, scales = "free") +
  ggtitle("Most frequent words in new post titles\nUM vs OSU subreddits")
