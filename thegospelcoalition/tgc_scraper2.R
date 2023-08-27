#TGC contributors andd their article titles
setwd('~/Desktop')

write.csv(ds %>%
            select(-X), file = 'tgc_authors_articles_data3.csv')

library(rvest)
library(tidyverse)
library(jsonlite)

ds <- read.csv('~/Desktop/tgc_authors_articles_data.csv')

#or, get it with titles:
ds <- read.csv('tgc_authors_articles_data3.csv')



#### Preliminary practice ####
#read html
authors_list_page <- read_html('https://www.thegospelcoalition.org/authors')

t1 <- html_nodes(authors_list_page, ".author_last_name") 
t1 <- html_text(t1) #convert the nodes to text
t1 <- gsub("\n","", t1_text)
t2 <- html_nodes(authors_list_page, ".author_first_name")
t2 <- html_text(t2)
t2 <- gsub("\n", "", t2)

#data frame of author first names and last names
ds <- data.frame(last_name = t1,
                 first_name = t2)

#links to profile pages
t4 <- html_nodes(authors_list_page, "a") %>%
  html_attr('href')
#got too many, cut to only include profile pages
t4 <- t4[133:2605]
t4 <- unique(t4) #were some duplicates
t4 <- t4[1:2439] #there were some other things at the end
#yay! the heads and tails match
ds <- ds %>%
  mutate(profile_urls = t4)




#This gets article titles! But have to manually get the js from the devtools in chrome
#I get this with devtools, network, the URL for tgc-ajax.php
joe_carter_json <- fromJSON("https://www.thegospelcoalition.org/wp-content/themes/sage/tgc-ajax.php?action=luther_profile_article_load_more&post_id=82082&args%5Bprofile_post_id%5D=82090&args%5Bfilter_type%5D%5B%5D=all&page=1&posts_per_page=30")
joe_carter_html <- joe_carter_json$data$html
read_html(joe_carter_html) %>%
  html_nodes('h2') %>%
  html_text() %>%
  gsub('\n\t\t\t\t\n\t\t\t\t\t', '', .) %>%
  gsub('\t\t\t\t\n\t\t\t', '', .)


#If i add 83197 as the profile_post_id (Tim Keller) can I get his articles?
#Yes!
tk_json <- fromJSON("https://www.thegospelcoalition.org/wp-content/themes/sage/tgc-ajax.php?action=luther_profile_article_load_more&post_id=82082&args%5Bprofile_post_id%5D=83197&args%5Bfilter_type%5D%5B%5D=all&page=1&posts_per_page=100")
tk_html <- tk_json$data$html
read_html(tk_html) %>%
  html_nodes('h2') %>%
  html_text() %>%
  gsub('\n\t\t\t\t\n\t\t\t\t\t', '', .) %>%
  gsub('\t\t\t\t\n\t\t\t', '', .)

#what happens if I allow more articles than the author has?
#nothing; just pulls the results
tk_json <- fromJSON("https://www.thegospelcoalition.org/wp-content/themes/sage/tgc-ajax.php?action=luther_profile_article_load_more&post_id=82082&args%5Bprofile_post_id%5D=1006&args%5Bfilter_type%5D%5B%5D=all&page=1&posts_per_page=100")
tk_html <- tk_json$data$html
read_html(tk_html) %>%
  html_nodes('h2') %>%
  html_text() %>%
  gsub('\n\t\t\t\t\n\t\t\t\t\t', '', .) %>%
  gsub('\t\t\t\t\n\t\t\t', '', .)

#but didn't always work. For this author post_id is different, not just profile_post_id:
#https://www.thegospelcoalition.org/wp-content/themes/sage/tgc-ajax.php?action=luther_profile_article_load_more&post_id=106083&args%5Bprofile_post_id%5D=106127&args%5Bfilter_type%5D%5B%5D=all&page=1&posts_per_page=10
#dean Abbott


#So we need each author's profile_post_id
#Can we get that from each author's page?
#When I just look at the html I can see it at the top
jc_profile <- read_html("https://www.thegospelcoalition.org/profile/joe-carter/") %>%
  html_nodes('script') #the ID is in there

#can treat it as text and split around the ' that bracket the number
jc_profile_split <- strsplit(as.character(jc_profile[[2]]), "'")
jc_profile_split <- jc_profile_split %>%
  unlist() #unlist it to make it a vector
jc_profile_split[2] #grab the number which is the second element

#Does this work with another profile?
#Yes, can grab for Tim Keller the same way
tk_profile <- read_html("https://www.thegospelcoalition.org/profile/tim-keller/") %>%
  html_nodes('script') #the ID is in there

#can treat it as text and split around the ' that bracket the number
tk_profile_split <- strsplit(as.character(tk_profile[[2]]), "'")
tk_profile_split <- tk_profile_split %>%
  unlist() #unlist it to make it a vector
tk_profile_split[2] #grab the number which is the second element


#### Script to get author IDs ####

authid <- function(url) {
    profile <- read_html(url) %>%
      html_nodes('script') #the ID is in there
    
    profile_split <- strsplit(as.character(profile[[2]]), "'")
    profile_split <- profile_split %>%
      unlist() #unlist it to make it a vector
    author_id <- profile_split[2] #grab the number which is the second element
    
    return(author_id)
    
  }

author_ids <- rep(NA, 2439)
for(i in 2201:2439) {
  author_ids[i] <- authid(ds$profile_urls[i])
}

author_ids_a <- author_ids #1-450
author_ids_b <- author_ids[451:712]
author_ids_c <- author_ids[713:900]
author_ids_d <- author_ids[901:1436]
author_ids_e <- author_ids[1437:1600]
author_ids_f <- author_ids[1601:1900]
author_ids_g <- author_ids[1901:2200]
author_ids_h <- author_ids[2201:2439]

author_id0 <- c(author_ids_a,
                author_ids_b,
                author_ids_c,
                author_ids_d,
                author_ids_e,
                author_ids_f,
                author_ids_g,
                author_ids_h)
ds <- ds %>%
  mutate(author_id = author_id0)


#spot check
ds[100,] #averbeck, 1888

ds[200,] #Guillaume, 439106

ds[2100,] #stoddard, 3011

ds[2439,] #Zylstra, 224011


#### Script to get article titles ajax urls ####

half1 <- "https://www.thegospelcoalition.org/wp-content/themes/sage/tgc-ajax.php?action=luther_profile_article_load_more&post_id=82082&args%5Bprofile_post_id%5D="

half2 <- "&args%5Bfilter_type%5D%5B%5D=all&page=1&posts_per_page=500" #limit to 500

urls <- data.frame(h1 = rep(half1, 2439),
                   h2 = rep(half2, 2439),
                   ids = ds$author_id)
final <- unite(urls %>%
    select(h1, ids, h2), col = final, sep = "")

urls$final <- final

ds$ajax_urls <- final$final

#### Script to get json then html ####

html_extractor <- function(url) {
  tk_json <- fromJSON(url)
  tk_html <- tk_json$data$html
  
  return(tk_html)
}

jsons <- rep(NA, 2439)

#for(i in 1:length(ds$ajax_urls)) {
#  jsons[i] <- html_extractor(ds$ajax_urls[i])
#}
#stopped, continued at 1133
for(i in 1133:length(ds$ajax_urls)) {
  jsons[i] <- html_extractor(ds$ajax_urls[i])
}

#write.csv(jsons, file = "jsons.csv")

#### Run to get titles ####

#generic script
titles_extractor <- function(url) {
  titles <- read_html(url) %>%
    html_nodes('h2') %>%
    html_text() %>%
    gsub('\n\t\t\t\t\n\t\t\t\t\t', '', .) %>%
    gsub('\t\t\t\t\n\t\t\t', '', .)

titles <- paste(as.character(titles), collapse="// ") #going to get a cell with every title, separated by "/" so I can expand it later

return(titles)
}

ds$publication_titles <- NA

for(i in 1:length(ds$ajax_urls)) {
  tryCatch({
    ds$publication_titles[i] <- titles_extractor(jsons[i])}, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



#### Run to get media types ####
#this gets the media type (book review, article, podcast, conference media, sermon, video)
#generic script
media_extractor <- function(url) {
  media <- read_html(url) %>%
    html_nodes('.post-type-label') %>%
    html_text()
  
  media <- paste(as.character(media), collapse="// ") #going to get a cell with every title, separated by "/" so I can expand it later
  
  return(media)
}

ds$media_types <- NA

for(i in 1:length(ds$author_id)) {
  tryCatch({
    ds$media_types[i] <- media_extractor(jsons[i])}, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#### Run to get publication date ####

date_extractor <- function(url) {
  datez <- read_html(url) %>%
    html_nodes('time') %>%
    html_text() %>%
    gsub("\n\t\t\t\t\t\t\t", "", .) %>%
    gsub("\t\t\t\t\t\t", "", .)
  
  datez <- paste(as.character(datez), collapse="// ") #going to get a cell with every title, separated by "/" so I can expand it later
  
  return(datez)
}


ds$publication_dates <- NA

for(i in 1:length(ds$author_id)) {
  tryCatch({
    ds$publication_dates[i] <- date_extractor(jsons[i])}, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#### Dedupe ####
#should have done this before oops
#de-dup names
ds$full_name <- unite(data = ds %>%
                        select(first_name,
                               last_name), sep = "_",
                      col = 'full_name')

ds <- ds %>%
  filter(duplicated(full_name) == FALSE)

ds <- ds %>%
  select(-full_name)



write.csv(ds, file = 'tgc_authors_articles_data4.csv')


#### Author Bios ####

#the script in this section gets the bios
#but since I had them in ds3 I will just join them in
bios <- read.csv("tgc_authors_articles_data3.csv")

ds<- ds %>%
  left_join(bios %>%
              select(author_id, author_bio), by = 'author_id')

#script to get the bio of each author
bios <- function(url) {
  bios <- read_html(url) %>%
    html_nodes('p') %>%
    html_text()
  
  return(bios)
}

#bios(ds$profile_urls[6])

ds$author_bio <- NA

for(i in 1:length(ds$author_id)) {
  tryCatch({
    ds$author_bio[i] <- bios(ds$profile_urls[i])}, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
} 


#sanity check
ds$author_bio[5]
ds$author_bio[50]
ds$author_bio[500]


write.csv(ds, file = 'tgc_authors_articles_data4.csv')

#### Final data formatting ####

#get longform versions of titles, media and dates

trunc1 <- ds %>%
  separate_rows(publication_titles,
                sep = "// ") 

#expand media 
trunc2 <- ds %>% 
  separate_rows(media_types,
                sep = "// ") 

#dates
trunc3 <- ds %>%
  separate_rows(publication_dates,
                sep = "// ") 

#If trunc1, 2, 3 have the same number of rows, combine together

ds_long <- trunc1 %>%
  select(-media_types,
         -publication_dates) 

ds_long$media_types <- trunc2$media_types
ds_long$publication_dates <- trunc3$publication_dates
 

#separate dates into years, months, days

ds_long <- ds_long %>%
  separate(col=publication_dates, sep = ', ', c('Month', 'Year'))
ds_long <- ds_long %>%
  separate(col = Month,
           sep = " ", 
           c('month', 'day'))




write.csv(ds_long, file = 'tgc_authors_articles_data_long2.csv')
#I will manually do a sanity check on the above (do titles
#match dates and media type)



#### Gender data ####
#I manually coded by name (and often using bios/links to check)

gend <- read.csv('author_genders.csv') %>%
  select(author_id, gender)

gend$gender <- factor(gend$gender,
                      levels = c(0, 9),
                      labels = c('male', 'female'))
table(gend$gender) %>%
  prop.table()

ds <- ds %>%
  left_join(gend %>%
              select(author_id, gender), by = 'author_id')
ds_long <- ds_long %>%
  left_join(gend %>%
              select(author_id, gender), by = 'author_id')

write.csv(ds, file = 'tgc_authors_articles_data4.csv')
write.csv(ds_long, file = 'tgc_authors_articles_data_long2.csv')


