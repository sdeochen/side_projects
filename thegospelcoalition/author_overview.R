
#get data, short form
ds <- read.csv('tgc_authors_articles_data4.csv')

#color palette
#TGC site and women's conference 2022
pal <- c('#84AC57', #tgc green
         '#F1F1F1', #site light gray
         '#CD8F8C', #dusty red
         '#E9C883', #yellow
         '#98AABF', #blue
         '#212D40', #dark blue?green?
         '#68766D' #olive green
         )


#filter for those who have publications
#(new data file already has them filtered)
ds <- ds %>%
  filter(is.na(publication_titles) == FALSE) %>%
  filter(author_id != 2405)



#Number of distinct authors
#from this I created a filter (above) since not everyone
#listed as an author actually has publications up
#For example Ravi Zacharias and Mark Driscoll were removed.
length(unique(ds$author_id[which(is.na(ds$publication_titles) == FALSE)]))
#2389
length(unique(ds$author_id))

#Number of distinct authors over time and
#Gender breakdown over time
dsl <- read.csv('tgc_authors_articles_data_long.csv')
dsl <- dsl %>%
  filter(is.na(publication_titles) == FALSE) %>%
  filter(author_id != 2405)
dsl$gender <- factor(dsl$gender)

time_trend <- dsl %>%
  group_by(Year) %>%
  summarise(author_count = length(unique(author_id)),
            women_count = length(unique(author_id[which(gender == 'female')]))) %>%
  mutate(percent_women = round(women_count / author_count * 100, 0))


#Unique authors over time
ggplot(time_trend %>%
         filter(Year >= '2005' &
                  Year != '2022'), aes(Year, author_count, group = 1)) +
  geom_point(color = pal[1]) +
  geom_line(color = pal[1],
            size = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_text(face="bold", colour= pal[1], size=20),
          axis.text.x  = element_text(#angle=90, 
            #vjust=0.5, 
            size=16),
        axis.title.y = element_text(face="bold", colour= pal[1], size=20),
        axis.text.y  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16)) 


#Percent unique women over time
ggplot(time_trend %>%
         filter(Year >= '2005' &
                  Year != '2022'), aes(Year, percent_women, group = 1)) +
  geom_point(color = pal[1]) +
  geom_line(color = pal[1],
            size = 1) +
  geom_area(fill = pal[1], alpha = .25) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_text(face="bold", colour= pal[1], size=20),
        axis.text.x  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16),
        axis.title.y = element_text(face="bold", colour= pal[1], size=20),
        axis.text.y  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16)) +
  coord_cartesian(ylim = c(0, 100))


#Gender breakdown
prop.table(table(ds$gender[ds$author_id != 2405])) #don't count Andrea Salik

table(ds$gender)


## How many people produced 80% of stuff

.8 * length(dsl$author_id[duplicated(dsl$publication_titles) == FALSE]) #15,644 is 80% of stuff
#see note below and results doc
#

#Top authors - most-published of all time

#bar plot for top 5 contributors, male
#had to manually get their total article count from the ajax urls

top5male <- data.frame(Contributor = c('Justin Taylor',
                                       'Scotty Smith',
                                       'Trevin Wax',
                                       'Kevin DeYoung',
                                       'Erik Raymond'),
                       Total_Publications = c(10000,
                                              4855,
                                              3471,
                                              2626,
                                              2284))

ggplot(top5male, aes(reorder(Contributor, Total_Publications), Total_Publications)) +
  geom_col(fill = pal[1]) +
  geom_label(label = top5male$Total_Publications) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_text(face="bold", colour= pal[1], size=20),
        axis.text.x  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16),
        axis.title.y = element_text(face="bold", colour= pal[1], size=20),
        axis.text.y  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16)) +
  coord_flip()


#female authors
#andrea is  just the person who uploaded; she doesn't contribute

#top 11 female contributors
dsl %>%
  filter(gender == 'female') %>%
  group_by(author_id) %>%
  summarise(number_articles = length(author_id)) %>%
  arrange(-number_articles) %>%
  head(., 11) %>%
  data.frame() %>%
  left_join(ds %>%
              select(first_name, last_name, author_id), by = 'author_id')

top5fem <- data.frame(Contributor = c(#'Andrea Salik',
                                       'Melissa Kruger',
                                       'Nancy Guthrie',
                                       'Bethany Jenkins',
                                       'Sarah E. Zylstra',
                                       'Jen Wilkin'),
                       Total_Publications = c(#347,
                                              336,
                                              208,
                                              165,
                                              163,
                                              98))

ggplot(top5fem, aes(reorder(Contributor, Total_Publications), Total_Publications)) +
  geom_col(fill = pal[3]) +
  geom_label(label = top5fem$Total_Publications) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_text(face="bold", colour= pal[3], size=20),
        axis.text.x  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16),
        axis.title.y = element_text(face="bold", colour= pal[3], size=20),
        axis.text.y  = element_text(#angle=90, 
          #vjust=0.5, 
          size=16)) +
  coord_flip()


#### percent of publication by women####

#there are dup titles because some are blog posts (Monday Morning Humor)
#and some have multiple contributors
dsl$publication_titles %>%
  table() %>%
  data.frame() %>%
  arrange(-Freq) %>%
  head()

dupe <- duplicated(dsl$publication_titles)
table(dupe)


dsl$gender_rank <- 0
dsl$gender_rank[dsl$gender == 'female'] <- 1
gend_unique_titles <- dsl %>%
  filter(author_id != 2405) %>%
  arrange(-gender_rank) %>%
  filter(duplicated(publication_titles) == FALSE) 
  
gend_unique_titles %>%
  select(gender) %>%
  table() %>%
  prop.table()

#### media types####
media <- data.frame(
  prop.table(table(dsl$media_types)
)) %>%
  arrange(-Freq) 

remainder <- sum(media$Freq[5:9])

media <- media[1:4,]

add <- data.frame(Var1 = 'other formats',
                  Freq = remainder)

media <- rbind(media, add)

media$Var1 <- factor(media$Var1,
                     levels = c('article',
                                'blog',
                                'sermon',
                                'podcast',
                                'other formats'))

  ggplot(media, aes("", Freq, fill = Var1)) +
  geom_bar(stat = 'identity',
           width = 1) + 
  coord_flip() +
  scale_fill_manual(values=pal[c(1, 7, 4, 6, 5)])
  
#### Delete ####


