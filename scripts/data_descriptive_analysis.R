
###############################################################################
# Packages
###############################################################################
# for data preparation 
library(dplyr)
library(tidyr)
library(stringr)
# for text cleaning
library(tm)
# for sentiment data
library(tidytext)
# for grade scores
library(quanteda)

###############################################################################
# Get Data
###############################################################################

# read in data
data <- read.csv(file = './data/clean_data/clean.csv')

# rename Actor: as this files creates graphs, it's nicer to have not every 
# character capitalized
data <- data %>% mutate(actor= str_to_title(actor),
                        actor_guest= str_to_title(actor_guest))


# split if actors say the same thing (e.g. ASHLEY AND LAURA: that's great into
# ASHLEY: that's great ; LAURA: that's great)  
individual_cast_member <- data %>% 
  mutate(actor_guest = strsplit(as.character(actor_guest), " And ")) %>% 
  unnest(actor_guest) 

# splits text into words
individual_split_words <- individual_cast_member %>% 
  unnest_tokens(word, text)  

# time per segment
segment <- data %>% 
  mutate(segment = replace(segment, 
                           segment == "first_half"  | segment=="second_half",
                           "Role Play")) %>% 
  group_by(segment) %>% 
  summarise(time = sum(time_in_sec))



###############################################################################
# Time Combat versus RP by Arc
###############################################################################

# data frame
combat_rp_arc <- data %>% 
  filter(!is.na(rp_combat)) %>% 
  group_by(arc, arc_no,  rp_combat) %>% 
  summarise(time = sum(time_in_sec)) %>%
  mutate(total_time = sum(time),
         percent = (time / total_time)*100,
         arc = str_replace(arc, "_"," "),
         rp_combat = str_to_title(str_replace(rp_combat, "_"," "))) 

# export data frame
write.csv(combat_rp_arc,
          "./data/data_for_graphs/combat_rp_arc.csv", 
          row.names = FALSE)  



###############################################################################
# Attendance 
###############################################################################

# create data frame, which counts how often a cast member was present as percent
# of episodes
attendance <- individual_cast_member %>%
  filter(staff != 1) %>% 
  group_by(episode, actor_guest) %>% 
  tally() %>%
  group_by(actor_guest) %>%
  tally() %>% 
  filter(n != 1) %>%
  rename(episodes = n) %>%
  mutate(percent = (episodes/115)*100 ) 

# export data frame
write.csv(attendance,"./data/data_for_graphs/attendance.csv", row.names = FALSE)  



###############################################################################
# Words and Time Per Actor
###############################################################################

# Actor: Words
actor_words_time <- individual_split_words %>%
  filter(staff != 1) %>% 
  group_by(actor_guest) %>% 
  count() %>%
  rename(words = n) %>%
  ungroup() %>% 
  mutate(total_words = sum(words)) %>% 
  filter(words != 3) %>% 
  mutate(words_percent = (words / total_words)*100 ) 

# Actor: Time
actor_words_time <- individual_split_words %>%
  filter(staff != 1) %>% 
  group_by(actor_guest) %>% 
  summarise(time = sum(time_in_sec)) %>%
  mutate(total_time = sum(time)) %>% 
  filter(time > 1000) %>% 
  mutate(time_percent = (time / total_time)*100 ) %>%
  right_join(.,actor_words_time)

# export data frame
write.csv(actor_words_time,
          "./data/data_for_graphs/actor_words_time.csv", 
          row.names = FALSE)  


###############################################################################
# Top Words 5 per Actor
###############################################################################

# amounts of words spoken in total per actor
total_words <- individual_split_words %>%
  filter(staff != 1) %>% 
  group_by(actor_guest) %>% 
  count() %>% 
  filter(n > 5) %>% 
  rename(total_words = n)  


# delete words, which are not really telling like stopwords, puncutation etc.
top_words_actor <- individual_split_words 
# Remove english common stopwords
top_words_actor$word = removeWords(top_words_actor$word, stopwords("english"))
# Remove punctuation
top_words_actor$word = removePunctuation(top_words_actor$word)
# Eliminate extra white spaces
top_words_actor$word = stripWhitespace(top_words_actor$word)
# Drop rows, which contained words, which were dropped in previous steps
top_words_actor <- top_words_actor %>% filter(word!="")

# top 5 words per actor
top_words_actor <- top_words_actor %>%
  filter(staff != 1) %>% 
  group_by(actor_guest, word) %>% 
  count() %>% 
  filter(n>1) %>%
  group_by(actor_guest) %>%
  slice_max(n,n=5) %>% 
  rename(count = n) 

# add total words
top_words_actor <- top_words_actor %>% 
  left_join(.,total_words, by="actor_guest") %>% 
  mutate(percent = (count/total_words)*100) 

# remove not needed data frame
rm(total_words)

# export data frame
write.csv(top_words_actor,
          "./data/data_for_graphs/top_words_actor.csv", 
          row.names = FALSE)  


###############################################################################
# Grade Equivalent 
###############################################################################


# grade equivalent text: https://rdrr.io/cran/quanteda/man/textstat_readability.html

# get grades for each text longer than 5.
readability <- textstat_readability(
  individual_cast_member$text,
  measure=c("Coleman.Liau.grade"),
  remove_hyphens = TRUE,
  min_sentence_length = 5,
  max_sentence_length = 10000) %>% 
  as.data.frame() %>% 
  select(Coleman.Liau.grade) %>% 
  bind_cols(individual_cast_member,.)

readability <- readability%>%
  filter(staff != 1) %>% 
  group_by(actor_guest) %>% 
  summarise_at(vars(Coleman.Liau.grade), 
               list(mean), 
               na.rm=TRUE) %>% 
  filter(!is.na(Coleman.Liau.grade))

# export data frame
write.csv(readability,
          "./data/data_for_graphs/readability.csv", 
          row.names = FALSE)  


###############################################################################
# Network graph: Who speaks with whom 
###############################################################################

# get only actors column and kick out staff and rename all guests as 'guests'
network <- data %>% 
  filter(staff != 1) %>% 
  select(actor_guest) 

# kick out stuff said by multiple actors simultaneously
# and then get the previous speaker   
network <- network %>% 
  filter(!grepl('And', actor_guest)) %>% 
  mutate(previous = lag(actor_guest)) %>% 
  filter(actor_guest != previous) %>% 
  group_by(actor_guest, previous) %>% 
  tally()

combinations <- network %>% select(actor_guest,previous)

# make the combination a new variable
network <- network %>% unite("combination",
                             actor_guest:previous, 
                             sep =" And ",
                             remove = FALSE)

# order the combination always the same way: Liam and Sam = Sam and Liam
network$combination = unname(sapply(network$combination, function(x) {
  paste(sort(trimws(strsplit(x[1], ' And ')[[1]])), collapse=' And ')} ))

# get number of occurrences for each combination and then split combinations again
network <- network %>% 
  select(combination,n) %>% 
  group_by(combination) %>%   
  summarise(total = sum(n)) %>% 
  separate(combination, 
           c("actor_guest","previous"),  
           sep = " And ", 
           fill = "left", 
           remove=TRUE) %>% 
  left_join(combinations, .) 

# prepare the network: links
network <- network %>% 
  filter(actor_guest != "Matt") %>% 
  filter(previous != "Matt") %>% 
  mutate(actor_guest=str_to_title(actor_guest),
         previous=str_to_title(previous)) %>% 
  rename(from = actor_guest, to = previous, weights = total) %>% 
  mutate(type = "hyperlink") %>% 
  mutate(weights = replace(weights, is.na(weights), 1))

# export data frame
write.csv(network,
          "./data/data_for_graphs/network.csv", 
          row.names = FALSE)  



###############################################################################
# Heat Map
###############################################################################


multiple_speaker <- data %>%
  filter(staff != 1) %>% 
  select(actor_guest) %>% 
  filter(grepl("And", actor_guest)) 

multiple_speaker$total_words <- sapply(multiple_speaker$actor_guest, 
                                       function(x) length(unlist(
                                         strsplit(as.character(x), "\\W+"))))
# only two speakers already
heatmap <- multiple_speaker %>% filter(total_words == 3) %>% select(actor_guest)

# three speakers
heatmap <- multiple_speaker %>% filter(total_words == 5) %>% 
  mutate(first = word(actor_guest, start = 1, end = 3)) %>% 
  mutate(second = word(actor_guest, start = 3, end = 5)) %>% 
  mutate(third1 = word(actor_guest, start = 1, end = 2), 
         third2 = word(actor_guest, 5),
         third = paste(third1, third2, sep=" ")) %>%
  select(first,second,third) %>% 
  pivot_longer(first:third , 
               names_to = "position", 
               values_to = "actor_guest") %>% 
  select(actor_guest) %>% 
  bind_rows(.,heatmap)

# four speakers
heatmap <- multiple_speaker %>% filter(total_words == 7) %>% 
  mutate(first = word(actor_guest, start = 1, end = 3)) %>% 
  mutate(second = word(actor_guest, start = 3, end = 5)) %>%
  mutate(third = word(actor_guest, start = 5, end = 7)) %>%
  mutate(fourth1 = word(actor_guest, start = 1, end = 2),
         fourth2 = word(actor_guest, 5),
         fourth = paste(fourth1, fourth2, sep=" ")) %>%
  mutate(fifth1 = word(actor_guest, start = 1, end = 2),
         fifth2 = word(actor_guest, 7),
         fifth = paste(fifth1, fifth2, sep=" ")) %>%
  mutate(sixth1 = word(actor_guest, start = 3, end = 4),
         sixth2 = word(actor_guest, 7),
         sixth = paste(sixth1, sixth2, sep=" ")) %>%
  select(3,4,5,8,11,14) %>% 
  pivot_longer(first:sixth , 
               names_to = "position", 
               values_to = "actor_guest") %>% 
  select(actor_guest) %>% 
  bind_rows(.,heatmap) 

# clean up
rm(multiple_speaker)

# same order for speaker
heatmap$actor_guest = unname(sapply(heatmap$actor_guest, function(x) {
  paste(sort(trimws(strsplit(x[1], ' And ')[[1]])), collapse=' And ')} ))


# split after And
heatmap <- heatmap %>% 
  separate(actor_guest, c("actor1","actor2"), sep = " And ", 
           fill = "left", remove=TRUE, extra = "merge") %>%
  group_by(actor1, actor2) %>%
  tally() %>% 
  rename(occurrence = n) %>% 
  filter(occurrence != 1) 

# add variable for ordering the rows 
heatmap <- data.frame(actor1 = c("Guests","Ashley", "Laura", "Liam", "Marisha", "Matt", 
                                 "Orion", "Sam", "Taliesin"),
                      order = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>% 
  right_join(. , heatmap)

# export data frame
write.csv(heatmap,
          "./data/data_for_graphs/heatmap.csv", 
          row.names = FALSE)  


###############################################################################
# Sentiment Analysis Episodes
###############################################################################

sentiment_data <- individual_cast_member %>%
  filter(staff != 1) %>% 
  unnest_tokens(word, text)

sentiment_data <- sentiment_data %>% 
  inner_join(get_sentiments("afinn")) %>% 
  inner_join(get_sentiments("bing")) %>% 
  rename(afinn_sentiment = value,
         bing_sentiment = sentiment) %>% 
  mutate(bing_sentiment=recode(bing_sentiment, 
                               negative = -1,
                               positive = 1))

sentiment_episodes <- sentiment_data %>% 
  group_by(episode, arc, arc_no) %>%
  summarise_at(vars(bing_sentiment, afinn_sentiment), 
               list(mean = mean), 
               na.rm=TRUE)

# export data frame
write.csv(sentiment_episodes,
          "./data/data_for_graphs/sentiment_episodes.csv", 
          row.names = FALSE) 




###############################################################################
# Sentiment per Arc
###############################################################################

sentiment_arc <- sentiment_episodes %>% 
  group_by(arc, arc_no) %>% 
  summarise_at(vars(bing_sentiment_mean, afinn_sentiment_mean), 
               list(mean = mean), 
               na.rm=TRUE) %>% 
  rename(bing_sentiment_mean = bing_sentiment_mean_mean,
         afinn_sentiment_mean = afinn_sentiment_mean_mean)

# export data frame
write.csv(sentiment_arc,
          "./data/data_for_graphs/sentiment_arc.csv", 
          row.names = FALSE) 


###############################################################################
# Sentiment per Actor
###############################################################################

# prepare data 
sentiment_actor <- sentiment_data %>% 
  filter(staff != 1) %>% 
  group_by(actor_guest) %>%
  summarise_at(vars(bing_sentiment, afinn_sentiment), 
               list(mean = mean), 
               na.rm=TRUE)

# export data frame
write.csv(sentiment_actor,
          "./data/data_for_graphs/sentiment_actor.csv", 
          row.names = FALSE) 



###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE)) 