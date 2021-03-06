
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
data <- data %>% mutate(Actor= str_to_title(Actor),
                        CR_GUEST= str_to_title(CR_GUEST))


# split if actors say the same thing (e.g. ASHLEY AND LAURA: that's great into
# ASHLEY: that's great ; LAURA: that's great)  
individual_cast_member <- data %>% 
  mutate(CR_GUEST = strsplit(as.character(CR_GUEST), " And ")) %>% 
  unnest(CR_GUEST) 

# splits Text into words
individual_split_words <- individual_cast_member %>% 
  unnest_tokens(word, Text)  

# time per segment
segment <- data %>% 
  mutate(segment = replace(segment, 
                           segment=="first_half"  | segment=="second_half",
                           "Role Play")) %>% 
  group_by(segment) %>% 
  summarise(time = sum(timeDiffInSecs))



###############################################################################
# Time Combat versus RP by Arc
###############################################################################

# data frame
combat_rp_arc <- data %>% 
  filter(!is.na(rp_combat)) %>% 
  group_by(Arc,Arc_no, rp_combat) %>% 
  summarise(time = sum(timeDiffInSecs)) %>%
  mutate(total_time = sum(time),
         percent = (time / total_time)*100,
         Arc = str_replace(Arc, "_"," "),
         rp_combat = str_to_title(str_replace(rp_combat, "_"," "))) %>% 
  mutate(Arc = factor(Arc, levels = c("End", "Daring Deeds","Fall Conclave",
                                      "Vestiges", "Attack Conclave","Briarwoods", 
                                      "Vasselheim","Kraghammer")))

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
  group_by(Episode, CR_GUEST) %>% 
  tally() %>%
  group_by(CR_GUEST) %>%
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
  group_by(CR_GUEST) %>% 
  count() %>%
  rename(words = n) %>%
  ungroup() %>% 
  mutate(total_words = sum(words)) %>% 
  filter(words != 3) %>% 
  mutate(words_percent = (words / total_words)*100 ) 

# Actor: Time
actor_words_time <- individual_split_words %>%
  filter(staff != 1) %>% 
  group_by(CR_GUEST) %>% 
  summarise(time = sum(timeDiffInSecs)) %>%
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
  group_by(CR_GUEST) %>% 
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
  group_by(CR_GUEST, word) %>% 
  count() %>% 
  filter(n>1) %>%
  group_by(CR_GUEST) %>%
  slice_max(n,n=5) %>% 
  rename(count = n) 

# add total words
top_words_actor <- top_words_actor %>% 
  left_join(.,total_words, by="CR_GUEST") %>% 
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
  individual_cast_member$Text,
  measure=c("Coleman.Liau.grade"),
  remove_hyphens = TRUE,
  min_sentence_length = 5,
  max_sentence_length = 10000) %>% 
  as.data.frame() %>% 
  select(Coleman.Liau.grade) %>% 
  bind_cols(individual_cast_member,.)

readability <- readability%>%
  filter(staff != 1) %>% 
  group_by(CR_GUEST) %>% 
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
  select(CR_GUEST) 

# kick out stuff said by multiple actors simultaneously
# and then get the previous speaker   
network <- network %>% 
  filter(!grepl('And', CR_GUEST)) %>% 
  mutate(Previous = lag(CR_GUEST)) %>% 
  filter(CR_GUEST != Previous) %>% 
  group_by(CR_GUEST, Previous) %>% 
  tally()

combinations <- network %>% select(CR_GUEST,Previous)

# make the combination a new variable
network <- network %>% unite("Combination",
                             CR_GUEST:Previous, 
                             sep =" And ",
                             remove = FALSE)

# order the combination always the same way: Liam and Sam = Sam and Liam
network$Combination = unname(sapply(network$Combination, function(x) {
  paste(sort(trimws(strsplit(x[1], ' And ')[[1]])), collapse=' And ')} ))

# get number of occurrences for each combination and then split combinations again
network <- network %>% 
  select(Combination,n) %>% 
  group_by(Combination) %>%   
  summarise(total = sum(n)) %>% 
  separate(Combination, 
           c("CR_GUEST","Previous"),  
           sep = " And ", 
           fill = "left", 
           remove=TRUE) %>% 
  left_join(combinations, .) 

# prepare the network: links
network <- network %>% 
  filter(CR_GUEST != "Matt") %>% 
  filter(Previous != "Matt") %>% 
  mutate(CR_GUEST=str_to_title(CR_GUEST),
         Previous=str_to_title(Previous)) %>% 
  rename(from = CR_GUEST, to = Previous, weights = total) %>% 
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
  select(CR_GUEST) %>% 
  filter(grepl("And", CR_GUEST)) 

multiple_speaker$total_words <- sapply(multiple_speaker$CR_GUEST, 
                                       function(x) length(unlist(
                                         strsplit(as.character(x), "\\W+"))))
# only two speakers already
heatmap <- multiple_speaker %>% filter(total_words == 3) %>% select(CR_GUEST)

# three speakers
heatmap <- multiple_speaker %>% filter(total_words == 5) %>% 
  mutate(first = word(CR_GUEST, start = 1, end = 3)) %>% 
  mutate(second = word(CR_GUEST, start = 3, end = 5)) %>% 
  mutate(third1 = word(CR_GUEST, start = 1, end = 2), 
         third2 = word(CR_GUEST, 5),
         third = paste(third1, third2, sep=" ")) %>%
  select(first,second,third) %>% 
  pivot_longer(first:third , 
               names_to = "position", 
               values_to = "CR_GUEST") %>% 
  select(CR_GUEST) %>% 
  bind_rows(.,heatmap)

# four speakers
heatmap <- multiple_speaker %>% filter(total_words == 7) %>% 
  mutate(first = word(CR_GUEST, start = 1, end = 3)) %>% 
  mutate(second = word(CR_GUEST, start = 3, end = 5)) %>%
  mutate(third = word(CR_GUEST, start = 5, end = 7)) %>%
  mutate(fourth1 = word(CR_GUEST, start = 1, end = 2),
         fourth2 = word(CR_GUEST, 5),
         fourth = paste(fourth1, fourth2, sep=" ")) %>%
  mutate(fifth1 = word(CR_GUEST, start = 1, end = 2),
         fifth2 = word(CR_GUEST, 7),
         fifth = paste(fifth1, fifth2, sep=" ")) %>%
  mutate(sixth1 = word(CR_GUEST, start = 3, end = 4),
         sixth2 = word(CR_GUEST, 7),
         sixth = paste(sixth1, sixth2, sep=" ")) %>%
  select(3,4,5,8,11,14) %>% 
  pivot_longer(first:sixth , 
               names_to = "position", 
               values_to = "CR_GUEST") %>% 
  select(CR_GUEST) %>% 
  bind_rows(.,heatmap) 

# clean up
rm(multiple_speaker)

# same order for speaker
heatmap$CR_GUEST = unname(sapply(heatmap$CR_GUEST, function(x) {
  paste(sort(trimws(strsplit(x[1], ' And ')[[1]])), collapse=' And ')} ))


# split after And
heatmap <- heatmap %>% 
  separate(CR_GUEST, c("Actor1","Actor2"), sep = " And ", 
           fill = "left", remove=TRUE, extra = "merge") %>%
  group_by(Actor1,Actor2) %>%
  tally() %>% 
  rename(Occurrence = n) %>% 
  filter(Occurrence != 1) 

# add variable for ordering the rows 
heatmap <- data.frame(Actor1 = c("Guests","Ashley", "Laura", "Liam", "Marisha", "Matt", 
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
  unnest_tokens(word, Text)

sentiment_data <- sentiment_data %>% 
  inner_join(get_sentiments("afinn")) %>% 
  inner_join(get_sentiments("bing")) %>% 
  rename(afinn_sentiment = value,
         bing_sentiment = sentiment) %>% 
  mutate(bing_sentiment=recode(bing_sentiment, 
                               negative = -1,
                               positive = 1))

sentiment_episodes <- sentiment_data %>% 
  group_by(Episode,Arc,Arc_no) %>%
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
  group_by(Arc,Arc_no) %>% 
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
  group_by(CR_GUEST) %>%
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




