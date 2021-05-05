
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
# text log odds
library(tidylo)
# sentiment analysis
library(sentimentr)

###############################################################################
# Get Data
###############################################################################

# read in data but drop episode 12 as this a tipps and tricks episode and no cannon
data <- read.csv(file = "./data/clean_data/clean_full.csv") %>% filter(episode != 12)


# rename Actor: as this files creates graphs, it's nicer to have not every
# character capitalized
data <- data %>% mutate(
  actor = str_to_title(actor),
  actor_guest = str_to_title(actor_guest)
)


# split if actors say the same thing (e.g. ASHLEY AND LAURA: that's great ->
# ASHLEY: that's great ; LAURA: that's great)
individual_cast_member <- data %>%
  mutate(actor_guest = strsplit(as.character(actor_guest), " And ")) %>%
  unnest(actor_guest)

# splits text into words
individual_split_words <- individual_cast_member %>%
  unnest_tokens(word, text)


###############################################################################
# Time Combat versus RP by Arc
###############################################################################

# data frame
combat_rp_arc <- data %>%
  filter(!is.na(rp_combat)) %>%
  filter(!is.na(time_in_sec)) %>% 
  group_by(arc, arc_no, rp_combat) %>%
  summarise(time = sum(time_in_sec, na.rm = TRUE)) %>%
  mutate(
    total_time = sum(time, na.rm = TRUE),
    percent = (time / total_time) * 100,
    arc = str_replace(arc, "_", " "),
    rp_combat = str_to_title(str_replace(rp_combat, "_", " "))
  )

# export data frame
write.csv(combat_rp_arc,
          "./data/data_for_graphs/combat_rp_arc.csv",
          row.names = FALSE
)


###############################################################################
# Miss spellings
###############################################################################

miss_spells <- read.csv(file = "./data/clean_data/miss_spells.csv")

miss_spells <- miss_spells %>%
  filter(Actor != "zac") %>% 
  mutate(Actor = str_to_sentence(Actor)) %>% 
  pivot_longer(!Actor, 
               names_to = "miss_spelling", 
               values_to = "count") %>% 
  mutate(miss_spelling = str_replace_all(miss_spelling,"_", " "))

write.csv(miss_spells,
          "./data/data_for_graphs/miss_spells.csv",
          row.names = FALSE
)

###############################################################################
# Attendance
###############################################################################

attendance <- read.csv(file = "./data/clean_data/attendance.csv")

attendance <- attendance %>% 
  mutate(Guests = replace(Guests, Guests=="Jo", NA)) %>%
  mutate(Guests = replace(Guests, Guests=="ri", NA)) %>%
  mutate(Guests = as.numeric(Guests)) %>% 
  pivot_longer(Laura:Guests, names_to = "actor", values_to = "episodes") %>% 
  select(Episode,actor,episodes) %>% 
  filter(Episode <= 115) %>% 
  group_by(actor) %>% 
  summarise(episodes = sum(episodes, na.rm = TRUE))
  
# export data frame
write.csv(attendance,
          "./data/data_for_graphs/attendance.csv",
          row.names = FALSE
)

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
  mutate(words_percent = (words / total_words) * 100)

# Actor: Time
actor_words_time <- individual_split_words %>%
  filter(staff != 1 ) %>%
  group_by(actor_guest) %>%
  summarise(time = sum(time_in_sec, na.rm = TRUE)) %>%
  mutate(total_time = sum(time)) %>%
  mutate(time_percent = (time / total_time) * 100) %>%
  right_join(., actor_words_time)

# export data frame
write.csv(actor_words_time,
          "./data/data_for_graphs/actor_words_time.csv",
          row.names = FALSE
)


###############################################################################
# Top Words 5 per Actor
###############################################################################

# log odds 
top_words_actor <- individual_cast_member %>%
  filter(staff != 1 )  %>%
  unnest_tokens(word, text) %>%
  count(actor_guest, word) %>%
  bind_log_odds(actor_guest, word, n) %>%
  arrange(-log_odds_weighted) %>% 
  group_by(actor_guest) %>% 
  top_n(5,log_odds_weighted) %>% 
  rename(count = n)

# amounts of words spoken in total per actor
top_words_actor <- individual_cast_member %>%
  filter(staff != 1 )  %>%
  unnest_tokens(word, text) %>%
  count(actor_guest, word) %>% 
  group_by(actor_guest) %>% 
  summarise(total_words = sum(n)) %>%
  right_join(top_words_actor,., by = "actor_guest") %>% 
  mutate(percent = (count / total_words) * 100) %>% 
  rename(actor = actor_guest)

# export data frame
write.csv(top_words_actor,
          "./data/data_for_graphs/top_words_actor.csv",
          row.names = FALSE
)


###############################################################################
# Grade Equivalent
###############################################################################


# grade equivalent text: 
# https://rdrr.io/cran/quanteda/man/textstat_readability.html

# get grades for each text longer than 5.
readability_grade <- textstat_readability(
  individual_cast_member$text,
  measure = c("Coleman.Liau.grade"),
  remove_hyphens = TRUE,
  min_sentence_length = 5,
  max_sentence_length = 10000
) %>%
  as.data.frame() %>%
  select(Coleman.Liau.grade) %>%
  bind_cols(individual_cast_member, .)

readability_grade <- readability_grade %>%
  filter(staff != 1) %>%
  group_by(actor_guest) %>%
  summarise_at(vars(Coleman.Liau.grade),
               list(mean),
               na.rm = TRUE
  ) 

# export data frame
write.csv(readability_grade,
          "./data/data_for_graphs/readability_grade.csv",
          row.names = FALSE
)


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
  filter(!grepl("And", actor_guest)) %>%
  mutate(previous = lag(actor_guest)) %>%
  filter(actor_guest != previous) %>%
  group_by(actor_guest, previous) %>%
  tally()

combinations <- network %>% select(actor_guest, previous)

# make the combination a new variable
network <- network %>% unite("combination",
                             actor_guest:previous,
                             sep = " And ",
                             remove = FALSE
)

# order the combination always the same way: Liam and Sam = Sam and Liam
network$combination <- unname(sapply(network$combination, function(x) {
  paste(sort(trimws(strsplit(x[1], " And ")[[1]])), collapse = " And ")
}))

# get number of occurrences for each combination and then split combinations again
network <- network %>%
  select(combination, n) %>%
  group_by(combination) %>%
  summarise(total = sum(n)) %>%
  separate(combination,
           c("actor_guest", "previous"),
           sep = " And ",
           fill = "left",
           remove = TRUE
  ) %>%
  left_join(combinations, .)

# prepare the network: links
network <- network %>%
  filter(actor_guest != "Matt") %>%
  filter(previous != "Matt") %>%
  mutate(
    actor_guest = str_to_title(actor_guest),
    previous = str_to_title(previous)
  ) %>%
  rename(from = actor_guest, to = previous, weights = total) %>%
  mutate(type = "hyperlink") %>%
  mutate(weights = replace(weights, is.na(weights), 1))

# export data frame
write.csv(network,
          "./data/data_for_graphs/network.csv",
          row.names = FALSE
)



###############################################################################
# Network graph: Same thought
###############################################################################

# select text spoken by actor simultaneously 
same_thought <- data %>%
  filter(staff != 1) %>%
  select(actor_guest) %>%
  filter(grepl("And", actor_guest))




# get number of words in speaker row as this indicates how many speaker are in 
# in the row
same_thought$total_words <- sapply(
  same_thought$actor_guest,
  function(x) {
    length(unlist(
      strsplit(as.character(x), "\\W+")
    ))
  }
)

# only two speakers already
same_thought_network <- same_thought %>%
  filter(total_words == 3) %>%
  select(actor_guest)

# three speakers
same_thought_network <- same_thought %>%
  filter(total_words == 5) %>%
  mutate(first = word(actor_guest, start = 1, end = 3)) %>%
  mutate(second = word(actor_guest, start = 3, end = 5)) %>%
  mutate(
    third1 = word(actor_guest, start = 1, end = 2),
    third2 = word(actor_guest, 5),
    third = paste(third1, third2, sep = " ")
  ) %>%
  select(first, second, third) %>%
  pivot_longer(first:third,
               names_to = "position",
               values_to = "actor_guest"
  ) %>%
  select(actor_guest) %>%
  bind_rows(., same_thought_network)

# four speakers
same_thought_network <- same_thought %>%
  filter(total_words == 7) %>%
  mutate(first = word(actor_guest, start = 1, end = 3)) %>%
  mutate(second = word(actor_guest, start = 3, end = 5)) %>%
  mutate(third = word(actor_guest, start = 5, end = 7)) %>%
  mutate(
    fourth1 = word(actor_guest, start = 1, end = 2),
    fourth2 = word(actor_guest, 5),
    fourth = paste(fourth1, fourth2, sep = " ")
  ) %>%
  mutate(
    fifth1 = word(actor_guest, start = 1, end = 2),
    fifth2 = word(actor_guest, 7),
    fifth = paste(fifth1, fifth2, sep = " ")
  ) %>%
  mutate(
    sixth1 = word(actor_guest, start = 3, end = 4),
    sixth2 = word(actor_guest, 7),
    sixth = paste(sixth1, sixth2, sep = " ")
  ) %>%
  select(3, 4, 5, 8, 11, 14) %>%
  pivot_longer(first:sixth,
               names_to = "position",
               values_to = "actor_guest"
  ) %>%
  select(actor_guest) %>%
  bind_rows(., same_thought_network)

# clean up
rm(same_thought)

# same order for speaker
same_thought_network$actor_guest <- unname(sapply(same_thought_network$actor_guest, function(x) {
  paste(sort(trimws(strsplit(x[1], " And ")[[1]])), collapse = " And ")
}))


# split after And
same_thought_network <- same_thought_network %>%
  separate(actor_guest, c("actor1", "actor2"),
           sep = " And ",
           fill = "left", remove = TRUE, extra = "merge"
  ) %>%
  group_by(actor1, actor2) %>%
  tally() %>%
  rename(occurrence = n) %>%
  filter(occurrence != 1)

# add variable for ordering the rows
same_thought_network <- data.frame(
  actor1 = c(
    "Guests", "Ashley", "Laura", "Liam", "Marisha", "Matt",
    "Orion", "Sam", "Taliesin"
  ),
  order = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
) %>%
  right_join(., same_thought_network, by = "actor1") %>% 
  filter(actor1 != actor2) %>% 
  select(-order) %>% 
  mutate(
    actor1 = str_to_title(actor1),
    actor2 = str_to_title(actor2)
  ) %>%
  rename(from = actor1, to = actor2, weights = occurrence) %>%
  mutate(type = "hyperlink") %>%
  mutate(weights = replace(weights, is.na(weights), 1)) 

# export data frame
write.csv(same_thought_network,
          "./data/data_for_graphs/same_thought_network.csv",
          row.names = FALSE
)


###############################################################################
# Sentiment Analysis 
###############################################################################



sentences_sentiment <- data %>% 
  rename(number_words = word_count) %>% 
  get_sentences(text) %>% 
  sentiment() 

# by arc
sentiment_by_arc <- sentences_sentiment %>% 
  group_by(arc) %>% 
  summarise(mean_sentiment = mean(sentiment),
            sd_sentiment = sd(sentiment))

# export data frame
write.csv(sentiment_by_arc,
            "./data/data_for_graphs/sentiment_by_arc.csv",
          row.names = FALSE
)


# by actor
sentiment_by_actor <- sentences_sentiment %>% 
  mutate(actor_guest = str_replace(actor_guest, ",", " And ")) %>% 
  mutate(actor_guest = strsplit(as.character(actor_guest), " And ")) %>%
  unnest(actor_guest) %>% 
  group_by(actor_guest) %>% 
  summarise(mean_sentiment = mean(sentiment),
            sd_sentiment = sd(sentiment))

# export data frame
write.csv(sentiment_by_actor,
          "./data/data_for_graphs/sentiment_by_actor.csv",
          row.names = FALSE
)


# by episode  
sentiment_by_episode <- sentences_sentiment %>% 
  group_by(episode) %>% 
  summarise(mean_sentiment = mean(sentiment),
            sd_sentiment = sd(sentiment))


# add dice rolls
dice_pc_1 <- read.csv(file = "./data/clean_data/dice_pc_1.csv")
sentiment_by_episode <- dice_pc_1 %>% 
  group_by(episode) %>% 
  tally() %>% 
  rename(pc_1 = n) %>% 
  left_join(sentiment_by_episode,. , by = "episode")


dice_pc_20 <- read.csv(file = "./data/clean_data/dice_pc_20.csv")
sentiment_by_episode <- dice_pc_20 %>% 
  group_by(episode) %>% 
  tally() %>% 
  rename(pc_20 = n) %>% 
  left_join(sentiment_by_episode,., by = "episode")


dice_dm_1 <- read.csv(file = "./data/clean_data/dice_dm_1.csv")
sentiment_by_episode <- dice_dm_1 %>% 
  group_by(episode) %>% 
  tally() %>% 
  rename(dm_1 = n) %>% 
  left_join(sentiment_by_episode,., by = "episode")

dice_dm_20 <- read.csv(file = "./data/clean_data/dice_dm_20.csv")
sentiment_by_episode <- dice_dm_20 %>% 
  group_by(episode) %>% 
  tally() %>% 
  rename(dm_20 = n) %>% 
  left_join(sentiment_by_episode,., by = "episode")


face_palms <- read.csv(file = "./data/clean_data/face_palms.csv")
sentiment_by_episode <- face_palms %>% 
  group_by(episode) %>% 
  tally() %>% 
  rename(face_palms = n) %>% 
  left_join(sentiment_by_episode,., by = "episode") %>% 
  replace(is.na(.), 0)

# Ashley
sentiment_by_episode <- read.csv(file = "./data/clean_data/attendance.csv") %>% 
  select(Episode, Ashley) %>% 
  rename(episode = Episode) %>% 
  left_join(sentiment_by_episode, . , by = "episode")


# export data frame
write.csv(sentiment_by_episode,
          "./data/data_for_graphs/sentiment_by_episode.csv",
          row.names = FALSE
)


###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))