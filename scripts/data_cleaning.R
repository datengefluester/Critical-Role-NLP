###############################################################################
# Packages
###############################################################################
library(tidyr) # data manipulation
library(dplyr) # data manipulation
library(stringr) # structure replacements
library(SRTtools) # for reading in SRT files

###############################################################################
# Read in Data
###############################################################################

# Get a list of all files named in the folder so we can loop over the list
filenames <- list.files("./data/raw_data/srt_files", pattern = "*.srt", full.names = TRUE)

# kick out the original (not edited) files
filenames <- head(filenames, -5)

# delete edited files if you wanted to:
# 32,38,85,120,121 (E031-2,E035-2,E082,E113-1,E113-2)
# filenames <- filenames[-c(32,38,85,120,121)]

# Get episode name: kick out path
file_name <- substring(filenames, 30)

# kick out file ending
file_name <- sapply(strsplit(file_name, "_"), "[", 1)

# Conversion of file number to episode numbers in vector for conversion, as
# some episodes have two files the name in the files and the position in the
# list does not directly correspond to the final episode.
conversion <- as.data.frame(cbind(
  File_number = seq(1:length(filenames)),
  file_name
))


# read in files (creates one large list, which contains lists for each
# episode/file)
raw <- lapply(filenames, srt.read)

# create data set:
# create individual rows with readable time stamps for each subtitle segment
# within the lists.
# Note: this takes time!
raw <- lapply(raw, function(x) {
  con <- textConnection(x)
  lines <- readLines(con)
  listOfEntries <-
    lapply(
      split(1:length(lines), cumsum(grepl("^\\s*$", lines))),
      function(blockIdx) {
        block <- lines[blockIdx]
        block <- block[!grepl("^\\s*$", block)]
        return(data.frame(
          times = block[2],
          textString = paste0(block[3:length(block)],
            collapse = "\n"
          ),
          stringsAsFactors = FALSE
        ))
      }
    )
  do.call(rbind, listOfEntries)
})

# create data frame from the lists
raw <- bind_rows(raw, .id = "File_number")
# add episode number and delete file number as more intuitive
raw <- merge(raw, conversion, all.x = TRUE)
raw <- select(raw, -c(File_number))

# remove not needed files
rm(file_name, filenames, conversion)

# export to csv to save time as this does to have to be run each time
write.csv(raw, "data/raw_data/raw_csv/raw.csv", row.names = FALSE)

rm(raw)

###############################################################################
# clean/manipulate data
## Preparation
###############################################################################

# read in raw data
clean <- read.csv(file = "data/raw_data/raw_csv/raw.csv")


# dropping everything which is contained within brackets, as these
# indicate descriptive text, i.e. Matt: "(heavy accent) Hi"
clean$textString <- gsub("\\s*\\([^\\)]+\\)", "", clean$textString)

# delete every row, which is empty (""), which indicates a text only contains
# description previously
clean <- clean %>% filter(textString != "")

# get example for text
example_raw <- clean %>%
  filter(file_name == 100) %>%
  slice(1:2) %>%
  select(-file_name)
write.csv(example_raw, "data/data_for_graphs/example_raw", row.names = FALSE)


# to be able to analysis things like words per minute etc.:
# split start and end times
tmp <- do.call(rbind, strsplit(clean[, "times"], " --> "))
clean$startTime <- tmp[, 1]
clean$endTime <- tmp[, 2]
clean <- select(clean, -times)
rm(tmp)

# start and end time in seconds for easier calculation:
# parse times to add second videos for episodes to episode
tmp <- do.call(rbind, lapply(strsplit(clean$startTime, ":|,"), as.numeric))
clean$startTime <- tmp %*% c(60 * 60, 60, 1, 1 / 1000)
tmp <- do.call(rbind, lapply(strsplit(clean$endTime, ":|,"), as.numeric))
clean$endTime <- tmp %*% c(60 * 60, 60, 1, 1 / 1000)
rm(tmp)

# Consisting naming to allow for text analysis:
# replace "&" and "&amp;" with "and"
clean$textString <- gsub("&amp;", "and", clean$textString)
clean$textString <- gsub("&", "and", clean$textString)

# rename DandD (comes from the editing previous step) to D&D
clean$textString <- gsub("DandD", "D&D", clean$textString)


###############################################################################
## Extract Speaker
###############################################################################

# So far the speaker is still contained in the 'textString' variable. To have
# labels for the spoken text we need to extract it. Problem: we need to extract
# all speakers including misspelled ones. Doing this we obtain 3918 "Speakers"

# separate speaker from text. Speakers are indicated using ":" in the files
# e.g. Liam: I open the door; as a line can have multiple speakers e.g. "Liam:
# I open the door. Travis: I drink ale", I extract all speakers from text to
# in order to catch all miss spellings
clean <- clean %>% separate(textString, c("Speaker1", "Text1"),
  sep = ":",
  fill = "left", remove = FALSE, extra = "merge"
)
clean <- clean %>% separate(Text1, c("Speaker2", "Text2"),
  sep = ":",
  fill = "left", remove = FALSE, extra = "merge"
)
clean <- clean %>% separate(Text2, c("Speaker3", "Text3"),
  sep = ":",
  fill = "left", remove = FALSE, extra = "merge"
)
clean <- clean %>% separate(Text3, c("Speaker4", "Text4"),
  sep = ":",
  fill = "left", remove = FALSE, extra = "merge"
)

# combine all distinct speakers for speaker "slot"
unique <- clean %>%
  distinct(select(clean, Speaker1)) %>%
  rename(Speaker = Speaker1)

unique <- clean %>%
  distinct(select(clean, Speaker2)) %>%
  rename(Speaker = Speaker2) %>%
  bind_rows(unique, .)

unique <- clean %>%
  distinct(select(clean, Speaker3)) %>%
  rename(Speaker = Speaker3) %>%
  bind_rows(unique, .)

unique <- clean %>%
  distinct(select(clean, Speaker4)) %>%
  rename(Speaker = Speaker4) %>%
  bind_rows(unique, .)

unique <- unique %>% distinct(select(unique, Speaker))

# This leaves us with 4438 identified speakers. However, some text
# got miss matched: e.g. 'I open the door: What do I see?' becomes
# Speaker1: 'I open the door'; Text1: 'What do I see?'
# Final Goal: get list of individual speakers, which need to be extract from
# 'textString' column

# extract speakers with capital letters
# if does not contain any capital letters: drop it as no speaker involved
# Note: this can be checked using !str_detect to see that no actor is dropped
unique <- unique %>% filter(str_detect(Speaker, "[[:upper:]]"))

# Apply consistent naming (different coders write all capital,
# others only capitalize first letter etc.): make everything in small letters
unique$Speaker <- tolower(as.character(unique$Speaker))

# check uniqueness again, as TRAVIS and Travis are no both travis
unique <- distinct(select(unique, Speaker))

# Get amount of words for each speaker. We get these as we go by length from
# here on. The reason for doing this, is the fact that it makes it easy to
# follow as a lot of manual adjustments have to be made, due to the fact that
# the subtitles are hand coded by volunteers. Hence, there will inconsistencies
# and spelling mistakes (an average episode is around 3.5 hours after all).
# get word count
unique$total_words <- sapply(
  unique$Speaker,
  function(x) {
    length(unlist(
      strsplit(as.character(x), "\\W+")
    ))
  }
)



###############################################################################
### Lists for Actors, Characters and Guests
###############################################################################

# Create loop for actors, characters and guests so they can be drop from list of
# speakers. Note this takes long as there are many potential combinations of
# speakers. Afterwards, we still need to classify 3769 speakers, but have already
# assigned 149 speakers.


actors <- c(
  "matt", "travis", "orion", "taliesin", "marisha", "liam",
  "laura", "ashley", "sam"
)
character <- c(
  "tiberius", "vex", "vex'ahlia", "vax'ildan", "vax", "keyleth",
  "percy", "scanlan", "pike", "grog", "taryon"
)
guests <- c(
  "will", "wil", "mary", "dan", "ify", "zac", "felicia",
  "patrick", "kit", "joe", "jason", "chris", "jon", "darin", "noelle"
)

names <- c(actors, character, guests)
and <- paste(names, "and", sep = " ")
comma <- paste(names, ",", sep = "")
points <- paste(names, ":", sep = "")
multiple <- c(and, comma, points)
rm(and, comma)

# creating combinations of names with " and " and "," with right spelling
additions <- c(" and ", ", ", ":")
combinations <- data.frame(
  name1 = names,
  name2 = names,
  name3 = names
)

combinations_names <- data.frame(combinations = as.character())
for (type in additions) {
  # create data frame where variables are created with names to be combine lists
  # for 2 and 3 speakers respectively
  combinations2 <- expand(combinations, name1, name2, name3)
  combinations3 <- expand(combinations, name1, name2, name3)
  # get all possible combinations
  combinations2 <- combinations2 %>%
    unite("combinations", name1:name2, sep = type) %>%
    select(combinations)
  combinations3 <- combinations3 %>%
    unite("combinations", name1:name3, sep = type) %>%
    select(combinations)
  # combine the combination data frames
  combinations4 <- as.data.frame(rbind(combinations2, combinations3))
  # symmetrical so combinations created twice
  combinations4 <- unique(combinations4)
  # get double names (e.g matt and matt) so they can be deleted from list
  name2 <- paste(names, names, sep = type)
  # delete double names
  for (double_name in name2) {
    combinations4 <- combinations4 %>%
      filter(!str_detect(combinations, double_name))
    rm(double_name)
  }
  # combine with data frame
  combinations_names <- rbind(combinations_names, combinations4)
  rm(combinations2, combinations3, combinations4, name2, type)
}


# combine with single names
all_names <- as.list(c(combinations_names$combinations, names))

# extract these combinations from unique list
acutal_combintations <- data.frame(Speaker = as.character())
for (name in all_names) {
  acutal_combintations <- unique %>%
    filter(Speaker == name) %>%
    bind_rows(., acutal_combintations)
  rm(name)
}

# drop these combinations from the unique list
unique <- anti_join(unique, acutal_combintations, by = "Speaker")

# Create Data Frame for actual Speakers
Speakers <- acutal_combintations

# drop data frame of acutal_combintations and all_names
rm(acutal_combintations, all_names, additions, combinations, combinations_names)

# get description of how actors say something
brackets <- clean %>%
  filter(grepl("):", textString, fixed = TRUE)) %>%
  filter(grepl(")", Speaker1, fixed = TRUE)) %>%
  select(Speaker1) %>%
  filter(Speaker1 != "(others join in singing)") %>%
  add_row(Speaker1 = "Guard: (whispers)") %>%
  add_row(Speaker1 = "Vex: (whispers)") %>%
  mutate(total_words <- sapply(
    Speaker1,
    function(x) {
      length(unlist(
        strsplit(as.character(x), "\\W+")
      ))
    }
  )) %>%
  rename(Speaker = 1, total_words = 2)



###############################################################################
### Parts of dialog mismatched (>=4 words)
###############################################################################

# Keep cast and guests of show and combinations of them: logic >4 words and then
# go smaller. Long phrases can be excluded first, as they do not indicate a
# player/NPC is speaking (see door example above) OR they indicate multiple
# actors speaking. Doing so puts 299 Speakers into a data frame and brings the number
# to be sorted to 731.

# select only the ones with four or more words
phrases <- unique %>%
  filter(total_words >= 4) %>%
  subset(select = c(Speaker, total_words))

# create data list for ", and" to get a combination of speakers not yet
# detected e.g. "laura, liam, and sam"
comma_and_list <- paste(", and", names, sep = " ")
comma_and <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (phrase in comma_and_list) {
  comma_and <- phrases %>%
    filter(str_detect(Speaker, phrase)) %>%
    bind_rows(., comma_and)
  rm(phrase)
}

# manual clean up
# missmatch
comma_and <- comma_and %>%
  filter(
    !str_detect(Speaker, "grog's there"),
    !str_detect(Speaker, "smoke slowly subsides")
  )
# delete some missmatched parts
comma_and <- comma_and %>% mutate(
  Speaker = replace(
    Speaker,
    grepl("what's that?", Speaker),
    "matt, sam, and orion"
  ),
  Speaker = replace(
    Speaker,
    grepl("huh", Speaker),
    "liam, matt, and travis"
  )
)

# add to Speakers and delete from phrases and unique
Speakers <- bind_rows(Speakers, comma_and)
phrases <- anti_join(phrases, comma_and, by = "Speaker")
unique <- anti_join(unique, comma_and, by = "Speaker")

# rest of character combinations: combinations of "," and "and"
tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (name in multiple) {
  tmp <- phrases %>%
    filter(grepl(name, Speaker)) %>%
    bind_rows(., tmp)
}
tmp <- tmp %>% filter(total_words == 4)

tmp <- tmp %>% filter(!grepl("keyleth.|and say", Speaker))


# add to Speakers and delete from phrases and unique
Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")
rm(tmp, name)

# manual tweaking: get the last speakers and combinations of actors
tmp <- phrases %>% filter(Speaker == "off-screen voice #1" |
  Speaker == "off-screen voice #2" |
  Speaker == "matt, laura, taliesin and marisha" |
  grepl("/", Speaker))

# adding and dropping
Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")
rm(tmp)

# the rest can be dropped. As description. This can be manually checked.
# However, to make things easier we only keep the values, which do not contain
# one the actors, to able to check by hand.
testing <- phrases %>% mutate(Temp = Speaker)
for (word in names) {
  testing <- testing %>% mutate(Temp = str_replace_all(Temp, paste(word), ""))
}
testing <- testing %>%
  mutate(Speaker = replace(Speaker, Temp != Speaker, NA)) %>%
  select(-c(Temp)) %>%
  drop_na(Speaker)

# drop everything left over in phrases from unique data frame
unique <- anti_join(unique, phrases, by = "Speaker")
rm(phrases, word, testing)
unique <- distinct(unique)



###############################################################################
### Examining Phrases, which are exactly three words long
###############################################################################

# Afterwards, the list to be edited contains to only 246 and the list of speakers
# already identified is 287.

# dropping of speaker which contain only actors/characters
# (no adjustments needed)
#  for (name in names){
#      unique <-  unique %>% filter(Speaker!= name)
#    rm(name)}

# dropping all three words rows as they imply miss matched actor and
# do some manual edits for things to keep
phrases <- unique %>% filter(total_words == 3)

# extract groups of speaker left over. Note: 'Travis, pretending Ticket' is an
# actual speaker.
tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (name in multiple) {
  tmp <- phrases %>%
    filter(grepl(name, Speaker)) %>%
    bind_rows(., tmp)
}
# adding and dropping
Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")

# extract other NPC and voices
tmp <- phrases %>%
  filter(Speaker == "computer-generated voice" |
    Speaker == "man with chicken" |
    Speaker == "marish and laura")

Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")

# extract speaker (plus description):

tmp <- phrases %>%
  filter(Speaker == "travis (as scanlan)" |
    Speaker == "liam (heavy accent)" |
    Speaker == "matt (as tiberius)" |
    Speaker == "liam (partially sung)" |
    Speaker == "orion (imitating marisha)" |
    Speaker == "all (except orion)" |
    Speaker == "sam (singing softly)" |
    Speaker == "laura (as pike)" |
    Speaker == "patrick")

Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")

# drop everything left over in phrases from unique data frame
# NOTE TO MYSELF: ♪ stuff is here
unique <- anti_join(unique, phrases, by = "Speaker")
rm(phrases)



###############################################################################
### Phrases, which are exactly two words long
###############################################################################

# Afterwards, the list to be edited contains only 137 and the list of speakers
# already identified is 305.

# kick out phrases, which contain 2 words
phrases <- unique %>% filter(total_words == 2)

# extract if contains "/" (other form of writing 'and')
tmp <- phrases %>% filter(grepl("/", Speaker))
# adding and dropping
Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")


# drop if equal to names
tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (name in names) {
  tmp <- phrases %>%
    filter(str_detect(Speaker, name)) %>%
    bind_rows(., tmp)
  rm(name)
}

# dropping
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")

# NPCs (found by manually looking through data frame)
NPC_list <- c(
  "guard 1", "dwarven barkeep", "nearby dwarves", "dwarf 1",
  "drinking dwarf", "guard 2", "carver 2", "bar patron",
  "dwarf patron", "dwarven patrons", "dwarven guard",
  "nostoc greyspine", "dwarf guard", "audience member",
  "seeker asum", "uriel taldorei", "arbiter braum",
  "crewmember 1", "crewmember 2",
  "man with chicken", "off-screen"
)

tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)

for (npc in NPC_list) {
  tmp <- phrases %>%
    filter(str_detect(Speaker, npc)) %>%
    bind_rows(., tmp)
  rm(npc)
}
# adding and dropping
Speakers <- bind_rows(Speakers, tmp)
phrases <- anti_join(phrases, tmp, by = "Speaker")
unique <- anti_join(unique, tmp, by = "Speaker")

# drop the rest from the unique list
#  unique <- anti_join(unique, phrases, by = "Speaker")

# manual edits:
Speakers <- Speakers %>% add_row(Speaker = "patrick rothfuss", total_words = 2)




###############################################################################
### One word: Extracting misspellings of CR members
###############################################################################

# Now, the list to be edited contains only 91 and the list of speakers
# already identified is 351.

# create data frame for all miss spellings per actor - this is used in the
# blog for counting amount of mis spells by volunteers
miss_spells <- data.frame(Speaker = NA, total_words = NA, Actor = NA)


# --- Ashley
Ashley <- unique %>% filter(grepl("^ash", Speaker) |
  grepl("^ahs", Speaker))
# Merge / drop
miss_spells <- bind_rows(miss_spells, Ashley) %>%
  filter(!is.na(Speaker)) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Ashley"))

Speakers <- bind_rows(Speakers, Ashley)
unique <- anti_join(unique, Ashley, by = "Speaker")
rm(Ashley)

# --- Grog (logic follows Ashley's)
Grog <- unique %>% filter(grepl("gorg", Speaker))
miss_spells <- bind_rows(miss_spells, Grog) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Grog"))
Speakers <- bind_rows(Speakers, Grog)
unique <- anti_join(unique, Grog, by = "Speaker")
rm(Grog)

# --- Laura (logic follows Ashley's)
Laura <- unique %>% filter(grepl("larua", Speaker) |
  grepl("lauar", Speaker) |
  grepl("laura:", Speaker) |
  grepl("laura", Speaker))
miss_spells <- bind_rows(miss_spells, Laura) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Laura"))
Speakers <- bind_rows(Speakers, Laura)
unique <- anti_join(unique, Laura, by = "Speaker")
rm(Laura)

# ---  Marisha (logic follows Ashley's)
Marisha <- unique %>% filter(grepl("^mar", Speaker) |
  grepl("^mai", Speaker))
Marisha <- Marisha %>% filter(
  Speaker != "mark",
  Speaker != "margrim"
)
miss_spells <- bind_rows(miss_spells, Marisha) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Marisha"))
Speakers <- bind_rows(Speakers, Marisha)
unique <- anti_join(unique, Marisha, by = "Speaker")
rm(Marisha)

# --- Matt (logic follows Ashley's)
Matt <- unique %>% filter(grepl("^mat", Speaker))
miss_spells <- bind_rows(miss_spells, Matt) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Matt"))
Speakers <- bind_rows(Speakers, Matt)
unique <- anti_join(unique, Matt, by = "Speaker")
rm(Matt)

# --- Orion (logic follows Ashley's)
Orion <- unique %>% filter(grepl("^or", Speaker))
miss_spells <- bind_rows(miss_spells, Orion) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Orion"))
Speakers <- bind_rows(Speakers, Orion)
unique <- anti_join(unique, Orion, by = "Speaker")
rm(Orion)

# --- Sam (logic follows Ashley's)
Sam <- unique %>% filter(grepl("sma", Speaker) |
  grepl("sam ", Speaker))
miss_spells <- bind_rows(miss_spells, Sam) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Sam"))
Speakers <- bind_rows(Speakers, Sam)
unique <- anti_join(unique, Sam, by = "Speaker")
rm(Sam)

# --- Taliesin (logic follows Ashley's)
Taliesin <- unique %>% filter(grepl("^tal", Speaker) |
  grepl("^tai", Speaker))
miss_spells <- bind_rows(miss_spells, Taliesin) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Taliesin"))
Speakers <- bind_rows(Speakers, Taliesin)
unique <- anti_join(unique, Taliesin, by = "Speaker")
rm(Taliesin)

# --- Travis (logic follows Ashley's)
Travis <- unique %>% filter(grepl("^ta", Speaker) |
  grepl("^tra", Speaker))
miss_spells <- bind_rows(miss_spells, Travis) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Travis"))
Speakers <- bind_rows(Speakers, Travis)
unique <- anti_join(unique, Travis, by = "Speaker")
rm(Travis)

# --- Vax (logic follows Ashley's)
Vax <- unique %>% filter(grepl("vax", Speaker))
miss_spells <- bind_rows(miss_spells, Vax) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Vax"))
Speakers <- bind_rows(Speakers, Vax)
unique <- anti_join(unique, Vax, by = "Speaker")
rm(Vax)

# --- Vex (logic follows Ashley's)
Vex <- unique %>% filter(grepl("vex", Speaker))
miss_spells <- bind_rows(miss_spells, Vex) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Vex"))
Speakers <- bind_rows(Speakers, Vex)
unique <- anti_join(unique, Vex, by = "Speaker")
rm(Vex)

# --- Liam (logic follows Ashley's)
Liam <- unique %>% filter(grepl("liam ", Speaker))
miss_spells <- bind_rows(miss_spells, Liam) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Liam"))
Speakers <- bind_rows(Speakers, Liam)
unique <- anti_join(unique, Liam, by = "Speaker")
rm(Liam)



###############################################################################
### One word: Rest
###############################################################################

# Dropping misidentified names/words and the number of speakers identified to 385.

#--- drop words (found by manually searching):
words <- c(
  "like", "finding", "themselves", "chamber", "books", "magic",
  "caveat", "going", "else", "decide", "call", "one", "attacks", "is",
  "though", "chasers", "announce", "nest", "detail", "say",
  "yesterday", "which", "companion", "titansgrave", "bonfires",
  "need", "want", "undergrowth", "away", "dragonborn", "things",
  "egypt", "announcements", "enlarge", "yellow", "green", "for",
  "again", "quarry", "trinket", "#1", "#2"
)

for (word in words) {
  unique <- unique %>% filter(Speaker != word)
  rm(word)
}
rm(words)


# --- Guest: Jore (ep. 17)
Jore <- unique %>% filter(grepl("jore", Speaker) |
  grepl("jroe", Speaker))
miss_spells <- bind_rows(miss_spells, Jore) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Jore"))
Speakers <- bind_rows(Speakers, Jore)
unique <- anti_join(unique, Jore, by = "Speaker")
rm(Jore)

# --- Guest: Brian
Brian <- unique %>% filter(grepl("brain", Speaker) |
  grepl("brian", Speaker))
miss_spells <- bind_rows(miss_spells, Brian) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Brian"))
Speakers <- bind_rows(Speakers, Brian)
unique <- anti_join(unique, Brian, by = "Speaker")
rm(Brian)

# --- Producer: Zac
Zac <- unique %>% filter(grepl("zack", Speaker) |
  grepl("zac", Speaker))
miss_spells <- bind_rows(miss_spells, Zac) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Zac"))
Speakers <- bind_rows(Speakers, Zac)
unique <- anti_join(unique, Zac, by = "Speaker")
rm(Zac)

# NPC: Giles
Giles <- unique %>% filter(grepl("gile", Speaker))
miss_spells <- bind_rows(miss_spells, Giles) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "Giles"))
Speakers <- bind_rows(Speakers, Giles)
unique <- anti_join(unique, Giles, by = "Speaker")
rm(Giles)

# numerous speakers
numerous <- c("several", "others", "everyone", "group", "vox machina", "all")
tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (word in numerous) {
  tmp <- unique %>%
    filter(Speaker == word) %>%
    rbind(., tmp)
}
Speakers <- bind_rows(Speakers, tmp)
unique <- anti_join(unique, tmp, by = "Speaker")
rm(tmp)

# offscreen
offscreen <- c(
  "lucas", "ryan", "offscreen", "erika", "audience", "crew", "
             kai", "producer", "luke", "alec", "offstage", "ivan", "becca",
  "hector", "announcer", "iphone", "mark"
)
tmp <- data.frame(
  Speaker = character(),
  total_words = as.numeric()
)
for (word in offscreen) {
  tmp <- unique %>%
    filter(Speaker == word) %>%
    rbind(., tmp)
}
Speakers <- bind_rows(Speakers, tmp)
unique <- anti_join(unique, tmp, by = "Speaker")
rm(tmp)

# add the rest to NPCs and Speakers
Speakers <- bind_rows(Speakers, unique)
rm(unique, phrases)

# delete duplicates
Speakers <- unique(Speakers)

miss_spells <- miss_spells %>%
  mutate(miss_spells = Speaker) %>%
  mutate(Actor = tolower(Actor)) %>%
  mutate(Speaker = str_trim(Speaker)) %>%
  mutate(Speaker = str_squish(Speaker)) %>%
  filter(Speaker != Actor) %>%
  unique()



###############################################################################
## Cleaning up and preparing Speaker List
###############################################################################

# drop if speaker=="" or NA or duplicate
Speakers <- Speakers %>% filter(
  Speaker != "",
  !is.na(Speaker)
)
Speakers <- Speakers %>% filter(Speaker != " and")
Speakers <- Speakers %>% distinct(Speaker)

# sort by words and characters, as later on longer phrases
# overwrite short phrases: e.g. Matt and Sam overwrite only Sam and Matt
# individually if contained in a cell. Also Matt overwrites Mat
Speakers$total_characters <- nchar(Speakers$Speaker)
Speakers <- Speakers %>% arrange(total_characters, Speaker)


# last manual edit:
last_drops <- c(
  " csi", " one", " yes", "male", "na 00", " alone", " first",
  " magic", " avatar", " dumoco", " tiamat", " tone17", "he goes",
  "he says", "about 10", "which is", " question", " reminder,",
  " \"second", "\"goldhand", "\"remember", "lost hope",
  "of sundry", " important", "alone. one", "july theme",
  " vasselheim", " voractin24", "night books", " subappendix",
  "bonus action", "for instance", "role-related", " constitution",
  "horrible forms", " nixdeathsticks", " thefallentitan",
  " zanderinvictus", " lightninggemini", "wyrmwood giveaway",
  "darkness, werewolf", "notification. this", " frisbee",
  " ironwolf", " sickwizard", " warmcustard", " ravendreamer",
  " wheelyfactoid", " try", " reminder", "(background"
)

for (edit in last_drops) {
  Speakers <- Speakers %>% filter(Speaker != edit)
}

# add ":" in front of Speaker to be able to extract it
Speakers$Speaker_dot <- sapply(Speakers$Speaker, paste, ":", sep = "")
Speakers$Speaker_dot_2 <- sapply(Speakers$Speaker, paste, " :", sep = "")

# make list for loop
list_Speaker <- c(Speakers$Speaker)
list_Speaker_dot <- c(Speakers$Speaker_dot)
list_Speaker_dot_2 <- c(Speakers$Speaker_dot_2)
list_Speaker_dot <- c(list_Speaker_dot, list_Speaker_dot_2)
rm(list_Speaker_dot_2, last_drops)




###############################################################################
# Extract Speaker
###############################################################################

# first actors saying stuff in the subtitle segment
# everything to lower for consistent naming

# to make the following easier to follow I drop the different text columns and
# create a new one for full text

clean <- clean %>%
  mutate(
    actual_Speaker_1 = NA,
    actual_Speaker_2 = NA,
    actual_Speaker_3 = NA,
    actual_text_1 = NA,
    actual_text_2 = NA,
    actual_text_3 = NA
  )

# extract speakers per segment
for (entry in list_Speaker) {
  clean <- clean %>% mutate(
    actual_Speaker_1 = replace(
      actual_Speaker_1,
      grepl(entry, Speaker1, ignore.case = TRUE),
      entry
    ),
    actual_Speaker_2 = replace(
      actual_Speaker_2,
      grepl(entry, Speaker2, ignore.case = TRUE),
      entry
    ),
    actual_Speaker_3 = replace(
      actual_Speaker_3,
      grepl(entry, Speaker3, ignore.case = TRUE),
      entry
    )
  )
}

# extract speaker with brackets per segment
brackets$Speaker <- sapply(brackets$Speaker, paste, ":", sep = "")
tmp <- brackets %>% select(Speaker)
tmp <- as.list(unlist(tmp$Speaker))

for (entry in tmp) {
  clean <- clean %>% mutate(actual_Speaker_1 = replace(
    actual_Speaker_1,
    grepl(entry, Speaker1, ignore.case = TRUE),
    entry
  ), )
}


# add : for extraction
clean$actual_Speaker_1 <- sapply(clean$actual_Speaker_1, paste, ":", sep = "")
clean$actual_Speaker_2 <- sapply(clean$actual_Speaker_2, paste, ":", sep = "")
clean$actual_Speaker_3 <- sapply(clean$actual_Speaker_3, paste, ":", sep = "")

# if "name:" not contained in segment = NA
clean <- clean %>% mutate(
  actual_Speaker_1 = replace(
    actual_Speaker_1,
    actual_Speaker_1 == "NA:",
    NA
  ),
  actual_Speaker_2 = replace(
    actual_Speaker_2,
    actual_Speaker_2 == "NA:",
    NA
  ),
  actual_Speaker_3 = replace(
    actual_Speaker_3,
    actual_Speaker_3 == "NA:",
    NA
  )
)

# last edits: like all: etc.
clean <- clean %>%
  rowwise() %>%
  mutate(
    actual_Speaker_1 = replace(
      actual_Speaker_1,
      !grepl(actual_Speaker_1, textString, ignore.case = TRUE),
      NA
    ),
    actual_Speaker_2 = replace(
      actual_Speaker_2,
      !grepl(actual_Speaker_2, textString, ignore.case = TRUE),
      NA
    ),
    actual_Speaker_3 = replace(
      actual_Speaker_3,
      !grepl(actual_Speaker_3, textString, ignore.case = TRUE),
      NA
    )
  )


# assign text to speakers
clean <- clean %>% select(-c(2:9))


# replace actor1 with #### for splitting afterwards
clean$text <- tolower(clean$textString)
clean$actual_text_1 <- ifelse(
  is.na(clean$actual_Speaker_1),
  clean$text,
  str_replace(
    clean$text,
    clean$actual_Speaker_1,
    "####"
  )
)
clean$actual_text_1 <- str_replace(
  clean$actual_text_1,
  "####",
  ""
)
# replace actor 2 with #### for splitting afterwards
clean$actual_text_1 <- ifelse(
  is.na(clean$actual_Speaker_2),
  clean$actual_text_1,
  str_replace(
    clean$actual_text_1,
    clean$actual_Speaker_2,
    "####"
  )
)

clean <- clean %>%
  separate(actual_text_1, c("actual_text_1", "actual_text_2"),
    sep = "#### ",
    fill = "left",
    remove = FALSE,
    extra = "merge"
  ) %>%
  select(-text)

# edit missmatches:
clean <- clean %>%
  mutate(actual_text_1 = replace(
    actual_text_1,
    is.na(actual_text_1),
    actual_text_2[is.na(actual_text_1)]
  )) %>%
  mutate(actual_text_2 = replace(
    actual_text_2,
    actual_text_1 == actual_text_2,
    NA
  ))



# clean up
clean$actual_text_1 <- str_replace(
  clean$actual_text_1,
  "####",
  ""
)


# replace actor 3 with #### for splitting afterwards
clean$actual_text_2 <- ifelse(
  is.na(clean$actual_Speaker_3),
  clean$actual_text_2,
  str_replace(
    clean$actual_text_2,
    clean$actual_Speaker_3,
    "####"
  )
)

clean <- clean %>% separate(actual_text_2, c("actual_text_2", "actual_text_3"),
  sep = "#### ",
  fill = "left",
  remove = FALSE,
  extra = "merge"
)

# edit missmatches:
clean <- clean %>%
  rowwise() %>%
  mutate(actual_text_2 = replace(
    actual_text_2,
    is.na(actual_text_2),
    actual_text_3[actual_text_2]
  )) %>%
  mutate(actual_text_3 = replace(
    actual_text_3,
    actual_text_3 == actual_text_2,
    NA
  ))

# clean up
clean$actual_text_2 <- str_replace(
  clean$actual_text_2,
  "####",
  ""
)



###############################################################################
# Assign Segments
###############################################################################

# split and merge: get actor1 and actor2 as individual rows so text
# can be assigned to the corresponding speaker.
# example row1 Sam: Hi ; Laura: Hi I'm ; row2: Laura.
# So expanding should be for actor2. To ensure rightly expanding the rows split
# data set and create unique segment names: Actor_file_nameSpeaker

# Actor 1
Actor1 <- select(clean, -c(
  actual_Speaker_2, actual_Speaker_3,
  actual_text_2, actual_text_3
))
Actor1 <- Actor1 %>%
  rename(
    Actor = actual_Speaker_1,
    Text = actual_text_1
  )
Actor1$number <- 1
# Actor 2
Actor2 <- select(clean, -c(
  actual_Speaker_1, actual_text_1,
  actual_Speaker_3, actual_text_3
))
Actor2 <- Actor2 %>% drop_na(actual_Speaker_2)
Actor2 <- Actor2 %>%
  rename(
    Actor = actual_Speaker_2,
    Text = actual_text_2
  )
Actor2$number <- 2
# Actor3
Actor3 <- select(clean, -c(
  actual_Speaker_1, actual_text_1,
  actual_Speaker_2, actual_text_2
))
Actor3 <- Actor3 %>% drop_na(actual_Speaker_3)
Actor3 <- Actor3 %>%
  rename(
    Actor = actual_Speaker_3,
    Text = actual_text_3
  )
Actor3$number <- 3
# combine
clean <- bind_rows(Actor1, Actor2, Actor3)
rm(Actor1, Actor2, Actor3)



###############################################################################
### Consistent Naming of Actor to simplify analysis
###############################################################################

# replace ",","/" and "And" with "and"
clean$Actor <- gsub(",", " and", clean$Actor)
clean$Actor <- gsub("/", " and ", clean$Actor)
clean$Actor <- gsub("and and", "and", clean$Actor)

# Unique Actors so you can check all the way in which names are wrongly spelled:
unique <- distinct(select(clean, Actor))

# split at "and"
unique <- unique %>%
  mutate(Actor = strsplit(as.character(Actor), " and ")) %>%
  unnest(Actor)

# list of unique Actors:
unique <- distinct(select(unique, Actor))


###############################################################################
### count number miss spells for graph
###############################################################################

# why separate? if you compare the unique and the miss_spells data frame, you
# see that many miss spells contain also "spelling:" and "spelling :", however this
# is irrelevant for number of misspellings

# add spelling + : & + " :" + names to lower and then loop
miss_spells <- miss_spells %>%
  mutate(Speaker = str_c(Speaker, " ", sep = "")) %>%
  rbind(miss_spells)

miss_spells <- miss_spells %>%
  mutate(Speaker = str_c(Speaker, ":", sep = "")) %>%
  rbind(miss_spells)

miss_spells <- miss_spells %>%
  mutate(count = NA) %>%
  arrange(Actor)

list <- as.list(miss_spells$Speaker)

# get number of occurrences per miss spelling
for (miss_spelling in list) {
  spelling <- miss_spelling

  miss_spells <- miss_spells %>%
    mutate(count = replace(
      count,
      Speaker == paste(spelling),
      length(which(clean$Actor == paste(spelling)))
    ))
}

# keep full list for extracting speakers
all_miss_typings <- miss_spells

# delete ":" and see if equal cause then no miss spelling
# drop if equal (guest problem)
miss_spells <- miss_spells %>%
  mutate(Speaker = str_replace(Speaker, ":", "")) %>%
  mutate(Speaker = str_replace(Speaker, " ", "")) %>%
  filter(Speaker != Actor) %>%
  filter(Speaker != "brian") %>%
  filter(Speaker != "jore")


# get number of miss spellings and how many different miss spellings
miss_spells <- miss_spells %>%
  group_by(Actor) %>%
  mutate(amount_miss_spellings = sum(count))

# get number of occurences per miss spelling
miss_spells <- miss_spells %>%
  filter(count > 0) %>%
  count() %>%
  rename(types_miss_spellings = n) %>%
  right_join(miss_spells, by = "Actor") %>%
  select(Actor, amount_miss_spellings, types_miss_spellings) %>%
  ungroup() %>%
  unique()

# add giles and grog to Matt and Travis respectively; both have both variables as one
miss_spells <- miss_spells %>%
  mutate(amount_miss_spellings = replace(
    amount_miss_spellings,
    Actor == "matt",
    amount_miss_spellings[Actor == "matt"] + 1
  )) %>%
  mutate(amount_miss_spellings = replace(
    amount_miss_spellings,
    Actor == "travis",
    amount_miss_spellings[Actor == "travis"] + 1
  )) %>%
  mutate(types_miss_spellings = replace(
    types_miss_spellings,
    Actor == "matt",
    types_miss_spellings[Actor == "matt"] + 1
  )) %>%
  mutate(types_miss_spellings = replace(
    types_miss_spellings,
    Actor == "travis",
    types_miss_spellings[Actor == "travis"] + 1
  )) %>%
  filter(Actor != "giles" & Actor != "grog") %>%
  # add Liam by hand, as he has no miss spells
  add_row(Actor = "liam", types_miss_spellings = 0, amount_miss_spellings = 0) %>%
  # delete non player miss spells
  filter(Actor != "zac" & Actor != "brian" & Actor != "jore")


# export miss spells:
write.csv(miss_spells, "./data/clean_data/rest/miss_spells.csv", row.names = FALSE)


###############################################################################
### cleanup misspelling:
###############################################################################
### logic for most steps:
### 1.) grab all forms misspelling
### 2.) create list
###  3.) loop over list and replace name with right name


# note: the reason for extracting miss spellings for speakers again is the fact
# that this time dots may or may not be extracted depending on whether an actor
# was the only one speaking.

# Sam
Sam1 <- unique[grepl("^sam", unique$Actor), ]
Sam2 <- unique[grepl("^sma", unique$Actor), ]
Sam <- as.list(unlist(rbind(Sam1, Sam2)))
for (spelling in Sam) {
  clean$Actor <- gsub(spelling, "sam", clean$Actor)
}
rm(Sam1, Sam2, Sam)

# Ashley
Ashley1 <- unique[grepl("^ash", unique$Actor), ]
Ashley2 <- unique[grepl("^ahs", unique$Actor), ]
Ashley <- as.list(unlist(rbind(Ashley1, Ashley2)))

for (spelling in Ashley) {
  clean$Actor <- gsub(spelling, "ashley", clean$Actor)
}
rm(Ashley1, Ashley2, Ashley)

# Matt: Mat ; second step necessary as otherwise does not work. Don't know why
Matt1 <- unique[grepl("^mat", unique$Actor), ]
Matt <- as.list(unlist(rbind(Matt1)))
for (spelling in Matt) {
  clean$Actor <- gsub(spelling, "Mat", clean$Actor)
}
clean$Actor <- gsub("Mat", "matt", clean$Actor)
clean$Actor <- gsub("mattr", "matt", clean$Actor)
rm(Matt1, Matt)

# Laura
Laura1 <- unique[grepl("^la", unique$Actor), ]
Laura <- as.list(unlist(rbind(Laura1)))
for (spelling in Laura) {
  clean$Actor <- gsub(spelling, "laura", clean$Actor)
}
rm(Laura1, Laura)

# Liam
Liam1 <- unique[grepl("^liam", unique$Actor), ]
Liam <- as.list(unlist(rbind(Liam1)))
for (spelling in Liam) {
  clean$Actor <- gsub(spelling, "liam", clean$Actor)
}
rm(Liam1, Liam)

# Taliesin
Taliesin1 <- unique[grepl("^tal", unique$Actor), ]
Taliesin3 <- c("tailesin")
Taliesin <- as.list(unlist(rbind(Taliesin1, Taliesin3)))
for (spelling in Taliesin) {
  clean$Actor <- gsub(spelling, "taliesin", clean$Actor)
}
rm(Taliesin1, Taliesin3, Taliesin)

# Orion
Orion1 <- unique[grepl("^or", unique$Actor), ]
Orion1 <- as.list(unlist(rbind(Orion1)))
for (spelling in Orion1) {
  clean$Actor <- gsub(spelling, "orion", clean$Actor)
}
rm(Orion1)

# Travis
Travis1 <- unique[grepl("^tra", unique$Actor), ]
Travis3 <- data.frame("Actor" = c("tavis", "tarvis:"))
Travis <- as.list(unlist(rbind(Travis1, Travis3)))
for (spelling in Travis) {
  clean$Actor <- gsub(spelling, "travis", clean$Actor)
}
rm(Travis, Travis1, Travis3)

#  Marisha
# * Mar - Mary - Mark
# + Marsha + Mairsha
Marisha1 <- unique[grepl("^mar", unique$Actor), ]
Marisha3 <- data.frame("Actor" = c(
  "marsha", "mairsha", "marishaa", "marishah",
  "marishaha", "marishaia", "marishaa",
  "♪ marisha"
))
Marisha <- unique(as.list(unlist(rbind(Marisha1, Marisha3))))
Marisha <- Marisha[Marisha != "mary"]
Marisha <- Marisha[Marisha != "mark"]
Marisha <- Marisha[Marisha != "margrim"]
for (spelling in Marisha) {
  clean$Actor <- gsub(spelling, "marisha", clean$Actor)
}
clean$Actor <- gsub("marishaaaa", "marisha", clean$Actor)
clean$Actor <- gsub("marishaa", "marisha", clean$Actor)
clean$Actor <- gsub("mairsha", "marisha", clean$Actor)
clean$Actor <- gsub("marishah", "marisha", clean$Actor)
clean$Actor <- gsub("marishaha", "marisha", clean$Actor)
clean$Actor <- gsub("♪", "", clean$Actor)
clean$Actor <- gsub("marishaa", "marisha", clean$Actor)
rm(Marisha1, Marisha3, Marisha)

# amandine
clean$Actor <- gsub("amandine:", "amandine", clean$Actor)

# delete unnecessary white space
clean$Actor <- gsub("  ", " ", clean$Actor)



###############################################################################
# Characters and guests
###############################################################################

# Gorg/Grog
clean$Actor <- gsub("gorg", "grog", clean$Actor)
# Vax
clean$Actor <- gsub("vax’ildan", "vax", clean$Actor)
# Vex
clean$Actor <- gsub("vex'ahlia", "vex", clean$Actor)
clean$Actor <- gsub("vex'alia", "vex", clean$Actor)
clean$Actor <- gsub("na na vex", "vex", clean$Actor)

# Uriel
clean$Actor <- gsub("uriel taldorei", "uriel", clean$Actor)
# Zac
clean$Actor <- gsub("zack", "zac", clean$Actor)
# Gile/Giles
clean$Actor <- gsub("gile", "giles", clean$Actor)
clean$Actor <- gsub("giless", "giles", clean$Actor)
# Brian
clean$Actor <- gsub("brain", "brian", clean$Actor)
# Patrick Rothfuss
clean$Actor <- gsub("patrick rothfuss", "patrick", clean$Actor)
# Nostoc
clean$Actor <- gsub("nostoc greyspine", "nostoc", clean$Actor)
# Jore
clean$Actor <- gsub("jroe", "jore", clean$Actor)

# Audience / offscreen / offstage / crew to one "off" class
Off <- as.list(c(
  "offscreen", "off-screen voice #1", "off-screen voice #2", "off member",
  "offmember 1", "offmember 2", "off voice #1", "off voice #2", "offscreen",
  "offstage", "audience", "audience member", "crew", "crewmember 1",
  "crewmember 2", "producer", "iphone", "computer-generated voice"
))
for (spelling in Off) {
  clean$Actor <- gsub(spelling, "off-screen", clean$Actor)
}
clean$Actor <- gsub("offmember 1", "off-screen", clean$Actor)
clean$Actor <- gsub("offmember 2", "off-screen", clean$Actor)
clean$Actor <- gsub("off member", "off-screen", clean$Actor)
clean$Actor <- gsub("offscreen member", "off-screen", clean$Actor)
clean$Actor <- gsub("computergenerated voice", "off-screen", clean$Actor)
clean$Actor <- gsub("offscreen member", "off-screen", clean$Actor)
rm(Off)


# all / everyone etc. to numerous
numerous <- as.list(c("several", "others", "everyone", "group", "vox machina", "all"))
# replace allura as otherwise "all"-part of the name is replaced as well
clean$Actor <- gsub("allura", "zzzzzzzzz", clean$Actor)
for (spelling in numerous) {
  clean$Actor <- gsub(spelling, "numerous", clean$Actor)
}
clean$Actor <- gsub("zzzzzzzzz", "allura", clean$Actor)
rm(numerous)


# Chris Perkins vs.Chris Hardwick
clean$Actor <- ifelse(clean$Actor == "chris" & clean$file_name == "046", "chrisH", clean$Actor)
clean$Actor <- ifelse(clean$Actor == "chris" & clean$file_name == "055", "chrisP", clean$Actor)

# fix mistake in the data: laura and laura to laura and travis
clean$Actor <- ifelse(clean$Actor == "laura and laura", "laura and travis", clean$Actor)
clean <- clean %>%
  mutate(Actor = replace(
    Actor,
    Actor == "pretending trinket and travis",
    "travis"
  )) %>%
  mutate(Actor = replace(
    Actor,
    Actor == "offscreen member",
    "offscreen"
  ))



# Replace characters with Actors
Actor <- c("orion", "laura", "liam", "marisha", "taliesin", "ashley", "travis", "sam", "sam")
PC <- c("tiberius", "vex", "vax", "keyleth", "percy", "pike", "grog", "scanlan", "taryon")
for (i in seq(1:9)) {
  clean$Actor <- gsub(PC[i], Actor[i], clean$Actor)
}


# cleanup "-"
clean$Actor <- gsub("-", "", clean$Actor)
# cleanup ":"
clean$Actor <- gsub(":", "", clean$Actor)

# delete white space
clean$Actor <- trimws(clean$Actor)

# replace empty cells in actor with NA
clean <- clean %>% mutate(Actor = replace(Actor, Actor == "", NA))


###############################################################################
# Add Attendance to kick out misplaced speakers
###############################################################################

# we create a new variable as we do not care about videos but episodes
# in this step. Hence we create a new variable.
clean <- clean %>%
  mutate(Episode = file_name) %>%
  mutate(Actor = replace(Actor, is.na(Actor), "")) %>%
  separate(Episode, c("Episode", "Tmp"),
    sep = "-",
    fill = "right", remove = TRUE, extra = "merge"
  ) %>%
  select(-Tmp) %>%
  mutate(Episode = as.numeric(Episode))



# read attendance and add to data frame
attendance <- read.csv(file = "data/raw_data/rest/attendance.csv")

# clean
clean <- attendance %>%
  filter(Episode != "Total") %>%
  select(-c(X, X.1, Player.Present, X1)) %>%
  mutate(Guests = replace(Guests, Guests == "Jo" | Guests == "ri", 1)) %>%
  mutate(Ashley = replace(Ashley, is.na(Ashley), 0)) %>%
  mutate(Skype = replace(Skype, is.na(Skype), 0)) %>%
  mutate(Episode = str_replace(Episode, "C1E", "")) %>%
  mutate(Episode = as.numeric(Episode)) %>%
  left_join(clean, ., by = "Episode")

# kick out if speaker was not indeed present in an episode
clean <- clean %>%
  mutate(Actor = replace(Actor, Actor == "ashley" & Ashley == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "laura" & Laura == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "marisha" & Marisha == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "sam" & Sam == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "travis" & Travis == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "liam" & Liam == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "taliesin" & Taliesin == 0, NA)) %>%
  mutate(Actor = replace(Actor, Actor == "orion" & Orion == 0, NA))

# mutate artefacts from Mary and Marisha replacement
clean <- clean %>%
  mutate(Actor = replace(Actor, grep("MARY:", textString), "mary")) %>%
  mutate(
    Actor = replace(Actor, grep("TALIESIN and MARY:", textString), "taliesin and mary"),
    Actor = replace(Actor, grep("LAURA and MARY:", textString), "laura and mary"),
    Actor = replace(Actor, grep("TALIESIN, LAURA, and MARY:", textString), "taliesin, laura and mary"),
    Actor = replace(Actor, grep("TRAVIS and MARY:", textString), "travis and mary")
  )

# Mutate Actor such that is represents the actual speaker (for example Matt
# speaking Pike, when Ashley was not there)
clean <- clean %>%
  mutate(
    Actor = replace(Actor, is.na(Actor) & Episode == 1, "matt"),
    Actor = replace(Actor, is.na(Actor) & Episode == 5, "sam"),
    Actor = replace(
      Actor, is.na(Actor) & Episode == 7 & startTime < 300,
      "travis"
    ),
    Actor = replace(Actor, is.na(Actor) & Episode == 7, "matt"),
    Actor = replace(Actor, is.na(Actor) & Episode == 8, "travis"),
    Actor = replace(Actor, is.na(Actor) & Episode == 9, "ashley"),
    Actor = replace(
      Actor, is.na(Actor) & Episode == 14 & startTime < 1000,
      "sam"
    ),
    Actor = replace(Actor, is.na(Actor) & Episode == 14, "matt"),
    Actor = replace(Actor, is.na(Actor) & Episode == 17, "ashley"),
    Actor = replace(
      Actor, is.na(Actor) & Episode == 18 & grepl(
        "MARISHA:",
        textString
      ),
      "marisha"
    ),
    Actor = replace(
      Actor, is.na(Actor) & Episode == 18 & grepl(
        "LIAM:",
        textString
      ),
      "liam"
    ),
    Actor = replace(
      Actor, is.na(Actor) & Episode == 18 & grepl(
        "ORION:",
        textString
      ),
      "orion"
    ),
    Actor = replace(Actor, is.na(Actor) & Episode == 19, "marisha"),
    Actor = replace(Actor, is.na(Actor) & Episode == 21, "taliesin"),
    Actor = replace(Actor, is.na(Actor) & Episode == 36, "ashley"),
    Actor = replace(Actor, is.na(Actor) & Episode == 63, "ashley"),
    Actor = replace(Actor, is.na(Actor) & Episode == 84, "sam")
  ) %>%
  filter(!is.na(Actor))

# delete last artifact and edit text accordingly
clean <- clean %>%
  mutate(
    endTime = as.numeric(endTime),
    startTime = as.numeric(startTime)
  ) %>%
  filter(Text != "driven,") %>%
  rowwise() %>%
  mutate(Text = replace(
    Text,
    Actor == "sam" & startTime == 10861.448,
    textString
  )) %>%
  mutate(Text = replace(
    Text,
    Actor == "sam" & startTime == 10861.448,
    str_replace(Text, "SAM:", "")
  ))


# replace "," with "and"
clean <- clean %>% mutate(Actor = str_replace(Actor, ",", " and"))

# same order for numerous actors: Travis and Sam = Sam and Travis
clean$Actor <- unname(sapply(clean$Actor, function(x) {
  paste(sort(trimws(strsplit(x[1], " and ")[[1]])), collapse = " and ")
}))


# replace "" with NAs so next coding works
clean <- clean %>%
  mutate(Actor = replace(Actor, Actor == "", NA))

###############################################################################
# Extend Speaker Segment
###############################################################################


# get number of text segment for actors (e.g. Matt1, Matt2)
# for the unique identifier and proper expanding of text segments
segment <- clean %>%
  filter(!is.na(Actor)) %>%
  group_by(file_name, Actor) %>%
  mutate(num = 1:n())

segment <- segment %>%
  group_by(file_name, startTime, Actor) %>%
  mutate(segment = paste(Actor, file_name, num, sep = "_"))
clean <- left_join(clean, segment)
rm(segment)

# sort for expanding to ensure text segments are assigned to right actor
clean <- clean[order(
  clean$file_name,
  clean$startTime,
  clean$number
), ]


# Expand segment for each row spoken
clean <- clean %>%
  ungroup() %>%
  fill(segment, .direction = c("down"))

# get start and endtime for each segment
clean <- clean %>%
  group_by(segment) %>%
  mutate(startTime = min(startTime)) %>%
  mutate(endTime = max(endTime))

# merge cells for text
clean <- clean %>%
  group_by(segment) %>%
  mutate(Text = paste0(Text, collapse = " ")) %>%
  mutate(textString = paste0(textString, collapse = " "))

#  only keep first observations. Matt: 'Hello everyone and welcome to
#  critical role' as one as opposed to multiple rows)
clean <- clean[!duplicated(clean$segment), ]

# drop segment number
clean <- clean %>%
  ungroup() %>%
  select(-segment)



###############################################################################
# Clean up text
###############################################################################


# some clean up: delete \
clean$Text <- gsub("\\\\", "", clean$Text)

# delete --
clean$Text <- gsub("--", "", clean$Text)

# delete unnecessary white space from text string
clean <- clean %>% mutate(Text = str_trim(Text, side = "both"))

# drop textString
clean <- clean %>% select(-textString)

# replace "- " with ""
clean$Text <- gsub("- ", "", clean$Text)

# again, drop if Text is empty (don't ask me why they survived after earlier
# dropping)
clean <- clean %>% filter(Text != "")


###############################################################################
# Dummy coding
###############################################################################

clean <- clean %>%
  mutate(Actor = replace(
    Actor,
    Actor == "pretending trinket and travis",
    "travis"
  )) %>%
  mutate(Actor = replace(
    Actor,
    Actor == "offscreen member",
    "offscreen"
  ))


# Vox Machina dummy
clean$vox_machina <- 0
for (member in Actor) {
  clean$vox_machina <- ifelse(grepl(member, clean$Actor), 1, clean$vox_machina)
}

# guests dummy
guests <- guests[guests != "zac"]
clean$guest <- 0
for (guest in guests) {
  clean$guest <- ifelse(grepl(guest, clean$Actor), 1, clean$guest)
}

# staff dummy
clean$staff <- 0
clean$staff <- ifelse(grepl("zac", clean$Actor), 1, 0)
clean$staff <- ifelse(grepl("offscreen", clean$Actor), 1, clean$staff)
clean$staff <- ifelse(grepl("brian", clean$Actor), 1, clean$staff)

# Matt dummy
clean$Matt <- 0
clean$Matt <- ifelse(clean$vox_machina == 0 &
  clean$guest == 0 &
  clean$staff == 0,
1, 0
)

# replace NPCs with Matt
clean$Actor <- ifelse(clean$Matt == 1, "matt", clean$Actor)

# all capital letters
clean$Actor <- toupper(clean$Actor)



###############################################################################
# Cleaning Episodes, Add Arcs and speed talking per segment
###############################################################################

# Episode numbers manual adjustments: some episodes split into two videos:

# get end time for first video
max31 <- max(clean$startTime[clean$file_name == "031-1"], na.rm = TRUE)
max33 <- max(clean$startTime[clean$file_name == "033-1"], na.rm = TRUE)
max35 <- max(clean$startTime[clean$file_name == "035-1"], na.rm = TRUE)

# adjust time stamps
clean <- clean %>%
  rowwise() %>%
  mutate(
    startTime = replace(
      startTime,
      file_name == "031-2",
      startTime + max31
    ),
    startTime = replace(
      startTime,
      file_name == "033-2",
      startTime + max33
    ),
    startTime = replace(
      startTime,
      file_name == "035-2",
      startTime + max35
    ),
    endTime = replace(
      endTime,
      file_name == "031-2",
      endTime + max31
    ),
    endTime = replace(
      endTime,
      file_name == "033-2",
      endTime + max33
    ),
    endTime = replace(
      endTime,
      file_name == "035-2",
      endTime + max35
    )
  )

# give the endings episode numbers for the time being (they are dropped from
# final data frame but the individual data csvs are included)
clean <- clean %>%
  mutate(
    Episode = replace(
      Episode,
      file_name == "115-GROG",
      "116"
    ),
    Episode = replace(
      Episode,
      file_name == "115-HH",
      "117"
    ),
    Episode = replace(
      Episode,
      file_name == "115-VAMP1",
      "118"
    ),
    Episode = replace(
      Episode,
      file_name == "115-VAMP2",
      "119"
    )
  )

# add arcs
clean <- clean %>%
  mutate(Episode = as.numeric(Episode)) %>%
  mutate(Arc = NA) %>%
  mutate(
    Arc = replace(Arc, Episode <= 16 & is.na(Arc), "Kraghammer"),
    Arc = replace(Arc, Episode <= 23 & is.na(Arc), "Vasselheim"),
    Arc = replace(Arc, Episode <= 38 & is.na(Arc), "Briarwoods"),
    Arc = replace(Arc, Episode <= 56 & is.na(Arc), "Attack_Conclave"),
    Arc = replace(Arc, Episode <= 69 & is.na(Arc), "Vestiges"),
    Arc = replace(Arc, Episode <= 84 & is.na(Arc), "Fall_Conclave"),
    Arc = replace(Arc, Episode <= 99 & is.na(Arc), "Daring_Deeds"),
    Arc = replace(Arc, Episode <= 119 & is.na(Arc), "End")
  )


# add arcs number
clean <- clean %>%
  mutate(Arc_no = NA) %>%
  mutate(
    Arc_no = replace(Arc_no, Arc == "Kraghammer", 1),
    Arc_no = replace(Arc_no, Arc == "Vasselheim", 2),
    Arc_no = replace(Arc_no, Arc == "Briarwoods", 3),
    Arc_no = replace(Arc_no, Arc == "Attack_Conclave", 4),
    Arc_no = replace(Arc_no, Arc == "Vestiges", 5),
    Arc_no = replace(Arc_no, Arc == "Fall_Conclave", 6),
    Arc_no = replace(Arc_no, Arc == "Daring_Deeds", 7),
    Arc_no = replace(Arc_no, Arc == "End", 8)
  )


# talking speed
# word count, compute time difference in seconds, words per millisecond
clean <- clean %>%
  mutate(timeDiffInSecs = endTime - startTime) %>%
  mutate(Text = str_trim(Text)) %>%
  mutate(Text = str_squish(Text)) %>%
  mutate(wordCount = vapply(gregexpr("\\W+", Text), length, 0) + 1) %>%
  mutate(words_per_minute = wordCount / (timeDiffInSecs / 60))


###############################################################################
# time stamps for episodes
###############################################################################

# Source:
# https://docs.google.com/spreadsheets/d/1Zx1N0cQcd1fJadUwar7f2hJ2p61qoX7lctsVaIEa5uM/edit#gid=744793917

# Break stamps
time_stamps <- read.csv("./data/raw_data/time_stamps/running_times.csv",
  na.strings = c("", "NA")
)

# Edit for better readability
time_stamps <- time_stamps %>%
  select(1, 10:13) %>%
  slice(-(1:3))


# Combat stamps
combat_stamps <- read.csv("./data/raw_data/time_stamps/combat_times.csv",
  na.strings = c("", "NA")
)

combat_stamps <- combat_stamps %>%
  slice(-(1:2)) %>%
  select(2:6)

# merge
time_stamps <- left_join(time_stamps, combat_stamps, by = "Episode") %>%
  rename(
    first_start = X1st.start,
    second_start = X2nd.start,
    first_end = X1st.end,
    second_end = X2nd.end,
    encounter_length = Total.Length,
    encounter_start = Start.Time,
    encounter_end = End.Time
  ) %>%
  mutate(Episode = gsub("C1E", "", Episode)) %>%
  mutate(Episode = as.numeric(Episode))

# Transform Time
tmp <- do.call(rbind, lapply(strsplit(time_stamps$first_start, ":|,"), as.numeric))
time_stamps$first_start <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$first_end, ":|,"), as.numeric))
time_stamps$first_end <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$second_start, ":|,"), as.numeric))
time_stamps$second_start <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$second_end, ":|,"), as.numeric))
time_stamps$second_end <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$encounter_length, ":|,"), as.numeric))
time_stamps$encounter_length <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$encounter_start, ":|,"), as.numeric))
time_stamps$encounter_start <- tmp %*% c(60 * 60, 60, 1)
tmp <- do.call(rbind, lapply(strsplit(time_stamps$encounter_end, ":|,"), as.numeric))
time_stamps$encounter_end <- tmp %*% c(60 * 60, 60, 1)
rm(tmp)


# replace second_end with first_end if first_end==NA
# (indicates no break in episode)
time_stamps <- time_stamps %>%
  rowwise() %>%
  mutate(second_end = replace(
    second_end,
    is.na(second_end),
    first_end
  )) %>%
  mutate(first_end = replace(
    first_end,
    first_end == second_end,
    NA
  ))

# join time stemps into clean data frame
clean <- clean %>%
  mutate(Episode = as.numeric(Episode))

clean <- time_stamps %>%
  select(1:5) %>%
  mutate(Episode = as.numeric(Episode)) %>%
  left_join(clean, ., by = "Episode") %>%
  distinct()


# assign segments (extra, prologue, first half, second half) to the text
clean <- clean %>%
  group_by(Episode) %>%
  mutate(segment = NA) %>%
  mutate(segment = replace(segment, startTime >= second_end, "extra")) %>%
  mutate(segment = replace(segment, startTime <= first_start, "prologue")) %>%
  mutate(segment = replace(
    segment,
    startTime >= first_end & startTime <= second_start,
    "break"
  )) %>%
  mutate(segment = replace(
    segment,
    startTime >= first_start & startTime <= first_end,
    "first_half"
  )) %>%
  mutate(segment = replace(
    segment,
    startTime >= second_start & startTime <= second_end,
    "second_half"
  ))

# combat times
combat_stamps <- time_stamps %>%
  select(1, 6:9) %>%
  group_by(Episode) %>%
  mutate(encounter_number = row_number()) %>%
  pivot_wider(
    names_from = encounter_number,
    values_from = c(encounter_start, encounter_end)
  ) %>%
  mutate(encounter_number = as.numeric(row_number())) %>%
  ungroup() %>%
  mutate_if(is.character, as.numeric) %>%
  as.data.frame()

for (row in 1:nrow(combat_stamps)) {
  if (combat_stamps[row, 12] == "2") {
    combat_stamps[row - 1, 5] <- paste(combat_stamps[row, 5])
    combat_stamps[row - 1, 9] <- paste(combat_stamps[row, 9])
  }
  if (combat_stamps[row, 12] == "3") {
    combat_stamps[row - 2, 6] <- paste(combat_stamps[row, 6])
    combat_stamps[row - 2, 10] <- paste(combat_stamps[row, 10])
  }
  if (combat_stamps[row, 12] == "4") {
    combat_stamps[row - 3, 7] <- paste(combat_stamps[row, 7])
    combat_stamps[row - 3, 11] <- paste(combat_stamps[row, 11])
  }
}

clean <- combat_stamps %>%
  mutate_if(is.character, as.numeric) %>%
  filter(encounter_number == 1) %>%
  select(-c(2, 3, 12)) %>%
  mutate(Episode = as.numeric(Episode)) %>%
  left_join(clean, ., by = "Episode")

# create combat counter and role play/combat variable
clean <- clean %>%
  mutate(encounter_count = NA) %>%
  mutate(
    encounter_count = replace(
      encounter_count,
      startTime >= encounter_start_1 &
        startTime <= encounter_end_1,
      1
    ),
    encounter_count = replace(
      encounter_count,
      startTime >= encounter_start_2 &
        startTime <= encounter_end_2,
      2
    ),
    encounter_count = replace(
      encounter_count,
      startTime >= encounter_start_3 &
        startTime <= encounter_end_3,
      3
    ),
    encounter_count = replace(
      encounter_count,
      startTime >= encounter_start_4 &
        startTime <= encounter_end_4,
      4
    )
  ) %>%
  mutate(
    rp_combat = NA,
    rp_combat = replace(rp_combat, !is.na(encounter_count), "combat"),
    rp_combat = replace(
      rp_combat,
      is.na(rp_combat),
      "role_play"
    ),
    rp_combat = replace(
      rp_combat,
      segment == "extra" | segment == "proglog" | segment == "break",
      NA
    )
  )


# incorporate combat and RP into segment
clean <- clean %>%
  mutate(
    segment = replace(segment, rp_combat == "combat", "combat"),
    segment = replace(segment, segment == "first_half", "first_half_rp"),
    segment = replace(segment, segment == "second_half", "second_half_rp"),
    segment = replace(segment, segment == "extra", "epilog"),
    segment = replace(segment, segment == "prologue" & startTime >= 10000, "epilog")
  )

# turns per episode
clean <- clean %>%
  group_by(Episode) %>%
  mutate(turn_number = row_number())


# add variable where all guests are labeled as 'GUESTS'
clean <- clean %>% mutate(Actor_Guest = Actor)
guests <- toupper(guests)

for (spelling in guests) {
  clean$Actor_Guest <- gsub(spelling, "GUESTS", clean$Actor_Guest)
}



# get attendance data frame and delete it from clean data frame
attendance <- clean %>%
  select(Episode, 8:19) %>%
  distinct_all()

write.csv(attendance, "./data/clean_data/rest/attendance.csv", row.names = FALSE)

clean <- clean %>% select(-c(8:19, number))
clean <- clean %>% select(-c(17:20, 22:29))

# reorder variables
clean <- clean %>% select(
  Arc, Arc_no, Episode, turn_number, segment, Actor, Text,
  startTime, endTime, timeDiffInSecs, wordCount,
  words_per_minute, encounter_count, rp_combat, Actor_Guest,
  vox_machina, guest, staff, Matt
)

# rename variables
# (in theory this could quicker up by replacing with crtl+f.
# However, this breaks the code. Since I am lazy I just rename the variables
# manually.)
clean <- clean %>% rename(
  arc = Arc,
  arc_no = Arc_no,
  episode = Episode,
  actor = Actor,
  text = Text,
  start_turn = startTime,
  end_turn = endTime,
  time_in_sec = timeDiffInSecs,
  word_count = wordCount,
  actor_guest = Actor_Guest,
  matt = Matt
)

# make arc a factor for nicer graphs
clean <- clean %>%
  mutate(arc = factor(arc, levels = c(
    "Kraghammer", "Vasselheim", "Briarwoods",
    "Attack_Conclave", "Vestiges", "Fall_Conclave",
    "Daring_Deeds", "End"
  )))


# export episode as individual files
files <- split(clean, clean$episode)

lapply(files, function(x) {
  write.csv(x,
    file = paste0(
      "./data/clean_data/individual_episodes/E_",
      x[1, "episode"],
      ".csv"
    )
  )
})


###############################################################################
# Natural Rolls
###############################################################################

# --- PC natural 20s
pc_20 <- read.csv("./data/raw_data/dice_rolls/pc_20.csv")

# extract episode numbers
pc_20 <- pc_20 %>%
  filter(raw != "None") %>%
  mutate(episode = raw) %>%
  mutate(description = sub("^\\d+\\s+", "", raw)) %>%
  mutate(episode = gsub("([[:digit:]]+).*", "\\1", episode)) %>%
  mutate(episode = replace(episode, grepl("\\(", episode), NA)) %>%
  mutate(episode = replace(episode, grepl("\\.", episode), NA)) %>%
  mutate(episode = replace(episode, episode == "Scanlan 1", NA))

# expand episode numbers
pc_20 <- pc_20 %>% fill(episode, .direction = c("down"))

# only time stamps
pc_20 <- pc_20 %>% filter(grepl("\\:", description))

# preparations: correct formatting errors
pc_20 <- pc_20 %>%
  mutate(description = gsub("1\\(", "\\(", description)) %>%
  mutate(description = gsub("Nat20=45, ", "", description)) %>%
  mutate(description = gsub("p2. ", "", description)) %>%
  mutate(description = gsub("p1. ", "", description)) %>%
  mutate(description = gsub("p1.", "", description)) %>%
  mutate(description = gsub(" 1 ", " ", description))

# separate time stamp from description and actor and drop raw text
pc_20 <- pc_20 %>%
  separate(description, c("time_stamp", "description"), sep = "\\)", extra = "drop") %>%
  mutate(time_stamp = str_replace(
    time_stamp,
    "\\(",
    ""
  )) %>%
  separate(time_stamp, c("actor", "time_stamp"), sep = " ", extra = "drop") %>%
  select(-raw) %>%
  mutate(description = str_squish(description))

# Replace characters with Actors
Actor <- c("Orion", "Laura", "Laura", "Liam", "Liam", "Marisha", "Taliesin", "Ashley", "Travis", "Sam", "Sam")
PC <- c("Tiberius", "Vex'ahlia", "Vex’ahlia", "Vax'ildan", "Vax’ildan", "Keyleth", "Percy", "Pike", "Grog", "Scanlan", "Taryon")
for (i in seq(1:9)) {
  pc_20$actor <- gsub(PC[i], Actor[i], pc_20$actor)
}

pc_20 <- pc_20 %>%
  mutate(actor = replace(actor, actor == "Scanlan", "Sam")) %>%
  mutate(guest = 1) %>%
  mutate(
    guest = replace(guest, actor == "Orion", 0),
    guest = replace(guest, actor == "Laura", 0),
    guest = replace(guest, actor == "Liam", 0),
    guest = replace(guest, actor == "Marisha", 0),
    guest = replace(guest, actor == "Taliesin", 0),
    guest = replace(guest, actor == "Ashley", 0),
    guest = replace(guest, actor == "Travis", 0),
    guest = replace(guest, actor == "Sam", 0),
    guest = replace(guest, actor == "Trinket", 0),
    guest = replace(guest, actor == "Doty", 0)
  ) %>%
  mutate(actor = replace(actor, guest == 1, "Guests")) %>%
  mutate(episode = as.numeric(episode))

# save file
write.csv(pc_20, "./data/clean_data/dice_rolls/pc_20.csv", row.names = FALSE)


# --- PC natural 1s
pc_1 <- read.csv("./data/raw_data/dice_rolls/pc_1.csv")

# extract episode numbers
pc_1 <- pc_1 %>%
  filter(raw != "None stated") %>%
  filter(raw != "None") %>%
  mutate(episode = raw) %>%
  mutate(description = sub("^\\d+\\s+", "", raw)) %>%
  mutate(episode = gsub("([[:digit:]]+).*", "\\1", episode)) %>%
  mutate(episode = replace(episode, grepl("\\(", episode), NA)) %>%
  mutate(episode = replace(episode, grepl("\\.", episode), NA)) %>%
  mutate(episode = replace(episode, episode == "Scanlan 1", NA)) %>%
  mutate(episode = replace(episode, episode == "Scanlan 2", NA))

# expand episode numbers
pc_1 <- pc_1 %>% fill(episode, .direction = c("down"))

# only time stamps
pc_1 <- pc_1 %>% filter(grepl("\\:", description))

# preparations: correct formatting errors
pc_1 <- pc_1 %>%
  mutate(description = gsub("1\\( ", "\\(", description)) %>%
  mutate(description = gsub("Nat1, ", "", description)) %>%
  mutate(description = gsub("p2. ", "", description)) %>%
  mutate(description = gsub("p1. ", "", description)) %>%
  mutate(description = gsub("Unknown, ", "", description)) %>%
  mutate(description = gsub("Elemental \\(Keyleth\\)", "Keyleth", description)) %>%
  mutate(description = str_replace(description, "Scanlan  ", "Scanlan ")) %>%
  mutate(description = str_replace(description, " 2 ", " "))


# separate time stamp from description and actor and drop raw text
pc_1 <- pc_1 %>%
  separate(description, c("time_stamp", "description"), sep = "\\)", extra = "drop") %>%
  mutate(time_stamp = str_replace(
    time_stamp,
    "\\(",
    ""
  )) %>%
  separate(time_stamp, c("actor", "time_stamp"), sep = " ", extra = "drop") %>%
  # because of the warning: manual tweak (I don't know why it bugs tbh)
  mutate(time_stamp = replace(time_stamp, is.na(time_stamp), "3:22:30")) %>%
  mutate(actor = str_replace(actor, "3:22:30", "")) %>%
  select(-raw) %>%
  mutate(description = str_squish(description)) %>%
  mutate(actor = str_squish(actor))


# Replace characters with Actors
Actor <- c("Orion", "Laura", "Laura", "Liam", "Liam", "Marisha", "Taliesin", "Ashley", "Travis", "Sam", "Sam")
PC <- c("Tiberius", "Vex'ahlia", "Vex’ahlia", "Vax'ildan", "Vax’ildan", "Keyleth", "Percy", "Pike", "Grog", "Scanlan", "Taryon")
for (i in seq(1:9)) {
  pc_1$actor <- gsub(PC[i], Actor[i], pc_1$actor)
}

# add guests
pc_1 <- pc_1 %>%
  mutate(actor = replace(actor, actor == "Scanlan", "Sam")) %>%
  mutate(guest = 1) %>%
  mutate(
    guest = replace(guest, actor == "Orion", 0),
    guest = replace(guest, actor == "Laura", 0),
    guest = replace(guest, actor == "Liam", 0),
    guest = replace(guest, actor == "Marisha", 0),
    guest = replace(guest, actor == "Taliesin", 0),
    guest = replace(guest, actor == "Ashley", 0),
    guest = replace(guest, actor == "Travis", 0),
    guest = replace(guest, actor == "Sam", 0),
    guest = replace(guest, actor == "Trinket", 0),
    guest = replace(guest, actor == "Doty", 0)
  ) %>%
  mutate(actor = replace(actor, guest == 1, "Guests"))

# save
write.csv(pc_1, "./data/clean_data/dice_rolls/pc_1.csv", row.names = FALSE)


# --- DMs natural 1s
dm_1 <- read.csv("./data/raw_data/dice_rolls/dm_1.csv")

# preparations: correct formatting errors
dm_1 <- dm_1 %>%
  mutate(raw = gsub("Ep83 ", "Ep83,", raw)) %>%
  mutate(raw = gsub("p1.", "", raw)) %>%
  mutate(raw = str_squish(raw))


# split the text such that we get a description, a episode number, a time stamp,
# and the NPC, which rolls the natural 1
# note: we dont care about the warnings as these as NA in temporary variables
dm_1 <- dm_1 %>%
  mutate(episode = raw) %>%
  mutate(description = sub("^\\d+\\s+", "", raw)) %>%
  mutate(NPC = gsub("([[:digit:]]+).*", "\\1", episode)) %>%
  separate(NPC, c("NPC", "tmp"), sep = " \\(", extra = "drop") %>%
  separate(description, c("tmp", "time"), sep = " \\(", extra = "drop") %>%
  select(-tmp) %>%
  separate(time, c("time", "description"), sep = "\\)", extra = "drop") %>%
  separate(time, c("episode", "time"), sep = ",", extra = "drop")

# clean up
dm_1 <- dm_1 %>%
  mutate(episode = gsub("Ep", "", episode)) %>%
  mutate(episode = gsub("Ep", "", episode)) %>%
  mutate(episode = gsub("E", "", episode)) %>%
  mutate(episode = as.numeric(episode)) %>%
  mutate_all(na_if, "") %>%
  mutate(description = str_squish(description)) %>%
  select(-raw)


write.csv(dm_1, "./data/clean_data/dice_rolls/dm_1.csv", row.names = FALSE)


# --- DM natural 20s
dm_20 <- read.csv("./data/raw_data/dice_rolls/dm_20.csv")

# preparations: correct formatting errors
dm_20 <- dm_20 %>%
  mutate(raw = gsub("Ep108 ", "Ep108,", raw))

# split the text such that we get a description, a episode number, a time stamp,
# and the NPC, which rolls the natural 20
# note: we dont care about the warnings as these as NA in temporary variables
dm_20 <- dm_20 %>%
  mutate(episode = raw) %>%
  mutate(description = sub("^\\d+\\s+", "", raw)) %>%
  mutate(NPC = gsub("([[:digit:]]+).*", "\\1", episode)) %>%
  separate(NPC, c("NPC", "tmp"), sep = " \\(", extra = "drop") %>%
  separate(description, c("tmp", "time"), sep = " \\(", extra = "drop") %>%
  select(-tmp) %>%
  separate(time, c("time", "description"), sep = "\\)", extra = "drop") %>%
  separate(time, c("episode", "time"), sep = ",", extra = "drop") %>%
  mutate(time = gsub("p1.", "", time))

# clean up
dm_20 <- dm_20 %>%
  select(-raw) %>%
  mutate(episode = gsub("Ep", "", episode)) %>%
  mutate(episode = gsub("Ep", "", episode)) %>%
  mutate(episode = as.numeric(episode)) %>%
  mutate_all(na_if, "") %>%
  mutate(description = str_squish(description))

write.csv(dm_20, "./data/clean_data/dice_rolls/dm_20.csv", row.names = FALSE)

###############################################################################
# All dice roles
###############################################################################

dice_rolls <- read.csv("./data/raw_data/dice_rolls/dice_roles.csv",
  na.strings = c("", "NA")
)

dice_rolls <- dice_rolls %>%
  filter(!is.na(Episode)) %>%
  mutate(roll = 1) %>%
  rename(file_name = Episode) %>%
  mutate(
    file_name = str_replace(file_name, " p1", "-1"),
    file_name = str_replace(file_name, " p2", "-2")
  )

write.csv(dice_rolls, "./data/clean_data/dice_rolls/dice_rolls.csv", row.names = FALSE)

###############################################################################
# Face Palms
###############################################################################


face_palms <- read.csv("./data/raw_data/rest/face_palms_raw.csv")

# extract episode numbers
face_palms <- face_palms %>%
  mutate(episode = raw) %>%
  mutate(description = sub("^\\d+\\s+", "", raw)) %>%
  mutate(episode = gsub("([[:digit:]]+).*", "\\1", episode)) %>%
  mutate(episode = replace(episode, grepl("\\(", episode), NA)) %>%
  mutate(episode = replace(episode, grepl("\\.", episode), NA))

# expand episode numbers
face_palms <- face_palms %>% fill(episode, .direction = c("down"))

# only time stamps
face_palms <- face_palms %>% filter(grepl("\\:", description))

# separate time stamp from description and drop raw text
face_palms <- face_palms %>%
  separate(description, c("time_stamp", "description"), sep = "\\)", extra = "drop") %>%
  mutate(time_stamp = str_replace(
    time_stamp,
    "\\(",
    ""
  )) %>%
  select(-raw)

write.csv(face_palms, "./data/clean_data/rest/face_palms.csv", row.names = FALSE)


###############################################################################
# Seating Order
###############################################################################

seating_order <- read.csv("./data/raw_data/rest/seating_order.csv")

# change into long format
seating_order <- seating_order %>%
  pivot_longer(!Actor,
    names_to = "Actor2",
    values_to = "Distance"
  ) %>%
  filter(Actor != Actor2) %>%
  filter(!is.na(Distance))

# Same order
seating_order <- seating_order %>%
  mutate(Actor = paste(Actor, " And ", Actor2)) %>%
  select(-Actor2)

seating_order$Actor <- unname(sapply(seating_order$Actor, function(x) {
  paste(sort(trimws(strsplit(x[1], " And ")[[1]])), collapse = " And ")
}))

seating_order <- seating_order %>%
  separate(Actor,
    c("from", "to"),
    sep = " And ",
    fill = "left",
    remove = TRUE
  )

write.csv(seating_order, "./data/clean_data/rest/seating_order.csv", row.names = FALSE)


###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))
