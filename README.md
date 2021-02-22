Critical Role
================

# Packages

``` r
# data manipulation
library(tidyverse)
library(dplyr)
#structure replacement Überhang to Ueberhang etc.
library(stringr)
# plots
library(grid)
library(ggplot2)
# package for table
library(knitr)
# for reading in SRT files
library(SRTtools)
```

## Theme

``` r
hp_theme <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75,
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0",
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0,
                                       lineheight = 0.9, margin = margin(),
                                       debug = FALSE),
      plot.margin =       margin(12,10,5,10),
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(0.75), family = '' ,
                                       face = 'bold', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B'),
      plot.subtitle =     element_text(size = rel(0.4), family = '' ,
                                       face = 'plain', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B', 
                                       margin = margin(0,0,15,0)),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}
```

# Get Data

``` r
# Note: E031-2,E035-2,E082,E113-1,E113-2 have been run once through
# error fixing* with Subtitle editor** in Ubuntu 20.04. This is needed as
# the files contain formatting errors, which renders them unusable for the 
# following analysis. 
# The original files are included in the same folder and are named such as
# 'original_C1E031-1_FINAL.srt'
# To exclude the modified files from the analysis delete the # for lines 92-94 
# and a ad hashtag in line 91.
# * Tools -> Error checking -> Error -> Try to fix all 
# ** (install via 'sudo apt install subtitleeditor' on 16.09.2020)


# Get a list of all files named in the folder so we can loop over the list
  filenames <- list.files("./Season 1", pattern="*.srt", full.names=TRUE)
# kick out the original (not edited) files
  filenames <- head(filenames, -5)
      
# delete edited files if you wanted to: 
# 32,38,85,120,121 (E031-2,E035-2,E082,E113-1,E113-2)    
# filenames <- filenames[-c(32,38,85,120,121)]

# Get episode name: kick out path     
  Episode_number <- substring(filenames, 15)
# kick out file ending  
  Episode_number <- sapply(strsplit(Episode_number, "_"), "[", 1)
  
# Conversion of file number to episode numbers in vector for conversion, as 
# some episodes have two files the name in the files and the position in the   
# list does not directly correspond to the final episode.
  conversion <- as.data.frame(cbind(File_number = seq(1:length(filenames)),
                                    Episode_number))
  

# read in files (creates one large list, which contains lists for each  
# episode/file)
  raw<- lapply(filenames,srt.read)
  
# create data set:
# create individual rows with readable time stamps for each subtitle segment
# within the lists. 
# Note: this takes time!
  raw <- lapply(raw,function(x){            
      con<-textConnection(x)
      lines <- readLines(con)
      listOfEntries <- 
        lapply(split(1:length(lines),cumsum(grepl("^\\s*$",lines))),
               function(blockIdx){
                    block <- lines[blockIdx]
                    block <- block[!grepl("^\\s*$",block)]
                    return(data.frame(times=block[2], 
                            textString=paste0(block[3:length(block)],
                                              collapse="\n"),
                                              stringsAsFactors = FALSE)) })
        do.call(rbind,listOfEntries) })
   
# create data frame from the lists    
  raw <- bind_rows(raw, .id = "File_number")
# add episode number and delete file number as more intuitive   
  raw <- merge(raw,conversion,all.x=TRUE)
  raw <- select(raw,-c(File_number))

# remove not needed files
  rm(Episode_number,filenames,conversion)
  
# export to csv to save time as this does to have to be run each time  
  write.csv(raw,"raw.csv", row.names = FALSE)  
```

# clean/manipulate data

## Preparation

``` r
# read in raw data
  raw <- read.csv(file = 'raw.csv')

# backup
  clean <- raw

# to be able to analysis things like words per minute etc.: 
# split start and end times 
  tmp <- do.call(rbind,strsplit(clean[,'times'],' --> '))
  clean$startTime <- tmp[,1]
  clean$endTime <- tmp[,2]
  clean = select(clean, -times)  
  rm(tmp)
# start and end time in seconds for easier calculation:
# parse times to add second videos for episodes to episode
  tmp <- do.call(rbind,lapply(strsplit(clean$startTime,':|,'),as.numeric))
  clean$startTime  <- tmp %*% c(60*60,60,1,1/1000)
  tmp <- do.call(rbind,lapply(strsplit(clean$endTime,':|,'),as.numeric))
  clean$endTime  <- tmp %*% c(60*60,60,1,1/1000)
  rm(tmp)
  
# Consisting naming to allow for text analysis: 
# replace "&" and "&amp;" with "and" 
  clean$textString<- gsub("&amp;", "and", clean$textString)  
  clean$textString<- gsub("&", "and", clean$textString)
# rename DandD (comes from the editing previous step) to D&D  
  clean$textString<- gsub("DandD", "D&D", clean$textString)
  
# first though, delete everything between brackets as these indicate 
# descriptions  
  clean$textString <- gsub("\\s*\\([^\\)]+\\)",
                         "",
                         as.character(clean$textString))
```

## Extract Speaker

So far the speaker is still contained in the ‘textString’ variable. To
have labels for the spoken text we need to extract it. Problem: we need
to extract all speakers including misspelled ones.

``` r
head(clean)

# seperate speaker from text. Speakers are indicated using ":" in the files
# e.g. Liam: I open the door
  clean <- clean %>% separate(textString, c("Speaker","Text"), sep = ": ", 
                              fill = "left", remove=FALSE, extra = "merge")
  
# Some text got miss matched 
# e.g. 'I open the door: What do I see?' becomes 
# Speaker: 'I open the door'; Tect: 'What do I see?'
# Final Goal: get list of individual speakers, which need to be extract from
# textString
  
# get all unique capitalized speakers as unlikely a relevant speakers were 
# never(!) capitalized
  unique = distinct(select(clean, Speaker))
      
# Apply consistent naming (different coders write all capital, 
# others only capitalize first letter etc.): all characters in small letters 
  unique$Speaker <- tolower(as.character(unique$Speaker))   

# Get amount of words for each speaker. We get these as we go by length from 
# here. The reason for doing this, is the fact that it makes it easy to follow
# as a lot of manual adjustments have to be made, due to the fact that the 
# subtitles are handcoded by volunteers. Hence, there will inconsistencies and 
# spelling mistakes (an average episode is around 3.5hours after all).
  unique = distinct(select(unique, Speaker))
# get word count  
  unique$total_words <- sapply(unique$Speaker, 
                               function(x) length(unlist(
                                 strsplit(as.character(x), "\\W+"))))
```

### Lists for Actors, Characters and Guests

Create loop for actors, characters and guests so they can be drop from
list of speakers. Note this takes long as there are many potential
combinations of speakers.

``` r
    actors <- c("matt","travis","orion","taliesin", "marisha","liam",
                "laura","ashley","sam")
    character <- c("tiberius","vex","vex'ahlia","vax'ildan","vax","keyleth",
                   "percy","scanlan","pike","grog","taryon")
    guests <- c("will","wil","mary","dan","ify","zac","felicia", 
                "patrick", "kit","joe","jason", "chris","jon","darin", "noelle")
    names <- c(actors,character,guests)
    and <- paste(names, "and", sep=" ")
    comma <- paste(names, ",", sep="")
    multiple <- c(and,comma)
    rm(and,comma)
    


  
# creating combinations of names with " and " and "," with right spelling
additions <- c(" and ", ", ")
combinations <- data.frame(name1=names,
                           name2=names,
                           name3=names)
combinations_names <- data.frame(combinations=as.character())
for (type in additions){
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
  combinations4 <- as.data.frame(rbind(combinations2,combinations3))
# as symmetrical so combinations created twice
  combinations4 <- unique(combinations4)
# get double names (e.g matt and matt) so they can be deleted from list
  name2 <- paste(names, names, sep=type)
# delete double names  
  for (double_name in name2){
     combinations4 <- combinations4 %>% 
       filter(!str_detect(combinations, double_name)) 
    rm(double_name)}
# combine with data frame  
  combinations_names <- rbind(combinations_names,combinations4) 
 rm(combinations2,combinations3,combinations4,name2,type)}


# combine with single names
all_names <- as.list(c(combinations_names$combinations,names))

# extract these combinations from unique list
  acutal_combintations <- data.frame(Speaker=as.character())
  for (name in all_names){
  acutal_combintations <- unique %>% 
    filter(Speaker==name) %>%
    bind_rows(., acutal_combintations)
  rm(name)}

# drop these combinations from the unique list
  unique <- anti_join(unique, acutal_combintations, by = "Speaker")         

# Create Data Frame for actual Speakers
  Speakers <- acutal_combintations
# drop data frame of acutal_combintations and all_names
  rm(acutal_combintations, all_names,additions,combinations,combinations_names)
```

### Parts of dialog mismatched (\>=4 words)

Keep cast and guests of show and combinations of them: logic \>4 words
and then go smaller. Long phrases can be excluded first, as they do not
indicate a player/NPC is speaking (see door example above) OR they
indicate multiple actors speaking. This puts 278 Speakers into a data
frame and brings the number to be sort to 316.

``` r
# select only the ones with four or more words
  phrases <- unique %>% 
    filter(total_words>=4) %>% 
    subset(select = c(Speaker,total_words))   

# create data list for ", and" to get a combination of speakers not yet 
# detected due to the structure of: "laura, liam, and sam"
comma_and_list <- paste(", and", names, sep=" ")
comma_and <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (phrase in comma_and_list){
     comma_and <- phrases %>% 
       filter(str_detect(Speaker, phrase)) %>%
       bind_rows(., comma_and)
     rm(phrase)}
# clean up
comma_and <- comma_and %>% 
        filter(!str_detect(Speaker, "grog's there"))

# add to Speakers and delete from phrases and unique
Speakers <- bind_rows(Speakers, comma_and)
phrases <- anti_join(phrases, comma_and, by = "Speaker")
unique <- anti_join(unique, comma_and, by = "Speaker")

# rest of character combinations: combinations of "," and "and" 
  tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (name in multiple){
  tmp <- phrases %>% filter(grepl(name, Speaker)) %>%
                      bind_rows(., tmp)}
  tmp <- tmp %>% filter(total_words==4)
# add to Speakers and delete from phrases and unique
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
  rm(tmp)
# manual tweaking:
  tmp <- phrases %>% filter(Speaker=="off-screen voice #1" |
                        Speaker=="off-screen voice #2" |
                        grepl("/", Speaker))
# adding and dropping  
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
  rm(tmp)
# drop everything left over in phrases from unique data frame
  unique <- anti_join(unique, phrases, by = "Speaker")
  rm(phrases)
```

### Phrases, which are exactly three words long and kicking out characters

Now, the list to be edited contains to only 246 and the list of speakers
already identified is 287.

``` r
# dropping of speaker which contain only actors/characters 
# (no adjustments needed)
  for (name in names){
      unique <-  unique %>% filter(Speaker!= name) 
    rm(name)}

# dropping all three words rows as they imply miss matched actor and 
# do some manual edits for things to keep  
  phrases <- unique %>% filter(total_words==3)

# extract groups of speaker left over. Note Travis, pretending Twicket is an 
# actual speaker.
  tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (name in multiple){
  tmp <- phrases %>% filter(grepl(name, Speaker)) %>%
                      bind_rows(., tmp)}
# adding and dropping  
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
  
# extract other NPC and voices
  tmp <- phrases %>% 
    filter(Speaker=="computer-generated voice" |
           Speaker=="man with chicken")
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
  
# drop everything left over in phrases from unique data frame
# NOTE TO MYSELF: ♪ stuff is here
  unique <- anti_join(unique, phrases, by = "Speaker")
  rm(phrases)  
```

### Phrases, which are exactly two words long

Now, the list to be edited contains only 137 and the list of speakers
already identified is 305.

``` r
# kick out phrases, which contain 2 words
  phrases <- unique %>% filter(total_words==2)
      
# extract if contains "/" (other form of writing and...)
  tmp <- phrases %>% filter(grepl("/", Speaker))
# adding and dropping  
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
  
  
# drop if names
  tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (name in names){
      tmp <- phrases %>% 
       filter(str_detect(Speaker, name)) %>%
       bind_rows(., tmp)
     rm(name)}

# dropping  
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")
      
# NPCs      
  NPC_list<- c("guard 1","dwarven barkeep","nearby dwarves","dwarf 1",
                     "drinking dwarf","guard 2","carver 2","bar patron",
                     "dwarf patron","dwarven patrons","dwarven guard",
                     "nostoc greyspine","dwarf guard","audience member",
                     "seeker asum","uriel taldorei","arbiter braum",
                     "crewmember 1","crewmember 2",
                     "man with chicken")
      
      tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
      
      for (npc in NPC_list){
     tmp <- phrases %>% 
       filter(str_detect(Speaker, npc)) %>%
       bind_rows(., tmp)
     rm(npc)}
# adding and dropping  
  Speakers <- bind_rows(Speakers, tmp)
  phrases <- anti_join(phrases, tmp, by = "Speaker")
  unique <- anti_join(unique, tmp, by = "Speaker")      

# drop the rest from the unique list      
  unique <- anti_join(unique, phrases, by = "Speaker")  
```

*Vox Machina, patrick rothfuss, offmember 1, offmember2, crewmember
\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!*

### One word: Extracting misspellings of CR members

Now, the list to be edited contains only 91 and the list of speakers
already identified is 351.

``` r
# --- Ashley
  Ashley <- unique %>% filter(grepl("^ash", Speaker) | 
                             grepl("^ahs", Speaker)) 
# Merge / drop
  Speakers <- bind_rows(Speakers, Ashley)
  unique <- anti_join(unique, Ashley, by = "Speaker")                             
# make list of miss spellings for Ashley as needed for loops later on
  Ashley <- as.vector(Ashley$Speaker)

# --- Grog
  Grog <- unique %>% filter(grepl("gorg", Speaker))                                 
  Speakers <- bind_rows(Speakers, Grog)
  unique <- anti_join(unique, Grog, by = "Speaker")                             
  Grog <- as.vector(Grog$Speaker)

# --- Laura
  Laura <- unique %>% filter(grepl("larua", Speaker) | 
                             grepl("lauar", Speaker) |
                             grepl("laura:", Speaker) |
                             grepl("laura", Speaker))
  Speakers <- bind_rows(Speakers, Laura)
  unique <- anti_join(unique, Laura, by = "Speaker")  
  Laura <- as.vector(Laura$Speaker)

# ---  Marisha
  Marisha <- unique %>% filter(grepl("^mar", Speaker) |
                                    grepl("^mai", Speaker))
  Marisha <- Marisha %>% filter(Speaker!="mark",
                              Speaker!="margrim")
  Speakers <- bind_rows(Speakers, Marisha)
  unique <- anti_join(unique, Marisha, by = "Speaker")  
  Marisha <- as.vector(Marisha$Speaker)

# --- Matt
  Matt <- unique %>% filter(grepl("^mat", Speaker))
  Speakers <- bind_rows(Speakers, Matt)
  unique <- anti_join(unique, Matt, by = "Speaker")  
  Matt <- as.vector(Matt$Speaker)

# --- Orion
  Orion <- unique %>% filter(grepl("^or", Speaker))
  Speakers <- bind_rows(Speakers, Orion)
  unique <- anti_join(unique, Orion, by = "Speaker")  
  Orion <- as.vector(Orion$Speaker)

# --- Sam
  Sam <- unique %>% filter(grepl("sma", Speaker) |
                            grepl("sam ", Speaker))

  Speakers <- bind_rows(Speakers, Sam)
  unique <- anti_join(unique, Sam, by = "Speaker")  
  Sam <- as.vector(Sam$Speaker)

# --- Taliesin
  Taliesin <- unique %>% filter(grepl("^tal", Speaker) |
                                   grepl("^tai", Speaker))      
  Speakers <- bind_rows(Speakers, Taliesin)
  unique <- anti_join(unique, Taliesin, by = "Speaker")  
  Taliesin <- as.vector(Taliesin$Speaker)

# --- Travis
  Travis <- unique %>% filter(grepl("^ta", Speaker) | 
                             grepl("^tra", Speaker)) 
  Speakers <- bind_rows(Speakers, Travis)
  unique <- anti_join(unique, Travis, by = "Speaker")  
  Travis <- as.vector(Travis$Speaker)

# --- Vax
  Vax <- unique %>% filter(grepl("vax", Speaker)) 
  Speakers <- bind_rows(Speakers, Vax)
  unique <- anti_join(unique, Vax, by = "Speaker")  
  Vax <- as.vector(Vax$Speaker)

# --- Vex
  Vex <- unique %>% filter(grepl("vex", Speaker)) 
  Speakers <- bind_rows(Speakers, Vex)
  unique <- anti_join(unique, Vex, by = "Speaker")  
  Vex <- as.vector(Vex$Speaker)
  
# --- Liam
  Liam <- unique %>% filter(grepl("liam ", Speaker)) 
  Speakers <- bind_rows(Speakers, Liam)
  unique <- anti_join(unique, Liam, by = "Speaker")  
  Liam <- as.vector(Liam$Speaker)
```

### One word: Rest

Dropping missidenitified and the number of speakers identified to 385.

``` r
#--- drop words:
  words <- c("like","finding","themselves","chamber","books","magic",
              "caveat","going","else","decide","call","one","attacks","is",
                 "though","chasers","announce","nest","detail","say",
                 "yesterday","which","companion","titansgrave","bonfires",
                 "need","want","undergrowth","away","dragonborn","things",
                 "egypt","announcements","enlarge","yellow","green","for",
                 "again","quarry","trinket","#1","#2")

  for (word in words){
    unique <- unique %>% filter(Speaker!=word)
  rm(word)}
  rm(words)


# --- Guest: Jore (ep. 17)
  Jore <- unique %>% filter(grepl("jore", Speaker) | 
                             grepl("jroe", Speaker)) 
  Speakers <- bind_rows(Speakers, Jore)
  unique <- anti_join(unique, Jore, by = "Speaker")  
  Jore <- as.vector(Jore$Speaker)
  
# --- Guest: Brian
  Brian <- unique %>% filter(grepl("brain", Speaker) | 
                             grepl("brian", Speaker)) 
  Speakers <- bind_rows(Speakers, Brian)
  unique <- anti_join(unique, Brian, by = "Speaker")  
  Brian <- as.vector(Brian$Speaker)  
  
# --- Producer: Zac
  Zac <- unique %>% filter(grepl("zack", Speaker) | 
                             grepl("zac", Speaker)) 
  Speakers <- bind_rows(Speakers, Zac)
  unique <- anti_join(unique, Zac, by = "Speaker")  
  Zac <- as.vector(Zac$Speaker)    
  
#NPC: Giles
  Giles <- unique %>% filter(grepl("gile", Speaker)) 
  Speakers <- bind_rows(Speakers, Giles)
  unique <- anti_join(unique, Giles, by = "Speaker")  
  Giles <- as.vector(Giles$Speaker)  

# numerous speakers  
  numerous <- c("several", "others", "everyone","group", "vox machina","all")
    tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (word in numerous){
  tmp <- unique %>%  
    filter(Speaker==word) %>%
    rbind(.,tmp)
  }
  Speakers <- bind_rows(Speakers, tmp)
  unique <- anti_join(unique, tmp, by = "Speaker")  
  rm(tmp)

# offscreen
  offscreen <-c("lucas", "ryan", "offscreen", "erika", "audience", "crew", "
             kai", "producer", "luke", "alec", "offstage", "ivan", "becca", 
             "hector", "announcer", "iphone", "mark")
      tmp <- data.frame(Speaker=character(),
                 total_words=as.numeric())
  for (word in offscreen){
  tmp <- unique %>%  
    filter(Speaker==word) %>%
    rbind(.,tmp)
  }
  Speakers <- bind_rows(Speakers, tmp)
  unique <- anti_join(unique, tmp, by = "Speaker")  
  rm(tmp)
  
# add the rest to NPCs and Speakers
Speakers <- bind_rows(Speakers, unique)
unique  <- as.vector(unique$Speaker)
NPC_list <- c(NPC_list,unique)
rm(unique)

# delete duplicates
  Speakers <- unique(Speakers)
```

## Cleanining up and preparing Speaker List

``` r
  Speakers <- Speakers_backup

#drop if speaker=="" or NA
  Speakers <- Speakers %>% filter(Speaker != "",
                                  !is.na(Speaker))

# sort by words and characters here as then later on longer phrases 
# overwrite short phrases: e.g. Matt and Sam overwrite only Sam and Matt 
# individually if contained in a cell. Also Matt overwrites Mat
  Speakers$total_characters <- nchar(Speakers$Speaker)
  Speakers <- Speakers %>% arrange(total_characters,Speaker)

# add ":" in front of Speaker to be able to extract it
  Speakers$Speaker_dot <-sapply(Speakers$Speaker, paste, ":", sep="")

# make list for loop
  list_unique_dot <- c(Speakers$Speaker_dot)
```

# Extract Speaker (2.)

``` r
# first actors saying stuff in the subtitle segment
# everything to lower for consistent naming
  clean$Speaker <- tolower(as.character(clean$Speaker))
  clean$Text <- tolower(as.character(clean$textString))
      
# grab first actors in the segment saying something 
  clean$Actor <- NA
      for (speaker in list_unique){
        clean$Actor <- ifelse(grepl(speaker, 
                                         clean$Speaker),
                                   speaker,clean$Actor) }
     
# get : after actor to get make it dropable from text
      clean$Actor_dot <-sapply(clean$Actor, paste, ":", sep="")
      clean$Actor_dot <-ifelse(clean$Actor_dot=="NA:" , NA , clean$Actor_dot)

# drop first speaker from text
      clean$Text1 <- str_remove(clean$Text, clean$Actor_dot)
      clean$Text1 <-ifelse(is.na(clean$Text1), clean$Text ,clean$Text1)
 
# find second actor in speaking segment so you can split and attribute 
# in the next step
      clean$Actor2 <- NA
      for (speaker in list_unique_dot){
        clean$Actor2 <- ifelse(grepl(speaker, clean$Text1),
                                    speaker, clean$Actor2)}
      
# replace actor2 with #### for splitting afterwards
  clean$Text2 <- ifelse(is.na(clean$Actor2),
                      clean$Text1,
                      str_replace(clean$Text1, 
                                  clean$Actor2, "####"))
  
  clean <- clean %>% separate(Text2, c("Text2","Text3"), 
                                        sep = "#### ", 
                                        fill = "left", 
                                        remove=FALSE, 
                                        extra = "merge")

# assign text to actor 1 as not rightly assigned in previous step 
# (into text2 instead of text1 and text3 instead text2)
  clean$Text2 <- ifelse(is.na(clean$Text2),
                                 clean$Text1,
                                 clean$Text2)
      
  clean$Text3 <- ifelse(clean$Text2==clean$Text3 & 
                                      is.na(clean$Actor2),
                                      NA,
                                      clean$Text3)
      
# cleanup:
# drop: textString, Speaker, Text, Actor_dot, Text and
# rename: Text2 to Text1 and  Text3 to Text2
      clean <- clean %>% 
        select(-c(Speaker,Actor_dot,Text,Text1))  %>%
      rename(
        Actor1=Actor,
        Text1=Text2,
        Text2=Text3) 

      rm(list_unique,list_unique_dot,unique)     
           
# split and merge: get actor1 and actor2 as individual rows so text
# can be rightly.
# example row1 Sam: Hi ; Laura: Hi I'm ; row2: Laura. 
# So expanding should be for actor2. To ensure rightly expanding the rows split
# data set and create unique segment names: Actor_Episode_numberSpeaker
      Actor1 = select(clean, -c(Text2,Actor2))
      Actor1 <- Actor1 %>%
        rename(Actor=Actor1,
                Text=Text1)
      Actor1$number <- 1
      Actor2 =select(clean,-c(Text1,Actor1))
      Actor2 <- Actor2 %>% drop_na(Actor2 | Text2)
      Actor2 <- Actor2 %>%
        rename(Actor=Actor2,
                Text=Text2)
      Actor2$number <- 2
    
      clean <- bind_rows(Actor1, Actor2)
      rm(Actor1,Actor2) 
      
# get number of text segment for actors (e.g. Matt1, Matt2) 
# for the unique identifier and proper expanding of text segments
      segment <- clean %>%
        filter(!is.na(Actor)) %>%
        group_by(Episode_number,Actor) %>%
        mutate(num = 1:n())
    
      segment <- segment %>% 
        group_by(Episode_number,startTime,Actor) %>% 
        mutate(segment= paste(Actor, Episode_number,num ,sep = "_"))
      clean <-  left_join(clean,segment)
      rm(segment)

# sort for expanding to ensure text segments are assigned to right actor
      clean <- clean[order(clean$Episode_number, 
                                     clean$startTime,
                                     clean$number),]
# Expand segment for each row spoken
      clean <- clean %>% fill(segment) 

# get start and endtime for each segment
      clean <- clean %>% 
        group_by(segment) %>% 
        mutate(startTime=min(startTime))
      clean <- clean %>% 
        group_by(segment) %>%
        mutate(endTime=max(endTime))
      clean <- clean %>% 
        group_by(segment) %>% 
        mutate(fromSeconds=min(fromSeconds))
      clean <- clean %>% 
        group_by(segment) %>% 
        mutate(toSeconds=max(toSeconds))
      
# merge cells for text. 
      clean <- clean %>%
        group_by(segment) %>% 
        mutate(Text = paste0(Text, collapse = " "))
    
      clean <- clean %>%
        group_by(segment) %>% 
        mutate(textString = paste0(textString, collapse = " "))

#  only keep first observations. Matt: 'Hello everyone and welcome to 
#  critical role' as one as opposed to multiple rows)
      clean <- clean[!duplicated(clean$segment), ]
      
# drop actor from original text string
      clean$Actor_dot <- paste(clean$Actor, ":", sep="")
      clean$textString <- str_remove(clean$textString, 
                                          regex(clean$Actor_dot, 
                                                ignore_case = T))
      clean <- clean %>% select(-c(Actor_dot))

# drop segment number and reorder variables for clarity
      clean <- clean %>% 
        ungroup() %>% 
        select(c(Episode_number, startTime, endTime, Actor, Text,
                 fromSeconds, toSeconds, textString))
```

### Consistent Naming of Actor to simplify analysis

``` r
# replace ",","/" and "And" with "and"
  clean$Actor <-  gsub(",", " and", clean$Actor)
  clean$Actor <-  gsub("/", " and ", clean$Actor)
  clean$Actor <-  gsub("and and", "and", clean$Actor)

# Unique Actors so you can check all the way in which names are wrongly spelled: 
  unique = distinct(select(clean, Actor))
      
# split at "and"
  unique <- unique %>% 
      mutate(Actor = strsplit(as.character(Actor), " and ")) %>% 
       unnest(Actor)
      
# list of unique Actors: 
  unique = distinct(select(unique, Actor))
```

### 

cleanup misspelling: logic for most steps 1.) grab all forms misspelling
2.) create list \# 3.) loop over list and replace name with right name

``` r
# cleanup "-" 
  clean_data$Actor <-  gsub("-", "", clean_data$Actor)
# cleanup ":"
  clean_data$Actor <-  gsub(":", "", clean_data$Actor)
      
  clean_data$Actor <- trimws(clean_data$Actor)    
      
# Sam
  Sam1 <- unique[grepl("^sam", unique$Actor),]
  Sam1 <- Sam1[!grepl("and", Sam1$Actor),]
  Sam2 <- unique[grepl("^sma", unique$Actor),]
  Sam2 <- Sam2[!grepl("and", Sam2$Actor),]
  Sam <- as.list(unlist(rbind(Sam1,Sam2)))
  for (spelling in Sam){
        clean_data$Actor <-  gsub(spelling, "sam", clean_data$Actor) 
      }
     rm(Sam1,Sam2,Sam) 
      
# Ashley
  Ashley1 <- unique[grepl("^ash", unique$Actor),]
  Ashley1 <- Ashley1[!grepl("and", Ashley1$Actor),]
  Ashley2 <- unique[grepl("^ahs", unique$Actor),]
  Ashley2 <- Ashley2[!grepl("and", Ashley2$Actor),]
  Ashley <- as.list(unlist(rbind(Ashley1,Ashley2)))
      
  for (spelling in Ashley){
        clean_data$Actor <-  gsub(spelling, "ashley", clean_data$Actor) 
      }
      rm(Ashley1,Ashley2,Ashley)
      
# Matt: Mat ; second step necessary as otherwise does not work. Don't know why
  Matt1 <- unique[grepl("^mat", unique$Actor),]
  Matt1 <- Matt1[!grepl("and", Matt1$Actor),]
  Matt <- as.list(unlist(rbind(Matt1)))
  for (spelling in Matt){
        clean_data$Actor <-  gsub(spelling, "Mat", clean_data$Actor) 
      }
  clean_data$Actor <-  gsub("Mat", "matt", clean_data$Actor) 
  clean_data$Actor <-  gsub("mattr", "matt", clean_data$Actor)
   rm(Matt1,Matt)    

# Travis
  Travis1 <- unique[grepl("^tra", unique$Actor),]
  Travis1 <- Travis1[!grepl("and", Travis1$Actor),]
  Travis1 <- as.list(unlist(Travis1))
  Travis3 <- as.list(c("tavis","tarvis"))
  Travis <- unique(as.list(unlist(rbind(Travis1,Travis3))))
  for (spelling in Travis){
        clean_data$Actor <-  gsub(spelling, "travis", clean_data$Actor) 
      }
  rm(Travis,Travis1,Travis3)
 
# Laura
  Laura1 <- unique[grepl("^la", unique$Actor),]
  Laura1 <- Laura1[!grepl("and", Laura1$Actor),]
  Laura <- as.list(unlist(rbind(Laura1)))
  for (spelling in Laura){
        clean_data$Actor <-  gsub(spelling, "laura", clean_data$Actor) 
      }
  rm(Laura1,Laura)
      
# Liam        
  Liam1 <- unique[grepl("^liam", unique$Actor),]
  Liam1 <- Liam1[!grepl("and", Liam1$Actor),]
  Liam <- as.list(unlist(rbind(Liam1)))
      for (spelling in Liam){
        clean_data$Actor <-  gsub(spelling, "liam", clean_data$Actor) 
      }
      rm(Liam1,Liam)
      
# Taliesin
  Taliesin1 <- unique[grepl("^tal", unique$Actor),]
  Taliesin1 <- Taliesin1[!grepl("and", Taliesin1$Actor),]
  Taliesin3 <- c("tailesin")
  Taliesin <- as.list(unlist(rbind(Taliesin1,Taliesin3)))
  for (spelling in Taliesin){
        clean_data$Actor <-  gsub(spelling, "taliesin", clean_data$Actor) 
      }
  rm(Taliesin1,Taliesin3,Taliesin)
      
# Orion
  Orion1 <- unique[grepl("^or", unique$Actor),]
  Orion1 <- Orion1[!grepl("and", Orion1$Actor),]
  Orion <- as.list(unlist(rbind(Orion1)))
  for (spelling in Orion){
        clean_data$Actor <-  gsub(spelling, "orion", clean_data$Actor) 
      }
  rm(Orion,Orion1)
      
#  Marisha
# * Mar - Mary - Mark
# + Marsha + Mairsha 
  Marisha1 <- unique[grepl("^mar", unique$Actor),]
  Marisha1 <- Marisha1[!grepl("and", Marisha1$Actor),]
  Marisha1 <- as.list(Marisha1$Actor)
  Marisha3 <- c("marsha","mairsha","marishaa","marishah","marishaha","marishaia","marishaa", "♪ marisha")
  Marisha <- unique(as.list(unlist(rbind(Marisha1,Marisha3))))
  Marisha <- Marisha[Marisha != "mary"] 
  Marisha <- Marisha[Marisha != "mark"]
  Marisha <- Marisha[Marisha != "margrim"]
# delete \ (so \marisha gets replaced)
  clean_data$Actor<- gsub("\\\\", "",clean_data$Actor)  
      
  for (spelling in Marisha){
        clean_data$Actor <-  gsub(spelling, "marisha", clean_data$Actor) 
      }
      clean_data$Actor<- gsub("marishaaaa", "marisha",clean_data$Actor)
      clean_data$Actor<- gsub("marishaa", "marisha",clean_data$Actor)
      clean_data$Actor<- gsub("mairsha", "marisha",clean_data$Actor)
      clean_data$Actor<- gsub("marishah", "marisha",clean_data$Actor)
      clean_data$Actor<- gsub("marishaha", "marisha",clean_data$Actor)
      clean_data$Actor<- gsub("♪", "",clean_data$Actor)
      clean_data$Actor<- gsub("marishaa", "marisha",clean_data$Actor)
      rm(Marisha1,Marisha3,Marisha)
      
# 'actor'and replace
      clean_data$Actor <-  gsub("amandine", "xxxxx", clean_data$Actor)
      clean_data$Actor <-  gsub("and", " and ", clean_data$Actor)
      clean_data$Actor <-  gsub("xxxxx", "amandine", clean_data$Actor)
# delete unnessary white space   
      clean_data$Actor <-  gsub("  ", " ", clean_data$Actor)
```
