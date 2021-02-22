library(tidytext)
library(tidyverse)
library(textdata)
library(stringr)
library(wordcloud)
library(RColorBrewer)  
library(tm)

get_sentiments("afinn")
get_sentiments("bing")


# splits Text into words
    clean_data_tiny <- clean_data %>% 
      mutate(linenumber = row_number()) %>%
              ungroup() %>%
      unnest_tokens(word, Text_no_description)


# sentiment analysis: get sentiment
# bing et. al
    bing_sentiment <- clean_data_tiny %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 

bing_sentiment <- bing_sentiment %>%
                  rename(bing_negative=negative,
                         bing_positive=positive,
                         bing_sentiment=sentiment)


# afinn 
afinn_sentiment <- clean_data_tiny %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(linenumber) %>% 
  summarise(sentiment = sum(value)) 

afinn_sentiment <- afinn_sentiment %>%
  rename(afinn_sentiment=sentiment)  
  

# merge 
sentiment_data <- clean_data
sentiment_data <- sentiment_data %>% 
  mutate(linenumber = row_number())
sentiment_data <- left_join(sentiment_data,bing_sentiment, by=c('linenumber'))
sentiment_data <- left_join(sentiment_data,afinn_sentiment, by=c('linenumber'))
rm(afinn_sentiment,bing_sentiment,clean_data_tiny)

# arc number

 sentiment_data$arc_no[sentiment_data$Arc=="Kraghammer"] <- 1
 sentiment_data$arc_no[sentiment_data$Arc=="Vasselheim"] <- 2
 sentiment_data$arc_no[sentiment_data$Arc=="Briarwoods"] <- 3
 sentiment_data$arc_no[sentiment_data$Arc=="Attack_Conclave"] <- 4
 sentiment_data$arc_no[sentiment_data$Arc=="Vestiges"] <- 5
 sentiment_data$arc_no[sentiment_data$Arc=="Fall_Conclave"] <- 6
sentiment_data$arc_no[sentiment_data$Arc=="Daring_Deeds"] <- 7 
sentiment_data$arc_no[sentiment_data$Arc=="End"] <- 8 

#----- # sentiment per episode
episode_sentiment <-sentiment_data %>%
  group_by(Episode_number,Arc) %>%
  summarise_at(vars(bing_sentiment,afinn_sentiment), list(mean=mean), na.rm=TRUE)

#----- # sentiment per arc
arc_sentiment <-sentiment_data %>%
  group_by(Arc,arc_no) %>%
  summarise_at(vars(bing_sentiment,afinn_sentiment), list(mean=mean), na.rm=TRUE)

#----- sentiment per actor

## split by "and"
actor_episode_data <- sentiment_data %>% 
  mutate(Actor = strsplit(as.character(Actor), " and ")) %>% 
  unnest(Actor)

# recode NPC_Matt to actually represent actors names in NPC_Matt after "and" split
actor_episode_data$NPC_Matt <- ifelse(grepl("and",actor_episode_data$NPC_Matt),actor_episode_data$Actor,actor_episode_data$NPC_Matt)
actor_episode_data$NPC_Matt <-  ifelse( actor_episode_data$NPC_Matt=="matt" | actor_episode_data$NPC_Matt=="orion"| actor_episode_data$NPC_Matt=="laura"| actor_episode_data$NPC_Matt=="liam"| actor_episode_data$NPC_Matt=="marisha"| actor_episode_data$NPC_Matt=="taliesin"| actor_episode_data$NPC_Matt=="ashley"| actor_episode_data$NPC_Matt=="travis"| actor_episode_data$NPC_Matt=="sam"| actor_episode_data$NPC_Matt=="sam",actor_episode_data$NPC_Matt,"other")


# per episode
actor_episode_sentiment <-actor_episode_data %>%
  group_by(Episode_number,NPC_Matt) %>%
  summarise_at(vars(bing_sentiment,afinn_sentiment), list(mean=mean), na.rm=TRUE)



# per arc
actor_arc_sentiment <-actor_episode_data %>%
  group_by(Arc,NPC_Matt,arc_no) %>%
  summarise_at(vars(bing_sentiment,afinn_sentiment), list(mean=mean), na.rm=TRUE)



#-------- graphs

# number of episodes present per actor  
actor_episode_sentiment %>%
  group_by(NPC_Matt) %>% 
  filter(NPC_Matt!="other") %>% 
  tally() %>%
  ggplot(aes(x=reorder(NPC_Matt,n),y=n)) + geom_bar(stat = "identity") + coord_flip() +
  hp_theme()

# graph: sentiment per episode NORMALIZATION
episode_sentiment$Episode_number <- as.numeric(episode_sentiment$Episode_number)
ggplot(episode_sentiment, aes(Episode_number, afinn_sentiment_mean,group = 1 )) +
  geom_point(color="#009E73") +
  geom_line(color="#009E73") +
  scale_x_continuous(breaks = seq(5, 115, by = 10), limits=c(0,120),expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.0, 2.001),expand = c(0, 0))+
  labs(title = "Sentiment score per arc", subtitle="the higher the more better", caption = "Quelle: XXX") +
  geom_hline(yintercept=0, alpha=0.7) +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), plot.title.position = "plot", axis.title.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.2, color="#656565"), axis.line.x=element_line( size=.3, color="black"), legend.position = "right", legend.key = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x =element_line( size=.3, color="black"), plot.caption=element_text(size=5), axis.text.x=element_text(color="black"))

# graph: sentiment per arc NORMALIZATION

ggplot(arc_sentiment, aes(reorder(Arc, arc_no), afinn_sentiment_mean,group = 1 )) +
  geom_point(color="#009E73") +
  geom_line(color="#009E73") +
  scale_y_continuous(limits = c(-0.01, 1.001),breaks = c(seq(0,25,0.25)), expand = c(0, 0)) +
  labs(title = "Sentiment score per arc", subtitle="the higher the more better", caption = "Quelle: XXX") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), plot.title.position = "plot", axis.title.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.2, color="#656565"), axis.line.x=element_line( size=.3, color="black"), legend.position = "right", legend.key = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x =element_line( size=.3, color="black"), plot.caption=element_text(size=5), axis.text.x=element_text(color="black"))
  

# graph: sentiment actor/arc NORMALIZATION
actor_arc_sentiment_main <- filter(actor_arc_sentiment, NPC_Matt!="other" & NPC_Matt!="orion")

ggplot(actor_arc_sentiment_main, aes(reorder(Arc, arc_no), afinn_sentiment_mean, fill = NPC_Matt, group=1)) +
  geom_line() +
  facet_wrap(~NPC_Matt, ncol = 2, scales = "free_x") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), plot.title.position = "plot", axis.title.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.2, color="#656565"), axis.line.x=element_line( size=.3, color="black"), legend.position = "right", legend.key = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x =element_line( size=.3, color="black"), plot.caption=element_text(size=5), axis.text.x=element_text(color="black"))








# word clouds
docs <- Corpus(VectorSource(clean_data$Text_no_description))
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
ap_td <- tidy(dtm)

dev.new(width = 1000, height = 1000, unit = "px")
set.seed(1234)
ap_td %>%
  count(term) %>%
  with(wordcloud(term, n, max.words = 100, min.freq=5, colors=brewer.pal(8, "Dark2")))


# grade equivalent text: https://rdrr.io/cran/quanteda/man/textstat_readability.html
library(quanteda)

d <- actor_episode_data
d$rownumber = 1:nrow(d)

test<-   textstat_readability(actor_episode_data$textString_no_description,measure="Coleman.Liau.grade",
                         remove_hyphens = TRUE,
                         min_sentence_length = 5,
                         max_sentence_length = 10000)
test$rownumber = 1:nrow(test)

d <- left_join(d,test, by="rownumber")
d <- d %>% select(-c(rownumber,document))
rm(test) 



# per actor: # words ; grade
 per_actor <- d %>%
   group_by(NPC_Matt) %>%
   summarise_at(vars(Coleman.Liau.grade), list(Coleman.Liau.grade_mean=mean), na.rm=TRUE)
 
words <- d %>%
   group_by(NPC_Matt) %>%
   summarise_at(vars(wordCount), list(number_words=sum), na.rm=TRUE)
per_actor <- left_join(per_actor,words)
rm(words)

per_actor$sum_wordCount <- sum(per_actor$number_words)
per_actor <- per_actor %>% mutate(relative_words=(number_words/sum_wordCount))











 
# actual ML:

library(tidymodels)
library(textrecipes)
library(themis)
library(textfeatures)
# NOTE: renaming text variable!!!
Matt_data <- clean_data %>% select(c(Matt,Episode_number,startTime,endTime,text =Text_no_description,Arc,timeDiffInSecs,millisecsPerWord))
rm(list= ls()[!(ls() %in% c('Matt_data'))])
Matt_data$Matt <- as.factor(Matt_data$Matt)

set.seed(123)

cr_split <- initial_split(Matt_data, strata = Matt)
cr_train <- training(cr_split)
cr_test <- testing(cr_split)

set.seed(234)
cr_folds <- vfold_cv(cr_train, strata = Matt)
cr_folds



cr_recipe <- recipe(Matt ~ text, data = cr_train) %>%
  step_downsample(Matt) %>%
  step_textfeature(text) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())


cr_prep <- prep(cr_recipe)
cr_prep
juice(cr_prep)

# random forrest 
rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_spec
# svm
svm_spec <- svm_rbf(cost = 0.5) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_spec

# workflow
cr_wf <- workflow() %>%
  add_recipe(cr_recipe)

cr_wf


# parallel computing
#library(parallel)
#no_cores <- detectCores() - 2  
#doParallel::registerDoParallel((cores=no_cores)) 

# random forest estimation
set.seed(1234)
rf_rs <- cr_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = cr_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)


# svm
set.seed(2345)
svm_rs <- avatar_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = avatar_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )
collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)
















  
  library(grid)
  
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
                                         lineheight = 0.9, margin = margin(), debug = FALSE),
        plot.margin =       margin(12,10,5,10),
        # Modified inheritance structure of text element
        plot.title =        element_text(size = rel(0.75), family = '' ,
                                         face = 'bold', hjust = 0,
                                         vjust = 2.5, colour = '#3B3B3B'),
        plot.subtitle =     element_text(size = rel(0.4), family = '' ,
                                         face = 'plain', hjust = 0,
                                         vjust = 2.5, colour = '#3B3B3B', margin = margin(0,0,15,0)),
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
  
  library(gridExtra)
  