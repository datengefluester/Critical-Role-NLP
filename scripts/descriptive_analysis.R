###############################################################################
# Packages
###############################################################################

library(dplyr) # for data preparation
library(tidyr) # for data preparation
library(stringr) # for data preparation
library(tidytext) # for rearrange facet + bar charts
library(ggplot2) # for graphs
# library(ggpubr) # for graphs next to each other
library(ggraph) # for the network graph
library(igraph) # for the network graph


###############################################################################
# Plot themes
###############################################################################

source("./scripts/themes.R")


###############################################################################
# Miss spellings
###############################################################################

miss_spells <- read.csv(file = "./data/data_for_graphs/miss_spells.csv")

miss_spells %>%
  ggplot(aes(x = reorder(Actor, count), y = count, fill = miss_spelling)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
  ) +
  scale_fill_manual(
    breaks = c("amount miss spellings", "types miss spellings"),
    values = c("#1b9e77", "#ffa600"),
    labels = c("Amount Miss Spellings", "Different Variants Miss Spellings")
  ) +
  labs(
    title = "Miss Spellings per Actor",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  bar_chart_theme() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5.5),
    legend.title = element_blank(),
    legend.key.height = unit(0.4, "line"),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, unit = "cm")
  )

ggsave("./output/pictures/graphs/miss_spells.jpg", width = 4, height = 3)


###############################################################################
# Time Combat versus RP by Arc
###############################################################################

# read in data
combat_rp_arc <- read.csv(file = "./data/data_for_graphs/combat_rp_arc.csv")

# graph
combat_rp_arc %>%
  ggplot(aes(x = reorder(arc, -arc_no), y = percent, fill = rp_combat)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(seq(0, 100, 25)),
    labels = c("0" = "0", "25" = "25", "50" = "50", "75" = "75", "100" = "100%"),
    position = "right"
  ) +
  scale_fill_manual(
    breaks = c("Role Play", "Combat"),
    values = c("#1b9e77", "#ffa600")
  ) +
  labs(
    title = "Combat vs. Role Play Time per Arc",
    subtitle = "In percent of total time.",
    caption = "Source: Critical Role Subtitles"
  ) +
  bar_chart_theme() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5.5),
    legend.title = element_blank(),
    legend.key.height = unit(0.4, "line"),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, unit = "cm")
  )

ggsave("./output/pictures/graphs/combat_vs_roleplay.jpg", width = 4, height = 3)


###############################################################################
# Attendance
###############################################################################

# read in data
attendance <- read.csv(file = "./data/data_for_graphs/attendance.csv")

# create graph
attendance %>%
  ggplot(aes(x = reorder(actor, episodes), y = episodes)) +
  geom_bar(stat = "identity", fill = "#1b9e77") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    breaks = c(0, 25, 50, 50, 75, 100, 115),
    limits = c(0, 115)
  ) +
  labs(
    title = "Attendence by Actor",
    subtitle = "Number of Episodes, in which an Actor was present",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  bar_chart_theme()

# save graph
ggsave("./output/pictures/graphs/attendance.jpg", width = 4, height = 3)


###############################################################################
# Words and Time Per Actor
###############################################################################

# read in data
actor_words_time <- read.csv(file = "./data/data_for_graphs/actor_words_time.csv")

# keep only the percentages and make into long format for easier plotting
actor_words_time <- actor_words_time %>%
  select(1, 4, 7) %>%
  pivot_longer(
    cols = c("time_percent", "words_percent"),
    names_to = "variable",
    values_to = "percent"
  )

# actual graph
actor_words_time %>%
  ggplot(aes(x = reorder(as.factor(actor_guest), percent), y = percent, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    breaks = c(seq(0, 80, 20)),
    labels = c(
      "0" = "0", "20" = "20",
      "40" = "40", "60" = "60%", "80"="80%"
    ),
    limits = c(0,80)
  ) +
  scale_fill_manual(
    breaks = c("time_percent", "words_percent"),
    values = c("#1b9e77", "#ffa600"),
    labels = c("Time", "Words")
  ) +
  labs(
    title = "Words and Time per Actor",
    subtitle = "In percent of the respective total.",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  bar_chart_theme() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5.5),
    legend.title = element_blank(),
    legend.key.height = unit(0.4, "line"),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, unit = "cm")
  )

# save graph
ggsave("./output/pictures/graphs/time_words_per_actor.jpg", width = 4, height = 3)


###############################################################################
# Catch Words by Actor
###############################################################################

# read in data
top_words_actor <- read.csv(file = "./data/data_for_graphs/top_words_actor.csv")


top_words_actor %>%
  ggplot(aes(x = reorder_within(word, percent, actor), y = percent)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~actor, scales='free') +
  scale_x_reordered() +
  labs(
    title = "Top 5 Catch Words used by Cast*",
    subtitle = "Percent of total words used.",
    caption = "* as defined by log-odds,
Source: Critical-Role-Subtitles"
  ) + 
  bar_chart_theme() +
  theme(
    strip.background = element_rect(fill = "#F0F0F0"),
    panel.grid.major.x = element_line(size = rel(0.4), color = "#656565"),
    axis.line.y = element_line(size = rel(0.4), color = "black"),
    axis.text.x = element_text(color = "black", vjust = 5, size = rel(0.5), alpha(2)),
    axis.text.y = element_text(hjust = 1, color = "black", size = rel(0.75), margin = margin(r = -2)),
    strip.text = element_text(size = 5, face = "bold")
  )


ggsave("./output/pictures/graphs/top_words_per_actor.jpg", width = 4, height = 3)



###############################################################################
# Grade Equivalent
###############################################################################

# read in data
readability <- read.csv(file = "./data/data_for_graphs/readability.csv")

# create graph
readability %>%
  ggplot(aes(x = reorder(actor_guest, Coleman.Liau.grade), y = Coleman.Liau.grade)) +
  geom_bar(stat = "identity", fill = "#1b9e77") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    position = "left"
  ) +
  labs(
    title = "Grade Reading Levels",
    subtitle = "Mean Coleman and Liau (1975) Readbility Index*",
    caption = "*For Sentences longer than 5 Words.
       Source: Critical-Role-Subtitles"
  ) +
  bar_chart_theme()

# save
ggsave("./output/pictures/graphs/grades.jpg", width = 4, height = 3)

###############################################################################
# Network graph: Who speaks with whom
###############################################################################

# read in data
network <- read.csv(file = "./data/data_for_graphs/network.csv")

# prepare network: nodes
nodes <- unique(network$from)

# combine network
network <- graph_from_data_frame(d = network, vertices = nodes, directed = T)

# graph the network
ggraph(network, layout = "circle") +
  geom_edge_link(aes(
    color = weights,
    alpha = weights,
    width = 1,
    start_cap = label_rect(node1.name),
    end_cap = label_rect(node2.name)
  )) +
  geom_node_text(aes(label = name),
                 size = 2
  ) +
  scale_edge_colour_gradient(
    low = "yellow",
    high = "red"
  ) +
  labs(
    title = "Who Speaks With Whom?",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  base_theme() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = rel(0.5)),
    plot.caption = element_text(size = 3),
    legend.position = "none",
    plot.background = element_rect(fill = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0")
  )

# save graph
ggsave("./output/pictures/graphs/who_speaks_with_whom.jpg", width = 4, height = 3)


###############################################################################
# Same thoughts
###############################################################################

# read in data
same_thought_network <- read.csv(file = "./data/data_for_graphs/same_thought_network.csv")

# prepare network: nodes
nodes_thought <- unique(same_thought_network$from)

# combine network
same_thought_network <- graph_from_data_frame(d = same_thought_network, vertices = nodes_thought, directed = T)

# graph the network
ggraph(same_thought_network, layout = "circle") +
  geom_edge_link(aes(
    color = weights,
    alpha = weights,
    width = 1,
    start_cap = label_rect(node1.name),
    end_cap = label_rect(node2.name)
  )) +
  geom_node_text(aes(label = name),
                 size = 2
  ) +
  scale_edge_colour_gradient(
    low = "yellow",
    high = "red"
  ) +
  labs(
    title = "Who Has the Same Thoughts?",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  base_theme() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = rel(0.5)),
    plot.caption = element_text(size = 3),
    legend.position = "none",
    plot.background = element_rect(fill = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0")
  )


# save graph
ggsave("./output/pictures/graphs/same_thought.jpg", width = 4, height = 3)


###############################################################################
# Sentiment Analysis Arc 
###############################################################################

sentiment_arc <- read.csv(file = "./data/data_for_graphs/sentiment_by_arc.csv")

sentiment_arc %>% 
  mutate(arc = str_replace(arc,"_"," ")) %>% 
  ggplot(aes(x = arc, y = mean_sentiment)) +
  geom_point(color = "#1b9e77", size = 2) +
  geom_errorbar(aes(ymin=mean_sentiment-1.95*sd_sentiment, 
                    ymax=mean_sentiment+1.95*sd_sentiment),
                color = "#1b9e77",
                size = 0.3,
                width=.2) +
  scale_y_continuous(breaks=c(-0.3,0.3,0.6)) + 
  geom_hline(yintercept = 0, size = 0.25) +
  labs(
    title = "Mean Sentiment Level per Arc",
    subtitle = "on a per sentence basis.",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  coord_flip() +
  bar_chart_theme() 

# save graph
ggsave("./output/pictures/graphs/sentiment_arc.jpg", width = 4, height = 3)


###############################################################################
# Sentiment Analysis Actor 
###############################################################################

sentiment_actor <- read.csv(file = "./data/data_for_graphs/sentiment_by_actor.csv")

sentiment_actor %>% 
  filter(actor_guest != "Zac") %>% 
  filter(actor_guest != "Brian") %>% 
  filter(actor_guest != "Offscreen") %>% 
  ggplot(aes(x = reorder(actor_guest, mean_sentiment), y = mean_sentiment)) +
  geom_point(color = "#1b9e77", size = 2) +
  geom_errorbar(aes(ymin=mean_sentiment-1.95*sd_sentiment, 
                    ymax=mean_sentiment+1.95*sd_sentiment),
                color = "#1b9e77",
                size = 0.3,
                width=.2) +
  scale_y_continuous(breaks=c(-0.3,0.3,0.6)) + 
  geom_hline(yintercept = 0, size = 0.25) +
  labs(
    title = "Mean Sentiment Level per Actor",
    subtitle = "on a per sentence basis.",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  coord_flip() +
  bar_chart_theme() 

# save graph
ggsave("./output/pictures/graphs/sentiment_actor.jpg", width = 4, height = 3)


###############################################################################
# Sentiment Analysis Episodes 
###############################################################################

sentiment_episode <- read.csv(file = "./data/data_for_graphs/sentiment_by_episode.csv")

sentiment_episode_graph <- sentiment_episode %>% 
  top_n(mean_sentiment, n=5) %>% 
  mutate(order = 6-row_number()) 

sentiment_episode_graph <- sentiment_episode %>% 
  top_n(-mean_sentiment, n=5) %>% 
  mutate(order = 6-row_number()) %>% 
  rbind(sentiment_episode_graph) %>% 
  mutate(type = "Best Episodes") %>% 
  mutate(type = replace(type,mean_sentiment<0.05,"Worst Episodes")) 

sentiment_episode_graph %>% 
  ggplot(aes(x = reorder(episode, order), y = mean_sentiment)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  coord_flip() +
  facet_wrap(~type, scales='free_y') +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "The 5 Best and Worst Episode by Sentiment",
    subtitle = "on a per sentence basis.",
    caption = "Source: Critical-Role-Subtitles"
  ) + 
  bar_chart_theme() +
  theme(
    strip.background = element_rect(fill = "#F0F0F0"),
    strip.text = element_text(size = rel(0.5)),
    axis.text.y = element_text(hjust = 1, color = "black", size = rel(0.75)),
    axis.text.x = element_text(color = "black", vjust =3, size = rel(0.75)),
    panel.grid.major.x = element_line(size = rel(0.4), color = "#656565"),
    panel.spacing = unit(2, "lines")
  )

# save graph
ggsave("./output/pictures/graphs/top_worst_5_episodes.jpg", width = 4, height = 3)

###############################################################################
# Sentiment Analysis Episodes and Correlates: Dices and Face palms
###############################################################################

sentiment_episode <- read.csv(file = "./data/data_for_graphs/sentiment_by_episode.csv")

# correlation tests (to see if correlation is significant):
estimates = numeric(1)
pvalues = numeric(1)
names = character(1)

# calculate a correlation for each dice / face palm / Ashley variable: 
# with episode sentiment score and paste them into a new data frame
for (i in 4:9){
  j <- i-3
  tmp_df <- cor.test(sentiment_episode[,2], sentiment_episode[,i])
  names[j] <- names(sentiment_episode[i])
  estimates[j] = tmp_df$estimate
  pvalues[j] = tmp_df$p.value
}

correlation_test <-data.frame(names,estimates,pvalues) %>% 
  mutate(pvalues = round(pvalues, digits = 3))
rm(names,estimates,pvalues,tmp_df)


# change into wide format:
correlates_sentiment_episode <- sentiment_episode %>% 
  pivot_longer(pc_1:Ashley, names_to = "measure", values_to = "value") %>% 
  filter(measure != "Ashley") %>% 
  mutate(measure = str_replace(measure,"pc_1", "PC natural 1s"),
         measure = str_replace(measure,"pc_20", "PC natural 20s"),
         measure = str_replace(measure,"dm_1", "DM natural 1s"),
         measure = str_replace(measure,"dm_20", "DM natural 20s"),
         measure = str_replace(measure,"face_palms","Matt Face Palms"))                     
        
# graph
correlates_sentiment_episode %>% 
  ggplot(aes(x = value, y = mean_sentiment)) +
  geom_point(color = "#009E73", alpha = 0.5, size = 0.5) +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 0.5) +
  facet_wrap(~measure, scales='free') + 
  stat_regline_equation(label.y = 0.095, label.x = 4, size=1.5, aes(label = ..rr.label..)) + 
  labs(
    title = "The 5 Best and Worst Episode by Sentiment",
    subtitle = "on a per sentence basis.",
    caption = "Source: Critical-Role-Subtitles"
  ) +
  bar_chart_theme() +
  theme(
    strip.background = element_rect(fill = "#F0F0F0"),
    strip.text = element_text(size = rel(0.5)),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(size=.025, color="#656565"),  
    panel.grid.major.x = element_line(size=.025, color="#656565"),
    axis.text.y = element_text(hjust = 0, color = "black", size = rel(0.75)),
    axis.text.x = element_text(color = "black", vjust =3, size = rel(0.75)),
    panel.spacing = unit(1, "lines")
  )
# save graph
ggsave("./output/pictures/graphs/correlates_sentiment_episode.jpg", width = 4, height = 3)

###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))