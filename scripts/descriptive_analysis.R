
###############################################################################
# TO DO:
# Corr(Sentiment, #Nat20)
# Corr(Sentiment, duration combat)
# Check whether an actor was there adds up with other sources; then attendance graph
###############################################################################




###############################################################################
# Packages
###############################################################################
# for data preparation 
library(dplyr)
library(tidyr)
# for graphs
library(ggplot2)
# for graphs next to each other
library(ggpubr)
# for the network graph
library(ggraph)
library(igraph)

###############################################################################
# Plot themes
###############################################################################

source("./scripts/themes.R")


###############################################################################
# Time Combat versus RP by Arc
###############################################################################

# read in data 
combat_rp_arc <- read.csv(file = './data/data_for_graphs/combat_rp_arc.csv')


# graph
combat_rp_arc %>% 
  ggplot(aes(x = reorder(arc,-arc_no), y = percent, fill = rp_combat)) + 
  geom_bar(position="stack", stat = "identity") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(seq(0,100,25)),
                     labels = c("0"="0","25"="25","50"="50","75"="75","100"="100%"),
                     position = "right") +
  scale_fill_manual(breaks=c("Role Play","Combat"), 
                    values=c( "#1b9e77","#ffa600"))  +
  labs(title = "Combat vs. Role Play Time per Arc",
       subtitle = "In percent of total time.",
       caption = "Source: Critical Role Subtitles") +
  bar_chart_theme() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 5.5),
        legend.title = element_blank(), 
        legend.key.height= unit(0.4,"line"),
        legend.key = element_blank(), 
        legend.margin=margin(t = 0, unit='cm'))
  
ggsave("./output/pictures/graphs/combat_vs_roleplay.jpg",width=4, height=3)


###############################################################################
# Attendance 
###############################################################################

# read in data 
attendance <- read.csv(file = './data/data_for_graphs/attendance.csv')

# create graph
attendance %>% 
  ggplot(aes(x=reorder(actor_guest, episodes),y = episodes)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right",
                     breaks=c(0,25,50,50,75,100,115),
                     limits=c(0,115)) +
  labs(title = "Attendence by Actor",
       subtitle = "Percentage of Episodes, in which an Actor was present",
       caption = "Source: Critical-Role-Subtitles") +
  bar_chart_theme()

# save graph
ggsave("./output/pictures/graphs/attendance.jpg",width=4, height=3)


###############################################################################
# Words and Time Per Actor
###############################################################################

# read in data 
actor_words_time <- read.csv(file = './data/data_for_graphs/actor_words_time.csv')

# keep only the percentages and make into long format for easier plotting
actor_words_time <- actor_words_time %>% 
  select(1,4,7) %>% 
  pivot_longer(cols=c('time_percent', 'words_percent'), 
               names_to='variable', 
               values_to="percent") 

# actual graph
actor_words_time %>% 
  ggplot(aes(x=reorder(as.factor(actor_guest), percent), y = percent, fill = variable)) +
    geom_bar(stat = "identity", position='dodge') + 
    coord_flip() +
    scale_y_continuous(expand = c(0, 0),
                       position = "right",
                       breaks=c(seq(0,50,10)),
                       labels=c("0"="0","10"="10",
                                "20"="20","30"="30","40"="40","50"="50%")) +
    scale_fill_manual(breaks=c("time_percent", "words_percent"), 
                    values=c("#1b9e77", "#ffa600"),
                    labels = c("Time", "Words"))  +
    labs(title = "Words and Time per Actor",
       subtitle = "In percent of the respective total.",
       caption = "Source: Critical-Role-Subtitles") +
  bar_chart_theme() + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 5.5),
        legend.title = element_blank(), 
        legend.key.height= unit(0.4,"line"),
        legend.key = element_blank(), 
        legend.margin=margin(t = 0, unit='cm'))

# save graph
ggsave("./output/pictures/graphs/time_words_per_actor.jpg",width=4, height=3)


###############################################################################
# Top Words 5 per Actor
###############################################################################

# read in data 
top_words_actor <- read.csv(file = './data/data_for_graphs/top_words_actor.csv')


# function to create all the individual graphs: 
# (note: it's not that easy to make facet work with words)
# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

top_words <- function(Speaker) {
  top_words_actor %>% filter(actor_guest == Speaker) %>%  
    ggplot(aes(x = as.factor(reorder(word, percent)), y = percent)) + 
    geom_bar(stat = "identity", fill = "#009E73") +
    coord_flip()+
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = Speaker) +
  bar_chart_theme() + 
  theme( panel.grid.major.x = element_line(size=rel(0.4), color="#656565"), 
         axis.line.y = element_line(size=rel(0.4), color="black"),
         axis.text.x = element_text(color="black", vjust = 5, size=rel(0.5)),
         axis.text.y = element_text(hjust = 1, color="black", size=rel(0.5), margin = margin(r = -2)),
         plot.title = element_text(size = rel(0.35), hjust=0.5),
         strip.background =element_rect(fill="#F0F0F0"),
         plot.subtitle = element_blank(),
         plot.margin = unit(c(t = 0.6, r = 0.5, l = 0, b = 0.5), "lines"))
}

# create invidual graphs
Ashley    <- top_words("Ashley")
Laura     <- top_words("Laura")
Marisha   <- top_words("Marisha")
Travis    <- top_words("Travis")
Liam      <- top_words("Liam")
Sam       <- top_words("Sam")
Taliesin  <- top_words("Taliesin")
Matt      <- top_words("Matt")
Guests     <- top_words("Guests")
Orion     <- top_words("Orion")

# combine graphs:
top_words_graph <- ggarrange(Ashley, Laura, Marisha, Liam, Sam, 
                          Taliesin, Travis, Orion, Matt, Guests,
                          ncol = 3, 
                          nrow = 4,
                          common.legend = FALSE)

# add title and caption
top_words_graph <- annotate_figure(top_words_graph,
                                top = text_grob("Top 5 Words used by Cast*", 
                                                color = "#3B3B3B", face = "bold", 
                                                hjust = 0, x = 0.03, y=0.02,
                                                size = 9),
                                bottom = text_grob(" *Percent of total words used.
                                                   Source: Critical-Role-Subtitles", 
                                                   color = "#3B3B3B",
                                                   hjust = 1, x = 0.98, 
                                                   size = 2.5))

# add background color
top_words_graph <- top_words_graph  +  bgcolor("#F0F0F0")

# render graph and save 
top_words_graph
ggsave("./output/pictures/graphs/top_words_per_actor.jpg", width = 4, height = 3)

# clean up
rm(Ashley, Guests, Laura, Liam, Marisha, Matt, Orion, Sam, Travis, Taliesin)



###############################################################################
# Grade Equivalent 
###############################################################################

# read in data 
readability <- read.csv(file = './data/data_for_graphs/readability.csv')

# create graph
readability %>% 
  ggplot(aes(x = reorder(actor_guest ,Coleman.Liau.grade), y = Coleman.Liau.grade)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Grade Reading Levels",
       subtitle = "Mean Coleman and Liau (1975) Readbility Index*",
       caption = "*For Sentences longer than 5 Words.
       Source: Critical-Role-Subtitles") +
 bar_chart_theme()

# save
ggsave("./output/pictures/graphs/grades.jpg",width=4, height=3)

###############################################################################
# Network graph: Who speaks with whom 
###############################################################################

# read in data 
network <- read.csv(file = './data/data_for_graphs/network.csv')

# prepare network: nodes
nodes <- unique(network$from)

# combine network
network <- graph_from_data_frame(d=network, vertices=nodes, directed=T) 

# graph the network
ggraph(network, layout = 'circle') + 
  geom_edge_link(aes(color = weights, 
                     alpha = weights, 
                     width = weights, 
                     start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)
                     )) +
  geom_node_text(aes(label = name), 
                 size=2) +
  scale_edge_colour_gradient(low = "yellow",
                             high = "red") +
  labs(title = "Who Speaks With Whom?",
       caption = "Source: Critical-Role-Subtitles") +
  base_theme() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major=element_blank(),
        plot.title = element_text(size = rel(0.5)),
        plot.caption=element_text(size = 3),
        legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        panel.background = element_rect(fill = "#F0F0F0"))

# save graph
ggsave("./output/pictures/graphs/who_speaks_with_whom.jpg",width=4, height=3)





###############################################################################
# Heat Map
###############################################################################

# read in data 
heatmap <- read.csv(file = './data/data_for_graphs/heatmap.csv')

# heatmap
g1 <- ggplot(heatmap, aes(y = actor2, x=reorder(actor1,order) )) +
  geom_tile(aes(fill = occurrence)) +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand= c(0,0))


# add theme  
g1 + 
  scale_fill_continuous(low = "#31a354", high = "#1f6836") +
  labs(title = "Who has the same Thoughts?",
       subtitle = "Number of times cast says the same thing.",
       caption = "Source: Critical Role Subtitles") +
  labs(fill = "Occurrence") + 
  bar_chart_theme()+
  theme(       axis.line.y = element_blank(),
               axis.text.x.top = element_text(vjust = -1),
               panel.grid.major.x = element_blank(),
               legend.position = c(0.75, 0.25), 
               legend.text = element_text(size=5),
               legend.title = element_text(size=6), 
               legend.key.height= unit(0.8,"line"),
               legend.key = element_blank(), 
               legend.margin=margin(t = 0, unit = 'cm'))

ggsave("./output/pictures/graphs/heatmap.jpg",width=4, height=3)



###############################################################################
# Sentiment Analysis Episodes
###############################################################################

sentiment_episodes <- read.csv(file = './data/data_for_graphs/sentiment_episodes.csv')



# top 5 bing
top5_bing <- sentiment_episodes %>%
  ungroup() %>% 
  top_n(5, bing_sentiment_mean) %>% 
  ggplot(aes(x = reorder(episode, bing_sentiment_mean), y = bing_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Bing et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# worst 5 bing 
worst5_bing <- sentiment_episodes %>%
  ungroup() %>% 
  top_n(5, -bing_sentiment_mean) %>% 
  ggplot(aes(x = reorder(episode, -bing_sentiment_mean), y = bing_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Bing et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# top 5 afinn 
top5_afinn <- sentiment_episodes %>%
  ungroup() %>% 
  top_n(5, afinn_sentiment_mean) %>% 
  ggplot(aes(x = reorder(episode, afinn_sentiment_mean), y = afinn_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Afinn et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# worst 5 afinn 
worst5_afinn <- sentiment_episodes %>%
  ungroup() %>% 
  top_n(5, -afinn_sentiment_mean) %>% 
  ggplot(aes(x = reorder(episode, -afinn_sentiment_mean), y = afinn_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Afinn et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# combine graphs: top
sentiment_episodes_graph_top <- ggarrange(top5_bing, top5_afinn,
                                 ncol = 2, 
                                 nrow = 1,
                                 common.legend = FALSE) 
  
  
# add title and caption
sentiment_episodes_graph_top <- annotate_figure(sentiment_episodes_graph_top,
                                         top = text_grob("Best Episodes According to:", 
                                                         color = "#3B3B3B", face = "bold", 
                                                         hjust = 0, x = 0.03, y=0.02,
                                                         size = 9))

# add background color
sentiment_episodes_graph_top <- sentiment_episodes_graph_top  +  bgcolor("#F0F0F0")



# combine graphs: bottom
sentiment_episodes_graph_bottom <- ggarrange(worst5_bing, worst5_afinn,
                                          ncol = 2, 
                                          nrow = 1,
                                          common.legend = FALSE) 


# add title and caption
sentiment_episodes_graph_bottom <- annotate_figure(sentiment_episodes_graph_bottom,
                                                top = text_grob("Worst Episodes According to:", 
                                                                color = "#3B3B3B", face = "bold", 
                                                                hjust = 0, x = 0.03, y=0.02,
                                                                size = 9))

# add background color
sentiment_episodes_graph_bottom <- sentiment_episodes_graph_bottom  +  bgcolor("#F0F0F0")




# render graph  
sentiment_episodes_graph <- ggarrange(sentiment_episodes_graph_top, 
                                      sentiment_episodes_graph_bottom,
                                             ncol = 1, 
                                             nrow = 2,
                                             common.legend = FALSE) 

# add title and caption
sentiment_episodes_graph <- annotate_figure(sentiment_episodes_graph,
                                       top = text_grob("Sentiment Score Per Episode", 
                                                       color = "#3B3B3B", face = "bold", 
                                                       hjust = 0, x = 0.03, y=0.02,
                                                       size = 9),
                                       bottom = text_grob("Source: Critical-Role-Subtitles", 
                                                          color = "#3B3B3B",
                                                          hjust = 1, x = 0.98, 
                                                          size = 4))

# add background color
sentiment_episodes_graph <- sentiment_episodes_graph  +  bgcolor("#F0F0F0")

# render graph  
sentiment_episodes_graph

# save graph
ggsave("./output/pictures/graphs/sentiment_episodes.jpg",width=4, height=3)


###############################################################################
# Sentiment per Arc
###############################################################################


# read in data
sentiment_arc <- read.csv(file = './data/data_for_graphs/sentiment_arc.csv')


# Bing graph
bing_arc <- sentiment_arc %>% 
  ggplot(aes(x = reorder(arc , -arc_no), y = bing_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Bing et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

bing_arc

# Afinn graph
afinn_arc <- sentiment_arc %>% 
  ggplot(aes(x = reorder(arc , -arc_no), y = afinn_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Afinn et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# combine graphs:
sentiment_arc_graph <- ggarrange(bing_arc, afinn_arc,
                             ncol = 2, 
                             nrow = 1,
                             common.legend = FALSE) 
  

# add title and caption
sentiment_arc_graph <- annotate_figure(sentiment_arc_graph,
                                   top = text_grob("Sentiment Score Per Arc", 
                                                   color = "#3B3B3B", face = "bold", 
                                                   hjust = 0, x = 0.03, y=0.02,
                                                   size = 9),
                                   bottom = text_grob(" *A lower score corespond to more negativity.
                                                   Source: Critical-Role-Subtitles", 
                                                      color = "#3B3B3B",
                                                      hjust = 1, x = 0.98, 
                                                      size = 4))

# add background color
sentiment_arc_graph <- sentiment_arc_graph  +  bgcolor("#F0F0F0")

# render graph  
sentiment_arc_graph

# save graph
ggsave("./output/pictures/graphs/sentiment_arc.jpg",width=4, height=3)



###############################################################################
# Sentiment per Actor
###############################################################################

# read in data
sentiment_actor <- read.csv(file = './data/data_for_graphs/sentiment_actor.csv')

# Bing graph
bing_actor <- sentiment_actor %>% 
  ggplot(aes(x = reorder(actor_guest , bing_sentiment_mean), y = bing_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Bing et. al.") +
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))

# Afinn graph
afinn_actor <- sentiment_actor %>% 
  ggplot(aes(x = reorder(actor_guest , afinn_sentiment_mean), y = afinn_sentiment_mean)) + 
  geom_bar(stat = "identity", fill ="#1b9e77") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     position = "right") +
  labs(title = "Afinn et. al.") + 
  bar_chart_theme() + 
  theme(plot.title = element_text(size = rel(0.5), hjust=0.5))



# combine graphs:
sentiment_actor_graph <- ggarrange(bing_actor, afinn_actor,
                             ncol = 2, 
                             nrow = 1,
                             common.legend = FALSE)

# add title and caption
sentiment_actor_graph <- annotate_figure(sentiment_actor_graph,
                                   top = text_grob("Sentiment Score Per Actor", 
                                                   color = "#3B3B3B", face = "bold", 
                                                   hjust = 0, x = 0.03, y=0.02,
                                                   size = 9),
                                   bottom = text_grob(" *A lower score corespond to more negativity.
                                                   Source: Critical-Role-Subtitles", 
                                                      color = "#3B3B3B",
                                                      hjust = 1, x = 0.98, 
                                                      size = 4))

# add background color
sentiment_actor_graph <- sentiment_actor_graph  +  bgcolor("#F0F0F0")

# render graph  
sentiment_actor_graph

# save graph
ggsave("./output/pictures/graphs/sentiment_actor.jpg",width=4, height=3)


###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE)) 