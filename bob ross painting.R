library(tidyverse)
library(ggplot2)
theme_set(theme_light())

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

View(bob_ross)

#using the janitor package to clean the names 

boss_ross_gathered<- bob_ross %>% 
  janitor::clean_names() %>% 
  gather(element, present, -episode, -title) %>% 
  #the gather is used to make wide data long 
  filter(present == 1) %>% 
  mutate(title = str_to_title(str_remove_all(title, '"')), 
    element = str_to_title(str_replace(element, "_", " "))) %>% 
  select(-present) %>% #removing the present column
  #removing the episode number and season number from episode
  extract(episode, c("season", "episode_number"), 'S(.*)E(.*)', convert = TRUE, remove = FALSE) %>% 
  #this are called capturing groups S(.*)E(.*) which is saying anything tha comes after the S and e should be extracted 
  #convert true converst it to int and remove fals keeps the original column   
  arrange(season, episode_number)

#exploring the paintings 

#what are the most common elements 
boss_ross_gathered %>% 
  count(element, sort = TRUE) %>% 
  head(25) %>% 
  mutate(element = fct_reorder(element, n)) %>% 
  ggplot(aes(element, n))+
  geom_col()+
  coord_flip()

view(boss_ross_gathered)

#what are the most crowded painings with the most element in them 
boss_ross_gathered %>% 
  add_count(episode) %>% 
  arrange(desc(n)) %>% 
  view()

#how has ross painting been changinf over time 
boss_ross_gathered %>% 
  group_by(season) %>% 
  add_count(season, name = "total_element") %>% 
  view()
  summarise(episodes = n_distinct(episode))


boss_ross_gathered %>% 
  filter(!element %in% c("Tree", "Trees")) %>% 
  add_count(season, name = "total_element") %>% #couting the number of seasons will give you the number of element because all element has been groupeed to season 
  count(season, element, total_element, sort = TRUE) %>% 
  view() %>% # inn the view what it is saying is e.g season 2 had 114 element 13 where tress
  mutate(percentage_element = n/total_element)


by_season_element <- boss_ross_gathered %>% 
  filter(!element %in% c("Tree", "Trees")) %>% 
  group_by(season) %>% 
  mutate(number_episode = n_distinct(episode)) %>% 
  count(season, element, number_episode, sort = TRUE) %>%  
  mutate(percentage_included = n/number_episode)

by_season_element %>% 
  filter(element == "Mountain") %>% 
  ggplot(aes(season, percentage_included))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  expand_limits(y = 0)
  

by_season_element <- boss_ross_gathered %>% 
  filter(!element %in% c("Tree", "Trees")) %>% 
  group_by(season) %>% 
  mutate(number_episode = n_distinct(episode)) %>% 
  count(season, element, number_episode, sort = TRUE) %>%  
  mutate(percentage_included = n/number_episode) %>% 
  group_by(element) %>% 
  mutate(element_total = sum(n)) %>% 
  ungroup() 
  #view() # the word deciduous apeared 10 times in season 3 and 227 times acroos all seasons 

by_season_element %>% 
  filter(element_total >= 50) %>% 
  ggplot(aes(season, percentage_included, color = element))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  expand_limits(y = 0)+
  facet_wrap(~element)


#clustering 
#what tends to appear together 

library(widyr) #this is used to widen process and tidy a dataset
correlations <- boss_ross_gathered %>% 
  add_count(element) %>% 
  filter(n>=5) %>% 
  pairwise_cor(element, episode, sort = TRUE) #coorelation between element and episode

#what do waves appea with waves

correlations %>% 
  filter(item1== "Waves")

#what do waves appea with river

correlations %>% 
  filter(item1== "River") %>% 
  mutate(item2 = fct_reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation))+
  geom_col()+
  coord_flip()+
  labs(title = "what tends to appear with river",
       subtitle = "Amongs item that appears in at lest 10 paintings")


#looking into steve ross
correlations %>% 
  filter(item1== "Steve Ross") %>% 
  mutate(item2 = fct_reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation))+
  geom_col()+
  coord_flip()+
  labs(title = "what tends to appear with river",
       subtitle = "Amongs item that appears in at lest 10 paintings")


#looking into structure
correlations %>% 
  filter(item1== "Structure") %>% 
  mutate(item2 = fct_reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation))+
  geom_col()+
  coord_flip()+
  labs(title = "what tends to appear with river",
       subtitle = "Amongs item that appears in at lest 10 paintings")


#looking into snow
correlations %>% 
  filter(item1== "Snow") %>% 
  mutate(item2 = fct_reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation))+
  geom_col()+
  coord_flip()+
  labs(title = "what tends to appear with river",
       subtitle = "Amongs item that appears in at lest 10 paintings")


#clusters of things using network 
# we will be using ggraph and i graph
install.packages("ggraph")
install.packages("igraph")
library(ggraph) #this is adpted for network plots 
library(igraph)

set.seed(2019)
correlations %>% 
  head(100) %>% 
  graph_from_data_frame() %>% 
  ggraph()+
  geom_edge_link(aes(alpha = correlation))+
  geom_node_point()+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()

#principal components analysis 
#this tells what trends predicts ore about the analysis 

#what dimension drive allot of variaions amongs painting

library(reshape2)
library(broom)

#creating a binary matrix which is similar to orignal data 
binary_matrix <-  boss_ross_gathered %>% 
  acast(title ~ element)  #turning it into a inary matrix
  
#center the columns
centered_matrix <-t(t(binary_matrix)- colMeans(binary_matrix))

#singular value decomposition SVD

svd_result <- svd(centered_matrix)

element_wieght<- tidy(svd_result, matrix = "v") %>% 
  mutate(element = colnames(binary_matrix)[column])# what this is saying is how much is the column contributing to the PCA

#this is the first principle components
element_wieght %>% 
  filter(PC == 1) %>% 
  top_n(20, abs(value)) %>% 
  mutate(element = fct_reorder(element, value)) %>% 
  ggplot(aes(element, value))+
  geom_col()+
  coord_flip()

#this is the second principle component
element_wieght %>% 
  filter(PC == 2) %>% 
  top_n(20, abs(value)) %>% 
  mutate(element = fct_reorder(element, value)) %>% 
  ggplot(aes(element, value))+
  geom_col()+
  coord_flip()

#getting all 4 PC

element_wieght %>% 
  filter(PC <= 4) %>% 
  group_by(PC) %>% 
  top_n(10, abs(value)) %>% 
  mutate(element = reorder_within(element, value, PC)) %>% 
  ggplot(aes(element, value))+
  geom_col()+
  coord_flip()
