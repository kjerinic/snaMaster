# ucitavanje potrebnih prethodno instaliranih bibilioteka

library(rtweet)
library(httpuv)
library(tidyr)
library(visNetwork)
library(igraph)
library(ggplot2)
library(dplyr)

# postavljanje Twitter kredencijala koji se cuvaju u lokalnoj promenljivoj env
# radi prikupljanja tvitova koristeci Twitter API
access_token <- env.access_token
access_token_secret <- env.access_token_secret
api_key <- env.api_key
api_secret_key <- env.api_secret_key
token <- create_token(
  app = env.app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
get_token()

saveRDS(token, "rtweet.rds")

# prikpuljanje tvitova sa hashtag-om utisak i utisakNedelje
# koji su tvitovani 20. septembra  do 29. septembra 2020.
# iskljucujuci retvitovane tvitove
hashtagsUtisak <- c("#utisak", "#utisaknedelje")
qHashtagsUtisakOr <- paste(hashtagsUtisak, collapse = " OR ")

utisak_tweets <- search_tweets(q = qHashtagsUtisakOr,
                                  since="2020-09-20",
                                  include_rts = FALSE)

utisak_tweets <- utisak_tweets[order(utisak_tweets$created_at), ]
# tvitovi vezani za emisiju od 20.09.
utisak_20_09_tweets <- utisak_tweets[1:347, ]
# tvitovi vezani za emisiju od 27.09.
utisak_27_09_tweets <- utisak_tweets[348:nrow(utisak_tweets), ]

saveRDS(utisak_tweets, 'data/utisak_tweets.RData')
saveRDS(utisak_20_09_tweets, 'data/utisak_20_09_tweets.RData')
saveRDS(utisak_27_09_tweets, 'data/utisak_27_09_tweets.RData')

utisak_tweets <- readRDS("data/utisak_tweets.RData")

# pocetak

# mreza UN za 20.09.2020.
# rad sa grafovima
# biramo samo relevantne kolone
utisak_20_09_tweets_users <- utisak_20_09_tweets %>%
  select(screen_name, reply_to_screen_name, mentions_screen_name) %>%
  rename(sender=screen_name, replied_to=reply_to_screen_name, mentioned=mentions_screen_name)
# Neke vrednosti 'replied_to' i 'mentioned' ne postoje, proveravamo koliko ih je takvih:
length(which(is.na(utisak_20_09_tweets_users$replied_to)==TRUE))
length(which(is.na(utisak_20_09_tweets_users$mentioned)))
# Uzimamo samo tvitove u kojima je neko odgovorio drugom korisniku
# ili ga spomenuo
utisak_20_09_tweets_users <- utisak_20_09_tweets_users %>%
  filter( is.na(replied_to)==FALSE | is.na(mentioned)==FALSE )
nrow(utisak_20_09_tweets_users)
# radimo operaciju unnest za kolonu 'mentioned'
# Svaki element liste ide u nov red operacijom unnest
utisak_20_09_tweets_users <- unnest(utisak_20_09_tweets_users, mentioned)
# Pretvaramo utisak_20_09_tweets_users u dataframe zbog daljeg rada sa grafovima
# Pravimo 2 edge liste: jednu za relaciju 'reply_to' drugu za relaciju 'mentioned'
replied_to_edgelist_utisak_20_09 <- utisak_20_09_tweets_users %>%
  select(-mentioned) %>%              # bez kolone 'mentioned'
  filter(complete.cases(.)) %>%       # iskljucimo redove koji nemaju vrednost, tj. imaju vrednost NA
  group_by(sender, replied_to) %>%    # redovi se spajaju po vrednostima sender i replied_to
  summarise(weight = n()) %>%         # kolika je velicina svake grupe (veza izmedju kolona sender I replied_to)
  ungroup()
head(replied_to_edgelist_utisak_20_09, n=10)
# izmedju kojih parova cvorova je komunikacija najveca (sortiramo po velicini grupe tj. weight):
replied_to_edgelist_utisak_20_09 %>%
  arrange(desc(weight)) %>%
  head(n=10)
# isto radimo za kolonu 'mentioned'
mentioned_edgelist_utisak_20_09 <- utisak_20_09_tweets_users %>%
  select(-replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, mentioned) %>%
  summarise(weight = n()) %>%
  ungroup()
head(mentioned_edgelist_utisak_20_09, n=10)
# gde se najcesce vidi ova veza (sortiranje po vrednosti weight):
mentioned_edgelist_utisak_20_09 %>%
  arrange(desc(weight)) %>%
  head(n=10)
# Cvorovi u mrezama su jedinstvene vrednosti za korisnike
# jedinstvene vrednosti u replied_to edgelist
reply_to_unique_utisak_20_09 <- union(replied_to_edgelist_utisak_20_09$sender, replied_to_edgelist_utisak_20_09$replied_to)
reply_to_unique_utisak_20_09 <- with(replied_to_edgelist_utisak_20_09, union(sender, replied_to))
length(reply_to_unique_utisak_20_09)
# jedinstvene vrednosti u mentioned edgelist
mention_unique_utisak_20_09 <- with(mentioned_edgelist_utisak_20_09, union(sender, mentioned))
length(mention_unique_utisak_20_09)
summary(replied_to_edgelist_utisak_20_09$weight)
summary(mentioned_edgelist_utisak_20_09$weight)
# Dodajemo jos podataka o korisnicima (senders).
# Neke smo vec dobili (utisak_20_09_tweets)
all_senders_utisak_20_09 <- union(mentioned_edgelist_utisak_20_09$sender,
                            replied_to_edgelist_utisak_20_09$sender)
senders_data_utisak_20_09 <- utisak_20_09_tweets %>%
  filter(screen_name %in% all_senders_utisak_20_09) %>%
  users_data() %>%                          # funkcija paketa rtweet za uzimanje podataka iz dataset-a
  distinct(user_id, .keep_all = TRUE)
# Skupljamo podatke za “alters”, tj. korisnike koji su mentioned / replied_to
# za koje alters nemamo podatke
no_data_alters_utisak_20_09 <- setdiff(union(mention_unique_utisak_20_09, reply_to_unique_utisak_20_09), all_senders_utisak_20_09)
length(no_data_alters_utisak_20_09)
# koristimo funkciju paketa rtweet lookup_users da prikupimo podatke o njima
alters_data_utisak_20_09 <- lookup_users(no_data_alters_utisak_20_09)
# cuvamo ove podatke
saveRDS(alters_data_utisak_20_09, "data/utisak_alters_data_utisak_20_09.RData")
glimpse(alters_data_utisak_20_09)
# uzimamo samo podatke o korisnicima, bez njihovih tvitova
# prethodna funkcija je vratila I tvitova korisnika
alters_data_utisak_20_09 <- users_data(alters_data_utisak_20_09)
# provera da li nekome fale podaci
missing_alter_utisak <- setdiff(no_data_alters_utisak_20_09, alters_data_utisak_20_09$screen_name)
length(missing_alter_utisak)
mentioned_edgelist_utisak_20_09 %>%
  filter(!mentioned %in% missing_alter_utisak) %>%
  rename(ego=sender, alter=mentioned, mention_tie=weight) %>%
  saveRDS(file = "data/mentions_edgelist_utisak_20_09.RData")
replied_to_edgelist_utisak_20_09 %>%
  filter(!replied_to %in% missing_alter_utisak) %>%
  rename(ego=sender, alter=replied_to, reply_to_tie=weight) %>%
  saveRDS(file = "data/replied_to_edgelist_utisak_20_09.RData")
saveRDS(senders_data_utisak_20_09, file = "data/ego_data_utisak_20_09.RData")
saveRDS(alters_data_utisak_20_09, file = "data/alter_data_utisak_20_09.RData")
##
# MREZE KORISNIKA UN za 20.09.2020. na osnovu relacije mentioned
##
# edge list za pravljenje mreze na osnovu relacije 'mentioned'
mention_edgelist_utisak_20_09 <- readRDS("data/mentions_edgelist_utisak_20_09.RData")
summary(mention_edgelist_utisak_20_09$mention_tie)
# pravimo usmerenu mrezu (directed net)
mention_net_utisak_20_09 <- graph_from_data_frame(mention_edgelist_utisak_20_09)
summary(mention_net_utisak_20_09)
# proveravamo gustinu mreze
edge_density(mention_net_utisak_20_09)
plot(mention_net_utisak_20_09,
     layout = layout_with_lgl(mention_net_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# dodajemo neke nove vrednosti cvorovima
# pravimo funkciju get_user_attrs() koja prima:
# 1) podatke o korisnicima dobijene funkcijom paketa rtweet users_data(),
# 2) vector korisnickih imena onih korisnika koji su nam relevantni
# funkcija vraca data frame od 4 promenljive (kolone):
# screen_name, followers_count, friends_count, and statuses_count
get_user_attrs <- function(twitter_user_data, users_screen_names) {
  twitter_user_data %>%
    filter(screen_name %in% users_screen_names) %>%
    select(screen_name, followers_count, friends_count, statuses_count)
}
# ucitamo podatke o senders: potrebno za dodavanje atributa cvorovima koji su izvori
senders_data_utisak_20_09 <- readRDS("data/ego_data_utisak_20_09.RData")
ego_attrs_mentioned_utisak_20_09 <- get_user_attrs(senders_data_utisak_20_09, V(mention_net_utisak_20_09)$name)
glimpse(ego_attrs_mentioned_utisak_20_09)
# isto I za alters: one korisnike koji su spomenuti u tvitu onih koji su senders
alters_data_utisak_20_09 <- readRDS("data/alter_data_utisak_20_09.RData")
alter_attrs_mentioned_utisak_20_09 <- get_user_attrs(alters_data_utisak_20_09, V(mention_net_utisak_20_09)$name)
# dodeljujemo attribute onima koji su u mrezi 'mentioned'
node_attrs_mentioned_utisak_20_09 <- rbind(ego_attrs_mentioned_utisak_20_09, alter_attrs_mentioned_utisak_20_09) %>% # spoj dataframe-ova
  distinct(screen_name, .keep_all = TRUE) %>% # uzimamo samo redove koji nemaju duplicate tj. jedinstveni su
  arrange(screen_name) # sortiranje prema korisnickom imenu
head(node_attrs_mentioned_utisak_20_09, n=10)
summary(node_attrs_mentioned_utisak_20_09[,-1])
# attribute koji su vektori postavimo najpre na 0
V(mention_net_utisak_20_09)$followers_cnt <- rep(0, vcount(mention_net_utisak_20_09))
V(mention_net_utisak_20_09)$friends_cnt <- rep(0, vcount(mention_net_utisak_20_09))
V(mention_net_utisak_20_09)$posts_cnt <- rep(0, vcount(mention_net_utisak_20_09))
for (i in 1:vcount(mention_net_utisak_20_09)) {
  index <- which(node_attrs_mentioned_utisak_20_09$screen_name == V(mention_net_utisak_20_09)$name[i])
  V(mention_net_utisak_20_09)$followers_cnt[i] <- node_attrs_mentioned_utisak_20_09$followers_count[index]
  V(mention_net_utisak_20_09)$friends_cnt[i] <- node_attrs_mentioned_utisak_20_09$friends_count[index]
  V(mention_net_utisak_20_09)$posts_cnt[i] <- node_attrs_mentioned_utisak_20_09$statuses_count[index]
}
summary(mention_net_utisak_20_09)
summary(V(mention_net_utisak_20_09)$followers_cnt)
# koristimo funkciju sa SNA kursa
# ona pravi cvorove na grafu tako da su velicinom proprcionalni broju pratilaca korisnika
source('SNA_custom_functions.R')
friends_for_color_mentioned_utisak_20_09 <- attr_based_color_gradient(log1p(V(mention_net_utisak_20_09)$friends_cnt),
                                                                c('gold','steelblue3'))
# vektor na osnovu broja pratilaca za racunanje velicine cvora
followers_for_size_mentioned_utisak_20_09 <- log1p(V(mention_net_utisak_20_09)$followers_cnt)
# plot
plot(mention_net_utisak_20_09,
     layout = layout_with_lgl(mention_net_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = followers_for_size_mentioned_utisak_20_09,
     vertex.color = friends_for_color_mentioned_utisak_20_09,
     main = "Mreza UN na tviteru za 20.09.2020: boja cvora oznacava broj prijatelja, a velicina broj pratilaca")
library(visNetwork)
#
# VISNETWORK: vizualizacija mreze mentioned za utisak nedelje
#
m_net_comp_utisak_20_09 <- components(mention_net_utisak_20_09, mode = 'weak')
str(m_net_comp_utisak_20_09)
# najveca komponenta:
giant_comp_mentioned_utisak_20_09_size <- max(m_net_comp_utisak_20_09$csize)
giant_comp_mentioned_utisak_20_09_index <- which(m_net_comp_utisak_20_09$csize == giant_comp_mentioned_utisak_20_09_size)
# izdvajamo najvecu komponentu iz grafa mention_net_utisak_20_09
giant_comp_mentioned_utisak_20_09 <- induced_subgraph(mention_net_utisak_20_09,
                                                vids = V(mention_net_utisak_20_09)[m_net_comp_utisak_20_09$membership==giant_comp_mentioned_utisak_20_09_index])
summary(giant_comp_mentioned_utisak_20_09)
is_connected(giant_comp_mentioned_utisak_20_09, mode = 'weak')
# koristimo attribute friends_cnt i followers_cnt i njihove boje i velicine
gc_colors_mentioned_utisak_20_09 <- attr_based_color_gradient(log1p(V(giant_comp_mentioned_utisak_20_09)$friends_cnt), c('gold','steelblue3'))
gc_size_mentioned_utisak_20_09 <- log1p(V(giant_comp_mentioned_utisak_20_09)$followers_cnt)
set.seed(2501)
plot(giant_comp_mentioned_utisak_20_09,
     layout = layout_with_dh(giant_comp_mentioned_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = gc_size_mentioned_utisak_20_09,
     vertex.color = gc_colors_mentioned_utisak_20_09,
     main = "Najveca komponenta u mrezi mentioned za Utisak nedelje za 20.09.2020.")
# vizualizacija grafa
nodes_df_mentioned_utisak_20_09 <- data.frame(id=V(giant_comp_mentioned_utisak_20_09)$name, stringsAsFactors = FALSE)
head(nodes_df_mentioned_utisak_20_09)
edges_df_mentioned_utisak_20_09 <- data.frame(as_edgelist(giant_comp_mentioned_utisak_20_09), stringsAsFactors = FALSE)
colnames(edges_df_mentioned_utisak_20_09) <- c('from', 'to')
visNetwork(nodes = nodes_df_mentioned_utisak_20_09, edges = edges_df_mentioned_utisak_20_09,
           main="Najveca komponenta mentioned mreze za Utisak nedelje za 20.09.2020.")
#
nodes_df_mentioned_utisak_20_09$color <- gc_colors_mentioned_utisak_20_09
nodes_df_mentioned_utisak_20_09$size <- 12 + gc_size_mentioned_utisak_20_09
head(nodes_df_mentioned_utisak_20_09)
#
edges_df_mentioned_utisak_20_09$width <- 1 + (E(giant_comp_mentioned_utisak_20_09)$mention_tie / 3)
edges_df_mentioned_utisak_20_09$color <- 'slategray3'
edges_df_mentioned_utisak_20_09$smooth <- TRUE
edges_df_mentioned_utisak_20_09$arrows <- 'to'
head(edges_df_mentioned_utisak_20_09)
visNetwork(nodes = nodes_df_mentioned_utisak_20_09,
           edges = edges_df_mentioned_utisak_20_09,
           main="Najveca komponenta mentioned mreze za Utisak nedelje za 20.09.2020.",
           footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
nodes_df_mentioned_utisak_20_09$shadow <- FALSE
nodes_df_mentioned_utisak_20_09$title <- nodes_df_mentioned_utisak_20_09$id
nodes_df_mentioned_utisak_20_09$borderWidth <- 1.5
nodes_df_mentioned_utisak_20_09 <- nodes_df_mentioned_utisak_20_09 %>% select(-color)
nodes_df_mentioned_utisak_20_09$color.background <- gc_colors_mentioned_utisak_20_09
nodes_df_mentioned_utisak_20_09$color.border <- "black"
nodes_df_mentioned_utisak_20_09$color.highlight.background <- "orange"
nodes_df_mentioned_utisak_20_09$color.highlight.border <- "darkred"
visnet <- visNetwork(nodes = nodes_df_mentioned_utisak_20_09, edges = edges_df_mentioned_utisak_20_09,
                     main="Najveca komponenta mentioned mreze za Utisak nedelje za 20.09.2020.",
                     footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
visnet
#
# MREZE KORISNIKA UN za 20.09.2020. na osnovu relacije replied_to
##
# edge list za pravljenje mreze na osnovu relacije 'replied_to'
replied_to_edgelist_utisak_20_09 <- readRDS("data/replied_to_edgelist_utisak_20_09.RData")
summary(replied_to_edgelist_utisak_20_09$reply_to_tie)
# pravimo usmerenu mrezu (directed net)
replied_to_net_utisak_20_09 <- graph_from_data_frame(replied_to_edgelist_utisak_20_09)
summary(replied_to_net_utisak_20_09)
# proveravamo gustinu mreze
edge_density(replied_to_net_utisak_20_09)
plot(replied_to_net_utisak_20_09,
     layout = layout_with_lgl(replied_to_net_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# dodajemo neke nove vrednosti cvorovima
# koristimo prethodno napravljenu funkciju get_user_attrs() koja prima:
# 1) podatke o korisnicima dobijene funkcijom paketa rtweet users_data(),
# 2) vektor korisnickih imena onih korisnika koji su nam relevantni
# funkcija vraca data frame od 4 promenljive (kolone):
# screen_name, followers_count, friends_count, and statuses_count
# ucitamo podatke o senders: potrebno za dodavanje atributa cvorovima koji su izvori
senders_data_utisak_20_09 <- readRDS("data/ego_data_utisak_20_09.RData")
ego_attrs_replied_to_utisak_20_09 <- get_user_attrs(senders_data_utisak_20_09, V(replied_to_net_utisak_20_09)$name)
glimpse(ego_attrs_replied_to_utisak_20_09)
# isto i za alters: one korisnike kojima je odgovoreno u tvitu
alters_data_utisak_20_09 <- readRDS("data/alter_data_utisak_20_09.RData")
alter_attrs_replied_to_utisak_20_09 <- get_user_attrs(alters_data_utisak_20_09, V(replied_to_net_utisak_20_09)$name)
glimpse(alter_attrs_replied_to_utisak_20_09)
# dodeljujemo attribute onima koji su u mrezi 'replied_to_net'
node_attrs_replied_to_utisak_20_09 <- rbind(ego_attrs_replied_to_utisak_20_09, alter_attrs_replied_to_utisak_20_09) %>% # spoj dataframe-ova
  distinct(screen_name, .keep_all = TRUE) %>% # uzimamo samo redove koji nemaju duplicate tj. jedinstveni su
  arrange(screen_name) # sortiranje prema korisnickom imenu
head(node_attrs_replied_to_utisak_20_09, n=10)
summary(node_attrs_replied_to_utisak_20_09[,-1])
# attribute koji su vektori postavimo najpre na 0
V(replied_to_net_utisak_20_09)$followers_cnt <- rep(0, vcount(replied_to_net_utisak_20_09))
V(replied_to_net_utisak_20_09)$friends_cnt <- rep(0, vcount(replied_to_net_utisak_20_09))
V(replied_to_net_utisak_20_09)$posts_cnt <- rep(0, vcount(replied_to_net_utisak_20_09))
for (i in 1:vcount(replied_to_net_utisak_20_09)) {
  index <- which(node_attrs_replied_to_utisak_20_09$screen_name == V(replied_to_net_utisak_20_09)$name[i])
  V(replied_to_net_utisak_20_09)$followers_cnt[i] <- node_attrs_replied_to_utisak_20_09$followers_count[index]
  V(replied_to_net_utisak_20_09)$friends_cnt[i] <- node_attrs_replied_to_utisak_20_09$friends_count[index]
  V(replied_to_net_utisak_20_09)$posts_cnt[i] <- node_attrs_replied_to_utisak_20_09$statuses_count[index]
}
summary(replied_to_net_utisak_20_09)
summary(V(replied_to_net_utisak_20_09)$followers_cnt)
# koristimo funkciju sa SNA kursa, vec prethodno koriscenu za mrezu mention_net
# ona pravi cvorove na grafu tako da su velicinom proprcionalni broju pratilaca korisnika
source('SNA_custom_functions.R')
friends_for_color_replied_to_utisak_20_09 <- attr_based_color_gradient(log1p(V(replied_to_net_utisak_20_09)$friends_cnt),
                                                                 c('white','steelblue2'))
# vektor na osnovu broja pratilaca za racunanje velicine cvora
followers_for_size_replied_to_utisak_20_09 <- log1p(V(replied_to_net_utisak_20_09)$followers_cnt)
# plot
plot(replied_to_net_utisak_20_09,
     layout = layout_with_lgl(replied_to_net_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = followers_for_size_replied_to_utisak_20_09,
     vertex.color = friends_for_color_replied_to_utisak_20_09,
     main = "Mreza UN  za 20.09.2020. na tviteru prema odnosu replied_to: \nboja cvora oznacava broj prijatelja, a velicina broj pratilaca")
#
# VISNETWORK: vizualizacija mreze replied_to za utisak nedelje za 20.09.2020.
#
library(visNetwork)
r_net_comp_utisak_20_09 <- components(replied_to_net_utisak_20_09, mode = 'weak')
str(r_net_comp_utisak_20_09)
# najveca komponenta:
giant_comp_replied_to_utisak_size_20_09 <- max(r_net_comp_utisak_20_09$csize)
giant_comp_replied_to_utisak_index_20_09 <- which(r_net_comp_utisak_20_09$csize == giant_comp_replied_to_utisak_size_20_09)
# izdvajamo najvecu komponentu iz grafa replied_to_net_utisak_20_09
giant_comp_replied_to_utisak_20_09 <- induced_subgraph(replied_to_net_utisak_20_09,
                                                 vids = V(replied_to_net_utisak_20_09)[r_net_comp_utisak_20_09$membership==giant_comp_replied_to_utisak_index_20_09])
summary(giant_comp_replied_to_utisak_20_09)
is_connected(giant_comp_replied_to_utisak_20_09, mode = 'weak')
# koristimo attribute friends_cnt i followers_cnt i njihove boje i velicine
gc_colors_replied_to_utisak_20_09 <- attr_based_color_gradient(log1p(V(giant_comp_replied_to_utisak_20_09)$friends_cnt), c('white','steelblue4'))
gc_size_replied_to_utisak_20_09 <- log1p(V(giant_comp_replied_to_utisak_20_09)$followers_cnt)
set.seed(2501)
plot(giant_comp_replied_to_utisak_20_09,
     layout = layout_with_dh(giant_comp_replied_to_utisak_20_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = gc_size_replied_to_utisak_20_09,
     vertex.color = gc_colors_replied_to_utisak_20_09,
     main = "Najveca komponenta u mrezi replied_to za Utisak nedelje")
# vizualizacija grafa
nodes_df_replied_to_utisak_20_09 <- data.frame(id=V(giant_comp_replied_to_utisak_20_09)$name, stringsAsFactors = FALSE)
head(nodes_df_replied_to_utisak_20_09)
edges_df_replied_to_utisak_20_09 <- data.frame(as_edgelist(giant_comp_replied_to_utisak_20_09), stringsAsFactors = FALSE)
colnames(edges_df_replied_to_utisak_20_09) <- c('from', 'to')
visNetwork(nodes = nodes_df_replied_to_utisak_20_09, edges = edges_df_replied_to_utisak_20_09,
           main="Najveca komponenta replied_to mreze za Utisak nedelje 20.09.2020.")
#
nodes_df_replied_to_utisak_20_09$color <- gc_colors_replied_to_utisak_20_09
nodes_df_replied_to_utisak_20_09$size <- 12 + gc_size_replied_to_utisak_20_09
head(nodes_df_replied_to_utisak_20_09)
#
edges_df_replied_to_utisak_20_09$width <- 1 + (E(giant_comp_replied_to_utisak_20_09)$reply_to_tie / 3)
edges_df_replied_to_utisak_20_09$color <- 'slategray3'
edges_df_replied_to_utisak_20_09$smooth <- TRUE
edges_df_replied_to_utisak_20_09$arrows <- 'to'
head(edges_df_replied_to_utisak_20_09)
visNetwork(nodes = nodes_df_replied_to_utisak_20_09,
           edges = edges_df_replied_to_utisak_20_09,
           main="Najveca komponenta replied_to mreze za Utisak nedelje 20.09.2020.",
           footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
nodes_df_replied_to_utisak_20_09$shadow <- FALSE
nodes_df_replied_to_utisak_20_09$title <- nodes_df_replied_to_utisak_20_09$id
nodes_df_replied_to_utisak_20_09$borderWidth <- 1
nodes_df_replied_to_utisak_20_09 <- nodes_df_replied_to_utisak_20_09 %>% select(-color)
nodes_df_replied_to_utisak_20_09$color.background <- gc_colors_replied_to_utisak_20_09
nodes_df_replied_to_utisak_20_09$color.border <- "blue"
nodes_df_replied_to_utisak_20_09$color.highlight.background <- "orange"
nodes_df_replied_to_utisak_20_09$color.highlight.border <- "red"
visnet <- visNetwork(nodes = nodes_df_replied_to_utisak_20_09, edges = edges_df_replied_to_utisak_20_09,
                     main="Najveca komponenta replied_to mreze za Utisak nedelje za 20.09.2020.",
                     footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
visnet


# pocetak

# mreza UN za 27.09.2020.
# rad sa grafovima
# biramo samo relevantne kolone
utisak_27_09_tweets_users <- utisak_27_09_tweets %>%
  select(screen_name, reply_to_screen_name, mentions_screen_name) %>%
  rename(sender=screen_name, replied_to=reply_to_screen_name, mentioned=mentions_screen_name)
# Neke vrednosti 'replied_to' i 'mentioned' ne postoje, proveravamo koliko ih je takvih:
length(which(is.na(utisak_27_09_tweets_users$replied_to)==TRUE))
length(which(is.na(utisak_27_09_tweets_users$mentioned)))
# Uzimamo samo tvitove u kojima je neko odgovorio drugom korisniku
# ili ga spomenuo
utisak_27_09_tweets_users <- utisak_27_09_tweets_users %>%
  filter( is.na(replied_to)==FALSE | is.na(mentioned)==FALSE )
nrow(utisak_27_09_tweets_users)
# radimo operaciju unnest za kolonu 'mentioned'
# Svaki element liste ide u nov red operacijom unnest
utisak_27_09_tweets_users <- unnest(utisak_27_09_tweets_users, mentioned)
# Pretvaramo utisak_27_09_tweets_users u dataframe zbog daljeg rada sa grafovima
# Pravimo 2 edge liste: jednu za relaciju 'reply_to' drugu za relaciju 'mentioned'
replied_to_edgelist_utisak_27_09 <- utisak_27_09_tweets_users %>%
  select(-mentioned) %>%              # bez kolone 'mentioned'
  filter(complete.cases(.)) %>%       # iskljucimo redove koji nemaju vrednost, tj. imaju vrednost NA
  group_by(sender, replied_to) %>%    # redovi se spajaju po vrednostima sender i replied_to
  summarise(weight = n()) %>%         # kolika je velicina svake grupe (veza izmedju kolona sender I replied_to)
  ungroup()
head(replied_to_edgelist_utisak_27_09, n=10)
# izmedju kojih parova cvorova je komunikacija najveca (sortiramo po velicini grupe tj. weight):
replied_to_edgelist_utisak_27_09 %>%
  arrange(desc(weight)) %>%
  head(n=10)
# isto radimo za kolonu 'mentioned'
mentioned_edgelist_utisak_27_09 <- utisak_27_09_tweets_users %>%
  select(-replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, mentioned) %>%
  summarise(weight = n()) %>%
  ungroup()
head(mentioned_edgelist_utisak_27_09, n=10)
# gde se najcesce vidi ova veza (sortiranje po vrednosti weight):
mentioned_edgelist_utisak_27_09 %>%
  arrange(desc(weight)) %>%
  head(n=10)
# Cvorovi u mrezama su jedinstvene vrednosti za korisnike
# jedinstvene vrednosti u replied_to edgelist
reply_to_unique_utisak_27_09 <- union(replied_to_edgelist_utisak_27_09$sender, replied_to_edgelist_utisak_27_09$replied_to)
reply_to_unique_utisak_27_09 <- with(replied_to_edgelist_utisak_27_09, union(sender, replied_to))
length(reply_to_unique_utisak_27_09)
# jedinstvene vrednosti u mentioned edgelist
mention_unique_utisak_27_09 <- with(mentioned_edgelist_utisak_27_09, union(sender, mentioned))
length(mention_unique_utisak_27_09)
summary(replied_to_edgelist_utisak_27_09$weight)
summary(mentioned_edgelist_utisak_27_09$weight)
# Dodajemo jos podataka o korisnicima (senders).
# Neke smo vec dobili (utisak_27_09_tweets)
all_senders_utisak_27_09 <- union(mentioned_edgelist_utisak_27_09$sender,
                            replied_to_edgelist_utisak_27_09$sender)
senders_data_utisak_27_09 <- utisak_27_09_tweets %>%
  filter(screen_name %in% all_senders_utisak_27_09) %>%
  users_data() %>%                          # funkcija paketa rtweet za uzimanje podataka iz dataset-a
  distinct(user_id, .keep_all = TRUE)
# Skupljamo podatke za “alters”, tj. korisnike koji su mentioned / replied_to
# za koje alters nemamo podatke
no_data_alters_utisak_27_09 <- setdiff(union(mention_unique_utisak_27_09, reply_to_unique_utisak_27_09), all_senders_utisak_27_09)
length(no_data_alters_utisak_27_09)
# koristimo funkciju paketa rtweet lookup_users da prikupimo podatke o njima
alters_data_utisak_27_09 <- lookup_users(no_data_alters_utisak_27_09)
# cuvamo ove podatke
saveRDS(alters_data_utisak_27_09, "data/utisak_alters_data_utisak_27_09.RData")
glimpse(alters_data_utisak_27_09)
# uzimamo samo podatke o korisnicima, bez njihovih tvitova
# prethodna funkcija je vratila I tvitova korisnika
alters_data_utisak_27_09 <- users_data(alters_data_utisak_27_09)
# provera da li nekome fale podaci
missing_alter_utisak <- setdiff(no_data_alters_utisak_27_09, alters_data_utisak_27_09$screen_name)
length(missing_alter_utisak)
mentioned_edgelist_utisak_27_09 %>%
  filter(!mentioned %in% missing_alter_utisak) %>%
  rename(ego=sender, alter=mentioned, mention_tie=weight) %>%
  saveRDS(file = "data/mentions_edgelist_utisak_27_09.RData")
replied_to_edgelist_utisak_27_09 %>%
  filter(!replied_to %in% missing_alter_utisak) %>%
  rename(ego=sender, alter=replied_to, reply_to_tie=weight) %>%
  saveRDS(file = "data/replied_to_edgelist_utisak_27_09.RData")
saveRDS(senders_data_utisak_27_09, file = "data/ego_data_utisak_27_09.RData")
saveRDS(alters_data_utisak_27_09, file = "data/alter_data_utisak_27_09.RData")
##
# MREZE KORISNIKA UN za 27.09.2020. na osnovu relacije mentioned
##
# edge list za pravljenje mreze na osnovu relacije 'mentioned'
mention_edgelist_utisak_27_09 <- readRDS("data/mentions_edgelist_utisak_27_09.RData")
summary(mention_edgelist_utisak_27_09$mention_tie)
# pravimo usmerenu mrezu (directed net)
mention_net_utisak_27_09 <- graph_from_data_frame(mention_edgelist_utisak_27_09)
summary(mention_net_utisak_27_09)
# proveravamo gustinu mreze
edge_density(mention_net_utisak_27_09)
plot(mention_net_utisak_27_09,
     layout = layout_with_lgl(mention_net_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# dodajemo neke nove vrednosti cvorovima
# pravimo funkciju get_user_attrs() koja prima:
# 1) podatke o korisnicima dobijene funkcijom paketa rtweet users_data(),
# 2) vector korisnickih imena onih korisnika koji su nam relevantni
# funkcija vraca data frame od 4 promenljive (kolone):
# screen_name, followers_count, friends_count, and statuses_count
get_user_attrs <- function(twitter_user_data, users_screen_names) {
  twitter_user_data %>%
    filter(screen_name %in% users_screen_names) %>%
    select(screen_name, followers_count, friends_count, statuses_count)
}
# ucitamo podatke o senders: potrebno za dodavanje atributa cvorovima koji su izvori
senders_data_utisak_27_09 <- readRDS("data/ego_data_utisak_27_09.RData")
ego_attrs_mentioned_utisak_27_09 <- get_user_attrs(senders_data_utisak_27_09, V(mention_net_utisak_27_09)$name)
glimpse(ego_attrs_mentioned_utisak_27_09)
# isto I za alters: one korisnike koji su spomenuti u tvitu onih koji su senders
alters_data_utisak_27_09 <- readRDS("data/alter_data_utisak_27_09.RData")
alter_attrs_mentioned_utisak_27_09 <- get_user_attrs(alters_data_utisak_27_09, V(mention_net_utisak_27_09)$name)
# dodeljujemo attribute onima koji su u mrezi 'mentioned'
node_attrs_mentioned_utisak_27_09 <- rbind(ego_attrs_mentioned_utisak_27_09, alter_attrs_mentioned_utisak_27_09) %>% # spoj dataframe-ova
  distinct(screen_name, .keep_all = TRUE) %>% # uzimamo samo redove koji nemaju duplicate tj. jedinstveni su
  arrange(screen_name) # sortiranje prema korisnickom imenu
head(node_attrs_mentioned_utisak_27_09, n=10)
summary(node_attrs_mentioned_utisak_27_09[,-1])
# attribute koji su vektori postavimo najpre na 0
V(mention_net_utisak_27_09)$followers_cnt <- rep(0, vcount(mention_net_utisak_27_09))
V(mention_net_utisak_27_09)$friends_cnt <- rep(0, vcount(mention_net_utisak_27_09))
V(mention_net_utisak_27_09)$posts_cnt <- rep(0, vcount(mention_net_utisak_27_09))
for (i in 1:vcount(mention_net_utisak_27_09)) {
  index <- which(node_attrs_mentioned_utisak_27_09$screen_name == V(mention_net_utisak_27_09)$name[i])
  V(mention_net_utisak_27_09)$followers_cnt[i] <- node_attrs_mentioned_utisak_27_09$followers_count[index]
  V(mention_net_utisak_27_09)$friends_cnt[i] <- node_attrs_mentioned_utisak_27_09$friends_count[index]
  V(mention_net_utisak_27_09)$posts_cnt[i] <- node_attrs_mentioned_utisak_27_09$statuses_count[index]
}
summary(mention_net_utisak_27_09)
summary(V(mention_net_utisak_27_09)$followers_cnt)
# koristimo funkciju sa SNA kursa
# ona pravi cvorove na grafu tako da su velicinom proprcionalni broju pratilaca korisnika
source('SNA_custom_functions.R')
friends_for_color_mentioned_utisak_27_09 <- attr_based_color_gradient(log1p(V(mention_net_utisak_27_09)$friends_cnt),
                                                                c('gold','steelblue3'))
# vektor na osnovu broja pratilaca za racunanje velicine cvora
followers_for_size_mentioned_utisak_27_09 <- log1p(V(mention_net_utisak_27_09)$followers_cnt)
# plot
plot(mention_net_utisak_27_09,
     layout = layout_with_lgl(mention_net_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = followers_for_size_mentioned_utisak_27_09,
     vertex.color = friends_for_color_mentioned_utisak_27_09,
     main = "Mreza UN na tviteru za 27.09.2020: boja cvora oznacava broj prijatelja, a velicina broj pratilaca")
library(visNetwork)
#
# VISNETWORK: vizualizacija mreze mentioned za utisak nedelje
#
m_net_comp_utisak_27_09 <- components(mention_net_utisak_27_09, mode = 'weak')
str(m_net_comp_utisak_27_09)
# najveca komponenta:
giant_comp_mentioned_utisak_27_09_size <- max(m_net_comp_utisak_27_09$csize)
giant_comp_mentioned_utisak_27_09_index <- which(m_net_comp_utisak_27_09$csize == giant_comp_mentioned_utisak_27_09_size)
# izdvajamo najvecu komponentu iz grafa mention_net_utisak_27_09
giant_comp_mentioned_utisak_27_09 <- induced_subgraph(mention_net_utisak_27_09,
                                                vids = V(mention_net_utisak_27_09)[m_net_comp_utisak_27_09$membership==giant_comp_mentioned_utisak_27_09_index])
summary(giant_comp_mentioned_utisak_27_09)
is_connected(giant_comp_mentioned_utisak_27_09, mode = 'weak')
# koristimo attribute friends_cnt i followers_cnt i njihove boje i velicine
gc_colors_mentioned_utisak_27_09 <- attr_based_color_gradient(log1p(V(giant_comp_mentioned_utisak_27_09)$friends_cnt), c('gold','steelblue3'))
gc_size_mentioned_utisak_27_09 <- log1p(V(giant_comp_mentioned_utisak_27_09)$followers_cnt)
set.seed(2501)
plot(giant_comp_mentioned_utisak_27_09,
     layout = layout_with_dh(giant_comp_mentioned_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = gc_size_mentioned_utisak_27_09,
     vertex.color = gc_colors_mentioned_utisak_27_09,
     main = "Najveca komponenta u mrezi mentioned za Utisak nedelje za 27.09.2020.")
# vizualizacija grafa
nodes_df_mentioned_utisak_27_09 <- data.frame(id=V(giant_comp_mentioned_utisak_27_09)$name, stringsAsFactors = FALSE)
head(nodes_df_mentioned_utisak_27_09)
edges_df_mentioned_utisak_27_09 <- data.frame(as_edgelist(giant_comp_mentioned_utisak_27_09), stringsAsFactors = FALSE)
colnames(edges_df_mentioned_utisak_27_09) <- c('from', 'to')
visNetwork(nodes = nodes_df_mentioned_utisak_27_09, edges = edges_df_mentioned_utisak_27_09,
           main="Najveca komponenta mentioned mreze za Utisak nedelje za 27.09.2020.")
#
nodes_df_mentioned_utisak_27_09$color <- gc_colors_mentioned_utisak_27_09
nodes_df_mentioned_utisak_27_09$size <- 12 + gc_size_mentioned_utisak_27_09
head(nodes_df_mentioned_utisak_27_09)
#
edges_df_mentioned_utisak_27_09$width <- 1 + (E(giant_comp_mentioned_utisak_27_09)$mention_tie / 3)
edges_df_mentioned_utisak_27_09$color <- 'slategray3'
edges_df_mentioned_utisak_27_09$smooth <- TRUE
edges_df_mentioned_utisak_27_09$arrows <- 'to'
head(edges_df_mentioned_utisak_27_09)
visNetwork(nodes = nodes_df_mentioned_utisak_27_09,
           edges = edges_df_mentioned_utisak_27_09,
           main="Najveca komponenta mentioned mreze za Utisak nedelje za 27.09.2020.",
           footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
nodes_df_mentioned_utisak_27_09$shadow <- FALSE
nodes_df_mentioned_utisak_27_09$title <- nodes_df_mentioned_utisak_27_09$id
nodes_df_mentioned_utisak_27_09$borderWidth <- 1.5
nodes_df_mentioned_utisak_27_09 <- nodes_df_mentioned_utisak_27_09 %>% select(-color)
nodes_df_mentioned_utisak_27_09$color.background <- gc_colors_mentioned_utisak_27_09
nodes_df_mentioned_utisak_27_09$color.border <- "black"
nodes_df_mentioned_utisak_27_09$color.highlight.background <- "orange"
nodes_df_mentioned_utisak_27_09$color.highlight.border <- "darkred"
visnet <- visNetwork(nodes = nodes_df_mentioned_utisak_27_09, edges = edges_df_mentioned_utisak_27_09,
                     main="Najveca komponenta mentioned mreze za Utisak nedelje za 27.09.2020.",
                     footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
visnet
#
# MREZE KORISNIKA UN za 27.09.2020. na osnovu relacije replied_to
##
# edge list za pravljenje mreze na osnovu relacije 'replied_to'
replied_to_edgelist_utisak_27_09 <- readRDS("data/replied_to_edgelist_utisak_27_09.RData")
summary(replied_to_edgelist_utisak_27_09$reply_to_tie)
# pravimo usmerenu mrezu (directed net)
replied_to_net_utisak_27_09 <- graph_from_data_frame(replied_to_edgelist_utisak_27_09)
summary(replied_to_net_utisak_27_09)
# proveravamo gustinu mreze
edge_density(replied_to_net_utisak_27_09)
plot(replied_to_net_utisak_27_09,
     layout = layout_with_lgl(replied_to_net_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# dodajemo neke nove vrednosti cvorovima
# koristimo prethodno napravljenu funkciju get_user_attrs() koja prima:
# 1) podatke o korisnicima dobijene funkcijom paketa rtweet users_data(),
# 2) vektor korisnickih imena onih korisnika koji su nam relevantni
# funkcija vraca data frame od 4 promenljive (kolone):
# screen_name, followers_count, friends_count, and statuses_count
# ucitamo podatke o senders: potrebno za dodavanje atributa cvorovima koji su izvori
senders_data_utisak_27_09 <- readRDS("data/ego_data_utisak_27_09.RData")
ego_attrs_replied_to_utisak_27_09 <- get_user_attrs(senders_data_utisak_27_09, V(replied_to_net_utisak_27_09)$name)
glimpse(ego_attrs_replied_to_utisak_27_09)
# isto i za alters: one korisnike kojima je odgovoreno u tvitu
alters_data_utisak_27_09 <- readRDS("data/alter_data_utisak_27_09.RData")
alter_attrs_replied_to_utisak_27_09 <- get_user_attrs(alters_data_utisak_27_09, V(replied_to_net_utisak_27_09)$name)
glimpse(alter_attrs_replied_to_utisak_27_09)
# dodeljujemo attribute onima koji su u mrezi 'replied_to_net'
node_attrs_replied_to_utisak_27_09 <- rbind(ego_attrs_replied_to_utisak_27_09, alter_attrs_replied_to_utisak_27_09) %>% # spoj dataframe-ova
  distinct(screen_name, .keep_all = TRUE) %>% # uzimamo samo redove koji nemaju duplicate tj. jedinstveni su
  arrange(screen_name) # sortiranje prema korisnickom imenu
head(node_attrs_replied_to_utisak_27_09, n=10)
summary(node_attrs_replied_to_utisak_27_09[,-1])
# attribute koji su vektori postavimo najpre na 0
V(replied_to_net_utisak_27_09)$followers_cnt <- rep(0, vcount(replied_to_net_utisak_27_09))
V(replied_to_net_utisak_27_09)$friends_cnt <- rep(0, vcount(replied_to_net_utisak_27_09))
V(replied_to_net_utisak_27_09)$posts_cnt <- rep(0, vcount(replied_to_net_utisak_27_09))
for (i in 1:vcount(replied_to_net_utisak_27_09)) {
  index <- which(node_attrs_replied_to_utisak_27_09$screen_name == V(replied_to_net_utisak_27_09)$name[i])
  V(replied_to_net_utisak_27_09)$followers_cnt[i] <- node_attrs_replied_to_utisak_27_09$followers_count[index]
  V(replied_to_net_utisak_27_09)$friends_cnt[i] <- node_attrs_replied_to_utisak_27_09$friends_count[index]
  V(replied_to_net_utisak_27_09)$posts_cnt[i] <- node_attrs_replied_to_utisak_27_09$statuses_count[index]
}
summary(replied_to_net_utisak_27_09)
summary(V(replied_to_net_utisak_27_09)$followers_cnt)
# koristimo funkciju sa SNA kursa, vec prethodno koriscenu za mrezu mention_net
# ona pravi cvorove na grafu tako da su velicinom proprcionalni broju pratilaca korisnika
source('SNA_custom_functions.R')
friends_for_color_replied_to_utisak_27_09 <- attr_based_color_gradient(log1p(V(replied_to_net_utisak_27_09)$friends_cnt),
                                                                 c('white','steelblue2'))
# vektor na osnovu broja pratilaca za racunanje velicine cvora
followers_for_size_replied_to_utisak_27_09 <- log1p(V(replied_to_net_utisak_27_09)$followers_cnt)
# plot
plot(replied_to_net_utisak_27_09,
     layout = layout_with_lgl(replied_to_net_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = followers_for_size_replied_to_utisak_27_09,
     vertex.color = friends_for_color_replied_to_utisak_27_09,
     main = "Mreza UN  za 27.09.2020. na tviteru prema odnosu replied_to: \nboja cvora oznacava broj prijatelja, a velicina broj pratilaca")
#
# VISNETWORK: vizualizacija mreze replied_to za utisak nedelje za 27.09.2020.
#
library(visNetwork)
r_net_comp_utisak_27_09 <- components(replied_to_net_utisak_27_09, mode = 'weak')
str(r_net_comp_utisak_27_09)
# najveca komponenta:
giant_comp_replied_to_utisak_size_27_09 <- max(r_net_comp_utisak_27_09$csize)
giant_comp_replied_to_utisak_index_27_09 <- which(r_net_comp_utisak_27_09$csize == giant_comp_replied_to_utisak_size_27_09)
# izdvajamo najvecu komponentu iz grafa replied_to_net_utisak_27_09
giant_comp_replied_to_utisak_27_09 <- induced_subgraph(replied_to_net_utisak_27_09,
                                                 vids = V(replied_to_net_utisak_27_09)[r_net_comp_utisak_27_09$membership==giant_comp_replied_to_utisak_index_27_09])
summary(giant_comp_replied_to_utisak_27_09)
is_connected(giant_comp_replied_to_utisak_27_09, mode = 'weak')
# koristimo attribute friends_cnt i followers_cnt i njihove boje i velicine
gc_colors_replied_to_utisak_27_09 <- attr_based_color_gradient(log1p(V(giant_comp_replied_to_utisak_27_09)$friends_cnt), c('white','steelblue4'))
gc_size_replied_to_utisak_27_09 <- log1p(V(giant_comp_replied_to_utisak_27_09)$followers_cnt)
set.seed(2501)
plot(giant_comp_replied_to_utisak_27_09,
     layout = layout_with_dh(giant_comp_replied_to_utisak_27_09),
     edge.arrow.size=0.3,
     vertex.label = NA,
     vertex.size = gc_size_replied_to_utisak_27_09,
     vertex.color = gc_colors_replied_to_utisak_27_09,
     main = "Najveca komponenta u mrezi replied_to za Utisak nedelje")
# vizualizacija grafa
nodes_df_replied_to_utisak_27_09 <- data.frame(id=V(giant_comp_replied_to_utisak_27_09)$name, stringsAsFactors = FALSE)
head(nodes_df_replied_to_utisak_27_09)
edges_df_replied_to_utisak_27_09 <- data.frame(as_edgelist(giant_comp_replied_to_utisak_27_09), stringsAsFactors = FALSE)
colnames(edges_df_replied_to_utisak_27_09) <- c('from', 'to')
visNetwork(nodes = nodes_df_replied_to_utisak_27_09, edges = edges_df_replied_to_utisak_27_09,
           main="Najveca komponenta replied_to mreze za Utisak nedelje 27.09.2020.")
#
nodes_df_replied_to_utisak_27_09$color <- gc_colors_replied_to_utisak_27_09
nodes_df_replied_to_utisak_27_09$size <- 12 + gc_size_replied_to_utisak_27_09
head(nodes_df_replied_to_utisak_27_09)
#
edges_df_replied_to_utisak_27_09$width <- 1 + (E(giant_comp_replied_to_utisak_27_09)$reply_to_tie / 3)
edges_df_replied_to_utisak_27_09$color <- 'slategray3'
edges_df_replied_to_utisak_27_09$smooth <- TRUE
edges_df_replied_to_utisak_27_09$arrows <- 'to'
head(edges_df_replied_to_utisak_27_09)
visNetwork(nodes = nodes_df_replied_to_utisak_27_09,
           edges = edges_df_replied_to_utisak_27_09,
           main="Najveca komponenta replied_to mreze za Utisak nedelje 27.09.2020.",
           footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
nodes_df_replied_to_utisak_27_09$shadow <- FALSE
nodes_df_replied_to_utisak_27_09$title <- nodes_df_replied_to_utisak_27_09$id
nodes_df_replied_to_utisak_27_09$borderWidth <- 1
nodes_df_replied_to_utisak_27_09 <- nodes_df_replied_to_utisak_27_09 %>% select(-color)
nodes_df_replied_to_utisak_27_09$color.background <- gc_colors_replied_to_utisak_27_09
nodes_df_replied_to_utisak_27_09$color.border <- "blue"
nodes_df_replied_to_utisak_27_09$color.highlight.background <- "orange"
nodes_df_replied_to_utisak_27_09$color.highlight.border <- "red"
visnet <- visNetwork(nodes = nodes_df_replied_to_utisak_27_09, edges = edges_df_replied_to_utisak_27_09,
                     main="Najveca komponenta replied_to mreze za Utisak nedelje za 27.09.2020.",
                     footer = "Boja oznacava broj prijatelja, a velicina cvora broj pratilaca")
visnet
