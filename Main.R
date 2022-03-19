library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(tidytext)
library(stringr)
library(scales)
library(lubridate)
library(readr)
library(ktools)

df <- read_delim("Data/feed.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(has_num = !is.na(num)) %>% 
  group_by(has_num) %>% 
  mutate(id = n():1) %>% 
  ungroup() %>% 
  mutate(id = ifelse(has_num, id, NA)) %>% 
  select(-has_num) %>% 
  relocate(id)

df %>% 
  ggplot(aes(x = published, y = duration)) +
  geom_point() +
  geom_smooth() +
  #scale_x_continuous(breaks = seq(0, 600, 100)) +
  scale_x_datetime() +
  scale_y_time(labels = \(x) {paste(
    sprintf("%02d", hour(hms(x))), 
    sprintf("%02d", minute(hms(x))), 
    sprintf("%02d", second(hms(x))),
    sep = ":"
  )}) +
  theme_kudusch() +
  labs(
    title = "MBMBAM Episode Length",
    subtitle = "All items in the RSS feed are shown, including announcements etc.",
    x = "Episode Publish Date",
    y = "Duration (H:M)"
  )

ggsave(
  "Output/episode_length.png", 
  plot = last_plot(),
  width = 3000,
  height = 1500,
  scale = 1,
  units = "px"
)

df %>% 
  #filter(!is.na(num), duration > 10*60) %>% 
  mutate(published_before = lag(published)) %>% 
  mutate(delta = published_before-published) %>% 
  mutate(delta = delta/ddays(1)) %>% 
  ggplot(aes(x = delta)) +
  geom_density(fill = "grey") +
  scale_x_continuous(breaks = 1:16) +
  theme_kudusch() +
  labs(
    title = "Time between MBMBAM episodes",
    subtitle = "All items in the RSS feed are shown, including announcements etc.",
    x = "Days"
  )

ggsave(
  "Output/time_between_episodes.png", 
  plot = last_plot(),
  width = 3000,
  height = 1500,
  scale = 1,
  units = "px"
)

df %>% 
  #filter(!is.na(num), duration > 10*60) %>% 
  mutate(published_time = seconds(hms(format(published, "%H:%M:%S")))) %>% 
  ggplot(aes(x = published_time)) +
  geom_density(fill = "grey") +
  scale_x_time() +
  theme_kudusch() +
  labs(
    title = "Publishing time of MBMBAM episodes",
    subtitle = "All items in the RSS feed are shown, including announcements etc.",
    x = "Time (UTC±00:00)"
  )

ggsave(
  "Output/publishing_time.png", 
  plot = last_plot(),
  width = 3000,
  height = 1500,
  scale = 1,
  units = "px"
)

df.tidy_text <- df %>% filter(!is.na(num)) %>% select(id, title_no_number, summary)
#df.tidy_text$ca <- paste(df.tidy_text$title_no_number, df.tidy_text$summary)
df.tidy_text$ca <- df.tidy_text$summary
df.tidy_text$ca <- str_to_lower(df.tidy_text$ca)
df.tidy_text$ca <- str_remove_all(df.tidy_text$ca, regex("<\\/*.*?>"))
df.tidy_text$ca <- str_remove_all(df.tidy_text$ca, regex("['\\u00B4\\u2019]\\w*"))
df.tidy_text$ca <- preprocess.removeNonWordChars(df.tidy_text$ca)
df.tidy_text$ca <- preprocess.removeStopwords(
  df.tidy_text$ca, 
  c(stopwords::stopwords(language = "en"), "episode", "suggested", "points", "talking")
)

df.tidy_text <- df.tidy_text %>% 
  select(id, ca) %>% 
  unnest_tokens(
    "word_token",
    ca,
    token = "regex",
    pattern = "\\s") %>%
  filter(str_length(word_token) > 2) %>% 
  filter(!str_detect(word_token, pattern = "^\\s*$"),
         !word_token == "",
         !grepl(pattern = "\\x{200D}", word_token))

df.tidy_text %>% 
  count(word_token, sort = T) %>% 
  filter(word_token == "horse")

df

g.words <- df.tidy_text %>% 
  add_count(word_token) %>% 
  filter(between(n, 2, 25)) %>% 
  select(-n) %>% 
  pairwise_count(word_token, id, upper = FALSE, diag = FALSE) %>% 
  graph_from_data_frame(
    directed = F, 
    vertices = (df.tidy_text %>% count(word_token, name = "cnt"))
  )

# g.words <- delete.edges(
#     g.words,
#     E(g.words)[E(g.words)$n < 5]
#   )
# 
# g.words <- delete.vertices(
#     g.words,
#     degree(g.words) < 3
#   )

#g.words <- simplify(g.words)
g.ego <- make_ego_graph(g.words, order = 1, nodes = V(g.words)[V(g.words)$name == "ghost"])[[1]]

ggraph(g.ego) + 
  geom_edge_link0(aes(alpha = E(g.ego)$n)) + 
  #geom_node_point(aes(size = V(g.words)$cnt)) +
  geom_node_label(aes(label = V(g.ego)$name, alpha = V(g.ego)$cnt)) +
  theme_void()


