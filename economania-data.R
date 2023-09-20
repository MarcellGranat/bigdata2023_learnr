library(tidyverse)
library(magrittr)
library(rvest)

get_links <- (. %>% 
                read_html() %>% 
                html_nodes("a") %>% 
                html_attr("href") %>% 
                na.omit() %>% 
                unique() %>% 
                keep(str_detect, pattern = "https://economaniablog.hu/") %>% 
                keep(str_detect, pattern = "hu/tag/|hu/author/|hu/bloggerek/", negate = T) %>% 
                keep(str_detect, pattern = "hu/about/|hu/kapcsolat/|hu/tanulj_tolunk/|hu/szakmai_muhely/|hu/category/|#comments|hu/page/", negate = T) %>% 
                setdiff("https://economaniablog.hu/")) %>% 
  possibly(otherwise = NA_character_)



article_links <- str_c("https://economaniablog.hu/page/", 1:30, "/") %>% 
  map(get_links) %>% 
  reduce(c) %>% 
  na.omit()

economania_raw_df <- tibble(article_links) %>% 
  filter(article_links != "https://economaniablog.hu/uj-fenntarthato-kozgazdasagtan/") %>% 
  mutate(
    p = map(article_links, read_html, .progress = "scrape"),
    text = map(p, ~ html_text(html_nodes(., "p"))),
    title = map(p, ~ html_text(html_nodes(., "h1.entry-title"))),
    title = map_chr(title, first),
    author = map_chr(p, ~ html_text(html_nodes(., ".byline a"))),
  )

economania_df <- economania_raw_df %>% 
  select(-p) %>% 
  filter(author != "X") %>% 
  distinct(article_links, .keep_all = TRUE) %>% 
  mutate(
    text = map(text, setdiff, y = "Economania blog"),
    text = map(text, .f = function(t) discard(t, str_detect, "View all")),
    text = map(text, .f = function(t) discard(t, str_detect, "Főoldali kép forrása:")),
    text = map(text, .f = function(t) discard(t, str_detect, "Hozzászólások letiltva.")),
    text = map_chr(text, str_flatten, " "),
    text = map2_chr(text, author, ~ gsub(str_c(gsub("és.*", "", .y), ".*"), "", .x)),
    text = gsub(" Hivatkozások.*", "", text),
    text = gsub(" References.*", "", text),
  ) %>% 
  select(author, text)

write_csv(economania_df, "economania.csv")