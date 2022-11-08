# setup
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(jsonlite)
library(tidyr)

api_secret <- Sys.getenv("api_secret")

all_tags <- "https://technology.riotgames.com/" |> 
  read_html() |> 
  html_elements("a") |> 
  html_attr("href") |> 
  as_tibble() |> 
  filter(str_detect(value, "tags")) |> 
  pull()


# helper function to read blog
read_news_page <- function(url) {
  # url <-  "https://technology.riotgames.com/tags/architecture"
  url |> 
    read_html() |> 
    html_elements("a") |> 
    html_attr("href") |> 
    as_tibble() |> 
    filter(str_detect(value, "news/")) |> 
    distinct() |> 
    mutate(page = url,
           cate = httr::parse_url(url)$path,
           cate = str_remove(cate, "tags\\/"))
}

# loop over blog
parse_riot_blog <- function(url) {
  # url <- "/tags/architecture"
  print(url)
  links <- NULL
  links[[1]] <- read_news_page(paste0("https://technology.riotgames.com", url))
  i <- 2
  content <- paste0("https://technology.riotgames.com", url, "?page=", i-1) |> 
    read_news_page()
  while (!all(links[[i-1]]$value %in% content$value)) {
    print(i)
    links[[i]] <- content
    i <- i + 1
    
    content <- paste0("https://technology.riotgames.com", url, "?page=", i-1) |> 
      read_news_page()
  }
  return(bind_rows(links) |> distinct())
  
}

links <- map_df(all_tags, parse_riot_blog)
links_cate <- links |> select(links = value, categories = cate) |> 
  group_by(links) |> 
  mutate(test = list(categories)) |> 
  select(-categories) |> 
  distinct() |> 
  ungroup()

# #get blog links
# links_vec <- bind_rows(links) |> 
#   distinct(value) |> 
#   mutate(value = ifelse(grepl("^http", value), value, paste0("https://technology.riotgames.com", value))) |> 
#   pull()

#get blog entry information
get_entries <- function(url) {
  print(url)
  link_date <- url |> 
    read_html() |> 
    html_element(xpath = '//*[@id="content"]/div/div[1]/article/header/time') |> 
    html_text()
  
  link_text <- url |> 
    read_html() |> 
    html_element(xpath = '//*[@id="content"]/div/div[1]/article/div[1]') |> 
    html_text()
  
  link_title <- url |> 
    read_html() |> 
    html_element(xpath = '//*[@id="content"]/div/div[1]/article/h1') |> 
    html_text()
  
  tibble(
    "date_published" = link_date,
    "title" = link_title,
    "text" = link_text
  ) |> 
    mutate(across(c(date_published, title), function(x) str_remove_all(x, "\n")),
           across(c(date_published, title), str_trim),
           url = url,
           date_published = readr::parse_date(date_published, format = "%b %d, %Y"))
  
}

# loop over blogs
all <- map_df(links_cate$links, get_entries)

vec_to_tibble <- function(raw_json) {
  tibble(
    row = 1:(raw_json$response |> length()),
    json = raw_json$response
  )
  
}

# get metadata from disqus api
get_treads <- function(forum){
  forum <- "riotengineeringtechblog"
  url <- paste0("https://disqus.com/api/3.0/forums/listThreads.json?api_secret=", api_secret, "&limit=25&forum=", forum)
  res <- NULL
  i <- 1
  res[[i]] <- fromJSON(url, simplifyVector = FALSE)
  data <- NULL
  data[[i]] <- vec_to_tibble(res[[1]]) |> 
    mutate(run = i)
  
  while (res[[i]]$cursor$hasNext == TRUE) {
    print(i)
    
    new_url <- paste0(
      "https://disqus.com/api/3.0/forums/listThreads.json?api_secret=", api_secret, "&limit=25&cursor=", res[[i]]$cursor$`next`, "&forum=", forum
    )
    i <- i + 1
    res[[i]] <- fromJSON(new_url, simplifyVector = FALSE)
    data[[i]] <- vec_to_tibble(res[[i]]) |> 
      mutate(run = i)
  }
  
  bind_rows(data) |> 
    unnest_wider(json) 
}  

blog <- get_treads("riotengineeringtechblog")

# clean up all the mess oO
combined <- all |> 
  rowwise() |> 
  mutate(#merger = (url, "http\\:\\/\\/|https\\:\\/\\/"),
    merger = httr::parse_url(url)$path) |> distinct() |> 
  full_join(
    blog |> 
      select(link, clean_title, dislikes, likes, posts, id) |>
      mutate(link = case_when(
        
        str_detect(clean_title, "Fixing Divergences") ~ "https://technology.riotgames.com/news/determinism-league-legends-fixing-divergences",
        str_detect(clean_title, "Dynamic Applications Part 1") ~ "https://technology.riotgames.com/news/running-online-services-riot-part-iv",
        id == "5705501753" ~ "https://technology.riotgames.com/news/running-online-services-riot-part-iii-part-deux",
        id == "5507171835" ~ "https://technology.riotgames.com/news/running-online-services-riot-part-iii",
        id == "5286543440" ~ "https://technology.riotgames.com/news/running-online-services-riot-part-ii",
        id == "5165701567" ~ "https://technology.riotgames.com/news/running-online-services-riot-part-i",
        id == "5378513407" ~ "https://technology.riotgames.com/news/elementalist-lux-10-skins-30-megabytes",
        id == "4571910392" ~ "https://technology.riotgames.com/news/content-efficiency-game-data-server",
        id == "4511722260" ~ "http://engineering.riotgames.com/news/voyager-original-korean",
        id == "3923013852" ~ "https://technology.riotgames.com/news/compressing-skeletal-animation-data",
        id == "5531526256" ~ "https://technology.riotgames.com/news/under-hood-league-client%E2%80%99s-hextech-ui",
        TRUE ~ link
      )) |> 
      rowwise() |> 
      mutate(#merger = str_remove(link, "http\\:\\/\\/|https\\:\\/\\/"),
        merger = httr::parse_url(link)$path) 
  ) |> 
  ungroup() |> 
  left_join(
    links_cate |> 
      rename(url = links)
  )

# test |> select(-text) |> 
#   arrange(desc(date_published)) |> 
#   print(n = 300)

test <- combined |> 
  select(date_published, title, text, url, dislikes, likes, posts, categories = test) |> 
  distinct() |> 
  group_by(title) |> 
  filter(posts == max(posts)) |> 
  ungroup() 

# plot!
library(ggplot2)
g1 <- test |> 
  mutate(date_published = lubridate::floor_date(date_published, "quarter")) |> 
  count(date_published) |> 
  ggplot(aes(date_published, n)) +
  geom_col() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), 
                     breaks = 1:10) +
  theme_light() +
  labs(title = "Total Tech Threads per Quarter",
       y = "# Threads",
       x = NULL)

g2 <- test |> 
  drop_na() |> 
  mutate(engagements = likes+posts-dislikes,
         date_published = lubridate::floor_date(date_published, "quarter")) |> 
  count(date_published, wt = engagements) |> 
  ggplot(aes(date_published, n)) +
  geom_line() +
  theme_light() +
  labs(title = "Absolute Thread Engagements per Month",
       x = NULL,
       y = "# Engagements")

g3 <- test |> 
  drop_na() |> 
  mutate(engagements = likes+posts-dislikes,
         date_published = lubridate::floor_date(date_published, "quarter")) |> 
  add_count(date_published, name = "total") |> 
  count(date_published, total, wt = engagements) |> 
  mutate(perc = n/total) |> 
  ggplot(aes(date_published, perc)) +
  geom_line() +
  theme_light() +
  labs(title = "Thread Engagements per Month in %",
       x = NULL,
       y = "% Engagements")

library(patchwork)
g_all <- g1 / g2 / g3 +
  plot_annotation(caption = paste0("Data Sources:\n@https://technology.riotgames.com/ &\n@https://disqus.com/api/docs/\nDate: ", Sys.Date()))

library(forcats)
lm_fit <- test |> 
  drop_na() |> 
  mutate(engagements = likes+posts-dislikes,
         date_published = lubridate::floor_date(date_published, "quarter")) |> 
  select(url, engagements, categories) |>
  unnest(categories) |> 
  mutate(ind = 1) |> 
  distinct() |> 
  pivot_wider(names_from = categories, values_from = ind, values_fill = 0) |> 
  select(-url) |> 
  lm(engagements ~ ., data = _) |> 
  broom.helpers::tidy_and_attach(conf.int = TRUE) |> 
  broom.helpers::tidy_add_reference_rows() |> 
  broom.helpers::tidy_add_estimate_to_reference_rows() |> 
  broom.helpers::tidy_add_term_labels() |> 
  broom.helpers::tidy_remove_intercept() |> 
  mutate(
    plot_label = paste(var_label, label, sep = ":") %>% 
      forcats::fct_inorder() %>%
      forcats::fct_rev()
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2) +
  geom_point() +
  labs(x = "Estimate",
       y = NULL,
       title = "What Thread categories have more engagements?") +
  theme_light() +
  theme(legend.position = "none") 


save(g_all, lm_fit, file = "data/overall.RData")



