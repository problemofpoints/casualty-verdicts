
# Scrape top verdict information from topverdict.com

# ---------- load packages -----------------------------------------------------

library(tidyr) # using tidyr version 0.8.3.9000
library(dplyr)
library(purrr)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
library(stringr)
library(readr)

# sessionInfo()
# R version 3.5.3 (2019-03-11)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] readr_1.3.1      stringr_1.4.0    glue_1.3.1       jsonlite_1.6     httr_1.4.0       rvest_0.3.2      xml2_1.2.0       purrr_0.3.2
# [9] dplyr_0.8.1      tidyr_0.8.3.9000
#
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.1          pillar_1.4.1        compiler_3.5.3      prettyunits_1.0.2   remotes_2.0.2.9000  tools_3.5.3
# [7] zeallot_0.1.0       packrat_0.5.0       pkgbuild_1.0.3      tibble_2.1.1        pkgconfig_2.0.2     rlang_0.3.4.9003
# [13] cli_1.1.0           rstudioapi_0.10     curl_3.3            withr_2.1.2         hms_0.4.2           vctrs_0.1.0.9003
# [19] rprojroot_1.3-2     tidyselect_0.2.5    R6_2.4.0            processx_3.3.0      fansi_0.4.0         callr_3.2.0
# [25] selectr_0.4-1       magrittr_1.5        backports_1.1.4     ps_1.3.0            ellipsis_0.1.0.9000 assertthat_0.2.1
# [31] utf8_1.1.4          stringi_1.4.3       crayon_1.3.4

# ---------- initial links -----------------------------------------------------

base_url <- "https://topverdict.com/lists"
base_url2 <- "https://topverdict.com"

# ---------- scrape website ----------------------------------------------------

# grab the years from the website
years <-  read_html(base_url) %>%
  html_nodes("section.category-list") %>%
  html_nodes("div") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_sub(8) %>%
  as.numeric()

top_verdicts <- tibble(year = years)

top_verdicts <- top_verdicts %>%
  # get links for each state
  mutate(state_links = map(year, ~ read_html(glue(base_url,"/", .x)) %>%
                             html_nodes("section.category-list") %>%
                             html_nodes("div") %>%
                             html_nodes("a") %>%
                             html_attr("href"))) %>%
  unnest(state_links) %>%
  # within each state, get list of all the lists for that state
  mutate(state_lists = map(state_links, ~ read_html(glue(base_url2, .x)) %>%
                             html_nodes("tr") %>%
                             html_nodes("a") %>%
                             html_attr("href"))) %>%
  # pull the title of that list
  mutate(title = map(state_links, ~ read_html(glue(base_url2, .x)) %>%
                             html_nodes("tr") %>%
                             html_nodes("a") %>%
                             html_text(trim = TRUE))) %>%
  unnest(c(state_lists, title))

saveRDS(top_verdicts, "top_verdicts.rds")

# given links to all the lists, now pull the tables with the data we want
top_verdicts_wtable <- top_verdicts %>%
  mutate(tables = map(state_lists, ~ read_html(glue(base_url2, .x)) %>%
                             html_table(trim = TRUE, fill = TRUE)))

# fix issue in /lists/2017/florida/50-settlements - what should be #10 is labeled as #20
top_verdicts_wtable$tables[top_verdicts_wtable$state_lists == "/lists/2017/florida/50-settlements"][[1]][[1]][46,] <- c(10,10)

# clean the tables
# there is a hack with all the str_detect to handle ties in rank. Right now all ties (or shared, split, etc) are thrown out
# this is not ideal, but "pivot_wide" doesn't like duplicate ranks and I don't have time to figure it out now
top_verdicts_wtable_clean <- top_verdicts_wtable %>%
  mutate(tables = map(tables, ~ as_tibble(.x[[1]]) %>%
                        mutate(X1 = if_else(!is.na(as.numeric(X1)), "rank", X1)) %>%
                        mutate(X1 = if_else(str_detect(X1, "tie"), "rank2", X1)) %>%
                        mutate(X1 = if_else(str_detect(X1, "shared"), "rank2", X1)) %>%
                        mutate(X1 = if_else(str_detect(X1, "split"), "rank2", X1)) %>%
                        mutate(X1 = if_else(str_detect(X1, "96 - 100"), "rank2", X1)) %>%
                        mutate(rank = if_else(X1 == "rank", as.numeric(X2), NA_real_)) %>%
                        # mutate(rank = if_else(X1 == "rank2", as.numeric(str_sub(X2, 1, 3)), rank)) %>%
                        mutate(rank = if_else(X1 == "rank2", as.numeric(0), rank)) %>%
                        fill(rank, .direction = "down") %>%
                        filter(rank != 0) %>%
                        filter(!(X1 %in% c("rank", "rank2"))) %>%
                        mutate(X1 = tolower(str_replace(X1, ":",""))) %>%
                        pivot_wider(names_from = X1, values_from = X2))) %>%
  unnest(tables) %>%
  mutate(amount = as.numeric(str_replace_all(str_replace_all(amount, "[,$]", ""),",",""))) %>%
  mutate(state2 = map_chr(str_split(state_links, "/"), 4)) %>%
  select(-X2, -`NA`)

# write out tibbles to .rds and .csv
saveRDS(top_verdicts_wtable, "top_verdicts_wtable.rds")
saveRDS(top_verdicts_wtable_clean, "top_verdicts_wtable_clean.rds")
readr::write_excel_csv(top_verdicts_wtable_clean, "top_verdicts.csv")

