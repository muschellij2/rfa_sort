library(rvest)
library(lubridate)
library(tibble)
library(dplyr)
library(DT)
url = "https://grants.nih.gov/grants/guide/search_results.htm?scope=rfa&year=active"

doc = read_html(url)
bad_string = "ZZZZZ"
doc <- read_html(gsub("<br>", bad_string, doc))
nodes = html_nodes(doc, xpath = "//div[ @class='l-main-wrapper']//table//table")

tabs = html_table(nodes, fill = TRUE)
keep_tab = sapply(tabs, function(x) {
  cn = colnames(x)
  any(grepl("^Announcement", cn))
})
stopifnot(sum(keep_tab) == 1)

tab = tabs[[which(keep_tab)]]
cn = colnames(tab)
cn = gsub(bad_string, "\n", cn)
cn = gsub("\\s+", " ", cn)
colnames(tab) = cn
tab = tibble::as_tibble(tab)

tab = tab %>% 
  mutate_at(vars(contains("Date")), mdy)

gg = sapply(lapply(tab, grepl, pattern = bad_string), any)
tab[gg] = lapply(tab[gg], gsub, pattern = bad_string, " ")

DT::datatable(tab, filter = "top",
              options = list(
                autoWidth = TRUE,
                paging = FALSE,
                pageLength = 30))
