#' Use this file to store your data collection functions

library(httr2)
library(jsonlite)
library(rvest)
library(tidyverse)


# Collect from Transport for London API, Accident Stats from 2019
get_tfl_accident_stats <- function() {
  
  url <- "https://api.tfl.gov.uk/AccidentStats/2019"
  
  response <- request(url) %>% 
    req_auth_bearer_token("c6a81dc38e4242dd8b16f65ee76d9206") %>% 
    req_perform()
  
  body <- response %>% 
    resp_body_json()
  
  write_json(body, "data/raw/tfl_accident_stats.json")
}



# Scrape population by borough of London statistics
get_borough_population <- function() {
  
  url <- "http://www.citypopulation.de/en/uk/greaterlondon/"
  
  html <- read_html(url)
  
  borough_table <- html_elements(html, "table.data#ts") %>% 
    html_table()
  
  write_csv(borough_table[[1]], "data/raw/borough_population.csv")
}
