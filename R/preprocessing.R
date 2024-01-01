#' Add functions that automate your pre-processing steps here
library(tidyverse)
library(DBI)
library(RSQLite)
library(dbplyr)
library(lubridate)
library(jsonlite)

# Read in raw accident stats json file
accident_stats <- read_json("data/raw/tfl_accident_stats.json")

# Create database for my three tables
tfl_db <- dbConnect(drv = RSQLite::SQLite(),
                 "data/tidy/tfl.db") 




# Create first table containing important accident info without nested lists
preprocess_accidents_table <- function() {
  
  selected_cols <- c("id", "lat", 
                     "lon", "date", 
                     "severity", "borough")
  
  get_row <- function(list_item) {
    
    # Select rows
    row <- list_item[selected_cols]
    
    # Treat any NULL cases
    null_elements <- sapply(row, is.null)
    row[null_elements] <- NA
    
    df <- as_tibble(row)
    return(df)
  }
  
  # Create entire df
  df_accidents <- lapply(accident_stats, get_row) %>% 
    bind_rows() %>% 
    unnest(cols = c(id, lat, lon, date, severity, borough))
  
  # Change the `date` column into datetime format using lubridate
  df_accidents <- df_accidents %>%
    mutate(date = ymd_hms(date))
  
  # Copy into tfl.db
  copy_to(tfl_db, df_accidents, "accidents",
          types = c(id = "int",
                    lat = "double",
                    lon = "double",
                    date = "datetime",
                    severity = "varchar(7)",
                    borough = "varchar(22)"),
          unique = list("id"),
          temporary = FALSE, overwrite = TRUE)
}





# Create second table containing casualties information
preprocess_casualties_table <- function() {
  
  selected_casualties <- c("age", "class", 
                           "severity", "mode")
  
  
  get_casualty <- function(list_item) {
    
    # Select the accidentId which I will need to add into the table
    accident_id <- list_item[["id"]]
    
    # Select casualties nested list
    casualties_list <- list_item[["casualties"]]
    
    # lapply to this list
    df <- lapply(casualties_list, function(casualties_list_item) {
      
      row <- casualties_list_item[selected_casualties]
      
      # Treat any NULL cases
      null_elements <- sapply(row, is.null)
      row[null_elements] <- NA
      
      # Ensure colnames are correct in case of an NA result
      row <- setNames(row, selected_casualties)
      
      # Create each set of rows, being sure to add accidentId as well
      tibble <- as_tibble(row)
      tibble["accidentId"] <- accident_id
      
      return(tibble)
    }) %>% 
      bind_rows()
    
    return(df)
  }
  
  # Now bind tegether the entire df
  df_casualties <- lapply(accident_stats, get_casualty) %>%
    bind_rows() %>%
    unnest(cols = c(age, class, severity, mode, accidentId)) %>%
    
    # Create the casualties id primary index
    mutate(id = row_number()) %>%
    select(id, accidentId, everything()) %>%
    
    # Add spaces into the `mode` column to make more readable
    mutate(mode = gsub("([a-z]?)([A-Z])", 
                       "\\1 \\2", 
                       mode, 
                       perl = TRUE)) %>%
    mutate(mode = sub(" ", "", mode))
  
  # Copy into tfl.db
  copy_to(tfl_db, df_casualties, "casualties",
          types = c(id = "int",
                    accidentId = "int",
                    age = "tinyint",
                    class = "varchar(10)",
                    severity = "varchar(7)",
                    mode = "varchar(20)"),
          unique = list("id"),
          temporary = FALSE, overwrite = TRUE)
}





# Read in raw borough population csv
borough_population <- read_csv("data/raw/borough_population.csv")

# Create third table containing borough population info
preprocess_borough_population_table <- function() {
  
  # Filter the desired columns and remove the Greater London row.
  borough_population <- borough_population %>%
    select("borough" = "Name", "population" = "PopulationCensus2021-03-21") %>%
    filter(borough != "Greater London")
  
  # Now add this df into tfl.db
  
  copy_to(tfl_db, borough_population, "borough_population",
          types = c(borough = "varchar(22)",
                    population = "mediumint"),
          unique = list("borough"),
          temporary = FALSE, overwrite = TRUE)
}
