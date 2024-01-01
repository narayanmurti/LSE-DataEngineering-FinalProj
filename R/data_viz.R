#' Use this file to store functions that produce your plots

library(ggplot2)
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(forcats)

tfl_db <- dbConnect(SQLite(), "data/tidy/tfl.db")

accidents_tbl <- tbl(tfl_db, "accidents")
casualties_tbl <- tbl(tfl_db, "casualties")
population_tbl <- tbl(tfl_db, "borough_population")


#### Big Picture ####

# Accidents per borough plot
accidents_per_borough <- function(accidents_tbl) {
  ggplot(accidents_tbl %>%
           
           # Note: for these two related graphs, we omit City of London on the 
           # basis of it being an outlier due to abnormally small land area 
           # compared to large foot traffic
           filter(borough != "City of London"), 
         aes(y=fct_rev(fct_infreq(borough)))) +
    
    geom_bar(aes(fill=severity)) +
    
    scale_fill_manual(values = c("Slight" = "#2E86C1", 
                                 "Serious" = "#F39C12", 
                                 "Fatal" = "#B03A2E"),
                      name = "Severity") +
    
    scale_x_continuous(breaks = seq(0, 3000, 1000),
                       labels = scales::unit_format(unit="K", scale = 1e-3)) +
    
    labs(title = "Gross London Transportation Accidents in 2019",
         subtitle = "City of Westminster leads the pack by over 1,000 cases.",
         x = "Number of accidents",
         y = "Borough") +
    
    theme_minimal() +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.title.x = element_text(margin = margin(t=10)),
          axis.title.y = element_text(margin = margin(t=0)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}



# Accidents per borough per capita
accidents_per_borough_per_capita <- function(accidents_tbl, population_tbl) {
  plot_df <- accidents_tbl %>% 
    group_by(borough) %>% 
    count() %>%
    inner_join(population_tbl) %>%
    mutate("accidents_per_capita" = (as.double(n)/as.double(population))) %>%
    select(borough, accidents_per_capita) %>%
    
    # Note: removed City of London borough row because its low population & size 
    # relative to its high foot traffic skewed this graph considerably.
    filter(borough != "City of London")
  
  
  ggplot(plot_df, aes(y=fct_reorder(borough, accidents_per_capita), 
                      x=accidents_per_capita)) +
    
    geom_col(aes(fill = accidents_per_capita)) +
    
    scale_fill_gradient(low = "#FDEDEC",
                        high = "#CB4335") +
    
    labs(title = "Per Capita London Transportation Accidents in 2019",
         subtitle = "City of Westminister also leads the pack in accidents per capita.",
         x = "Accidents Per Capita",
         y = "Borough") +
    
    guides(fill = FALSE) +
    
    theme_minimal() +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.title.x = element_text(margin = margin(t=10)),
          axis.title.y = element_text(margin = margin(t=0)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}



# Accident numbers over the whole year
accidents_over_year <- function(accidents_tbl) {
  plot_df <- accidents_tbl %>% 
    collect() %>% 
    mutate(date = as_datetime(date)) %>%
    mutate(date = date(date)) %>%
    group_by(date) %>%
    count()
  
  ggplot(plot_df, aes(x=date, y=n)) +
    
    geom_col(fill = "#2E86C1") +
    
    geom_smooth(method = "gam", level = 0.95, color = "#A569BD") +
    
    scale_x_date(date_breaks="1 month", date_labels="%B") +
    
    labs(title = "November Saw the Most Accidents Per Month in 2019",
         x = "Month (2019)",
         y = "Accidents") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}




# Interpretation: drivers or passengers of cars experienced the most amount of 
# casualties (injuries) over other modes of transportation
casualties_by_mode <- function(casualties_tbl) {
  
  ggplot(casualties_tbl, aes(y=fct_rev(fct_infreq(mode)))) +
    
    geom_bar(aes(fill=severity)) +
    
    scale_fill_manual(values = c("Slight" = "#2E86C1", 
                                 "Serious" = "#F39C12", 
                                 "Fatal" = "#B03A2E"),
                      name = "Severity") +
    
    scale_x_continuous(breaks = seq(5000, 25000, 5000),
                       labels = scales::unit_format(unit="K", scale = 1e-3)) +
    
    labs(title = "Cars are the leading mode by which casualties (injuries) occur on TfL systems",
         x = "Casualties",
         y = "Mode of Transport") +
    
    theme_minimal() + 
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}





# Casualties by mode, only serious or fatal severity
casualties_by_mode_serious <- function(casualties_tbl) {
  
  ggplot(casualties_tbl %>%
           filter(severity != "Slight"), 
         aes(y=fct_rev(fct_infreq(mode)))) +
    
    geom_bar(aes(fill=severity)) +
    
    scale_fill_manual(values = c("Serious" = "#F39C12", 
                                 "Fatal" = "#B03A2E"),
                      name = "Severity") +
    
    scale_x_continuous(breaks = seq(1000, 3000, 1000),
                       labels = scales::unit_format(unit="K", scale = 1e-3)) +
    
    labs(title = "Pedestrians experience the most amount of serious or fatal injuries over other modes of transport",
         x = "Casualties (injuries)",
         y = "Mode of Transport") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}





#### Further Exploratory ####

accidents_per_hour_severity <- function(accidents_tbl) {
  plot_df <- accidents_tbl %>% 
    collect() %>% 
    mutate(date = as_datetime(date)) %>%
    mutate(hour = hour(date)) %>%
    group_by(hour)
  
  
  ggplot(plot_df, aes(x=hour)) +
    
    geom_bar(aes(fill = severity)) +
    
    annotate('segment', 
             x= seq(0, 23),
             y = 0, 
             xend = seq(0, 23),
             yend = 4204, 
             
             alpha = 0.2) +

    scale_y_continuous(
      limits = c(-1000, 4204),
      expand = c(0, 0)) +
    
    scale_x_continuous(breaks = seq(0, 23)) +
    
    labs(title = "Most Accidents Occer Between 3-6pm",
         subtitle = "Accidents also spike at 8am",
         x = "Hour",
         y = "") +
    
    theme_minimal() +
    theme(plot.title = element_text(size = rel(1.8),
                                    color="#5B2C6F"),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_blank()) +
    
    coord_polar() 
}





# Age range for everyone, filled by mode
casualty_age_and_mode <- function(accidents_tbl, casualties_tbl) {
  
  plot_df <- accidents_tbl %>%
    
    inner_join(casualties_tbl, 
               by = c("id" = "accidentId")) %>%
    
    select(borough, 
           age, 
           "severity" = severity.x, 
           mode) %>%
    
    mutate(age_range = as.character(cut(age,
                                        breaks = c(0, 10, 20, 30, 40, 50,
                                                   60, 70, 80, 90, 100),
                                        labels = c("0-10", "11-20",
                                                   "21-30", "31-40",
                                                   "41-50", "51-60",
                                                   "61-70", "71-80",
                                                   "81-90", "91-100"))))
  
  
  ggplot(plot_df) +
    
    geom_bar(aes(x = age_range, 
                 fill = mode)) +
    
    guides(fill = guide_legend(title = "Mode of Transport")) +
    
    labs(title = "Casualties by Age",
         subtitle = "21-30 year olds experience the highest number of transport accidents.",
         x = "Age",
         y = "Accidents") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}






# Age range for City of Westminster, severe or fatal cases,
# filled by mode
westminster_age_and_mode <- function(accidents_tbl, casualties_tbl) {
  
  plot_df <- accidents_tbl %>% 
    inner_join(casualties_tbl, by = c("id" = "accidentId")) %>%
    select(borough, age, "severity" = severity.x, mode) %>%
    filter(borough == "City of Westminster",
           severity != "Slight") %>%
    mutate(age_range = as.character(cut(age,
                                        breaks = c(0, 10, 20, 30, 40, 50,
                                                   60, 70, 80, 90, 100),
                                        labels = c("0-10", "11-20",
                                                   "21-30", "31-40",
                                                   "41-50", "51-60",
                                                   "61-70", "71-80",
                                                   "81-90", "91-100"))))
  
  ggplot(plot_df) +
    
    geom_bar(aes(x = age_range, 
                 fill = mode)) +
    
    guides(fill = guide_legend(title = "Mode of Transport")) +
    
    labs(title = "City of Westminster Severe Casualties by Age",
         subtitle = "Significantly more pedestrian injuries than car-driver injuries when compared to the London-wide statistics, especially for older travelers.",
         x = "Age",
         y = "Accidents") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
  
}





# 3: age range for Harrow, which had lowest accidents per capital of all.
harrow_age_and_mode <- function(accidents_tbl, casualties_tbl) {
  
  plot_df <- accidents_tbl %>% 
    inner_join(casualties_tbl, by = c("id" = "accidentId")) %>%
    select(borough, age, "severity" = severity.x, mode) %>%
    filter(borough == "Harrow") %>%
    mutate(age_range = as.character(cut(age,
                                        breaks = c(0, 10, 20, 30, 40, 50,
                                                   60, 70, 80, 90, 100),
                                        labels = c("0-10", "11-20",
                                                   "21-30", "31-40",
                                                   "41-50", "51-60",
                                                   "61-70", "71-80",
                                                   "81-90", "91-100"))))
  
  
  ggplot(plot_df) +
    
    geom_bar(aes(x = age_range, 
                 fill = mode)) +
    
    guides(fill = guide_legend(title = "Mode of Transport")) +
    
    labs(title = "Harrow Casualties by Age",
         subtitle = "More evenly spread distribution throughout the age ranges, expected proportion of car-driver injuries",
         x = "Age",
         y = "Accidents") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}




# 4: age range for Westminster, but only car or pedestrian 
# (which we saw were the most common)

westminster_only_car_pedestrian <- function(accidents_tbl, casualties_tbl) {
  
  plot_df <- accidents_tbl %>% 
    inner_join(casualties_tbl, by = c("id" = "accidentId")) %>%
    select(borough, age, "severity" = severity.x, mode) %>%
    filter(mode == "Pedestrian" | mode == "Car") %>%
    mutate(age_range = as.character(cut(age,
                                        breaks = c(0, 10, 20, 30, 40, 50,
                                                   60, 70, 80, 90, 100),
                                        labels = c("0-10", "11-20",
                                                   "21-30", "31-40",
                                                   "41-50", "51-60",
                                                   "61-70", "71-80",
                                                   "81-90", "91-100"))))
  
  ggplot(plot_df) +
    
    geom_bar(aes(x = age_range, 
                 fill = mode)) +
    
    scale_fill_manual(values = c("Car" = "#de8e00",
                                 "Pedestrian" = "#00bce8")) +
    
    labs(title = "City of Westminster Car or Pedestrian Casualties by Age",
         subtitle = "Older travelers experience a higher proportion of pedestrian injuries, but middle-aged travelers experience more driving injuries.",
         x = "Age",
         y = "Accidents") +
    
    theme_minimal() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          plot.title = element_text(size=rel(1.6), color="#5B2C6F"))
}






