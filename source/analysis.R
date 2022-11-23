library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#

# Average growth of the total jail population rate from 1970 to 2018
avg_total_jail_pop_rate <- incarceration %>%
  mutate(state_county = paste0(state, ", ", county_name)) %>%
  select(year, state_county, total_jail_pop_rate) %>%
  drop_na() %>%
  group_by(state_county) %>%
  filter(year == max(year) | year == min(year)) %>%
  spread(key = year, value = total_jail_pop_rate) %>%
  ungroup() %>%
  summarize(rate_diff = `2018` - `1970`) %>%
  summarize(avg_total_jail_pop_rate = mean(rate_diff, na.rm = T)) %>%
  pull(avg_total_jail_pop_rate)

# Average total jail population rate on 2018
avg_rate_total_jail_pop <- incarceration %>%
  filter(year == max(year)) %>%
  summarize(avg_rate_total_jail_pop = mean(total_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate_total_jail_pop)

# Average black people jail population rate on 2018
avg_rate_black_jail_pop <- incarceration %>%
  filter(year == max(year)) %>%
  summarize(avg_rate_black_jail_pop = mean(black_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate_black_jail_pop)

# Average white people jail population rate on 2018
avg_rate_white_jail_pop <- incarceration %>%
  filter(year == max(year)) %>%
  summarize(avg_rate_white_jail_pop = mean(white_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate_white_jail_pop)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# use the necessary functions to plot the bar chart of total jail population per year
#----------------------------------------------------------------------------#
# This function only shows the total jail population per year (1970 to 2018)
get_year_jail_pop <- function() {
  total_jail_pop_years <- incarceration %>%
    group_by(year) %>%
    summarize(total_jail_pop_years = sum(total_jail_pop, na.rm = T)) 
return(total_jail_pop_years)   
}

# This function plots a chart of total jail population increases by year (1970 to 2018) in the America
plot_jail_pop_for_us <- function()  {
    title <- "The increasing Jail Population in U.S. (1970-2018)"
    caption <- "The data comes from Incarceration Trends Dataset by Vera Institute of Justice"
    legend_x <- "Year"
    legend_y <- "Total Jail Population"
    plot <- get_year_jail_pop() %>%
      ggplot() +
      geom_col(mapping = aes(x = year, y = total_jail_pop_years)) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = legend_x,
        y = legend_y,
        title = title,
        caption = caption
      )
  return(plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# use the necessary functions to plot the line chart of total jail population at least one more but fewer than 10 states per year
# See Canvas
#----------------------------------------------------------------------------#

# This function filter out the data of the given states.
get_jail_pop_by_states <- function(states) {
  jail_pop_states <- incarceration %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(total_jail_pop_states = sum(total_jail_pop, na.rm = T))
  return(jail_pop_states)
}

# This function plots the line chart of total jail population at least one more but fewer than 10 states per year.
plot_jail_pop_by_states <- function(states) {
  title <- "The increases of Jail Population in States in the U.S. (1970-2018)"
  caption <- "The data comes from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "Year"
  legend_y <- "Total Jail Population"
  
  plot <- get_jail_pop_by_states(states) %>%
    ggplot() +
    geom_line(mapping = aes(x = year, y = total_jail_pop_states, group = state, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = legend_x,
      y = legend_y,
      title = title,
      caption = caption
    )
  return(plot)
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Proportion of black jail population versus their counties population
# See Canvas
#----------------------------------------------------------------------------#

# This function selects the black jail rate and the total population on 2018
get_prop_black_jail_pop_vs_county_pop <- function() {
  prop_black_jail_pop_vs_county_pop <- incarceration %>%
    filter(year == max(year)) %>%
    select(black_jail_pop_rate, total_pop) %>%
    drop_na() %>%
  return(prop_black_jail_pop_vs_county_pop)
}

# This function plots the chart of the correction of black jail population and their counties' total population
plot_prop_black_jail_pop_vs_county_pop <- function() {
  title <- "Black Jail Population Versus County Population"
  caption <- "The data comes from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "County Population"
  legend_y <- "Proportion of Black Jail Population"
  
  plot <- get_prop_black_jail_pop_vs_county_pop %>%
    ggplot() +
    geom_bar(mapping = aes(x = total_pop, y = black_jail_pop_rate), stat="identity", fill="red", width = 1, alpha=0.7) +
    geom_smooth(mapping = aes(x = total_pop, y = black_jail_pop_rate)) +
    labs(
      x = legend_x,
      y = legend_y,
      title = title,
      caption = caption
    )
  return(plot)
}
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# plots a map of the proportion of black people jail population in LA
# See Canvas
#----------------------------------------------------------------------------#

# This function selects black people jail rate in LA
get_prop_black_jail_pop_la <- function() {
  prop_la <- incarceration %>%
    filter(year == max(year), state == "LA") %>%
    mutate(prop_la = black_jail_pop / total_jail_pop) %>%
    select(prop_la)
  return(prop_la)
}

# This function plots the proportion of black people jail population in LA
plot_map_la <- function() {
  title <- "Black People Jail Population rate in LA on 2018"
  caption <- "The data comes from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "total jail population"
  legend_y <- "black jail population"
  
  plot <- get_prop_black_jail_pop_la() %>%
    ggplot() +
    geom_polygon(
      mapping = aes(x = total_jail_pop, y = black_jail_pop, fill = prop_la),
      color = "brown",
      linewidth = .5
    ) +
    coord_map() +
    scale_fill_continuous(low = "green", high = "yellow") +
    labs(
      fill = "Proportion of Black People Jail Population",
      title = title,
      caption = caption,
      x = legend_x,
      y = legend_y,
    ) +
    theme_minimal()
  return(plot)
}
## Load data frame ---- 


