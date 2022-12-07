library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

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
  jail_pop_years <- incarceration %>%
    group_by(year) %>%
    summarize(total_jail_pop_years = sum(total_jail_pop, na.rm = T)) 
return(jail_pop_years)   
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

# This function selects the black jail rate and the total population
black_pop_in_jail <- function() {
  incarceration %>%
    select(year, black_jail_pop) %>%
    drop_na() %>%
    return(year, black_jail_pop)
}

# This function plots the chart of the correction of black jail population and their counties' total population
plot_black_pop_in_jail <- function() {
  black_plot <- ggplot(data = black_pop_in_jail(), aes(x = year, y = black_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Black Jail Population")
  return(black_plot)
}
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# plots a map of the proportion of black people jail population in LA
# See Canvas
#----------------------------------------------------------------------------#

# Step 1: libraries
library(maps)

# Step 2: Get most recent assignment_data year from dataset
recent_pop <- incarceration %>%
  select(year, fips, state,county_name, black_jail_pop) %>%
  filter(year == max(year))

# Step 3: Use map_data function to join the 'assignment_data' dataset with the map_data for county
data_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Step 4: Merge map data and filtered jail data together
map_data <- data_shape %>%
  left_join(recent_pop, by = "fips")

# Step 5: Incorporate blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background =  element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
  )

# Step 6: Create the map
jail_map_black <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Black Jailing Population")
## Load data frame ---- 


