library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary
#----------------------------------------------------------------------------#

# Function finds mean male jail population for 2018 across all counties
male_jail_pop_2018 <- function() {
  mean_male <- incarceration %>%
  filter(year == 2018) %>%
  summarize(mean_male_pop = round(mean(male_jail_pop, na.rm = TRUE)))
  return(mean_male)
}

# Function finds mean female jail population for 2018 across all counties
female_jail_pop_2018 <- function() {
  mean_female <- incarceration %>%
    filter(year == 2018) %>%
    summarize(mean_female_pop = round(mean(female_jail_pop, na.rm = TRUE)))
  return(mean_female)
}

# Function finds county with highest male jail population in 2018
county_highest_male <- function() {
    highest_male <- incarceration %>%
      filter(year == 2018) %>%
      filter(male_jail_pop == max(male_jail_pop, na.rm = TRUE)) 
    return(highest_male$county_name[1])
}

# Function finds county with highest female jail population in 2018
county_highest_female <- function() {
  highest_female <- incarceration %>%
    filter(year == 2018) %>%
    filter(female_jail_pop == max(female_jail_pop, na.rm = TRUE)) 
  return(highest_female$county_name[1])
}

# Function finds change in male jail population in 2018 compared to 1970
male_change <- function() {
  male_1970 <- incarceration %>%
    filter(year == 1970) %>%
    summarize(total_1970 = round(sum(male_jail_pop, na.rm = TRUE)))
  male_2018 <- incarceration %>%
    filter(year == 2018) %>%
    summarize(total_2018 = round(sum(male_jail_pop, na.rm = TRUE)))
  return(male_2018 - male_1970)
}

# Function finds change in female jail population in 2018 compared to 1970
female_change <- function() {
  female_1970 <- incarceration %>%
    filter(year == 1970) %>%
    summarize(total_1970 = round(sum(female_jail_pop, na.rm = TRUE)))
  female_2018 <- incarceration %>%
    filter(year == 2018) %>%
    summarize(total_2018 = round(sum(female_jail_pop, na.rm = TRUE)))
  return(female_2018 - female_1970)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# Function returns a dataframe of the total jail population by year from 1970-2018
get_year_jail_pop <- function() {
  pop_year_df <- incarceration %>%
    group_by(year) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE))
return(pop_year_df)   
}

# Function plots a bar chart of the total jail population by year from 1970-2018
plot_jail_pop_for_us <- function()  {
  bar <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "Jail Populations in U.S by Year from 1970-2018") +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5))
  return(bar)   
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# Function takes vector of states and summarizes total jail population by year for each state fro 1970-2018
get_jail_pop_by_states <- function(states) {
  pop_state_df <- incarceration %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE), .groups = "drop")
  return(pop_state_df)
}

# Function takes vector of states and constructs a line graph of the total jail population by year for each state
plot_jail_pop_by_states <- function(states) {
  line <- ggplot(data = get_jail_pop_by_states(states), aes(x = year, y = total, group = state)) +
    geom_line(aes(linetype = state, color = state)) +
    geom_point(aes(shape = state, color = state)) +
    labs(title = "Increase of Jail Population in U.S. States (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "Jail Populations in U.S States by Year from 1970-2018")  +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) 
  return(line)
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# Male vs Female Incarceration in the U.S.
#----------------------------------------------------------------------------#

# Function gets total female and male jail populations by year from 1970-2018
get_female_male_pop <- function() {
  num <- incarceration %>%
    filter(!is.na(male_jail_pop) & !is.na(female_jail_pop)) %>%
    select(male_jail_pop, female_jail_pop) 
  return(num)
}

# Function plots bar chart of total female and male jail populations by year from 1970-2018
plot_get_female_male_pop <- function() {
  scatter_comparison <- ggplot(data = get_female_male_pop()) +
    expand_limits(y = c(0, 20000)) +
    geom_point(mapping = aes(x = male_jail_pop, y = female_jail_pop), color = "blue", alpha = 0.3) +
    guides(fill = guide_legend(title="Gender")) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    labs(title = "Male vs Female Jail Population in U.S. (1970-2018)",
         x = "Male Jail Population",
         y = "Female Jail Population",
         caption = "Jail Populations Comparison by Gender in the U.S from 1970-2018") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) 
  return(scatter_comparison)
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# Incarceration Proportions by State
#----------------------------------------------------------------------------#

# Function produces the mean proportion of population incarcerated by state from 1970-2018
get_prop_incarcerated_pop <- function() {
  incarceration$state <- tolower(state.name[match(incarceration$state,state.abb)])
  prop_jail_and_county_pop <- incarceration %>%
    filter(!is.na(total_pop) & !is.na(total_jail_pop)) %>%
    group_by(county_name) %>%
    summarize(mean_county_pop = sum(total_pop, na.rm = TRUE) / n(), mean_jail_pop = sum(total_jail_pop, na.rm = TRUE) / n(), state = state, .groups = "drop") %>%
    mutate(prop = mean_jail_pop / mean_county_pop)%>%
    group_by(state) %>%
    summarize(prop_by_state = sum(prop) / n())
  return(prop_jail_and_county_pop)
}

# Function creates map with shading in relation to proportion of population incarcerated by state from 1970-2018
plot_get_prop_incarcerated_pop <- function() {
  state_map <- map_data("state") %>%
    rename(state = region) %>%
    left_join(get_prop_incarcerated_pop(), by = "state")
  map <- ggplot(state_map) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = prop_by_state),
                 color = "white",
                 size = 0.1) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Green") +
    labs(title = "Proportion of Population Incarcerated (1970-2018)",
         caption = "Proportion of State Population Incarcerated 1970-2018") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title="Proportions")) +
    theme(axis.line= element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank())
  return(map) 
}

## Load data frame ---- 


