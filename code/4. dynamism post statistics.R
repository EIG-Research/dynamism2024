# all statistics cited in the post

rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(readxl)
library(ggplot2)
library(readr)

# set project paths

user_path = "/Users/sarah/Documents/GitHub"

  project_path = file.path(user_path, "dynamism2024")
  data_path = file.path(project_path, "data")
  output_path = file.path(project_path, "output")

  
  
########################
# BFS (sections 1 - 3) #
########################

# read in data - national and state level. constructed in wrangle_bfs.R
load(file.path(output_path, "bfs_nation.RData"))
load(file.path(output_path, "bfs_states.RData"))


################################################################################
# statistics for section 1 in report - BFS national

print("total business applications in 2024")

  bfs_nation %>% filter(date == "Nov-2024") %>%
    select(`Total Applications`)

  
print("total high-propensity business applications in 2024")

  bfs_nation %>% filter(date == "Nov-2024") %>%
    select(`Total High Propensity Applications`)

  
print("% change in business application filing since 2023, excluding December 2023")

  bfs_nation %>%
    filter(year == 2023 | year==2024) %>% filter(month!="Dec") %>%
    
    select(year, `Total Applications`) %>%
    
    ungroup() %>% group_by(year) %>% 
    
    summarise(`Total Applications` = sum(as.numeric(`Total Applications`))) %>%
    pivot_wider(names_from = year, values_from = `Total Applications`) %>%
    mutate(`change year over year` = 100*(`2024` - `2023`)/`2023`)

print("% change in high-propensity business application filing since 2023, excluding December 2023")
  
  bfs_nation %>%
    filter(year == 2023 | year==2024) %>% filter(month!="Dec") %>%
    
    select(year, `Total High Propensity Applications`) %>%
    
    ungroup() %>% group_by(year) %>% 
    
    summarise(`Total High Propensity Applications` = sum(as.numeric(`Total High Propensity Applications`))) %>%
    pivot_wider(names_from = year, values_from = `Total High Propensity Applications`) %>%
    mutate(`change year over year` = 100*(`2024` - `2023`)/`2023`)
  
  
print("Monthly average high-propensity applications in 2024")
  
  bfs_nation %>% filter(year ==2024) %>% ungroup() %>%
      mutate(`Total High Propensity Applications` = as.numeric(`Total High Propensity Applications`)) %>%
      summarise(`Monthly mean high propensity` = 
                  mean(`Total High Propensity Applications`,na.rm=TRUE))


print("Business applications in January 2024")

  bfs_nation %>% filter(year == 2024 & month == "Jan") %>%
    select(`Total Applications`)

  
print("Business applications in March 2024")

  bfs_nation %>% filter(year ==2024 & month == "Mar") %>%
    select(`Total Applications`)


print("High propensity applications low in 2024, and month of low")

bfs_nation %>% filter(year == 2024 ) %>% 
  filter(`Total High Propensity Applications` == min(`Total High Propensity Applications`)) %>%
  
  select(month, `Total High Propensity Applications`)

  
print("High propensity applications in November 2024")

  bfs_nation %>% filter(year == 2024 & month == "Nov") %>%
    select(`Total High Propensity Applications`)



################################################################################
# statistics for section 2 in report - BFS industry
  
  print("In how many industries are applications lower in 2024 than 2019?")
  
  vals = names(bfs_nation)[2:23]
  vals = vals[-c(9,19)]
  
  change_2019_2024_indsturies = bfs_nation %>%
    filter(year==2024 | year==2019) %>%
    filter(month !="Dec") %>%
    select(-c("Total High Propensity Applications", "Total Applications")) %>%
    select(-c(date, month)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(across(vals,
                  ~as.numeric(.))) %>%
    summarise(across(vals, sum)) %>%
    pivot_longer(cols = vals) %>%
    mutate(value = as.numeric(value)) %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(`higher in 2024 than 2019` = case_when(
      `2024` > `2019` ~ "True",
      `2024` <= `2019` ~ "False"
    ),
    `2024 - 2019` = 100*(`2024` - `2019`)/`2019`)
  
  change_2019_2024_indsturies %>%
    ungroup() %>% group_by(`higher in 2024 than 2019`) %>%
    count()
  
  # but which industries?
  change_2019_2024_indsturies %>% filter(`higher in 2024 than 2019` == "False")
  
  
  # growth for retail trade, healthcare and social assistance, accommodation and food services, and construction from 2019-2024
  bfs_nation %>% 
    filter(year == 2019 | year == 2024) %>% filter(month != "Dec") %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(healthcare = mean(as.numeric(`Health Care and Social Assistance: U.S. Total`)),
              retail_trade = mean(as.numeric(`Retail Trade: U.S. Total`)),
              food_service = mean(as.numeric(`Accommodation and Food Services: U.S. Total`)),
              construction = mean(as.numeric(`Construction: U.S. Total`))) %>%
    mutate(healthcare_perc = 100*(healthcare - lag(healthcare))/lag(healthcare),
           food_perc = 100*(food_service - lag(food_service))/lag(food_service),
           construction_perc = 100*(construction - lag(construction))/lag(construction),
           retail_perc = 100*(retail_trade - lag(retail_trade))/lag(retail_trade))
  
  # these 4 industries make up what % of total apps in 2024?
      
    change_2019_2024_indsturies %>%
      ungroup() %>%
      mutate(total_2024 = sum(`2024`),
             share_2024 = `2024`/total_2024) %>%
      filter(name %in% c(
        "Health Care and Social Assistance: U.S. Total",
        "Retail Trade: U.S. Total",
        "Accommodation and Food Services: U.S. Total",
        "Construction: U.S. Total"
      )) %>%
      summarise(sum(share_2024))


################################################################################
# statistics for section 3 in report - BFS states

paste("Number of states that saw a decline in high propensity applications 2023-2024")

    decline_df_2023_2024 = bfs_states %>%
      filter(year == 2023 | year==2024) %>% 
      filter(month !="Dec") %>% #remove December of 2019 for comparison
      
      select(-c(date, month)) %>%
      
      # pivot using state names
      pivot_longer(cols = names(bfs_states)[2:52],
                   names_to = "state", values_to = "applications") %>%
      
      ungroup() %>%
      group_by(year, state) %>%
      
      # total applications by state
      summarise(applications = sum(as.numeric(applications))) %>%
      pivot_wider(names_from = year, values_from = applications) %>%
      mutate(change_2023_2024 = 100*(`2024` - `2023`)/ `2023`)
      
        
      decline = count(decline_df_2023_2024 %>% filter(change_2023_2024<0))
      decline


paste("Top 3 states in terms of high propensity applications:")
    
    bfs_states %>%
      filter(year==2024) %>% filter(month!="Dec") %>%
      select(-c(date, month)) %>%
      
      # pivot using state names
      pivot_longer(cols = names(bfs_states)[2:52],
                   names_to = "state", values_to = "applications") %>%
      
      ungroup() %>%
      group_by(year, state) %>%
      
      # total applications
      summarise(applications = sum(as.numeric(applications))) %>%
      arrange(desc(applications))
    
paste("Texas overtook New York for #3 spot in # of high propensity application filings in 2024 relative to 2019")
    
    decline_df_2019_2024 = 
      bfs_states %>%
        filter(year == 2019 | year==2024) %>% 
        filter(month !="Dec") %>% #remove December of 2019 for comparison
        select(-c(date, month)) %>%
        pivot_longer(cols = names(bfs_states)[2:52],
                     names_to = "state", values_to = "applications") %>%
        
        ungroup() %>%
        group_by(year, state) %>%
        summarise(applications = sum(as.numeric(applications))) %>%
        pivot_wider(names_from = year, values_from = applications) %>%
        mutate(change_2019_2024 = 100*(`2024` - `2019`)/ `2019`)

    # sort to show ranking
        decline_df_2019_2024 %>%
          arrange(desc(`2019`))
        
        decline_df_2019_2024 %>%
          arrange(desc(`2024`))

    
paste("Texas high propensity applications in 2019 (excl. December)")

  bfs_states %>%
    filter(year==2019 & month!="Dec") %>%
    summarise(Texas = sum(as.numeric(Texas)))

  
paste("Texas high  propensity applications in 2024")

  bfs_states %>%
    filter(year==2024 & month!="Dec") %>%
    summarise(Texas = sum(as.numeric(Texas)))

paste("West Virginia increase in high propensity business applications 2019-2024")
  bfs_states %>%
    filter(month !="Dec") %>% filter(year==2019 | year==2024) %>%
    group_by(year) %>%
    summarise(`West Virginia` = sum(as.numeric(`West Virginia`))) %>%
    mutate(change = 100*(`West Virginia` - lag(`West Virginia`))/lag(`West Virginia`))


paste("Number of states that more than doubled high propensity applications:")
  
  decline_df_2019_2024 %>%
    filter(`2024` > 2*`2019`)
    
paste("Number of states that had > 50% increase in applications")
  count(decline_df_2019_2024 %>%
    filter(change_2019_2024 >50))
  
  
# cleaning
  rm(decline, decline_df_2019_2024, decline_df_2023_2024)

  
  
#######
# BDM #
#######
  
load(file.path(output_path, "bdm_nation.RData"))
load(file.path(output_path, "bdm_states.RData"))
  

################################################################################
# statistics for section 4 in report - BDM national
  
paste("Establishment births Q1 2024 (thousands):")
  bdm_nation %>% filter(year=="2024" & period=="March") %>% select(estab_births)
  
paste("Pre-pandemic trend in establishment births; trend line 1994 - 2019, (excluding 2008-2011), thousands")

  # rounding to nearest 100.
  bdm_nation %>%
    filter((year< 2008 | year > 2011) & year < 2020 ) %>%
    ungroup() %>%
    summarise(mean(estab_births))
  
  
paste("Births since Q2 2021 (thousands):")

  bdm_nation %>% filter(year >= 2021) %>%
    summarise(sum(estab_births))
  4434 - 308
  
paste("Births between Q3 2017 and Q1 2020 (thousands):")
  bdm_nation %>%
    filter((period == "June" & year ==2017) |
              (period=="September" & year==2017) | 
             (period=="December" & year==2017) | 
             (year >=2018 & year <2020) | 
             (period == "March" & year==2020)) %>%
    summarise(sum(estab_births))
    

################################################################################
# statistics for section 5 in report - BDM states
  
paste("States that had more establishment births in Q1 2024 versus Q1 2020")
  
  count(bdm_states %>%
          filter(state !="pr") %>%
    filter(Year==2024 | Year==2020) %>% filter(quarter == "March") %>%
    ungroup() %>%
    select(Year, state, estab_births) %>%
    pivot_wider(names_from = Year,
                values_from = estab_births) %>%
    filter(`2024`>`2020`))
  
  
  # correlations in figures document.
