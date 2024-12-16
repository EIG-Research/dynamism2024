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
# working out of drive

user_path = "/Users/sarah/Documents/GitHub"

project_path = file.path(user_path, "/dynamism2024")
data_path = file.path(project_path, "data")
output_path = file.path(project_path, "output")

#######
# BFS #
#######

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


print("High propensity applications low in 2024, in July")
  
  bfs_nation %>% filter(year == 2024 & month == "Jul") %>%
      select(`Total High Propensity Applications`)

  
print("High propensity applications in November 2024")

  bfs_nation %>% filter(year == 2024 & month == "Nov") %>%
    select(`Total High Propensity Applications`)



################################################################################
# statistics for section 2 in report - BFS industry
  
print("In how many industries are applications higher in 2024 than 2019")  
  vals = names(bfs_nation)[2:23]
  vals = vals[-c(9,19)]

  bfs_nation %>%
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
    )) %>%
    ungroup() %>% group_by(`higher in 2024 than 2019`) %>%
    count()
  
    

print("Retail trade high propensity applications Nov 2024")
    
  bfs_nation %>% filter(year==2024 & month=="Nov") %>%
    select("Retail Trade: U.S. Total")

  
print("Retail trade as a % of all high propensity applications in 2024")

  bfs_nation %>% filter(year==2024) %>%
    summarise(`Retail Trade: U.S. Total` = sum(as.numeric(`Retail Trade: U.S. Total`), na.rm=TRUE),
              `Total High Propensity Applications` = sum(as.numeric(`Total High Propensity Applications`), na.rm=TRUE)) %>%
    mutate(`Retail trade % of total` = 100*(`Retail Trade: U.S. Total`)/(`Total High Propensity Applications`)) %>%
    select(`Retail trade % of total`)

  
print("highest industries as a % of high propensity applications in 2024")

          # total high propensity 2024
          high_propensity = bfs_nation %>% filter(year==2024) %>%
            mutate(`Total High Propensity Applications` =as.numeric(`Total High Propensity Applications`)) %>%
            summarise(val = sum(`Total High Propensity Applications`, na.rm=TRUE))
          high_propensity = high_propensity$val

    bfs_nation %>% filter(year==2024) %>%
      ungroup() %>%
      
      # pivot longer using states
      pivot_longer(cols = names(bfs_nation)[2:23]) %>%
      ungroup() %>% group_by(year, name) %>%
      summarise(value = sum(as.numeric(value), na.rm=TRUE)) %>%
      arrange(desc(value)) %>%
      mutate(`share of high propensity` = 100*value/high_propensity)

    
print("Increase in healthcare & social service high propensity applications last 2 years Nov 2022 - Nov 2024")

  bfs_nation %>%
    filter((year==2024 & month == "Nov") | (year==2022 & month=="Nov")) %>%
    select(year, `Health Care and Social Assistance: U.S. Total`) %>%
    
    mutate(`Health Care and Social Assistance: U.S. Total` = 
             as.numeric(`Health Care and Social Assistance: U.S. Total`)) %>%
    
    mutate(growth =100*(`Health Care and Social Assistance: U.S. Total` - lag(`Health Care and Social Assistance: U.S. Total`))/lag(`Health Care and Social Assistance: U.S. Total`))

  
print("Increase in healthcare & social service high propensity applications Feb 2018-Feb 2020")

  bfs_nation %>%
    filter((year==2020 & month=="Feb") | (year==2018 & month=="Feb")) %>%
    select(month, `Health Care and Social Assistance: U.S. Total`) %>%
    mutate(`Health Care and Social Assistance: U.S. Total` = as.numeric(`Health Care and Social Assistance: U.S. Total`)) %>%
    
    mutate(growth =100*(`Health Care and Social Assistance: U.S. Total` - lag(`Health Care and Social Assistance: U.S. Total`))/lag(`Health Care and Social Assistance: U.S. Total`))
  

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

paste("% of states that saw a decline in high propensity applications 2023-2024")
  decline/51*100
  
paste("State with the largest decline 2023-2024, & rate:")
decline_df_2023_2024 %>%
      arrange(change_2023_2024)

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
  
  
paste("Births since Q3 2021 (thousands):")

  bdm_nation %>% filter(year>=2021) %>%
    summarise(sum(estab_births))

  
paste("Births between Q3 2017 and Q1 2020 (thousands):")
  bdm_nation %>%
    filter((period=="September" & year==2017) | 
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
    
  
paste("Georgia’s increase in establishment births: Q1 2020 was, Q1 2024 was")

    bdm_states %>%
      filter(Year==2024 | Year==2020) %>% filter(quarter == "March") %>%
      ungroup() %>%
      select(Year, state, estab_births) %>%
      pivot_wider(names_from = Year,
                  values_from = estab_births) %>%
      filter(state=="ga")


paste("Oregon’s drop: Q1 2020 was , Q1 2024 was")

  bdm_states %>%
    filter(Year==2024 | Year==2020) %>% filter(quarter == "March") %>%
    ungroup() %>%
    select(Year, state, estab_births) %>%
    pivot_wider(names_from = Year,
                values_from = estab_births) %>%
    filter(state=="or")


paste("Connecticut’s drop:  Q1 2020 was , Q1 2024 was")

  bdm_states %>%
    filter(Year==2024 | Year==2020) %>% filter(quarter == "March") %>%
    ungroup() %>%
    select(Year, state, estab_births) %>%
    pivot_wider(names_from = Year,
                values_from = estab_births) %>%
    filter(state=="ct")

  
########
# BTOS #
########  
  # note--- ALL BTOS STATS CAN BE ESTIAMTED BY LOOKING AT THE FIGURES.
  
  