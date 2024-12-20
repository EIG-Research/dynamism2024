# business dynamism in 2024; charts and stats

rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(readr)
library(tidycensus)

# set project paths

user_path = "ENTER USER PATH HERE"

project_path = file.path(user_path, "/dynamism2024")
data_path = file.path(project_path, "data")
output_path = file.path(project_path, "output")


#################################################################################

#######
# BFS #
#######

# read in data - national and state level. constructed in wrangle_bfs.R
load(file.path(output_path, "bfs_nation.RData"))
load(file.path(output_path, "bfs_states.RData"))


# Fig 1: national trend chart
    fig1 = bfs_nation %>%
      select(date,
             `Total High Propensity Applications`,
             `Total Applications`)
    
    # saving as a .csv for datawrapper 
    write.csv(fig1,
              paste(output_path, "figures", "fig1.csv",sep="/"))
    

    
# Fig 2. industries
    fig2 = bfs_nation %>%
      select(date,
             `Construction: U.S. Total`,
             `Retail Trade: U.S. Total`,
             `Transportation and Warehousing: U.S. Total`,
             `Accommodation and Food Services: U.S. Total`,
             `Health Care and Social Assistance: U.S. Total`)
        
    write.csv(fig2,
              paste(output_path, "figures", "fig2.csv",sep="/"))

    
# Fig 3. states

    # change 2019 to 2024.
    fig3 = bfs_states %>%
      filter(year == 2019 | year==2024) %>% 
      filter(month !="Dec") %>% #remove December of 2019 for comparison
      select(-c(date, month))
    
    fig3 = fig3 %>%
      
      pivot_longer(cols = names(fig3)[1:51],
                   names_to = "state", values_to = "applications") %>%
      
      ungroup() %>%
      group_by(year, state) %>%
      summarise(applications = sum(as.numeric(applications))) %>%
      pivot_wider(names_from = year, values_from = applications) %>%
      mutate(change_2019_2024 = 100*(`2024` - `2019`)/ `2019`,
             net_change_2019_2024 = `2024`-`2019`)
     
     
write.csv(fig3,
          paste(output_path, "figures", "fig3.csv",sep="/"))

    # clean up
    rm(fig1, fig2, fig3)

################################################################################
# BDM - business deaths versus births

load(file.path(output_path, "bdm_nation.RData"))
load(file.path(output_path, "bdm_states.RData"))
    

# Fig 4: nation total births, deaths.
    
fig4 = bdm_nation %>%
  mutate(date = as.Date(paste(period, "1", year), 
                        format = "%b %d %Y"),
         estab_births = estab_births*1000,
         estab_deaths = estab_deaths*1000)

write.csv(fig4,
          paste(output_path, "figures", "fig4.csv",sep="/"))


# Fig 6: establishment birth map by states
# Q2 2023 - Q1 2024
# Q2 2019 - Q1 2020

    fig6 = bdm_states %>%
      mutate(date = as.Date(paste(quarter, "1", Year), 
                            format = "%b %d %Y"),
             state = toupper(state)) %>%
      filter(date %in% c("2023-06-01", "2023-09-01", "2023-12-01", "2024-03-01",
                         "2019-06-01", "2019-09-01", "2019-12-01", "2020-03-01")) %>%
      
      mutate(period = case_when(
        Year < 2021 ~ "pre",
        Year > 2021 ~ "post"
      )) %>%
      
      group_by(state, period) %>%
      summarise(estab_births = sum(estab_births)) %>%
      
      pivot_wider(names_from = period, 
                  values_from = estab_births, 
                  id_cols = state) %>%
      
      mutate(perc_change_births = 100*(post-pre)/pre,
             net_change_births = post-pre) %>%
      rename(state_abrev = state) %>%
      mutate(state = state.name[match(state_abrev, state.abb)]) # for Figure 5 merging 

    write.csv(fig6,
              paste(output_path, "figures", "fig6.csv",sep="/"))
    

# Fig 5:
  # estabishment births assuming a 1 year lag --
        # Q2 2022 - Q1 2023
        # Q2 2018 - Q1 2019
  
    fig5 = bfs_states %>%
      mutate(keep = case_when(
        year==2022 & month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ~ 1,
        year == 2023 & month %in% c("Jan", "Feb", "Mar") ~ 1,
        
        year == 2018 & month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ~ 1,
        year == 2019 & month %in% c("Jan", "Feb", "Mar") ~ 1,
        TRUE ~ 0
      )) %>%
      filter(keep ==1) %>%
      
      mutate(period = case_when(
        year < 2021 ~ "pre",
        year > 2021 ~ "post")) %>%
      select(-c(year, month, date))
    
    
    fig5 = fig5 %>%
      ungroup() %>%
      mutate(across(names(fig5)[1:51],
                    ~ as.numeric(.))) %>% na.omit()
    
    fig5 = fig5 %>%
      group_by(period) %>%
      pivot_longer(cols = names(fig5)[1:51],
                   names_to = "state", values_to = "applications") %>%
      
      ungroup() %>%
      group_by(period, state) %>%
      summarise(applications = sum(as.numeric(applications))) %>%
      pivot_wider(names_from = period, values_from = applications) %>%
      mutate(perc_change_apps = 100*(post-pre)/pre,
             net_change_apps = post-pre)
    
  
           fig5 = merge(fig6, fig5, by = "state")
    
    
      # add in state population

          state_pop =  get_acs(
            geography = "state",
            survey = "acs5",
            variables = "B01003_001",  # Total population
            year = 2023
          ) %>%
            mutate(state = state.abb[match(NAME, state.name)])
          
  
          fig5 = left_join(fig5, state_pop, by = c("state_abrev" = "state"))
    
    
    write.csv(fig5, 
              paste(output_path, "figures", "fig5.csv",sep="/"))
