# business dynamism in 2024; charts and stats

rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(readr)

# set project paths

user_path = "/Users/sarah/Documents/GitHub"

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


# Fig 5:
# relative to pre-pandemic trends

fig5 = bdm_states %>%
  mutate(date = as.Date(paste(quarter, "1", Year), 
                        format = "%b %d %Y"),
         state = toupper(state)) %>%
  filter(date %in% c("2023-06-01", "2023-09-01", "2023-12-01", "2024-03-01",
                     "2019-03-01", "2019-06-01", "2019-09-01", "2019-12-01")) %>%
  
  mutate(period = case_when(
    Year < 2021 ~ "pre",
    Year > 2021 ~ "post"
  )) %>%
  group_by(state, period) %>%
  summarise(estab_births = sum(estab_births)) %>%
  pivot_wider(names_from = period, values_from = estab_births, id_cols = state) %>%
  mutate(perc_change_births = 100*(post-pre)/pre,
         net_change_births = post-pre)
  

write.csv(fig5, paste(output_path, "figures", "fig5.csv",sep="/"))


################################################################################
# BTOS - what might the future look like? how are employers reporting their current
# performance and future performance?

# btos date ids.
btos_date_varlist = c("202424", "202423",
                      "202422", "202421", "202420",
                      "202419", "202418", "202417",
                      "202416", "202415", "202414",
                      "202413", "202412", "202411",
                      "202410", "202409", "202408",
                      "202407", "202406", "202405",
                      "202404", "202403", "202402",
                      "202401", "202326", "202325",
                      "202324", "202323", "202322",
                      "202321", "202320", "202319" )

    btos = read_excel(
      paste(
        data_path,
        "BTOS",
        "National.xlsx",
        sep="/"
      ),
      sheet = "Response Estimates"
    )

    
# load in the cross-walk from date id's and actual date values.
    btos_date_key = read_excel(
      paste(
        data_path,
        "BTOS",
        "National.xlsx",
        sep="/"
      ),
      sheet = "Collection and Reference Dates"
    ) %>%
      select(Smpdt, `Ref End`) %>% na.omit()

    
    
# Fig 6.  future performance (nation level)
    
    fig6 = btos %>%
      filter(`Question ID` == 17) %>%
      select(-c(`Question ID`, `Question`, `Answer ID`)) %>%
      pivot_longer(cols = btos_date_varlist,
                   names_to = "Smpdt") %>%
      pivot_wider(names_from = Answer, values_from = value) %>%
      
      # extract reference period end
      left_join(btos_date_key, by = "Smpdt") %>%
      select(-c(Smpdt))
    
    # save output
    write.csv(fig6, paste(output_path, "figures", "fig6.csv", sep="/"))
    
    
    
    
# Fig 7. by industry
    fig7 = read_excel(
      paste(
        data_path,
        "BTOS",
        "Sector.xlsx",
        sep="/"
      ),
      sheet = "Response Estimates"
    ) %>%
      
      filter(`Question ID` == 17) %>% # future expectations
      
      filter(Sector %in% c("23", # construction
                           "44", "45", # retail trade
                           "48", "49", # transportation and warehousing
                           "72", # accommodation and food service
                           "62")) # healthcare and social assistance
    
    unique(fig7$Sector)
    
    fig7 = fig7 %>%
      mutate(`Sector Description` = case_when(
        Sector == "23" ~ "Construction",
        Sector == "44" ~ "Retail trade",
        Sector == "48" ~ "Transportation and warehousing",
        Sector == "62" ~ "Accommodation and food service",
        Sector == "72" ~ "Healthcare and social assistance",
      )) %>%
      select(-c(Sector, `Question ID`, `Question`, `Answer ID`)) %>%
      
      pivot_longer(cols = btos_date_varlist,
                   names_to = "Smpdt") %>% 
      
      left_join(btos_date_key, by = "Smpdt") %>%
      select(-c(Smpdt)) %>%
      
      pivot_wider(names_from = "Answer",
                  values_from = "value",
                  id_cols = c("Sector Description", "Ref End")) %>%
      
      mutate(above_average = as.numeric(gsub("%", "", `Excellent`)) + 
               as.numeric(gsub("%", "", `Above average`))) %>%
      select(`Sector Description`, `Ref End`, above_average)%>%
      pivot_wider(names_from = `Sector Description`, values_from = `above_average`)
    
    write.csv(fig7, paste(output_path, "figures", "fig7.csv",sep="/"))
    


# Fig 8. by firm size

  fig8 = read_excel(
    paste(
      data_path,
      "BTOS",
      "Employment Size Class.xlsx",
      sep="/"
    ),
    sheet = "Response Estimates"
  ) %>%
    filter(`Question ID` == 17,
    ) %>%
    mutate(`Employee Size` = case_when(
      Empsize == "A" ~ "1-4",
      Empsize == "B" ~ "5-9",
      Empsize == "C" ~ "10-19",
      Empsize == "D" ~ "20-49",
      Empsize == "E" ~ "50-99",
      Empsize == "F" ~ "100-149",
      Empsize == "G" ~ "200+",
    )) %>%
    
    select(-c(`Question ID`, `Question`, `Answer ID`, Empsize)) %>%
    pivot_longer(cols = btos_date_varlist,
                 names_to = "Smpdt") %>% 
    left_join(btos_date_key, by = "Smpdt") %>%
    select(-c(Smpdt)) %>%
    pivot_wider(names_from = "Answer",
                values_from = "value",
                id_cols = c("Employee Size", "Ref End")) %>%
    mutate(above_average = as.numeric(gsub("%", "", `Excellent`)) + 
             as.numeric(gsub("%", "", `Above average`))) %>%
    select(`Employee Size`, `Ref End`, above_average)%>%
    pivot_wider(names_from = `Employee Size`, values_from = `above_average`)
  
  write.csv(fig8, paste(output_path, "figures", "fig8.csv",sep="/"))
  
