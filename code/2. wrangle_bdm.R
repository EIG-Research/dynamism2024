
# read in Business Employment Dynamics data from the BLS
# and prepare for analysis

rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

user_path = "/Users/sarah/Documents/GitHub"

project_path = file.path(user_path, "/dynamism2024")
data_path = file.path(project_path, "data")
output_path = file.path(project_path, "output")

##########
# STATES #
##########

file_names = list.files(
  file.path(data_path, "BDM", "states"), pattern = "*.txt", full.names = TRUE)

state_list = list()

for (file_name in file_names) {
  
  # read in state data
  state = gsub(paste0(data_path, "/BDM/states/"), "", file_name)
  state = gsub("_table9.txt", "", state)
  print(state)
  
  
  state_df = read_delim(file_name, delim = " ",
                        skip = 7) %>%
    
    # handle issues with reading .txt file.
    mutate(quarter = str_trim(gsub("NA", "",
                                   paste(`3`, `Establishments...7`))),
           state = state) %>%
    mutate(`combined` = paste(`...19`, `Employment...20`),
           combined = gsub("NA", "", combined),
           combined = gsub("\\s+", " ", str_trim(combined))) %>%
    
    separate(combined, c("estab_births",
                         "empl_births", 
                         "estab_deaths",
                         "empl_deaths"),
             " ") %>%
    
    # convert strings to numeric
    mutate(across(c("estab_births",
                    "empl_births", 
                    "estab_deaths", 
                    "empl_deaths"),
                  ~as.numeric(gsub("[^[:digit:]]","",.)))) %>%
    select(Year, quarter, state, 
           "estab_births",
           "empl_births", 
           "estab_deaths", 
           "empl_deaths") %>%
    
    fill(Year) %>%
    filter(quarter != "for available")
  
  
  # add to full state list
  state_list[[file_name]] = state_df

}

bdm_states = bind_rows(state_list)

# save output
save(bdm_states, file = file.path(output_path, "bdm_states.RData"))


############
# NATIONAL #
############

bdm_nation = read.delim(
  file.path(data_path, "BDM", "nation", "table9_1.txt"), 
  skip=7) %>%
  mutate(Year..3.months.ended..Establishments.....Employment...Establishments.....Employment = 
           gsub("\\s+", " ", 
                Year..3.months.ended..Establishments.....Employment...Establishments.....Employment)) %>%
  separate(Year..3.months.ended..Establishments.....Employment...Establishments.....Employment,
           c("year","period", "estab_births","empl_births", "estab_deaths","empl_deaths"), " ")%>%
  mutate(across(c("estab_births",
                  "empl_births", 
                  "estab_deaths", 
                  "empl_deaths"),
                ~as.numeric(gsub("[^[:digit:]]","",.))),
         year = ifelse(year=="", NA, year)) %>%
  fill(year, .direction="down") %>% 
  mutate(empl_deaths = ifelse(is.na(empl_deaths), 0, empl_deaths),
        estab_deaths = ifelse(is.na(estab_deaths), 0, estab_deaths)) %>%na.omit()

save(bdm_nation, file = file.path(output_path, "bdm_nation.RData"))
