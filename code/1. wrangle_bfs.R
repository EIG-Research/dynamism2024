# wrangle county and national BFS data

rm(list = ls())


library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

user_path = "ENTER USER PATH HERE"

project_path = file.path(user_path, "/dynamism2024")
data_path = file.path(project_path, "data")
output_path = file.path(project_path, "output")

##########
# NATION #
##########

national_files = list.files(file.path(data_path,"BFS", "national"), full.names = TRUE)

national_list = list()

for(file_name in national_files) {
  
  # extract industry name
  industry_name = read_excel(file_name)[2,3]
  industry_name = sub("Total for All NAICS: ", "", toString(industry_name))
  
  # read in file
  file = read_excel(file_name,
                    skip=6)
  
  # apply industry as reported high-propensity business applications column name
  names(file) = c("Period",industry_name)
  
  # append industry's applications to full list
  national_list[[file_name]] = file
}


bfs_nation = bind_cols(national_list) %>%
  
  # clean up names
  rename(date = Period...1,
         `Total Applications` = `U.S. Total...38`,
         `Total High Propensity Applications` = `U.S. Total...18`) %>%
  select(-c(contains("Period"))) %>%
  mutate(year = as.numeric(substr(date, 5,8)),
         month = substr(date, 1,3))


# save file.
save(bfs_nation, file = file.path(output_path, "bfs_nation.RData"))


##########
# STATES #
##########

state_files =  list.files(file.path(data_path,"BFS", "state"), full.names = TRUE)
state_list = list()


for(file_name in state_files) {
  
  # extract state name
  state_name = read_excel(file_name)[2,3]
  state_name = sub("Total for All NAICS: ", "", toString(state_name))
  
  # read in file
  file = read_excel(file_name,
                    skip=6)
  
  # apply state name to be high propensity business applications column name
  names(file) = c("Period",state_name)

  # append data
  state_list[[file_name]] = file
  
}

bfs_states = bind_cols(state_list) %>%
  
  # clean up names
  rename(date = Period...1) %>%
  select(-c(contains("Period"))) %>%
  mutate(year = as.numeric(substr(date, 5,8)),
         month = substr(date, 1,3))


# save file.
save(bfs_states, file = file.path(output_path, "bfs_states.RData"))
