rm(list=ls())

## Comment out one of the below lines as needed; adjust range of years in line 55 accordingly
# xfilename <- "2017_031" ## 2017
# xfilename <- "2016_031" ## 2016
# xfilename <- "2015_031" ## 2015
xfilename <- "2014_031" ## 2014
# xfilename <- "2013_031" ## 2013

#================================================================================
require(tidyverse)
df <- readxl::read_excel(paste("data/",xfilename,".xlsx",sep="")
                         , skip=1) %>% ### skip=1 for 2014 and 2013 data
  rename(field_time = `Field of study and time to degree`) %>% # Rename the conjunctive column to something simpler
  filter(!is.na(field_time)) %>% 
  mutate( # Clean up weird names with superscripts
    field_time = case_when(field_time == "Otherc" ~ "Other",
                           field_time == "Life sciencesb" ~ "Life sciences",
                           field_time == "Since starting doctoral programa" ~ "Since starting doctoral program",
                            TRUE ~ field_time
                           ))

# Manually grabbed the broad fields (based on indentation)
major_fields <- c("Life sciences", 
                  "Physical sciences and earth sciences", 
                  "Mathematics and computer sciences",
                  "Psychology and social sciences", 
                  "Engineering",
                  "Education",
                  "Humanities and arts",
                  "Other",
                  "All fields")

# Manually grabbed time to degree
times_to_degree <- c("Since bachelor's",
                    "Since starting graduate school",
                    "Since starting doctoral program")


# Un-cross the first column which right now has time to degree AND field in one. Create new columns based on the matching of major and time to degree variables
df <- df %>% 
  mutate(time_to_degree = case_when(field_time %in% times_to_degree ~ field_time,
                                TRUE ~ NA_character_),
  major_field = case_when(field_time %in% major_fields ~ field_time,
                            TRUE ~ NA_character_))

# Use tidyr::fill() to fill in the repeats of each major/broad field
df_time <- df %>% 
  fill(major_field, .direction = "down") %>% 
  fill(time_to_degree, .direction = "down") %>%
  filter(!field_time %in% major_fields) ## Now take out the rows under field_time that have no values (since they're headers), 
                                        ## which happen to be the major_fields

# Gather the years, remove the commas, and rename to appropriate columns
df_clean <- df_time %>% 
  gather(year, md_years, `1990`:`2015`) %>% ## First gather the to-be-created vars on the left
  mutate(year = factor(parse_number(year)),
         md_years = parse_number(md_years)) %>% ## readr::parse_number() turns the character values into numeric values
  # rename(field = Field) %>% 
  select(major_field = major_field, time_to_degree, year, md_years)

# # Check to confirm numbers match
# df_clean %>% 
#   group_by(major_field, year) %>% 
#   summarize(sum(md_years, na.rm = TRUE))

# Write to .csv
df_clean  %>%
  write_csv(paste(xfilename,".csv",sep=""))
