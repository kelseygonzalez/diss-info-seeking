pacman::p_load(tidyverse, here)

#load Full Data
clean_data <- read_rds(here('data', 'qualtrics_fullsurvey_clean_2021-12-11.rds')) %>% 
  mutate(across(where(is.factor), as.character))


### Load Batch rejection data ###
mturk_batches <-
  mutate(read_csv('data/Batch_1_results.csv', show_col_types = FALSE), batch = 1) %>% 
  bind_rows(mutate(read_csv('data/Batch_2_results.csv', show_col_types = FALSE), batch = 2)) %>% 
  bind_rows(mutate(read_csv('data/Batch_3_results.csv', show_col_types = FALSE), batch = 3)) %>% 
  bind_rows(mutate(read_csv('data/Batch_4_results.csv', show_col_types = FALSE), batch = 4)) %>% 
  bind_rows(mutate(read_csv('data/Batch_5_results.csv', show_col_types = FALSE), batch = 5)) %>% 
  filter(AssignmentStatus == "Rejected")

### Remove Rejected Data ###

to_factor <- c('int_sought',
               'int_sought_useful',
               'int_sought_affect',
               'vacc_status',
               'vacc_status_future',
               'gender', 
               'race',
               'educ',
               'income',
               'employment',
               'hispanic')

clean_data <- clean_data %>% 
  anti_join(mturk_batches, by= c("MID" = "WorkerId")) %>% 
  select(ResponseId, MID, EndDate,
         receive_i_fr = Q170,             
         receive_i_fr_oth = Q170_13_TEXT,
         int_sought = Q171,
         int_sought_fr = Q172,       
         int_sought_fr_oth = Q172_8_TEXT,      
         int_sought_what = Q173,        
         int_sought_useful = Q174,        
         int_sought_affect = Q175,            
         vacc_status = Q179,             
         vacc_status_future = Q180,
         age, gender, race, race_5_TEXT, hispanic, educ, income, employment) %>% 
  
  # Clean up variable types 
  mutate(across(to_factor, as_factor),
         vacc_status = case_when(vacc_status == "Yes" ~ TRUE,
                                 vacc_status == "No" ~ FALSE,
                                 TRUE ~ NA)) %>%
  
  # race,  cleaning
  mutate(race_white   = str_detect(race, 'White'),
         race_black   = str_detect(race, 'Black'),
         race_nat     = str_detect(race, 'Native'),
         race_asian   = str_detect(race, 'Asian'),
         race_other   = ifelse(str_detect(race, "Other"), race_5_TEXT, ""),
         race_refused = str_detect(race, 'Prefer'),
         across(race_white:race_refused, ~ replace_na(.x, FALSE)))  %>% 
  
  # gender & income, cleaning
  mutate(gender = fct_other(gender, keep = c('Male', 'Female')),
         income = fct_recode(income, NULL = "Don't know"),
         income = fct_relevel(income, 
                              c("Under $1,000",
                                "$10,000 to $19,999",
                                "$20,000 to $29,999",
                                "$30,000 to $39,999",
                                "$40,000 to $49,999",
                                "$50,000 to $59,999",
                                "$60,000 to $74,999",
                                "$90,000 to $109,999",
                                "$110,000 to $129,999",
                                "$130,000 to $149,999",
                                "$150,000 or over"))) %>% 
  
  
  # educ, cleaning
  mutate(college = case_when(educ %in% c('Graduate or professional degree', 
                                         "Bachelor's degree",
                                         "Associate's or Technical degree",
                                         'Some college') ~ TRUE,
                             educ %in% c('High school graduate', 
                                         "Less than high school") ~ FALSE)) %>% 
  
  # received information from,  cleaning
  mutate(receive_i_fr_pers = str_detect(receive_i_fr, 'friend, neighbor, or family member'),
         receive_i_fr_onlnet = str_detect(receive_i_fr, 'social networking site'),
         receive_i_fr_onlgroup = str_detect(receive_i_fr, 'online discussion group'),
         receive_i_fr_tv = str_detect(receive_i_fr, 'television news channel'),
         receive_i_fr_dr = str_detect(receive_i_fr, 'doctor or other health professional'),
         receive_i_fr_other =  ifelse(str_detect(receive_i_fr, "Other"), receive_i_fr_oth, ""),
         across(receive_i_fr_pers:receive_i_fr_dr, ~ replace_na(.x, FALSE))) %>% 
  
  #  information sought from,  cleaning
  mutate(int_sought_fr_pers = str_detect(int_sought_fr, 'friend, neighbor, or family member'),
         int_sought_fr_dr = str_detect(int_sought_fr, 'doctor or another health professional'),
         int_sought_fr_onlgroup = str_detect(int_sought_fr, 'online discussion group'),
         int_sought_fr_onlnet = str_detect(int_sought_fr, 'social networking site'),
         int_sought_fr_onlsearch = str_detect(int_sought_fr, 'Google'),
         int_sought_fr_other =  ifelse(str_detect(int_sought_fr, "Other"), int_sought_fr_oth, ""),
         across(int_sought_fr_pers:int_sought_fr_onlsearch, ~ replace_na(.x, FALSE))) %>% 
  
  select(ResponseId, MID, EndDate, age, gender, hispanic, starts_with("race_"), -race_5_TEXT, educ, college, income, employment,
         receive_i_fr_dr, receive_i_fr_pers, receive_i_fr_tv, 
         receive_i_fr_onlnet, receive_i_fr_onlgroup, receive_i_fr_other,
         int_sought,       
         int_sought_fr_pers, int_sought_fr_dr, int_sought_fr_onlgroup,
         int_sought_fr_onlnet, int_sought_fr_onlsearch, int_sought_fr_other,
         int_sought_what, int_sought_useful,int_sought_affect, vacc_status, vacc_status_future) %>% 
  mutate(vacc_view_pos = case_when(vacc_status == TRUE ~ TRUE,
                                   (vacc_status == FALSE & vacc_status_future == "Definitely not")     ~ FALSE,
                                   (vacc_status == FALSE & vacc_status_future == "Probably not")       ~ FALSE,
                                   (vacc_status == FALSE & vacc_status_future == "Might or might not") ~ FALSE,
                                   (vacc_status == FALSE & vacc_status_future == "Probably yes")       ~ TRUE,
                                   (vacc_status == FALSE & vacc_status_future == "Definitely yes")     ~ TRUE) ) %>% 
  
         rowwise() %>%  
  mutate(across(receive_i_fr_dr:receive_i_fr_onlgroup, ~ifelse(.x == TRUE, 1, 0)), 
         receive_i = max(receive_i_fr_dr:receive_i_fr_onlgroup),
         receive_i = ifelse(receive_i_fr_other != '', 1, receive_i),
         across(receive_i_fr_dr:receive_i_fr_onlgroup, as.logical)) 


change_to_dich <- c(
  'receive_i_fr_dr',
  'receive_i_fr_pers',
  'receive_i_fr_tv',
  'receive_i_fr_onlnet',
  'receive_i_fr_onlgroup',
  'int_sought',
  'int_sought_fr_pers',
  'int_sought_fr_dr',
  'int_sought_fr_onlgroup',
  'int_sought_fr_onlnet',
  'int_sought_fr_onlsearch',
  'int_sought_affect',
  'vacc_status',
  'vacc_view_pos',
  'race_white',
  'race_black',
  'race_nat',
  'race_asian', 
  'hispanic',
  'college')

clean_data <- clean_data %>% 
  mutate(across(all_of(change_to_dich), ~ case_when(is.na(.x) ~ NA_real_,
                                     .x == 'Yes' ~ 1, 
                                     .x == TRUE ~ 1,
                                     TRUE ~ 0)))


write_rds(clean_data, here('data', 'RAiN_vacc_clean.rds'))

