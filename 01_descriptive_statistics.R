
############################################################################
######### Preambule  #######################################################
############################################################################

install.packages("tidyverse")
install.packages("here")
library(haven)
library(tidyverse)
afrobarometer_release_dataset_mor_r8_en_2023_03_01 <- read_sav("C:/Users/ASUS/Downloads/afrobarometer_release-dataset_mor_r8_en_2023-03-01.sav")
View(afrobarometer_release_dataset_mor_r8_en_2023_03_01)

t1 <- afrobarometer_release_dataset_mor_r8_en_2023_03_01 %>% 
  mutate(Q39A = case_when(
    Q39A ==1~ "Strongly disagree",
    Q39A ==2 ~ "Disagree",
    Q39A ==3 ~ "Neither agree nor disagree",
    Q39A ==4 ~ "Agree",
    Q39A ==5 ~ "Strongly agree",
    Q39A ==8 ~ "Refused",
    Q39A ==9 ~ "Don't know",
    Q39A ==-1 ~ "Missing"
  ) %>% 
    structure(label = Hmisc::label(afrobarometer_release_dataset_mor_r8_en_2023_03_01$Q39A))) %>% 
  
  # Selecting relevant variables
  select(Q39A) %>% 
  
  # Generate a summary table using "tbl_summary" for the specified columns
  tbl_summary(
    
    type = c(Q39A~"categorical"),
    
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "(Missing)",
    digits = everything() ~ c(0,0),
  ) %>% 
  
  # Modify the table header to provide a descriptive label
  modify_header(label ~ "") %>%
  add_n()%>% 
  bold_labels()


t2 <- afrobarometer_release_dataset_mor_r8_en_2023_03_01 %>% 
  mutate(Q39A = case_when(
    Q39A ==1~ "Strongly disagree",
    Q39A ==2 ~ "Disagree",
    Q39A ==3 ~ "Neither agree nor disagree",
    Q39A ==4 ~ "Agree",
    Q39A ==5 ~ "Strongly agree",
    Q39A ==8 ~ "Refused",
    Q39A ==9 ~ "Don't know",
    Q39A ==-1 ~ "Missing"
  ) %>% 
    structure(label = Hmisc::label(afrobarometer_release_dataset_mor_r8_en_2023_03_01$Q39A))) %>% 
  
  # Selecting relevant variables
  select(Q39A) %>% 
  
  # Generate a summary table using "tbl_summary" for the specified columns
  tbl_summary(
    
    type = c(Q39A~"categorical"),
    
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "(Missing)",
    digits = everything() ~ c(0,0)
  ) %>% 
  
  # Modify the table header to provide a descriptive label
  modify_header(label ~ "") %>%
  add_n() %>% 
  bold_labels()

# for merging the two tables
tbl_stack(list(t1, t2), 
          # adding the group header
          group_header = c("A1", "A2")) %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = paste0(here::here(), "/test.docx"))
