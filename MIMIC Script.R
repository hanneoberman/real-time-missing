library(ricu)
library(tidyverse)
library(janitor)

download_src("mimic")
import_src("mimic")
attach_src("mimic")
D <- "mimic" # replace with mimic database
admissions <- read_csv("ADMISSIONS.csv") %>% clean_names() # replace with mimic admissions
icustays <- read_csv("ICUSTAYS.csv") %>% clean_names() # replace with mimic icustays

hadm_icustay_ids <- inner_join(admissions, icustays, by = "hadm_id") %>%
  select(c(hadm_id, icustay_id))

dict <- explain_dictionary(src = D) # concepts and description
dict$category <- factor(dict$category)

dict$category %>% levels() # levels of categories to ease filtering

fio2 <- load_concepts("fio2", D, verbose = F) %>% # load fio2
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(fio2 == max(fio2)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "fio2")) # relevant column

load_concepts("fio2", "mimic")
setup_src_data("mimic")
cfg <- load_src_cfg("mimic")
str(cfg)
