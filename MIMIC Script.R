library(ricu) # use for mimic data
library(tidyverse)

#function to prepare data
mimic_prepare <- function(D = "mimic_demo"){
# prepare data (replace for final results)
D <- "mimic_demo" # replace with mimic database
admissions <- read_csv("ADMISSIONS.csv") # replace with mimic admissions
icustays <- read_csv("ICUSTAYS.csv") # replace with mimic icustays

hadm_icustay_ids <- inner_join(admissions, icustays, by = "hadm_id") %>%
  select(c(hadm_id, icustay_id))

dict <- explain_dictionary(src = D) # concepts and description
dict$category <- factor(dict$category)

dict$category %>% levels() # levels of categories to ease filtering

# FiO2 (%)
explain_dictionary(src = D) %>% filter(category == "blood gas") # get name

fio2 <- load_concepts("fio2", D, verbose = F) %>% # load fio2
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(fio2 == max(fio2)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "fio2")) # relevant columns
  
# PaO2 (mmHg)
# create concept
pao2_conc <- concept("pao2",
                item(D, "chartevents", "itemid", 770),
                description = "PaO2",
                unit = "mmHg")

pao2 <- load_concepts(pao2_conc, verbose = F) %>% # load PaO2
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(pao2 == min(pao2)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "pao2")) # relevant columns
 
# platelets * 10^3/mm^3
explain_dictionary(src = D) %>% filter(category == "hematology")  # get name

platelets <- load_concepts("plt", D, verbose = F) %>% # load platelets
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(plt == min(plt)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "plt")) # relevant columns

# bilirubin
bilirubin <- load_concepts("bili", D, verbose = F) %>% # load bili
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(bili == max(bili)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "bili")) # relevant columns

# Glasgow Coma Score
explain_dictionary(src = D) %>% filter(category == "neurological")  # get name

# non-sedated Glasgow Coma Scale
gcs <- load_concepts("gcs", D, verbose = F) %>% # load gcs (non-sedated)
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(gcs == min(gcs)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "gcs")) # relevant columns

# Total Glasgow Coma Scale
gcs_tot <- load_concepts("tgcs", D, verbose = F) %>% # load tgcs
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(tgcs == min(tgcs)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "tgcs")) # relevant columns

# MAP (mmHg)
explain_dictionary(src = D) %>% filter(category == "vitals") # get name

map <- load_concepts("map", D, verbose = F) %>% # load map
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(map == max(map)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "map")) # relevant columns

# Creatinine (mg/dL)
explain_dictionary(src = D) %>% filter(category == "chemistry") # get name

crea <- load_concepts("crea", D, verbose = F) %>% # load crea
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(crea == max(crea)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "crea")) # relevant columns

# Urine output
explain_dictionary(src = D) %>% filter(category == "output") # get name

urine <- load_concepts("urine24", D, verbose = F) %>% # load urine24
  na.omit() %>% # remove missing values
  full_join(icustays, by = "icustay_id") %>% # add icustay data for times
  group_by(icustay_id) %>%
  mutate(admission_length = difftime(intime, # calculate length of admission
                                     outtime, 
                                     units = "hours")) %>% 
  filter(charttime >= (admission_length - 24)) %>% # filter for last 24 hours
  filter(urine24 == min(urine24)) %>% # filter for worst value
  filter(row_number() == 1) %>% # only 1 per patient
  select(c("icustay_id", "urine24")) # relevant columns

# Mortality
death <- inner_join(admissions, icustays, by = "hadm_id") %>%
  select(c("icustay_id", hospital_expire_flag)) # 1 = death

# join all data
data <- fio2 %>%
  full_join(pao2, by = "icustay_id") %>%
  full_join(platelets, by = "icustay_id") %>%
  full_join(bilirubin, by = "icustay_id") %>%
  full_join(gcs, by = "icustay_id") %>%
  full_join(gcs_tot, by = "icustay_id") %>%
  full_join(map, by = "icustay_id") %>%
  full_join(crea, by = "icustay_id") %>%
  full_join(urine, by = "icustay_id") %>%
  full_join(death, by = "icustay_id")

data
}