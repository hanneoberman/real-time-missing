# case study SOFA model in MIMIC data

library(tidyverse)
`%nin%` = Negate(`%in%`)

# data
admin_demo <- ricu::mimic_demo$admissions %>% as_tibble()
patients_demo <- ricu::mimic_demo$patients %>% as_tibble()
admin_demo <- admin_demo[,-1] %>% full_join(patients_demo[-1])
chart_demo <- ricu::mimic_demo$chartevents %>% as_tibble()
lab_demo <- ricu::mimic_demo$labevents %>% as_tibble()
in_demo <- ricu::mimic_demo$inputevents_cv %>% as_tibble() %>%
  select(subject_id, charttime, itemid, amount) 
in_demo2 <- ricu::mimic_demo$inputevents_mv %>% as_tibble() %>% 
  select(subject_id, charttime = starttime, itemid, amount) 
in_demo <- in_demo %>% full_join(in_demo2)
out_demo <- ricu::mimic_demo$outputevents %>% as_tibble() %>%
  select(subject_id, charttime, itemid, value) #%>%
micro_demo <-
  ricu::mimic_demo$microbiologyevents %>% as_tibble() %>%
  select(subject_id, charttime, itemid = org_itemid, interpretation)

# variables in the model
sofa_var <- c(fio2 = 50816, pao2 = 779, vent = 50828, plat = 51265, bili = 50885, map = 220052, gcs = 198, vaso = 30051, crea = 1525, urin = 40055) #urin: 40061

# filter data
admin_sofa <- admin_demo %>% 
  select(subject_id, mortality = hospital_expire_flag)
chart_sofa <- chart_demo %>% 
  select(subject_id, charttime, itemid, value) %>% 
  filter(itemid %in% sofa_var) %>% 
  full_join(admin_demo, .) %>% 
  mutate(time_passed = round(as.numeric((charttime - admittime) / 60), 0),
         .keep = "unused") %>% 
  filter(time_passed >= 0) %>% 
  #select(subject_id, time_passed, itemid, value) %>% 
  pivot_wider(
    id_cols = c(subject_id, time_passed),
    names_from = itemid,
    values_from = value,
    values_fn = min
  )
lab_sofa <- lab_demo %>% 
  select(subject_id, charttime, itemid, value) %>% 
  filter(itemid %in% sofa_var) %>% 
  full_join(admin_demo, .) %>% 
  mutate(time_passed = round(as.numeric((charttime - admittime) / 60), 0),
         .keep = "unused") %>% 
  filter(time_passed >= 0) %>% 
  #select(subject_id, time_passed, itemid, value)
  pivot_wider(
    id_cols = c(subject_id, time_passed),
    names_from = itemid,
    values_from = value,
    values_fn = min
  )
in_sofa <- in_demo %>% 
  filter(itemid == 30051 ) %>%
  left_join(admin_demo[, c("subject_id", "admittime")]) %>%
  mutate(time_passed = round(as.numeric((charttime - admittime) / 60), 0),
         "30051" = amount, 
         .keep = "unused") %>% filter(time_passed >= 0) %>% select(-itemid)
out_sofa <- out_demo %>% 
  filter(itemid == 40055) %>%
  left_join(admin_demo[, c("subject_id", "admittime")]) %>%
  mutate(time_passed = round(as.numeric((charttime - admittime) / 60), 0),
         "40055" = value,
         .keep = "unused") %>% filter(time_passed >= 0) %>% select(-itemid)
full_sofa <- admin_sofa %>% 
  full_join(chart_sofa) %>% 
  full_join(lab_sofa) %>% 
  full_join(in_sofa) %>% 
  full_join(out_sofa) %>% 
  mutate(subject_id = subject_id %>% as.factor() %>% as.numeric() %>% as.factor(),
         across(everything()[-c(1:3)], .fns = as.factor),
         across(everything()[-c(1:3)], .fns = as.numeric))
