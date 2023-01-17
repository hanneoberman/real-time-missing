library(ricu)
library(tidyverse)
library(janitor)
library(BRRR)

# download_src("mimic")
# import_src("mimic")
# attach_src("mimic")
source("MIMIC Preparation Function.R")

dat <- mimic_prepare(D = "mimic")

saveRDS(dat, "mimic_cleaned.rds")

