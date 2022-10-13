library(ricu)
library(tidyverse)
library(janitor)
library(BRRR)

# download_src("mimic")
# import_src("mimic")
# attach_src("mimic")
source("MIMIC Preparation Function.R")

dat <- mimic_prepare(D = "mimic")
skrrrahh("snoop")

library(mice)
library(ggmice)
dat[, -1] %>% plot_pattern()
plot_pattern
missing <- md.pattern(dat[, -1], plot = FALSE)

View(missing)
nrow(missing)
