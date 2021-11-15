# visualize dgm

# packages
library(dplyr)

# labels
pred_lab <- paste0("X", 1:10)
pred_col <- c("#1269b0", "#7c1315") %>% setNames(c("Observed", "Missing"))


# dgm
dgm <- readRDS("Results/output.RDS") %>% .$DGM
# correlations
corr <- dgm$varcov %>% 
  cov2cor() %>% 
  cbind(pred_lab, .) %>% 
  as.data.frame() %>% 
  setNames(c("pred", pred_lab)) %>% 
  tidyr::pivot_longer(cols = everything()[-1]) %>% 
  mutate(value = as.numeric(value),
         text = round(value, 2))
corr[corr$pred==corr$name, "value"] <- NA
corr[c(11, 21, 22, 31:33, 41:44, 51:55, 61:66, 71:77, 81:88, 91:99), "value"] <- NA
corr[c(11, 21, 22, 31:33, 41:44, 51:55, 61:66, 71:77, 81:88, 91:99), "text"] <- NA
saveRDS(corr, "Data/correlations.RDS")

# coefficients
coef <- data.frame(
  value = dgm$betas, 
  pred = factor(pred_lab, levels = pred_lab), 
  type = c(rep("Y", 10), rep("Y*", 10))) %>% 
  mutate(value = value,
         text = round(value, 2))
saveRDS(coef, "Data/coefficients.RDS")

# missing data pattern
patt <- dgm$miss_pat[c(4,8,12), -c(1:2)] %>% 
  as.data.frame() %>% 
  cbind(row = 1:3, .) %>% 
  tidyr::pivot_longer(cols = everything()[-1]) %>% 
  mutate(value = factor(value, levels = c(0,1), labels = c("Missing", "Observed")))
saveRDS(patt, "Data/missing_data_pattern.RDS")
