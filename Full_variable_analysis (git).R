## Full Variable Analysis 

## Baseline Analysis 

# Libraries 

library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)

# Data 

dat <- read.csv(file.choose())
dat$race <- as.factor(dat$race)
dat$sex <- as.factor(dat$sex - 1)
dat <- dat %>% 
  mutate(
    sex = case_when(
      sex == 0 ~ "Female",
      sex == 1 ~ "Male"
    )
  )

dat$surgical_technique <- as.factor(dat$surgical_technique)
dat$visit_no <- as.factor(dat$visit_no)        

# Filter < 365 days (Stats chat 4/26/22)

# full_dat <- dat %>%
#   filter(time <= 365) 

dat <- dat %>%
  filter(time <= 365) %>% 
  filter(visit_no != "1")

# Filter PIDS 17, 77, 93, 110, & 128

dat <- dat %>% 
  filter(
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  )

## Initial Models 

## Best Structure, Compound Symmetric (from Baseline Grit analysis)
S = dat$pid
corr.n.best =  corCompSymm(form = ~1|S)
var.n.best = varIdent(form = ~-1) 

# HSS, physical activity
hss.fit <- gls(hss_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
sum_hss <- summary(hss.fit)
length(residuals(hss.fit))

sum_hss.df <- round(as.data.frame(cbind(sum_hss$tTable, confint(hss.fit))), 5)

sum_hss.coef <- sum_hss.df %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(sum_hss.coef) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(sum_hss.coef) <- c("(Intercept)", "Baseline Grit", "Time")

# IKDC, knee function  
ikdc.fit <- gls(ikdc_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
sum_ikdc <- summary(ikdc.fit)
length(ikdc.fit$residuals)

sum_ikdc.df <- round(as.data.frame(cbind(sum_ikdc$tTable, confint(ikdc.fit))), 5)

sum_ikdc.coef <- sum_ikdc.df %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(sum_ikdc.coef) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(sum_ikdc.coef) <- c("(Intercept)", "Baseline Grit", "Time")

# Lysholm, knee function 
lysholm.fit <- gls(lysholm_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
lysholm_sum <- summary(lysholm.fit)
length(residuals(lysholm.fit))

lysholm_sum <- round(as.data.frame(cbind(lysholm_sum$tTable, confint(lysholm.fit))), 5)

lysholm_sum.coef <- lysholm_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(lysholm_sum.coef) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(lysholm_sum.coef) <- c("(Intercept)", "Baseline Grit", "Time")


# Neuroqol, t-score low ext. function/mobility 
neuroqol.fit <- gls(neuroqol_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
neuroqol_sum <- summary(neuroqol.fit)
length(residuals(neuroqol.fit))

neuroqol_sum <- round(as.data.frame(cbind(neuroqol_sum$tTable, confint(neuroqol.fit))), 5)

neuroqol_sum.coef <- neuroqol_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(neuroqol_sum.coef) <-  c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(neuroqol_sum.coef) <- c("(Intercept)", "Baseline Grit", "Time")

# PROMIS Mobility 
mobility.fit <- gls(mobility_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
mobility_sum <- summary(mobility.fit)
length(residuals(mobility.fit))

mobility_sum <- round(as.data.frame(cbind(mobility_sum$tTable, confint(mobility.fit))), 5)

mobility_sum <- mobility_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(mobility_sum) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(mobility_sum) <- c("(Intercept)", "Baseline Grit", "Time")


# PROMIS Pain
pain.fit <- gls(pain_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
pain_sum <- summary(pain.fit)
length(residuals(pain.fit))
pain_sum <- round(as.data.frame(cbind(pain_sum$tTable, confint(pain.fit))), 5)

pain_sum.coef <- pain_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(pain_sum.coef) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(pain_sum.coef) <- c("(Intercept)", "Baseline Grit", "Time")


## Add covariates ## 

# HSS, physical activity + dems 
hss.fit.dem <- gls(hss_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
hss.fit.dem_sum <- summary(hss.fit.dem)
length(residuals(hss.fit.dem))

hss.fit.dem <- round(as.data.frame(cbind(hss.fit.dem_sum$tTable, confint(hss.fit.dem))), 5)

hss.fit.dem <- hss.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(hss.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(hss.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")


# IKDC, knee function + dems 
ikdc.fit.dem <- gls(ikdc_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
ikdc.fit.dem_sum <- summary(ikdc.fit.dem)
length(residuals(ikdc.fit.dem))

ikdc.fit.dem <- round(as.data.frame(cbind(ikdc.fit.dem_sum$tTable, confint(ikdc.fit.dem))), 5)

ikdc.fit.dem <- ikdc.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(ikdc.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value",  "95% LB", "95% UB")

rownames(ikdc.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")

# Lysholm, knee function + dems 
lysholm.fit.dem <- gls(lysholm_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
lysholm.fit.dem_sum <- summary(lysholm.fit.dem)
length(residuals(lysholm.fit.dem))

lysholm.fit.dem <- round(as.data.frame(cbind(lysholm.fit.dem_sum$tTable, confint(lysholm.fit.dem))), 5)

lysholm.fit.dem <- lysholm.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(lysholm.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(lysholm.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")

# Neuroqol, t-score low ext. function/mobility + dems 
neuroqol.fit.dem <- gls(neuroqol_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
neuroqol.fit.dem_sum <- summary(neuroqol.fit.dem)
length(residuals(neuroqol.fit.dem))

neuroqol.fit.dem <- round(as.data.frame(cbind(neuroqol.fit.dem_sum$tTable, confint(neuroqol.fit.dem))), 5)

neuroqol.fit.dem <- neuroqol.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(neuroqol.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(neuroqol.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")

# PROMIS Mobility + dems 
mobility.fit.dem <- gls(mobility_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
mobility.fit.dem_sum <- summary(mobility.fit.dem)
length(residuals(mobility.fit.dem))

mobility.fit.dem <- round(as.data.frame(cbind(mobility.fit.dem_sum$tTable, confint(mobility.fit.dem))), 5)

mobility.fit.dem <- mobility.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(mobility.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(mobility.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")

# PROMIS Pain + dems 
pain.fit.dem <- gls(pain_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
pain.fit.dem_sum <- summary(pain.fit.dem)
length(residuals(pain.fit.dem))

pain.fit.dem <- round(as.data.frame(cbind(pain.fit.dem_sum$tTable, confint(pain.fit.dem))), 5)

pain.fit.dem <- pain.fit.dem %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(pain.fit.dem) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% LB", "95% UB")

rownames(pain.fit.dem) <- c("(Intercept)", "Baseline Grit", "Time", "Sex (1 = Male)", "Age")

## Model Comparison 

# HSS, physical activity
hss.r <- gls(hss_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
hss.f <- gls(hss_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

hss_AIC <- round(AIC(hss.r, hss.f)[2], 2)
hss_BIC <- round(BIC(hss.r, hss.f)[2], 2)

# IKDC, knee function  
ikdc.r <- gls(ikdc_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
ikdc.f <- gls(ikdc_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

ikdc_AIC <- round(AIC(ikdc.r, ikdc.f)[2], 2)
ikdc_BIC <- round(BIC(ikdc.r, ikdc.f)[2], 2)

# Lysholm, knee function 
lysholm.r <- gls(lysholm_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
lysholm.f <- gls(lysholm_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

lys_AIC <- round(AIC(lysholm.r, lysholm.f)[2], 2)
lys_BIC <- round(BIC(lysholm.r, lysholm.f)[2], 2)

# Neuroqol, t-score low ext. function/mobility 
neuroqol.r <- gls(neuroqol_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
neuroqol.f <- gls(neuroqol_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

neu_AIC <- round(AIC(neuroqol.r, neuroqol.f)[2], 2)
neu_BIC <- round(BIC(neuroqol.r, neuroqol.f)[2], 2)

# PROMIS Mobility 
mobility.r <- gls(mobility_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
mobility.f <- gls(mobility_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

mob_AIC <- round(AIC(mobility.r, mobility.f)[2], 2)
mob_BIC <- round(BIC(mobility.r, mobility.f)[2], 2)

# PROMIS Pain
pain.r <- gls(pain_score ~ baseline_grit + time, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
pain.f <- gls(pain_score ~ baseline_grit + time + sex + age_at_surgery, data = dat, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)

pain_AIC <- round(AIC(pain.r, pain.f)[2], 2)
pain_BIC <- round(BIC(pain.r, pain.f)[2], 2)

full_var_comp <- data.frame(
  Model = c("HSS (Reduced)", "HSS (Full)", "-", "IKDC (Reduced)", "IKDC (Full)", "-", "Lysholm (Reduced)", "Lysholm (Full)", "-","Neuroqol (Reduced)", "Neuroqol (Full)", "-","Mobility (Reduced)", "Mobility (Full)", "-","Pain (Reduced)", "Pain (Full)"),
  AIC = rbind(hss_AIC, "-", ikdc_AIC, "-", lys_AIC, "-", neu_AIC,"-", mob_AIC,"-", pain_AIC),
  BIC = rbind(hss_BIC, "-", ikdc_BIC, "-", lys_BIC, "-", neu_BIC, "-", mob_BIC, "-", pain_BIC),
  check.names = F
)

rownames(full_var_comp) <- NULL


