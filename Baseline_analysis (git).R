## Baseline Analysis 

# Libraries 

library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)

# Data 

dat_bl <- read.csv(file.choose())
dat_bl$nih_race <- as.factor(dat_bl$nih_race)
dat_bl$nih_sex <- as.factor(dat_bl$nih_sex - 1)
dat_bl$surgical_technique <- as.factor(dat_bl$surgical_technique)
dat_bl$Visit_no <- as.factor(dat_bl$Visit_no)        

# Filter < 365 days (Stats chat 4/26/22)

dat_bl <- dat_bl %>%
  filter(time <= 365)

# Filter exclusionary PIDS 
dat_bl <- dat_bl %>% 
  filter(
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  )

# Test Covariance Structures 

y = dat_bl$grit_score
time = as.factor(dat_bl$Visit_no)
S = dat_bl$pid
baseline_grit = dat_bl$baseline_grit

## UN 
corr.n = corSymm(form = ~ 1|S)
var.n = varIdent(form = ~ 1|time) # Unequal variance
fit.gls = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    
fit.gls.r = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.un = AIC(fit.gls)
bic.un = BIC(fit.gls)

rm(fit.gls)

## CS 
corr.n = corCompSymm(form = ~1|S)
var.n = varIdent(form = ~-1)   # Equal variance  
fit.gls = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.cs = AIC(fit.gls)
bic.cs = BIC(fit.gls)
p.val.cs = anova(fit.gls.r, fit.gls)$`p-value`[2]

rm(fit.gls)

## CHS 
corr.n = corCompSymm(form = ~1|S)
var.n = varIdent(form = ~1|time) 
fit.gls = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.chs = AIC(fit.gls)
bic.chs = BIC(fit.gls)
p.val.chs = anova(fit.gls.r, fit.gls)$`p-value`[2]

rm(fit.gls)

## AR1
corr.n = corAR1(form = ~1|S)    
var.n = varIdent(form = ~-1)
fit.gls = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.ar1 = AIC(fit.gls)
bic.ar1 = BIC(fit.gls)
p.val.ar1 = anova(fit.gls.r, fit.gls)$`p-value`[2]

rm(fit.gls)

## ARH1
corr.n = corAR1(form = ~1|S) 
var.n = varIdent(form = ~1|time)    
fit.gls = gls(y ~ baseline_grit + time, weights = var.n, correlation = corr.n, method = "REML", na.action = na.omit)    

aic.arh1 = AIC(fit.gls)
bic.arh1 = BIC(fit.gls)
p.val.arh1 = anova(fit.gls.r, fit.gls)$`p-value`[2]

rm(fit.gls)

## Structure Comparison 
ic.comp = data.frame(
  Structure = c("UN", "CS", "CHS", "AR1", "ARH1"),
  AIC = c(aic.un, aic.cs, aic.chs, aic.ar1, aic.arh1),
  BIC = c(bic.un, bic.cs, bic.chs, bic.ar1, bic.arh1), 
  LRT.p.value = round(c(NA, p.val.cs, p.val.chs, p.val.ar1, p.val.arh1), 4)
)

names(ic.comp)[names(ic.comp) == 'LRT.p.value'] <- "LRT p-value"

## Best Structure 
corr.n.best =  corCompSymm(form = ~1|S)
var.n.best = varIdent(form = ~-1) 

# Models

fit.gls <- gls(grit_score ~ baseline_grit + time, data = dat_bl, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
fit.gls.sum <- summary(fit.gls) # Compound Symmetric CoVar matrix 
length(residuals(fit.gls))

fit.gls.coef <- round(as.data.frame(cbind(fit.gls.sum$tTable, confint(fit.gls))), 5)

fit.gls.coef <- fit.gls.coef %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(fit.gls.coef) <- c("Estimate", "Standard Error", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")
rownames(fit.gls.coef) <- c("(Intercept)", "Baseline Grit", "Time")

# W/ demographics 

fit.gls.dem <- gls(grit_score ~ baseline_grit + time + nih_sex + age_at_surgery, data = dat_bl, correlation = corr.n.best, weights = var.n.best, method = "REML", na.action = na.omit)
fit.gls.dem.sum <- summary(fit.gls.dem) 
length(residuals(fit.gls.dem))

fit.gls.dem.coef <- round(as.data.frame(cbind(fit.gls.dem.sum$tTable, confint(fit.gls.dem))), 5)

fit.gls.dem.coef <- fit.gls.dem.coef %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) 

names(fit.gls.dem.coef) <- c("Estimate", "Standard Error", "t-value", "p-value",  "95% CI (LB)", "95% CI (UB)")

rownames(fit.gls.dem.coef) <- c("(Intercept)", "Baseline Grit", "Time", "Sex", "Age")

## Model Comparisons 
model.comp <- data.frame(
  ` ` = c("Reduced Model", "Full Model"),
  AIC = c(AIC(fit.gls), AIC(fit.gls.dem)),
  BIC = c(BIC(fit.gls), BIC(fit.gls.dem)),
  check.names = F
  )
