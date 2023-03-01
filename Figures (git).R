## Figures ## 

# Libraries 
library(tidyverse)
library(cowplot)
library(lemon)
library(gridExtra)
library(gtable)
library(grid)

############################
## FULL VARIABLE ANALYSIS ##
############################

dat_p <- read.csv(file.choose())
dat_p$race <- as.factor(dat_p$race)
dat_p$sex <- as.factor(dat_p$sex - 1)
dat_p <- dat_p %>% 
  mutate(
    sex = case_when(
      sex == 0 ~ "Female",
      sex == 1 ~ "Male"
    )
  )
dat_p$surgical_technique <- as.factor(dat_p$surgical_technique)
dat_p$visit_no <- as.factor(dat_p$visit_no)        

# Filter < 365 days (Stats chat 4/26/22)

dat_p <- dat_p %>%
  filter(time <= 365) %>% 
  mutate(baseline = ifelse(test = visit_no == "1", 1, 0))

dat_p <- dat_p %>% 
  filter(
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  )

dat_p_group <- dat_p %>% 
  group_by(visit_no) %>% 
  summarize(
    grit_score_mean = mean(grit_score, na.rm = T),
    sd_grit = sd(grit_score, na.rm = T),
    hss_score_mean = mean(hss_score, na.rm = T),
    sd_hss = sd(hss_score, na.rm = T),
    ikdc_score_mean = mean(ikdc_score, na.rm = T),
    sd_ikdc = sd(ikdc_score, na.rm = T),
    lysholm_score_mean = mean(lysholm_score, na.rm = T),
    sd_lysholm = sd(lysholm_score, na.rm = T),
    neuroqol_score_mean = mean(neuroqol_score, na.rm = T),
    sd_neuroqol = sd(neuroqol_score, na.rm = T),
    mobility_score_mean = mean(mobility_score, na.rm = T),
    sd_mobility = sd(mobility_score, na.rm = T),
    pain_score_mean = mean(pain_score, na.rm = T),
    sd_pain = sd(pain_score, na.rm = T)
  )

dat_p_group_mean <- dat_p_group %>% 
  select(contains(c("visit_no", "mean")))

dat_p_group_sd <- dat_p_group %>% 
  select(contains(c("visit_no", "sd")))

dat_p_group_mean_g <- dat_p_group_mean %>% 
  gather(
    key = "mean",
    value = "mean_value",
    grit_score_mean:pain_score_mean, 
    factor_key = T
  )

dat_p_group_sd_g <- dat_p_group_sd %>% 
  gather(
    key = "sd",
    value = "sd_value",
    sd_grit:sd_pain, 
    factor_key = T
  )

dat_p_sum <- cbind(dat_p_group_mean_g, dat_p_group_sd_g)

dat_p_sum <- dat_p_sum[, 2:6]

dat_p_sum$mean <- str_to_sentence(str_sub(dat_p_sum$mean, end=-12))

dat_p_sum <- dat_p_sum %>% 
  mutate(
    mean = case_when(
      mean == "Hss" ~ "HSS",
      mean == "Ikdc" ~ "IKDC",
      mean == "Pain" ~ "Pain (PROMIS)",
      mean == "Mobility" ~ "Mobility (PROMIS)",
      TRUE ~ mean
    )
  )

dat_p_sum$mean <- factor(dat_p_sum$mean, labels = c(
  "Grit", 
  "HSS^{'*+'}", 
  "IKDC^{'+'}", 
  "Lysholm^{'+'}", 
  "Mobility~(PROMIS)^{'+'}",
  "Neuroqol^{'*+'}",
  "Pain~(PROMIS)^{'+'}"
  )
  )

## Plots  

# All Mean Measurements by Visit No
full_var_p <- ggplot(
  data = dat_p_sum,
  aes(
    x = as.numeric(visit_no), 
    y = mean_value,
    ymin = mean_value - sd_value, 
    ymax = mean_value + sd_value
  )
) + 
  geom_path(
    color = "grey"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed",
    color = "lightgrey"
    ) + 
  theme_classic(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "Mean Measurement Score",
    caption = "
    * = Significant relationship between Baseline Grit & Measurement (p < 0.05)
    + = Significant relationship between Time & Measurement (p < 0.05)"
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Baseline", "3", "6", "12")
  ) + 
  facet_wrap(
    ~mean,
    scales = "free",
    labeller = label_parsed
  )

full_var_p

# Plot with sex 

dat_p_group.sex <- dat_p %>% 
  drop_na(sex) %>% 
  group_by(visit_no, sex) %>% 
  summarize(
    grit_score_mean = mean(grit_score, na.rm = T),
    sd_grit = sd(grit_score, na.rm = T),
    hss_score_mean = mean(hss_score, na.rm = T),
    sd_hss = sd(hss_score, na.rm = T),
    ikdc_score_mean = mean(ikdc_score, na.rm = T),
    sd_ikdc = sd(ikdc_score, na.rm = T),
    lysholm_score_mean = mean(lysholm_score, na.rm = T),
    sd_lysholm = sd(lysholm_score, na.rm = T),
    neuroqol_score_mean = mean(neuroqol_score, na.rm = T),
    sd_neuroqol = sd(neuroqol_score, na.rm = T),
    mobility_score_mean = mean(mobility_score, na.rm = T),
    sd_mobility = sd(mobility_score, na.rm = T),
    pain_score_mean = mean(pain_score, na.rm = T),
    sd_pain = sd(pain_score, na.rm = T)
  )

dat_p_group_mean.sex <- dat_p_group.sex %>% 
  select(contains(c("visit_no", "mean", "sex")))

dat_p_group_sd.sex <- dat_p_group.sex %>% 
  select(contains(c("visit_no", "sd", "sex")))

dat_p_group_mean_g.sex <- dat_p_group_mean.sex %>% 
  gather(
    key = "mean",
    value = "mean_value",
    grit_score_mean:pain_score_mean, 
    factor_key = T
  )

dat_p_group_sd_g.sex <- dat_p_group_sd.sex %>% 
  gather(
    key = "sd",
    value = "sd_value",
    sd_grit:sd_pain, 
    factor_key = T
  )

dat_p_sum.sex <- cbind(dat_p_group_mean_g.sex, dat_p_group_sd_g.sex)

dat_p_sum.sex <- dat_p_sum.sex[, c(1:4, 8)]

dat_p_sum.sex$mean <- str_to_sentence(str_sub(dat_p_sum.sex$mean, end=-12))

dat_p_sum.sex <- dat_p_sum.sex %>% 
  mutate(
    mean = case_when(
      mean == "Hss" ~ "HSS",
      mean == "Ikdc" ~ "IKDC",
      mean == "Pain" ~ "Pain (PROMIS)",
      mean == "Mobility" ~ "Mobility (PROMIS)",
      TRUE ~ mean
    )
  )

names(dat_p_sum.sex) <- c("visit_no", "sex", "mean", "mean_value", "sd_value")

dat_p_sum.sex$mean <- factor(dat_p_sum.sex$mean, labels = c(
  "Grit", 
  "HSS^{'*+'}", 
  "IKDC^{'+#'}", 
  "Lysholm^{'+#'}", 
  "Mobility~(PROMIS)^{'+#'}",
  "Neuroqol^{'*+#'}",
  "Pain~(PROMIS)^{'+'}"
)
)

shift_legend2 <- function(p) {
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  reposition_legend(p, 'center', panel=names)
}

full_var_p.sex <- ggplot(
  data = dat_p_sum.sex,
  aes(
    x = as.numeric(visit_no), 
    y = mean_value,
    ymin = mean_value - sd_value, 
    ymax = mean_value + sd_value,
    color = sex
  )
) + 
  geom_path(

  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed",
    color = "lightgrey"
  ) + 
  theme_classic(
    
  ) + 
  guides(
    color = guide_legend(override.aes = list(size = 5))
    ) + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
    ) + 
  labs(
    x = "Months Post-Op",
    y = "Mean Measurement Score",
    caption = "
    * = Significant relationship between Baseline Grit & Measurement (p < 0.05)
    + = Significant relationship between Time & Measurement (p < 0.05)
    # = Signficant relationship between Sex & Measurement (p < 0.05)",
    color = ""
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Baseline", "3", "6", "12")
  ) + 
  facet_wrap(
    ~mean,
    scales = "free",
    labeller = label_parsed
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

shift_legend2(full_var_p.sex)

#######################
## BASELINE Analysis ##
#######################

dat_bl_p <- read.csv("grit_an_long.csv")
dat_bl_p$nih_race <- as.factor(dat_bl_p$nih_race)
dat_bl_p$nih_sex <- as.factor(dat_bl_p$nih_sex - 1)
dat_bl_p$surgical_technique <- as.factor(dat_bl_p$surgical_technique)
dat_bl_p$Visit_no <- as.factor(dat_bl_p$Visit_no)        

dat_bl_p <- dat_bl_p %>%
  filter(
    time <= 365,
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  ) %>%
  drop_na(grit_score, baseline_grit)

dat_bl_p$quantile <- ntile(dat_bl_p$baseline_grit, 3)

dat_bl_p_g <- dat_bl_p %>%
  group_by(Visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    grit_score_mean = mean(grit_score, na.rm = T),
    sd_grit = sd(grit_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

dat_bl_p_g$quantile <- factor(
  dat_bl_p_g$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

dat_bl_p_no <- dat_bl_p %>% 
  group_by(Visit_no) %>%
  summarize(
    grit_score_mean = mean(grit_score, na.rm = T),
    sd_grit = sd(grit_score, na.rm = T)
  ) 
  
bl_p <- ggplot(
  dat_bl_p_g,
  aes(
    x = as.numeric(Visit_no), 
    y = grit_score_mean,
    ymin = grit_score_mean - sd_grit, 
    ymax = grit_score_mean + sd_grit,
    color = as.factor(quantile)
    )
  )  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:3,
    labels = c("2", "3", "4")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

bl_no_p <- ggplot(
  dat_bl_p_no,
  aes(
    x = as.numeric(Visit_no), 
    y = grit_score_mean,
    ymin = grit_score_mean - sd_grit, 
    ymax = grit_score_mean + sd_grit
    )
  )  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:3,
    labels = c("2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

grid.arrange(bl_p, bl_no_p, left = textGrob("Mean Post-op Grit Score", rot = 90, vjust = 1))
# 
# ## Grit Distribution 
# all_grit <- dat_bl %>% 
#   select(contains(c("pid", "nih_sex", "grit"))) %>% 
#   gather(
#     key = "key",
#     value = "value",
#     baseline_grit:grit_score
#   ) %>% 
#   mutate(
#     nih_sex = case_when(
#       nih_sex == 0 ~ "Female",
#       nih_sex == 1 ~ "Male"
#     )
#   )
# 
# baseline_grit_p_sex <- ggplot(
#   data = all_grit,
#   aes(
#     x = value, 
#     fill = nih_sex
#   )
# ) + 
#   geom_density(
#     alpha = .6
#   ) + 
#   labs(
#     x = "Grit Score", 
#     y = "Density",
#     fill = ""
#   ) + 
#   theme_classic(
#     
#   ) + 
#   theme(
#     legend.position = "bottom"
#   ) + 
#   scale_fill_brewer(
#     palette = "Set1"
#   )
# 
# baseline_grit_p <- ggplot(
#   data = all_grit,
#   aes(
#     x = value
#     )
# ) + 
#   geom_density(
#     alpha = .6,
#     fill = "lightblue"
#   ) + 
#   labs(
#     x = "Grit Score", 
#     y = "Density",
#     fill = ""
#   ) + 
#   theme_classic(
#     
#   ) + 
#   theme(
#     legend.position = "bottom"
#   ) + 
#   scale_fill_brewer(
#     palette = "Set1"
#   )
# 
# ## Other Measurement Distributions 
# 
# full_dat_g <- full_dat %>% 
#   drop_na(sex) %>% 
#   select(contains(c("pid", "sex", "score"))) %>% 
#   gather(
#     key = "Measure",
#     value = "Score",
#     hss_score:pain_score,
#     factor_key = TRUE
#     ) %>% 
#   mutate(
#     Measure = case_when(
#       Measure == "hss_score" ~ "HSS",
#       Measure == "ikdc_score" ~ "IKDC",
#       Measure == "lysholm_score" ~ "Lysholm",
#       Measure == "neuroqol_score" ~ "Neuroqol",
#       Measure == "mobility_score" ~ "Mobility (PROMIS)",
#       Measure == "pain_score" ~ "Pain (PROMIS)"
#       )
#     )
# 
# all_var_p <- ggplot(
#   data = full_dat_g, 
#   aes(
#     x = Score,
#     fill = sex
#   )
# ) + 
#   geom_density(
#     alpha = .6
#   ) + 
#   labs(
#     x = "", 
#     y = "Density",
#     fill = ""
#   ) + 
#   theme_classic(
#     
#   ) + 
#   theme(
#     legend.position = "bottom"
#   ) + 
#   scale_fill_brewer(
#     palette = "Set1"
#   ) + 
#   facet_wrap(
#     ~Measure,
#     scales = "free"
#   )


## Baseline Grit by Other Domain Measurements 

dat_ot <- merge(dat_p, dat_bl_p, by = "pid")

dat_ot <- dat_ot[, c(7:12, 14, 24)]

# HSS 

hss_sum_p <- dat_ot %>% 
  select(contains(c("hss", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    hss_score_mean = mean(hss_score, na.rm = T),
    sd_hss = sd(hss_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

hss_sum_p$quantile <- factor(
  hss_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

hss_p_no <- dat_ot %>% 
  select(contains(c("hss", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    hss_score_mean = mean(hss_score, na.rm = T),
    sd_hss = sd(hss_score, na.rm = T)
  ) 


hss_p_bl <- ggplot(
  hss_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = hss_score_mean,
    ymin = hss_score_mean - sd_hss, 
    ymax = hss_score_mean + sd_hss,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("0", "3", "6", "12")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Months Post-Op",
    y = "Mean Post-op HSS Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

hss_p_bl

hss_no_p <- ggplot(
  hss_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = hss_score_mean,
    ymin = hss_score_mean - sd_hss,
    ymax = hss_score_mean + sd_hss
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("0", "3", "6", "12")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

hss_no_p

# hss_bl_merge <- grid.arrange(hss_p_bl, hss_no_p, left = textGrob("Mean Post-op HSS Score", rot = 90, vjust = 1))

# IKDC 

ikdc_sum_p <- dat_ot %>% 
  select(contains(c("ikdc", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    ikdc_score_mean = mean(ikdc_score, na.rm = T),
    sd_ikdc = sd(ikdc_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

ikdc_sum_p$quantile <- factor(
  ikdc_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

ikdc_p_no <- dat_ot %>% 
  select(contains(c("ikdc", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    ikdc_score_mean = mean(ikdc_score, na.rm = T),
    sd_ikdc = sd(ikdc_score, na.rm = T)
  ) 

ikdc_p_bl <- ggplot(
  ikdc_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = ikdc_score_mean,
    ymin = ikdc_score_mean - sd_ikdc, 
    ymax = ikdc_score_mean + sd_ikdc,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Visit Number",
    y = "Mean Post-op IKDC Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

ikdc_p_bl

ikdc_no_p <- ggplot(
  ikdc_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = ikdc_score_mean,
    ymin = ikdc_score_mean - sd_ikdc,
    ymax = ikdc_score_mean + sd_ikdc
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

ikdc_no_p

# ikdc_bl_merge <- grid.arrange(ikdc_p_bl, ikdc_no_p, left = textGrob("Mean Post-op IKDC Score", rot = 90, vjust = 1))


# Lysholm

lys_sum_p <- dat_ot %>% 
  select(contains(c("lysholm", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    lys_score_mean = mean(lysholm_score, na.rm = T),
    sd_lys = sd(lysholm_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

lys_sum_p$quantile <- factor(
  lys_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

lys_p_no <- dat_ot %>% 
  select(contains(c("lysholm", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    lys_score_mean = mean(lysholm_score, na.rm = T),
    sd_lys = sd(lysholm_score, na.rm = T)
  ) 


lys_p_bl <- ggplot(
  lys_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = lys_score_mean,
    ymin = lys_score_mean - sd_lys, 
    ymax = lys_score_mean + sd_lys,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Visit Number",
    y = "Mean Post-op Lysholm Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

lys_p_bl

lys_no_p <- ggplot(
  lys_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = lys_score_mean,
    ymin = lys_score_mean - sd_lys,
    ymax = lys_score_mean + sd_lys
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

lys_no_p

# lys_bl_merge <- grid.arrange(lys_p_bl, lys_no_p, left = textGrob("Mean Post-op Lysholm Score", rot = 90, vjust = 1))


# Neuroqol

neu_sum_p <- dat_ot %>% 
  select(contains(c("neuroqol", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    neu_score_mean = mean(neuroqol_score, na.rm = T),
    sd_neu = sd(neuroqol_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

neu_sum_p$quantile <- factor(
  neu_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

neu_p_no <- dat_ot %>% 
  select(contains(c("neuroqol", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    neu_score_mean = mean(neuroqol_score, na.rm = T),
    sd_neu = sd(neuroqol_score, na.rm = T)
  ) 


neu_p_bl <- ggplot(
  neu_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = neu_score_mean,
    ymin = neu_score_mean - sd_neu, 
    ymax = neu_score_mean + sd_neu,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("0", "3", "6", "12")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Months Post-Op",
    y = "Mean Post-op Neuroqol Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

neu_p_bl

neu_no_p <- ggplot(
  neu_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = neu_score_mean,
    ymin = neu_score_mean - sd_neu,
    ymax = neu_score_mean + sd_neu
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

neu_no_p

# neu_bl_merge <- grid.arrange(neu_p_bl, neu_no_p, left = textGrob("Mean Post-op Neuroqol Score", rot = 90, vjust = 1))

# Mobility 

mob_sum_p <- dat_ot %>% 
  select(contains(c("mobility", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    mob_score_mean = mean(mobility_score, na.rm = T),
    sd_mob = sd(mobility_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

mob_sum_p$quantile <- factor(
  mob_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

mob_p_no <- dat_ot %>% 
  select(contains(c("mobility", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    mob_score_mean = mean(mobility_score, na.rm = T),
    sd_mob = sd(mobility_score, na.rm = T)
  ) 

mob_p_bl <- ggplot(
  mob_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = mob_score_mean,
    ymin = mob_score_mean - sd_mob, 
    ymax = mob_score_mean + sd_mob,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Visit Number",
    y = "Mean Post-op Mobility Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

mob_p_bl

mob_no_p <- ggplot(
  mob_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = mob_score_mean,
    ymin = mob_score_mean - sd_mob,
    ymax = mob_score_mean + sd_mob
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = ""
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

mob_no_p

# mob_bl_merge <- grid.arrange(mob_p_bl, mob_no_p, left = textGrob("Mean Post-op Mobility Score", rot = 90, vjust = 1))


# Pain 

pain_sum_p <- dat_ot %>% 
  select(contains(c("pain", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no, quantile) %>%
  drop_na(quantile) %>%
  summarize(
    pain_score_mean = mean(pain_score, na.rm = T),
    sd_pain = sd(pain_score, na.rm = T)
  ) %>% 
  mutate(
    quantile = case_when(
      quantile == 1 ~ "Low Baseline Grit",
      quantile == 2 ~ "Medium Baseline Grit",
      quantile == 3 ~ "High Baseline Grit"
    )
  )

pain_sum_p$quantile <- factor(
  pain_sum_p$quantile, 
  levels = c("Low Baseline Grit", "Medium Baseline Grit", "High Baseline Grit"))

pain_p_no <- dat_ot %>% 
  select(contains(c("pain", "visit", "quantile"))) %>%
  filter(visit_no != "1") %>% 
  group_by(visit_no) %>%
  summarize(
    pain_score_mean = mean(pain_score, na.rm = T),
    sd_pain = sd(pain_score, na.rm = T)
  ) 


pain_p_bl <- ggplot(
  pain_sum_p,
  aes(
    x = as.numeric(visit_no), 
    y = pain_score_mean,
    ymin = pain_score_mean - sd_pain, 
    ymax = pain_score_mean + sd_pain,
    color = as.factor(quantile)
  )
)  + 
  geom_path(
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  facet_wrap(
    ~quantile
  ) + 
  labs(
    x = "Visit Number",
    y = "Mean Post-op Pain Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

pain_p_bl

pain_no_p <- ggplot(
  pain_p_no,
  aes(
    x = as.numeric(visit_no), 
    y = pain_score_mean,
    ymin = pain_score_mean - sd_pain,
    ymax = pain_score_mean + sd_pain
  )
)  + 
  geom_path(
    color = "grey",
    linetype = "dashed"
  ) + 
  geom_errorbar(
    width = .5,
    color = "darkgrey"
  ) +
  geom_point(
    size = 1.5
  ) +
  theme_classic(
    
  ) + 
  scale_x_continuous(
    breaks = 1:4,
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "Visit Number",
    y = "Mean Post-op Pain Score"
  ) + 
  theme(
    legend.position = "none"
  ) + 
  scale_color_brewer(
    palette = "Set1"
  )

pain_no_p

# pain_bl_merge <- grid.arrange(pain_p_bl, pain_no_p, left = textGrob("Mean Post-op Mobility Score", rot = 90, vjust = 1))


## Stats Chat 5/6/22
# Figures with just sig baseline grit 
# Replace general trend with regression table 

dat_p %>% 
  summarize(
    mean = mean(grit_score, na.rm = T),
    sd = sd(grit_score, na.rm = T)
  )

dat_p %>% 
  group_by(visit_no) %>% 
  summarize(
    mean = mean(grit_score, na.rm = T),
    sd = sd(grit_score, na.rm = T)
  )
