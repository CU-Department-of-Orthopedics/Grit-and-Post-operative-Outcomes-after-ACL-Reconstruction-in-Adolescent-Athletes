### Summaries 
library(readxl)
library(tidyverse)
library(table1)

## T-test and Chi-square Table Fn 

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}


pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}


# table1(
#   ~ variable | outcome, 
#   data = data, 
#   overall = F, 
#   extra.col=list(`P-value`= pvalue),
#   render.continuous = render.cont, 
#   render.categorical = render.cat,
#   render.missing = NULL
# )

# Data Import 
dat <- read.csv(file.choose())

# dat <- dat %>%
#   filter(time <= 365) %>% 
#   filter(visit_no != "1")

unique_pids <- dat %>% distinct(pid)
unique_pids <- unique_pids %>% 
  filter(
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  )

rm("dat")

dat_sum2 <- read_excel(file.choose())

# Clean 
dat_sum2_race <- dat_sum2[is.na(dat_sum2$redcap_repeat_instrument),]
names(dat_sum2_race)[names(dat_sum2_race) == 'ID'] <- 'pid'
dat_sum2_race <- dat_sum2_race[, colSums(is.na(dat_sum2_race))<nrow(dat_sum2_race)]
dat_sum2_race <- dat_sum2_race %>% 
  drop_na(pid)

dat_sum2_med <- dat_sum2 %>% 
  filter(
    redcap_repeat_instrument == "surgeon_form"
  ) 

rm("dat_sum2")

names(dat_sum2_med)[names(dat_sum2_med) == 'ID'] <- 'pid'
dat_sum2_med <- dat_sum2_med[, colSums(is.na(dat_sum2_med))<nrow(dat_sum2_med)]

dat_sum2_med <- dat_sum2_med %>% 
  drop_na("pid")

dat_sum2 <- merge(dat_sum2_race, dat_sum2_med, by = "pid")

dat_sum <- subset(dat_sum2, pid %in% unique_pids$pid)

dat_sum <- dat_sum %>% 
  filter(
    !pid %in% c(17, 77, 93, 110, 128, 126, 130, 137, 140, 141, 143)
  )

dat_sum <- dat_sum %>% 
  mutate(
    nih_sex = case_when(
      nih_sex == 1 ~ "Female",
      nih_sex == 2 ~ "Male",
      nih_sex == 3 ~ "Unknown or Not Reported"
      ),
    nih_race = case_when(
      nih_race == 1 ~ "American Indian or Alaska Native",
      nih_race == 2 ~ "Asian", 
      nih_race == 3 ~ "Black or African-American", 
      nih_race == 4 ~ "Native Hawaiian or Other Pacific Islander",
      nih_race == 5 ~ "White",
      nih_race == 6 ~ "More than one race",
      nih_race == 7 ~ "Unknown or not reported",
      nih_race == 8 ~ "Other"
      ),
    nih_ethnicity = case_when(
      nih_ethnicity == 1 ~ "Hispanic or Latino",
      nih_ethnicity == 2 ~ "Not Hispanic or Latino", 
      nih_ethnicity == 3 ~ "Unknown or not reported",
    ),
    athletic_involvment = case_when(
      athletic_involvment == 0 ~ "Non-Athlete", 
      athletic_involvment == 1 ~ "Playground Athlete",
      athletic_involvment == 2 ~ "Recreational Athlete",
      athletic_involvment == 3 ~ "Competitive Athlete - Organized Club Sport", 
      athletic_involvment == 4 ~ "Elite/Professional Athlete"
      ),
    mechanism_of_injury1 = case_when(
      mechanism_of_injury1 == 1 ~ "Slipped/Fell (non-sport/non-recreation)",
      mechanism_of_injury1 == 2 ~ "Recreation", 
      mechanism_of_injury1 == 3 ~ "Sports Practice", 
      mechanism_of_injury1 == 4 ~ "Athletic Game/Competition"
      ),
    how_did_your_injury_happen = case_when(
      how_did_your_injury_happen == 1 ~ "Contact (collision with another player)",
      how_did_your_injury_happen == 2 ~ "Non-contact (slipped/fell/other)"
      ),
    dominant_leg = case_when(
      dominant_leg == 1 ~ "Left", 
      dominant_leg == 2 ~ "Right"
      ),
    laterality = case_when(
      laterality == 1 ~ "Left", 
      laterality == 2 ~ "Right"
      ),
    primary_or_revision_surger = case_when(
      primary_or_revision_surger == 1 ~ "Primary",
      primary_or_revision_surger == 2 ~ "Revision",
      primary_or_revision_surger == 3 ~ "Non-ACL only", 
      primary_or_revision_surger == 4 ~ "Manipulation Under-Anesthesia",
      ),
    graft_repair = case_when(
      graft_repair == 1 ~ "QPA", 
      graft_repair == 2 ~ "Repair", 
      graft_repair == 3 ~ "Contralateral QPA", 
      graft_repair == 4 ~ "BTB",
      graft_repair == 5 ~ "Hamstring",
      graft_repair == 6 ~ "IT Band",
      graft_repair == 7 ~ "Allograft", 
      graft_repair == 8 ~ "Soft Tissue QT", 
      graft_repair == 9 ~ "Contralateral Soft Tissue QT", 
      graft_repair == 10 ~ "N/A", 
      graft_repair == 11 ~ "Other"
      ),
    surgical_technique = case_when(
      surgical_technique == 0 ~ "All-Epiphyseal", 
      surgical_technique == 1 ~ "Hybrid", 
      surgical_technique == 2 ~ "Transphyseal", 
      surgical_technique == 3 ~ "Repair", 
      surgical_technique == 4 ~ "Other", 
      surgical_technique == 5 ~ "N/A",
      surgical_technique == 6 ~ "Extra-Articular (IT Band)"
      ),
    primary_procedure = case_when(
      primary_associated_procedu___0 == 1 ~ "None", 
      primary_associated_procedu___1 == 1 ~ "Medial Meniscectomy", 
      primary_associated_procedu___2 == 1 ~ "Medial Repair",
      primary_associated_procedu___3 == 1 ~ "Lateral Meniscectomy",
      primary_associated_procedu___4 == 1 ~ "Lateral Repair", 
      primary_associated_procedu___5 == 1 ~ "MCL", 
      primary_associated_procedu___6 == 1 ~ "LCL", 
      primary_associated_procedu___7 == 1 ~ "PLC", 
      primary_associated_procedu___8 == 1 ~ "ALL", 
      primary_associated_procedu___9 == 1 ~ "PCL", 
      primary_associated_procedu___10 == 1 ~ "Other"
      )
    )

dat_sum <- dat_sum %>% select(-contains(c("redcap", "asso", "other")))

as.list(dat_sum %>% select(pid, primary_or_revision_surger) %>% 
  filter(primary_or_revision_surger == "Revision") %>% 
  select(pid))


## Summary Tables 

label(dat_sum$age_at_surgery) <- "Age at Surgery (Years)"
label(dat_sum$nih_sex) <- "Sex"
label(dat_sum$nih_race) <- "Race"
label(dat_sum$nih_ethnicity) <- "Ethnicity"
label(dat_sum$athletic_involvment) <- "Athletic Involvment"
label(dat_sum$mechanism_of_injury1) <- "Mechanism of Injury"
label(dat_sum$how_did_your_injury_happen) <- "How Did Injury Happen?"
label(dat_sum$dominant_leg) <- "Dominant Leg"
label(dat_sum$primary_or_revision_surger) <- "Primary or Revision Surgery"
label(dat_sum$graft_repair) <- "Graft Repair"
label(dat_sum$surgical_technique) <- "Surgical Technique"
label(dat_sum$primary_procedure) <- "Primary Procedure"
label(dat_sum$laterality) <- "Laterality"

table1(
  ~ .,
  data = dat_sum[, 2:14],
  # overall = F,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
  )
