
library(here)  # handles directories
library(readr)  # read in data
library(MASS) # stepwise regression
library(dplyr)  # manipulate data
library(forcats) # handle factors
library(ggplot2)  # plotting
library(broom) # tidy model output


prostate <- read_tsv(here("data", "prostate.txt"))


# =====================================
# ======== SETUP/CLEANING =============
# =====================================

dim(prostate)
names(prostate)
str(prostate)

# remove id, rename
prostate <- prostate %>%
  select(-id)

prostate <- prostate %>%
  rename(
    penetrate = capsule,
    dre = dpros,
    caps = dcaps
  )

# re-encode variables
prostate <- prostate %>%
  mutate(
    penetrate = as.factor(penetrate),
    race = as.factor(race),
    dre = as.factor(dre),
    caps = as.factor(caps)
  ) %>%
  mutate(
    penetrate = fct_recode(penetrate, yes = "1", no = "0"),
    race = fct_recode(race, white = "1", black = "2"),
    caps = fct_recode(caps, no = "1", yes = "2"),
    dre = fct_recode(dre,
                     "no nodule" = "1",
                     "unilobar left" = "2",
                     "unilobar right" = "3",
                     "bilobar" = "4")
  )

str(prostate)


# how many have experience capsule penetration?
prostate %>%
  group_by(penetrate) %>%
  summarise(n = n())

# any missing values?
apply(prostate, 2, function(x) sum(is.na(x)) )

# race has 3 missing values
summary(prostate)

prostate %>%
  filter(is.na(race))

# remove missing observations
prostate <- prostate %>%
  filter(!is.na(race))

dim(prostate)
prostate %>%
  group_by(penetrate) %>%
  summarise(n = n())

# ======================================
# =============== EDA ==================
# ======================================

# ----- categorical predictors -----
race_tab <- table(`Capsule Penetration` = prostate$penetrate,
                  Race = prostate$race)
dre_tab <- table(`Capsule Penetration` = prostate$penetrate,
                 `DRE Result` = prostate$dre)
caps_tab <- table(`Capsule Penetration` = prostate$penetrate,
                  `Capsular Involvement` = prostate$caps)
gleason_tab <- table(`Capsule Penetration` = prostate$penetrate,
                     `Gleason Score` = prostate$gleason)

race_tab
dre_tab
caps_tab
gleason_tab

# proportion tables marginalized by covariate
prop.table(race_tab, margin = 2) * 100
prop.table(dre_tab, margin = 2) * 100
prop.table(caps_tab, margin = 2) * 100
prop.table(gleason_tab, margin = 2) * 100


# ------------ numeric predictors --------------

# ---- age -----

# sumarize age by prostate
age_sum <- prostate %>% 
  group_by(penetrate) %>%
  summarise(
    min = min(age),
    median = median(age),
    mean = mean(age),
    max = max(age)
  )


# bar chart of age
ggplot(prostate, aes(x = age)) +
  geom_bar()

# penetrate vs. age bar plot
ggplot(prostate, aes(x = age, fill = penetrate)) +
  geom_bar(position = "dodge") +
  labs(fill = "Capsule Penetration")

# boxplot - age vs. penetrate
ggplot(prostate, aes(x = penetrate, y = age)) +
  geom_boxplot()

# ----- psa ----
# summary stats for psa by prostate
prostate %>% 
  group_by(penetrate) %>% 
  summarise(
    min = min(psa),
    median = median(psa),
    mean = mean(psa),
    max = max(psa)
  )

# histogram of psa
ggplot(prostate, aes(x = psa)) +
  geom_histogram(bins = 50)

# histogram of psa vs. penetrate
ggplot(prostate, aes(x = psa, fill = penetrate)) +
  geom_histogram(bins = 50)

# boxplot - psa vs. penetrate
ggplot(prostate, aes(x = penetrate, y = psa)) +
  geom_boxplot()


# =====================================
# ========== MODELING =================
# =====================================


# fit a 'full' model with all first-order interactions
full_fit <- glm(penetrate ~.*.,
                data = prostate,
                family = binomial(link = "logit"))

# 'best' fit by stepwise regression
best_step <- stepAIC(full_fit, data = prostate)

best_step
summary(best_step)


final_fit <- glm(penetrate ~ dre + psa + gleason,
                 data = prostate,
                 family = binomial(link = "logit"))

# =====================================
# ========== DIAGNOSTICS ==============
# =====================================






# =====================================
# ========== INFERENCE ================
# =====================================

final_fit
summary(final_fit)