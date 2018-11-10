
library(here)  # handles directories
library(readr)  # read in data
library(MASS) # stepwise regression
library(dplyr)  # manipulate data
library(forcats) # handle factors
library(ggplot2)  # plotting
library(broom) # tidy model output


prostate_raw <- read_tsv(here("data", "prostate.txt"))


# =====================================
# ======== SETUP/CLEANING =============
# =====================================

dim(prostate)
names(prostate)
str(prostate)

# remove id, rename
prostate <- prostate_raw %>%
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

# functions used for residual analysis
one_fourth_root=function(x){
  x^(0.25)
}
source(here("reports", "prostate-cancer","code","examine.logistic.reg.R"))

# note: we're using the 'raw' data here with the default encoding
pros_diag <- prostate_raw %>% 
  rename(
    penetrate = capsule,
    dre = dpros,
    caps = dcaps
  )


# Create EVPs by binning continuous covariates
g <- 5 # number of categories
psa_interval = cut(pros_diag$psa,
                   quantile(pros_diag$psa, 0:g/g), include.lowest = TRUE)

w <- aggregate(penetrate ~ psa_interval + gleason + dre,
               data = pros_diag,
               FUN = sum)
n <- aggregate(penetrate ~ psa_interval + gleason + dre,
               data = pros_diag,
               FUN = length)
wn <- data.frame(
  w,
  trials = n$penetrate, 
  prop = round(w$penetrate / n$penetrate, 2)
)

dim(wn)  # 74 EVPs

diag_fit <- glm(penetrate/trials ~ psa_interval + gleason + dre,
           data = wn,
           family = binomial(link = "logit"),
           weights = trials)

examine <- examine.logistic.reg(diag_fit,
                             identify.points = FALSE,
                             scale.n = one_fourth_root,
                             scale.cookd = sqrt)

wn_diag <- data.frame(
  wn,
  pi_hat = round(examine$pi.hat, 2),
  std_res = round(examine$stand.resid, 2),
  cooks_d = round(examine$cookd, 2),
  h = round(examine$h, 2)
)


p <- length(diag_fit$coefficients)
# locate points of interest
which_look_at <- 
  abs(wn_diag$std_res) > 2 | 
  wn_diag$cooks_d > 4 / nrow(wn) | 
  wn_diag$h > 3*p / nrow(wn)
look_at <- wn_diag[which_look_at, ]

# look at points of interest
look_at

# what are the actual values for the cutoffs
#  of cooks d and leverage h?
4/nrow(wn)
3 * p / nrow(wn)

# do any EVPs violate all criteria?
# no, they don't
which_violate_all <- 
  abs(wn_diag$std_res) > 2 & 
  wn_diag$cooks_d > 4 / nrow(wn) & 
  wn_diag$h > 3*p / nrow(wn)
any(which_violate_all)

# =====================================
# ========== INFERENCE ================
# =====================================

final_fit
summary(final_fit)