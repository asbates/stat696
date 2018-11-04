
library(here)
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)

prostate <- read_tsv(here("data", "prostate.txt"))

# ======================================
# =============== EDA ==================
# ======================================


# --------- numerical summaries ----------

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


# contingency tables

# age has 31 levels, probably treat as continuous
table(prostate$penetrate, prostate$age) 
table(prostate$penetrate, prostate$dre)
table(prostate$penetrate, prostate$caps)
table(prostate$penetrate, prostate$gleason)

# -------------- plots ----------------

# penetrate vs. age bar plot
ggplot(prostate, aes(x = age, fill = penetrate)) +
  geom_bar(position = "dodge") +
  labs(fill = "Capsule Penetration")

ggplot(prostate, aes(x = penetrate, y = psa)) +
  geom_boxplot()

ggplot(prostate, aes(x = penetrate, y = age)) +
  geom_boxplot()
