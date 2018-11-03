
library(here)
library(readr)
library(dplyr)

prostate <- read_tsv(here("data", "prostate.txt"))

# ======================
# ======= EDA ==========
# ======================

dim(prostate)

# how many have experience capsule penetration?
prostate %>%
  group_by(capsule) %>%
  summarise(n = n())

# any missing values?
apply(prostate, 2, function(x)sum(is.na(x)))



