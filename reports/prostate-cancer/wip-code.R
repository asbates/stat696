
library(here)  # handles directories
library(readr)  # read in data
library(dplyr)  # manipulate data
library(forcats) # handle factors
library(ggplot2)  # plotting
library(knitr)  # for outputting data.frames and tables
library(kableExtra) # formatting for outputted data.frames and tables

# note: knitr and KableExtra are used to format the .pdf output
# as such, they are not necessary to conduct the analysis

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

race_tab <- table(prostate$penetrate,
                  Race = prostate$race)
dre_tab <- table(`Capsule Penetration` = prostate$penetrate,
                 `DRE Result` = prostate$dre)
caps_tab <- table(`Capsule Penetration` = prostate$penetrate,
                  `Capsular Involvement` = prostate$caps) # deparse.level = 2
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

# function used to output clean table
#  dplyr, knitr, and KableExtra are needed for this to work
format_tab <- function(tab){
  m <- nrow(tab)
  n <- ncol(tab)
  tab %>%
    prop.table(., margin = 2) %>%
    round(., 2) %>%
    kable() %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(n, border_right = TRUE) %>%
    row_spec(1, bold = TRUE, hline_after = TRUE) %>%
    row_spec(m, hline_after = TRUE)
}

format_tab(race_tab)


# -------------- plots ----------------

# penetrate vs. age bar plot
ggplot(prostate, aes(x = age, fill = penetrate)) +
  geom_bar(position = "dodge") +
  labs(fill = "Capsule Penetration")

ggplot(prostate, aes(x = penetrate, y = psa)) +
  geom_boxplot()

ggplot(prostate, aes(x = penetrate, y = age)) +
  geom_boxplot()
