
library(ISLR)
library(dplyr)
library(purrr)
library(ggplot2)

# or do i want to use a tidymodels aproach?
# i would definitely like to but...
# do i have the time to learn the various packages?
# how many and what packages would i need?
# is the extra time cost worth it right now?
library(mlr) 


data("Hitters")


names(Hitters)

hitters <- Hitters %>% 
  rename(
    at_bat = AtBat,
    hits = Hits,
    home_runs = HmRun,
    runs = Runs,
    rbi = RBI,
    walks = Walks,
    years = Years,
    career_at_bat = CAtBat,
    career_hits = CHits,
    career_home_run = CHmRun,
    career_runs = CRuns,
    career_rbi = CRBI,
    career_walks = CWalks,
    league = League,
    division = Division,
    put_outs = PutOuts,
    assists = Assists,
    errors = Errors,
    salary = Salary,
    new_league = NewLeague
  ) %>% 
  as_tibble()

hitters

# =============================================
# ============ numerical summaries ============
# ============================================

# any NA's?
map(hitters, ~sum(is.na(.)))

# covariates don't have any missing values but response (salary) does
# i think we'll do imputation here
# possible methods: knn, random forest, median, mean (after transformation)

# let's go ahead and look at the rows with missing values first
# we might want to investigate this a bit further
# or not
# in reality i need to look into the various types of missing values
#  (MAR, MCAR, etc.) and try to figure out what method from there
# but that would probably be too much work and time

hitters %>% filter(is.na(salary))


# ----- response ---------

hitters %>% select(salary) %>%  summary()







