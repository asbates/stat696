
library(ISLR)
library(dplyr)
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
  )









