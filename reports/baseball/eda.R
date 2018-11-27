
library(ISLR)
library(dplyr)
library(purrr) # may or may not actually need. if only use once, don't include
library(ggplot2)
library(recipes)

# or do i want to use a tidymodels aproach?
# i would definitely like to but...
# do i have the time to learn the various packages?
# how many and what packages would i need?
# is the extra time cost worth it right now?
#library(mlr) 


data("Hitters")


names(Hitters)

bball_raw <- Hitters %>% 
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

bball_raw

# =============================
# ============ EDA ============
# =============================

# any NA's?
map(bball_raw, ~sum(is.na(.)))
#apply(hitters, 2, function(x)sum(is.na(x)))

# covariates don't have any missing values but response (salary) does
# i think we'll do imputation here
# possible methods: knn, random forest, median, mean (after transformation)

# let's go ahead and look at the rows with missing values first
# we might want to investigate this a bit further
# or not
# in reality i need to look into the various types of missing values
#  (MAR, MCAR, etc.) and try to figure out what method from there
# but that would probably be too much work and time

bball_raw %>% filter(is.na(salary))

bball_raw %>% select(salary) %>% summary()

summary(bball_raw)

str(bball_raw)


# ---- plots -----

ggplot(bball_raw, aes(salary)) +
  geom_histogram(bins = 30)

ggplot(bball_raw, aes(salary)) +
  geom_density()





# ====================================
# ===== feature engineering ==========
# ====================================


bball <- bball_raw %>% 
  mutate(
    hit_perc = hits / at_bat,
    hr_perc = home_runs / at_bat,
    rbi_perc = rbi / at_bat,
    walk_perc = walks / at_bat,
    career_hit_perc = career_hits / career_at_bat,
    career_hr_perc = career_home_run / career_at_bat,
    career_rbi_perc = career_rbi / career_at_bat,
    same_league = as.factor(ifelse(league == new_league, "yes", "no"))
  )

str(bball)

# correlation plot
for_cor <- bball %>% 
  select_if(is.numeric) %>% 
  filter(!is.na(salary))

bball_cor <- cor(for_cor)
corrplot::corrplot(bball_cor, 
                   method = "number",
                   type = "lower",
                   number.cex = 0.7)

corrplot::corrplot(bball_cor, 
                   type = "lower",
                   addCoef.col = "black",
                   addCoefasPercent = TRUE,
                   number.cex = 0.7)


names(bball)

# function to compute histograms
gghist <- function(data, var, bins = 30){
  var <- enquo(var)
  ggplot(data, aes(!!var)) +
    geom_histogram(bins = bins)
}

gghist(bball, salary) # needs transformation
gghist(bball, at_bat)
gghist(bball, hits)
gghist(bball, home_runs) # needs transformation
gghist(bball, runs)
gghist(bball, rbi)
gghist(bball, walks)
gghist(bball, years)
gghist(bball, career_at_bat)  # needs transformation
gghist(bball, career_hits) # needs transformation
gghist(bball, career_home_run) # needs transformation
gghist(bball, career_runs)  # needs transformation
gghist(bball, career_rbi)  # needs transformation
gghist(bball, career_walks)  # needs transformation
gghist(bball, put_outs)  # needs transformation
gghist(bball, assists)  # needs transformation
gghist(bball, errors)  # might need transformation
gghist(bball, hit_perc) # might need transformation
gghist(bball, hr_perc)
gghist(bball, rbi_perc)
gghist(bball, walk_perc)
gghist(bball, career_hit_perc)
gghist(bball, career_hr_perc)
gghist(bball, career_rbi_perc)

# categorical predictors
ggplot(bball, aes(x = league)) +
  geom_bar()

ggplot(bball, aes(x = division)) +
  geom_bar()

ggplot(bball, aes(x = new_league)) +
  geom_bar()

# looks like not very helpful b/c mostly yes
ggplot(bball, aes(x = same_league)) +
  geom_bar()

sum(bball$same_league == "no")
sum(bball$same_league == "yes")

names(bball)


bball_rec <- recipe(salary ~., data = bball) %>% 
  step_log(salary, home_runs, career_at_bat,
           career_hits, career_home_run, career_runs,
           career_rbi, career_walks, put_outs,
           assists, errors, hit_perc)

bball_rec <- bball_rec %>% 
  step



# maybe i should start with transformations
# then create features
# then see if those need transformations
# see step_mutate()
# make transformations, remove old vars, mutate, plot new vars


bball_rec <- recipe(salary ~., data = bball_raw) %>% 
  step_knnimpute(salary)



