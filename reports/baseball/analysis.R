library(ISLR)
library(dplyr)
library(purrr)
library(ggplot2)
library(recipes)


# ===============================
# ========= setup ===============
# ===============================

data("Hitters")

bball_raw <- Hitters %>% 
  rename(
    at_bats = AtBat,
    hits = Hits,
    home_runs = HmRun,
    runs = Runs,
    rbis = RBI,
    walks = Walks,
    years = Years,
    career_at_bats = CAtBat,
    career_hits = CHits,
    career_home_runs = CHmRun,
    career_runs = CRuns,
    career_rbis = CRBI,
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

str(bball_raw)

# any NA's?
map_int(bball_raw, ~sum(is.na(.)))
# covariates don't have any missing values but response (salary) has 59


# ------ impute missing values of salary -------
bball <- recipe(salary ~., data = bball_raw) %>% 
  step_knnimpute(salary) %>% 
  prep() %>% 
  bake(new_data = bball_raw)

map_int(bball, ~sum(is.na(.)))

# ================================
# ========= INITIAL EDA ==========
# ================================


# function to produce histograms
gghist <- function(data, var, bins = 30){
  var <- enquo(var)
  ggplot(data, aes(!!var)) +
    geom_histogram(bins = bins)
}

# histograms of continuous variables
# those with a * indicate a transformation is in order
gghist(bball, salary) # *
gghist(bball, at_bats)
gghist(bball, hits)
gghist(bball, home_runs)  # *
gghist(bball, runs)
gghist(bball, rbis)
gghist(bball, walks)
gghist(bball, years, bins = 20)
gghist(bball, career_at_bats) # *
gghist(bball, career_hits) # *
gghist(bball, career_home_runs) # *
gghist(bball, career_runs) # *
gghist(bball, career_rbis)  # *
gghist(bball, career_walks, bins = 30) # *
gghist(bball, put_outs) # *
gghist(bball, assists) # *
gghist(bball, errors) # *


# categorical variables
ggplot(bball, aes(x = league)) +
  geom_bar()

ggplot(bball, aes(x = division)) +
  geom_bar()

ggplot(bball, aes(x = new_league)) +
  geom_bar()


# correlation plot
# mention this and how highly correlated vars present issues
# put plot in appendix
for_cor <- bball %>% 
  select_if(is.numeric)

bball_cor <- cor(for_cor)
corrplot::corrplot(bball_cor, 
                   method = "number",
                   type = "lower",
                   number.cex = 0.7)
# this one
# is there a way to set a cutoff for plotting?
# for example, only put numbers in if > 0.8
corrplot::corrplot(bball_cor, 
                   type = "lower",
                   addCoef.col = "black",
                   addCoefasPercent = TRUE,
                   number.cex = 0.7)



# ==============================
# ====== TRANSFORMATIONS =======
# ==============================

# variables needing transformation:
# salary
# home_runs
# career_at_bats
# career_hits
# career_home_runs
# career_runs
# career_rbis
# career_walks
# put_outs
# assists
# errors

# we will use log transformations. but...
# to do log transformations, we need to find which variables contain zeros
# for those that do, we use a modified log transformation

map_int(bball, ~any(. == 0))

# function to perform modified log transformation
# note that this will produce warnings about NaNs being produced
#  but these can be ignored b/c the final transformation is correct
mod_log <- function(x){
  ifelse(x > 0, log(x + 0.15), -log(-x + 0.15))
}

bball_logged <- bball %>% 
  mutate(
    log_salary = log(salary), 
    log_career_abs = log(career_at_bats), 
    log_career_hits = log(career_hits), 
    log_career_runs = log(career_runs) 
  ) %>% 
  mutate(
    log_home_runs = mod_log(home_runs), 
    log_career_hrs = mod_log(career_home_runs),
    log_career_rbis = mod_log(career_rbis), 
    log_career_walks = mod_log(career_walks), 
    log_put_outs = mod_log(put_outs), 
    log_assists = mod_log(assists), 
    log_errors = mod_log(errors) 
  )

# just a check to make sure no -Inf produced b/c of zeros
map_int(bball_logged, ~any(. == -Inf))


# =======================================
# ====== POST TRANSFORMATION EDA ========
# =======================================

# --- histograms of transformed variables
# while not great in some sense, they look better than before
gghist(bball_logged, log_salary)
gghist(bball_logged, log_home_runs)
gghist(bball_logged, log_career_abs)
gghist(bball_logged, log_career_hits)
gghist(bball_logged, log_career_hrs)
gghist(bball_logged, log_career_runs)
gghist(bball_logged, log_career_rbis)
gghist(bball_logged, log_career_walks) # bins = 20
gghist(bball_logged, log_put_outs)
gghist(bball_logged, log_assists)
gghist(bball_logged, log_errors)


# ===========================
# ===== NEW FEATURES ========
# ===========================

bball_new_feats <- bball_logged %>% 
  mutate(
    
    # season level continous
    hits_to_abs = hits / at_bats,
    hrs_to_abs = home_runs / at_bats,
    rbis_to_abs = rbis / at_bats,
    walks_to_abs = walks / at_bats,
    hrs_to_hits = home_runs / hits,
    runs_to_hits = runs / hits,
    runs_to_rbis = runs / rbis,
    
    # career level continuous
    career_hits_to_abs = career_hits / career_at_bats,
    career_hrs_to_abs = career_home_runs / career_at_bats,
    career_rbis_to_abs = career_rbis / career_at_bats,
    career_walks_to_abs = career_walks / career_at_bats,
    career_hrs_to_hits = career_home_runs / career_hits,
    career_runs_to_hits = career_runs / career_hits,
    career_runs_to_rbis = career_runs / career_rbis,
    
    # season level categorical
    experience = case_when(
      years <= 1              ~ "rookie",
      years > 1 & years <= 6  ~ "mid",
      years > 6 & years <= 11 ~ "veteran",
      TRUE                    ~ "aging"
    ),
    power_hitter = ifelse(home_runs > 16, "yes", "no"),
    offense_helper = ifelse(rbis > 65, "yes", "no"),
    butterfinger = ifelse(errors > 11, "yes", "no"),
    defense_level = case_when(
      put_outs <= 109                  ~ "low",
      put_outs > 109 & put_outs <= 325 ~ "medium",
      put_outs > 325                   ~ "high"
    ),
    offense_helper = ifelse(assists > 166, "yes", "no"),
    walker = ifelse(walks > 53, "yes", "no"),
    
    # career level categorical
    career_power_hitter = ifelse(career_home_runs > 90, "yes", "no"),
    career_offense_helper = ifelse(career_rbis > 426, "yes", "no"),
    career_walker = ifelse(career_walks > 339, "yes", "no")
  )

# summary stats to determine cutoffs for categorical variables
# these are all based on quantiles of the distribution
bball %>% 
  select(years,
         home_runs,
         rbis,
         errors,
         put_outs,
         assists,
         walks,
         career_home_runs,
         career_rbis,
         career_walks) %>% 
  summary()




# ==========
# some notes on what to do from here

# create new data frames as new variables are created
#  instead of trying to do this all in one recipe

# ---- prep features ----
# 1. log transform vars identified in initial eda (need custom ?)
# 2. redo histograms for these vars
# 3. create new features
# 4. plots of these new features
# 5. transformations if necessary
# 6. plots if did any transformations

# don't forget to plot response vs. predictor

# don't forget to do a correlation plot of new features

# ---- prep for models ----
# 7. pick appropriate predictors based on 1-6
# 8. center and scale numerics
# 9. create dummy vars
# 10. step_nzv ?

# -- fit models ---
# 11. creat train and test
# 12. 5-fold CV?
# 13. predict on test













