

library(ISLR)
library(dplyr)
library(ggplot2)
library(recipes)
library(caret)


data("Hitters")


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

# 18% of salary is missing
# we want these to be in training set b/c otherwise we can't compute 
#  test set error
# 322 * .8 = 257 so (aside from missing values), if our train set has
#  200 rows then since 200 / 322 = .6 we should use a .60/.40 split
#  on the data that doesn't contain any NA

na_index <- which(is.na(bball_raw$salary))

bball_nona <- filter(bball_raw, !is.na(salary))

train_index <- createDataPartition(bball_nona$salary,
                                   p = 0.6,
                                   list = FALSE)

bball_train <- bball_raw[c(train_index, na_index), ]

bball_test <- bball_raw[-c(train_index, na_index), ]

dim(bball_train) # 220
dim(bball_test)  # 133

bball_recipe <- recipe(salary ~., data = bball_raw) %>% 
  step_knnimpute(salary)

bball_prep <- prep(bball_recipe)
bball_bake <- bake(bball_prep, new_data = bball_raw)

bball_prep
bball_bake

sum(is.na(bball_bake$salary))
apply(bball_bake, 2, function(x)sum(is.na(x)))

bball_bake[na_index, "salary" ]




# ===================================================
# ========== this should be starting point ==========
# ===================================================



# this imputes the missing values for salary
#  and returns the data frame
bball <- recipe(salary ~., data = bball_raw) %>% 
  step_knnimpute(salary) %>% 
  prep() %>% 
  bake(new_data = bball_raw)


# ------  do any EDA here  ----
# and any transformations
# then do EDA on transformed variables


train_index <- createDataPartition(bball$salary, p = 0.8, list = FALSE)

bball_train <- bball[train_index, ]
bball_test <- bball[-train_index, ]


# log transform variables

# might need to write a custom step here
# because e.g. home_runs contain 0 so log can't be used
# other option is to use boxcox transform
bball_recipe <- recipe(salary ~., data = bball) %>% 
  step_log(salary,
           home_runs,
           career_at_bat,
           career_hits,
           career_home_run,
           career_runs,
           career_rbi,
           career_walks,
           put_outs,
           assists,
           errors)

bball_recipe

# create dummy variables
bball_recipe <- bball_recipe %>% 
  step_dummy(league, division, new_league)

# just to check it out
with_dummy <- bball_recipe %>% 
  prep() %>% 
  bake(new_data = bball)
with_dummy

# center, scale, and remove any vars w/near-zero variance
bball_recipe <- bball_recipe %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_nzv(all_predictors())

final <- bball_recipe %>% 
  prep() %>% 
  bake(new_data = bball)
final


# wow, this leaves us with only 13 variables
# maybe we should leave the step_nzv off

again <- bball_recipe %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep() %>% 
  bake(new_data = bball)

again

# ok so this is messed up too
# maybe center and scale need to happen BEFORE creating dummy vars


# try again after realizing many vars are dropped
# i think i need to center and scale before creating dummy variables
bball_recipe <- recipe(salary ~., data = bball) %>% 
  step_log(salary,
           home_runs,
           career_at_bat,
           career_hits,
           career_home_run,
           career_runs,
           career_rbi,
           career_walks,
           put_outs,
           assists,
           errors)

bball_recipe

# center and scale numeric predictors
bball_recipe <- bball_recipe %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) #%>% 
  #step_nzv(all_predictors()) 
  
# seems like step_nzv() is removing 7 vars


# create dummy variables
bball_recipe <- bball_recipe %>% 
  step_dummy(league, division, new_league)

df <- bball_recipe %>% 
  prep() %>% 
  bake(new_data = bball)

df

# also note: any vars with zeros get turned to -Inf in log transform
# there are two things i can think of
#  1. use a different transformation (boxcox?)
#  2. write a new step (step_alt_log?) that adds e.g. 0.15 to each value
#     before taking the log similar to DAR 1