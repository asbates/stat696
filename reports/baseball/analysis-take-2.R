library(ISLR)
library(dplyr)
library(purrr)
library(recipes)
library(corrplot)
library(ggplot2)
library(caret)


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

# any NAs?
map_int(bball_raw, ~sum(is.na(.)))
# covariates don't have any missing values but response (salary) has 59


# ------ impute missing values of salary -------
bball <- recipe(salary ~., data = bball_raw) %>% 
  step_knnimpute(salary) %>% 
  prep() %>% 
  bake(new_data = bball_raw)

# just a check to make sure there are no NAs
map_int(bball, ~sum(is.na(.)))


# ============================
# =========== EDA ============
# ============================


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


bball %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot(type = "lower",
           addCoef.col = "black",
           addCoefasPercent = TRUE,
           number.cex = 0.7)

# is there a way to set a cutoff for plotting?
# for example, only put numbers in if > 0.8



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
# while not all of them look great, they are better than before
gghist(bball_logged, log_salary)
gghist(bball_logged, log_home_runs, bins = 20)
gghist(bball_logged, log_career_abs) 
gghist(bball_logged, log_career_hits)
gghist(bball_logged, log_career_hrs)
gghist(bball_logged, log_career_runs)
gghist(bball_logged, log_career_rbis)
gghist(bball_logged, log_career_walks, bins = 20)
gghist(bball_logged, log_put_outs)
gghist(bball_logged, log_assists)
gghist(bball_logged, log_errors, bins = 20)


# select appropriate variables for modeling
bball_log_only <- bball_logged %>% 
  select(log_salary,
         at_bats,
         hits,
         log_home_runs,
         runs,
         rbis,
         walks,
         years,
         league,
         division,
         new_league,
         log_put_outs,
         log_assists,
         log_errors,
         log_career_abs,
         log_career_hits,
         log_career_hrs,
         log_career_runs,
         log_career_rbis,
         log_career_walks
         )

# function to produce scatter plots
ggscatter <- function(data, var){
  var <- enquo(var)
  ggplot(data, aes(x = !!var, y = log_salary)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
}

# scatter plots of salary vs. covariate with smoothing line
# they are all approximately linear, no drastic deviations
ggscatter(bball_log_only, at_bats)
ggscatter(bball_log_only, hits)
ggscatter(bball_log_only, log_home_runs)
ggscatter(bball_log_only, runs)
ggscatter(bball_log_only, rbis)
ggscatter(bball_log_only, walks)
ggscatter(bball_log_only, years)
ggscatter(bball_log_only, log_put_outs)
ggscatter(bball_log_only, log_assists)
ggscatter(bball_log_only, log_errors)
ggscatter(bball_log_only, log_career_abs)
ggscatter(bball_log_only, log_career_hits)
ggscatter(bball_log_only, log_career_hrs)
ggscatter(bball_log_only, log_career_runs)
ggscatter(bball_log_only, log_career_rbis)
ggscatter(bball_log_only, log_career_walks)


# correlation matrix
bball_log_only %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot(type = "lower",
           addCoef.col = "black",
           addCoefasPercent = TRUE,
           number.cex = 0.7)

# =============================
# ====== MODELING SETUP =======
# =============================

# create train/test data
set.seed(42)
train_index <- createDataPartition(bball_log_only$log_salary,
                                   p = 0.8,
                                   list = FALSE)
training <- bball_log_only[train_index, ]
testing <- bball_log_only[-train_index, ]

dim(training) # 259 observations for training
dim(testing) # 63 observations for testing


# create model matrices
lr_recipe <- recipe(log_salary ~., data = training) %>% 
  step_dummy(league, division, new_league)

lr_model_mat <- lr_recipe %>% 
  prep() %>% 
  bake(new_data = training)

lasso_rf_recipe <- recipe(log_salary ~., data = training) %>% 
  step_dummy(league, division, new_league) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

lasso_rf_model_mat <- lasso_rf_recipe %>% 
  prep() %>% 
  bake(new_data = training)

# --------------
# NOTE: see also step_corr(., threshold = 0.9)
# this will remove any vars with correlation over 0.9

# -----------------
# also: may not need to prep() and bake() here

# specify training parameters
train_control <- trainControl(method = "cv", number = 5)


# ==============================
# ===== LINEAR REGRESSION ======
# ==============================

# we use 5-fold cross validation
# with stepwise model selection via AIC
lr_control <- trainControl(method = "cv", number = 5)

set.seed(30)
lr_train <- train(log_salary ~.,
                 data = training,
                 method = "lmStepAIC",
                 trControl = lr_control,
                 trace = 0) # keep MASS::stepAIC from printing every output

lr_train
names(lr_train)
lr_train$results # prediction results. RMSE, Rsquared, etc.
lr_train$finalModel # this gives the model

class(lr_train$finalModel)


lr_step <- lr_train$finalModel

# this actually doesn't look too bad
summary(lr_step)
plot(lr_step)  

# formula = .outcome ~ at_bats + hits + log_home_runs + rbis + 
# walks + divisionW + log_put_outs + log_assists + log_career_rbis

# high p-value: rbis (0.074), log_put_outs (0.133)
# possible problem points: 38, 118, 174, 228, 

car::vif(lr_step)  # wow! at_bats = 19, hits = 17

# not really surprising b/c correlation is 0.97
# let's remove one of these

# i think we should remove at_bats and keep hits
# b/c of the remaining variables in the model,
#   at_bats has higer correlations than hits

lr_final <- lm(log_salary ~ hits +
                 log_home_runs +
                 rbis +
                 walks +
                 division +
                 log_put_outs +
                 log_assists +
                 log_career_rbis,
               data = training)

summary(lr_final)
car::vif(lr_final)
# high p-values: log_put_outs (0.2), log_assists (0.2)
# largest vif: rbis (5.96). this is moderate according to lecture slides
#   week 5, Regression_modeling.pdf

plot(lr_final)
# looks decent enough. same possible problem points

lr_pred <- predict(lr_final, newdata = testing)

RMSE(lr_pred, testing$log_salary) # 0.687


# is this the final model i want to present?
# i don't think so b/c this is really a prediction problem as posed
# but then again he wants us to go through typical lm building
# so who knows
lr_full_fit <- lm(log_salary ~ hits +
                 log_home_runs +
                 rbis +
                 walks +
                 division +
                 log_put_outs +
                 log_assists +
                 log_career_rbis,
               data = bball_log_only)

summary(lr_full_fit)
plot(lr_full_fit)



# =======================
# ======= LASSO =========
# =======================

lasso_control <- trainControl(method = "cv", number = 5)

set.seed(30)
lasso_train <- train(log_salary ~.,
                     data = training,
                     method = "lasso",
                     trControl = lasso_control)

lasso_train

names(lasso_train)
lasso_train$results


lasso_fit <- lasso_train$finalModel

summary(lasso_fit) # not really helpful
names(lasso_fit)

lasso_fit$beta.pure
lasso_fit$param
coef(lasso_fit) # NULL
lasso_fit$lambda

broom::augment(lasso_fit) # no method for enet

# should i try to use glmnet instead?
# will that allow me to get a summary like lm?
# maybe i should just not worry about it and just present the
#  CV error, prediction error, and predictions



# don't extract model from train()
# use predict.train in caret instead



# linear model
set.seed(30)
lr_trained <- train(lr_recipe,
                  data = training,
                  method = "lmStepAIC",
                  trControl = lr_control,
                  trace = 0) # keep MASS::stepAIC from printing every output


lr_trained
lr_trained$finalModel

best_step_lr <- lr_trained$finalModel
summary(best_step_lr)
# this should be the same as before

# now let's try removing any variables with correlation > 0.9

lr_recipe_corr <- lr_recipe %>% 
  step_corr(all_predictors(), threshold = 0.9)

lr_trained_corr <- train(lr_recipe,
                         data = training,
                         method = "lmStepAIC",
                         trControl = lr_control,
                         trace = 0) # keep MASS::stepAIC from printing every output

lr_trained_corr
summary(lr_trained_corr$finalModel)




lr_recipe_corr %>% 
  prep() %>% 
  bake(new_data = training)


lr_recipe_corr <- recipe(log_salary ~., data = training) %>% 
  step_corr(all_numeric(), threshold = 0.9) %>% 
  step_dummy(league, division, new_league)


lr_trained_corr <- train(lr_recipe_corr,
                         data = training,
                         method = "lmStepAIC",
                         trControl = lr_control,
                         trace = 0)

summary(lr_trained_corr)

names(lr_trained_corr)
lr_trained_corr$recipe


lr_first_fit <- lr_trained_corr$finalModel

summary(lr_first_fit)
car::vif(lr_first_fit) # nice!

lr_trained_corr$results  # and the RMSE is actually lower!