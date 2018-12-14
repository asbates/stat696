library(ISLR)
library(MASS) # load MASS first to prevent from masking dplyr::select
library(dplyr)
library(purrr)
library(recipes)
library(corrplot)
library(ggplot2)
library(caret)
library(broom)
library(car)
library(glmnet)
library(randomForest)




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

# correlation plot
bball %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot(type = "lower",
           addCoef.col = "black",
           addCoefasPercent = TRUE,
           number.cex = 0.7,
           tl.srt = 10)


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
           number.cex = 0.7,
           tl.srt = 10)

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


# specify preprocessing which later gets used to create model matrices
lr_recipe <- recipe(log_salary ~., data = training) %>% 
  step_dummy(league, division, new_league)

lasso_rf_recipe <- recipe(log_salary ~., data = training) %>% 
  step_dummy(league, division, new_league) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

# --------------
# NOTE: see also step_corr(., threshold = 0.9)
# this will remove any vars with correlation over 0.9



# specify training scheme
train_control <- trainControl(method = "cv", number = 5)

# specify training parameters to try
lasso_grid <- expand.grid(alpha = 1,
                          lambda = seq(0.01, 1, length.out = 10))



# ==============================
# ===== LINEAR REGRESSION ======
# ==============================


set.seed(30)
lr_trained <- train(lr_recipe,
                 data = training,
                 method = "lmStepAIC",
                 trControl = train_control,
                 trace = 0) # keep MASS::stepAIC from printing every output

lr_trained # RMSE = 0.552
lr_step <- lr_trained$finalModel # this gives the model
summary(lr_step)
# 2 high p-values: log_put_outs & log_assists

vif(lr_step)  # at_bats = 19, hits = 17

# remove at_bats and keep hits
# b/c of the remaining variables in the model,
#   at_bats has higer correlations than hits

lr_step_sub <- lm(log_salary ~ hits +
                 log_home_runs +
                 rbis +
                 walks +
                 division +
                 log_put_outs +
                 log_assists +
                 log_career_rbis,
               data = training)

summary(lr_step_sub) # one high p-value (barely): log_home_runs (0.059)
vif(lr_step_sub) # this is much better

plot(lr_step_sub)

lr_step_sub_pred <- predict(lr_step_sub, newdata = testing)
sqrt(mean( (training$log_salary - lr_step_sub$fitted.values)^2 )) 
# RMSE = 0.512. lower than initial fit

# we could remove log_put_outs & log_assists as well
# or we could take another approach:
#  before model fitting, remove pairs of variables that have high correlations

lr_recipe_corr <- recipe(log_salary ~., data = training) %>% 
  step_corr(all_numeric(), threshold = 0.9) %>% 
  step_dummy(league, division, new_league)

set.seed(30)
lr_trained_corr <- train(lr_recipe_corr,
                         data = training,
                         method = "lmStepAIC",
                         trControl = train_control,
                         trace = 0)

lr_trained_corr 
# RMSE = 0.535. 
# lower than initial fit but higher than after removing at_bats
lr_step_corr <- lr_trained_corr$finalModel
summary(lr_step_corr) 
# one large p-value: log_put_outs (0.13)
vif(lr_step_corr) # better than after manually removing at_bats


# we will go with lr_step_sub as the final model
# both this and lr_step_corr have one high p-value 
#  with lr_step_corr being lower
# vif's for lr_step_sub are higher but the largest one is still only moderate
# additionally, lr_step_sub has slightly lower RMSE which is the main
#  goal for this analysis

# actually, we should re-fit the model using caret::train

lr_step_sub2 <- train(log_salary ~ hits +
                       log_home_runs +
                       rbis +
                       walks +
                       division +
                       log_put_outs +
                       log_assists +
                       log_career_rbis,
                     data = training,
                     method = "lm")

varImp(lr_step_sub2) # absolute value of t-statistic for each parameter

plot(varImp(lr_step_sub2))


# yet another approach that actually is probably better
# center and scale before doing linear regresion too
# this makes the interpretations: a one sd increase in blah is associated ...

set.seed(30)
lr_center_scale <- train(lasso_rf_recipe,
                    data = training,
                    method = "lmStepAIC",
                    trControl = train_control,
                    trace = 0) # keep MASS::stepAIC from printing every output

lr_center_scale

summary(lr_center_scale$finalModel)

# the resulting model should be the same but not the interpretation

# =======================
# ======= LASSO =========
# =======================

set.seed(30)
# note: this gives a warning about missing values in performance metrics
# this can be ignored because we are considering RMSE
lasso_trained <- train(lasso_rf_recipe,
                      data = training,
                      method = "glmnet",
                      trControl = train_control,
                      tuneGrid = lasso_grid)

lasso_trained # RMSE = 0.536

lambda <- lasso_trained$bestTune$lambda
coef(lasso_trained$finalModel, s = lambda) # model coefficients

plot(lasso_trained)

varImp(lasso_trained, lambda = lambda) # absolute value of coefficients
plot(varImp(lasso_trained, lambda = lambda))

# ------ trying to see if can get augment()
lasso_df <- lasso_rf_recipe %>% 
  prep() %>% 
  bake(new_data = training)

lasso_mod_mat <- model.matrix(log_salary ~., data = lasso_df)

lasso_fit <- cv.glmnet(lasso_mod_mat,
                    training$log_salary,
                    lambda = c(0.01, 0.02)) 
# cv.glmnet needs more than one lambda value

tidy(lasso_fit)
tidy(lasso_fit) 
summary(lasso_fit)
augment(lasso_fit)

broom::augment(lasso_trained$finalModel) # no method for enet
broom::augment.glmnet(lasso_trained$finalModel)


# ============================
# ===== RANDOM FOREST ========
# ============================

set.seed(30)
rf_trained <- train(lasso_rf_recipe,
                    data = training,
                    method = "rf",
                    trControl = train_control,
                    tuneLength = 10,
                    importance = TRUE)

rf_trained # RMSE = 0.429

( (0.536 - 0.429) / 0.536 ) * 100 # 20% reduction in error

plot(rf_trained)
varImp(rf_trained)
plot(varImp(rf_trained))


ggplot(rf_trained, mapping = NULL, top = dim(rf_trained$importance)[1])

rf_imp <- varImp(rf_trained)

ggplot(rf_imp)

rf_tick_labels <- rev(c("log career hits",
                    "log career at bats",
                    "log career runs",
                    "log career rbis",
                    "log career walks",
                    "log career home runs",
                    "years",
                    "at bats",
                    "hits",
                    "log home runs",
                    "runs",
                    "rbis",
                    "walks",
                    "log put outs",
                    "division west",
                    "log errors",
                    "league national",
                    "log assists",
                    "new league national"))
ggplot(rf_imp) +
  scale_x_discrete(labels = rf_tick_labels)
