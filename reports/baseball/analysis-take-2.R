library(here)
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


model_recipe <- recipe(log_salary ~., data = training) %>% 
  step_dummy(league, division, new_league) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())


# specify training scheme
train_control <- trainControl(method = "cv", number = 5)

# specify training parameters to try
lasso_grid <- expand.grid(alpha = 1,
                          lambda = seq(0.01, 1, length.out = 10))



# ==============================
# ===== LINEAR REGRESSION ======
# ==============================


set.seed(30)
lr_trained <- train(model_recipe,
                 data = training,
                 method = "lmStepAIC",
                 trControl = train_control,
                 trace = 0) # keep MASS::stepAIC from printing every output

lr_trained # RMSE = 0.552
lr_step <- lr_trained$finalModel # this gives the model
summary(lr_step)
# high p-values: log_put_outs (0.13)

vif(lr_step)  # at_bats = 19, hits = 17


# remove at_bats and keep hits
# b/c of the remaining variables in the model,
#   at_bats has higer correlations than hits

# new model specification with smaller # of variables
lr_small_recipe <- recipe(log_salary ~ hits +
                            log_home_runs +
                            rbis +
                            walks +
                            division +
                            log_put_outs +
                            log_assists +
                            log_career_rbis,
                          data = training) %>% 
  step_dummy(division) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

set.seed(30)
lr_step_small_trained <- train(lr_small_recipe,
                      data = training,
                      method = "lm",
                      trControl = train_control)

lr_step_small_trained # RMSE = 0.530
lr_step_small <- lr_step_small_trained$finalModel

# --- diagnostics -----
summary(lr_step_small) # high p-values: log_put_outs (0.2) 
#                                       & log_career_rbis (0.2)
vif(lr_step_small) # these are much better. largest is 5.9

lr_aug <- augment(lr_step_small)

# residuals vs. fitted values
ggplot(lr_aug, aes(x = .fitted, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "blue")

# qq plot
ggplot(lr_aug, aes(sample = .resid)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm, color = "blue")

# residual plot has one troublesome point
# the qq plot looks good
# since the primary goal of this analysis is predictions,
#  we will not worry about the lone point in the residual plot


varImp(lr_step_small) # absolute value of t-statistic for each parameter

plot(varImp(lr_step_small))


# save models to save time compiling report
# these are saved locally
saveRDS(lr_trained, here("reports", "baseball", "results", "lr_trained.rds"))
saveRDS(lr_step, here("reports", "baseball", "results", "lr_step.rds"))
saveRDS(lr_step_small_trained,
        here("reports", "baseball", "results", "lr_step_small_trained.rds"))
saveRDS(lr_step_small,
        here("reports", "baseball", "results", "lr_step_small.rds"))

# =======================
# ======= LASSO =========
# =======================

set.seed(30)
# note: this gives a warning about missing values in performance metrics
# this can be ignored because we are considering RMSE
lasso_trained <- train(model_recipe,
                      data = training,
                      method = "glmnet",
                      trControl = train_control,
                      tuneGrid = lasso_grid)

lasso_trained # RMSE = 0.536

lambda <- lasso_trained$bestTune$lambda
coef(lasso_trained$finalModel, s = lambda) # model coefficients

plot(lasso_trained)

# variable importance measures and plot
lasso_imp <- varImp(lasso_trained)$importance %>% 
  mutate(
    Term = c("At bats", "Hits", "Log home runs", "Runs",
             "RBIs", "Walks", "Years", "Log put outs",
             "Log assists", "Log errors", "Log career at bats",
             "Log career hits", "Log career home runs", "Log career runs",
             "Log career RBIs", "Log career walks", "League: National",
             "Division: West", "New league: National")
  ) %>% 
  filter(Overall > 0)

ggplot(lasso_imp, aes(x = reorder(Term, Overall), y = Overall)) +
  geom_point() +
  geom_segment(aes(x = Term, xend = Term, y = 0, yend = Overall)) +
  labs(x = "",
       y = "Lasso variable importance (%)") +
  coord_flip()

# save model
saveRDS(lasso_trained,
        here("reports", "baseball", "results", "lasso_trained.rds"))



# ============================
# ===== RANDOM FOREST ========
# ============================

set.seed(30)
rf_trained <- train(model_recipe,
                    data = training,
                    method = "rf",
                    trControl = train_control,
                    tuneLength = 10,
                    importance = TRUE)

rf_trained # RMSE = 0.429

plot(rf_trained)

# variable importance measures and plot
rf_imp <- varImp(rf_trained)$importance %>% 
  mutate(
    Term = c("At bats", "Hits", "Log home runs", "Runs",
             "RBIs", "Walks", "Years", "Log put outs",
             "Log assists", "Log errors", "Log career at bats",
             "Log career hits", "Log career home runs", "Log career runs",
             "Log career RBIs", "Log career walks", "League: National",
             "Division: West", "New league: National")
  ) %>% 
  filter(Overall > 0)

ggplot(rf_imp, aes(x = reorder(Term, Overall), y = Overall)) +
  geom_point() +
  geom_segment(aes(x = Term, xend = Term, y = 0, yend = Overall)) +
  labs(x = "",
       y = "Random forest variable importance (%)") +
  coord_flip()


# save model
saveRDS(rf_trained,
        here("reports", "baseball", "results", "rf_trained.rds"))



# ============================
# ====== TEST SET ERROR ======
# ============================


# --- linear regression ----
lr_test_pred <- predict(lr_step_small_trained, newdata = testing)

# testing RMSE = 0.687
postResample(pred = lr_test_pred, obs = testing$log_salary)


# ---- lasso ------
lasso_test_pred <- predict(lasso_trained, newdata = testing)

# testing RMSE = 0.706
postResample(pred = lasso_test_pred, obs = testing$log_salary)


# ----- random forest -----
rf_test_pred <- predict(rf_trained, newdata = testing)

# testing RMSE = 0.460
postResample(pred = rf_test_pred, obs = testing$log_salary)


