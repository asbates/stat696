
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(MASS)
library(car)
library(broom)

ny <- read_csv("reports/ny_expenditure/ny_expend.csv")

# =====================================================
# initial data cleaning. should have done this before.
# only need to run once
ny <- ny %>%
  select(-X1, -obs)
ny <- ny %>%
  select(-st, -co)
ny
ny <- ny %>%
  rename(
    expenditure = expen,
    perc_intergov = pint,
    pop_dens = dens,
    grow_rate = growr
  )
ny <- ny %>%
  select(-id) %>%
  select(expenditure,
         wealth,
         income,
         pop,
         pop_dens,
         perc_intergov,
         grow_rate)

write_csv(ny, "reports/ny_expenditure/ny_expend.csv")
# =======================================================



# =======================================================
# ----------------- EDA ------------------------
# =======================================================

# ------ numerical summaries --------
summary(ny)
ny <- filter(ny, !is.na(expenditure)) # remove 2 rows with missing values
dim(ny) # 914 x 7
summary(ny)
corr_mat <- cor(ny)
corrplot(corr_mat, type = "upper", diag = FALSE) # correlation matrix plot

# highest correlations
cor(ny$pop, ny$pop_dens) # 0.67
cor(ny$income, ny$pop_dens) # 0.49
cor(ny$income, ny$perc_intergov) # -0.30
cor(ny$income, ny$pop) # 0.29

# note: income and pop have lower correlation than income and pop_dens
# so may want to include pop instead of pop_dens

ny <- ny %>%
  select(-pop_dens)


# ----------- graphics -----------------------

# ---  histograms and density plots ----

# function to create histogram and density given variable name
# this is to minimize copy-pasting. 
# as is, it's really only useful in this script
hist_dens <- function(var){
  plot_var <- enquo(var)
  ggplot(ny, aes(x = !! plot_var, y = ..density..)) +
    geom_histogram(bins = 50) +
    geom_density()
}

hist_dens(expenditure)
hist_dens(wealth)
hist_dens(income)
hist_dens(pop)
hist_dens(perc_intergov)
hist_dens(grow_rate)


# all variables are right-skewed so we log transform them all
# however, since growth rate contains negative values,
# we use a modified log transform
ny <- ny %>%
  mutate(
    log_expenditure = log(expenditure),
    log_wealth = log(wealth),
    log_income = log(income),
    log_pop = log(pop),
    log_perc_intergov = log(perc_intergov),
    log_grow_rate = ifelse(grow_rate > 0,
                           log(grow_rate + 0.15),
                           -log(-grow_rate + 0.15))
  )

hist_dens(log_expenditure)
hist_dens(log_wealth)
hist_dens(log_income)
hist_dens(log_pop)
hist_dens(log_perc_intergov)
hist_dens(log_grow_rate)

# these all look much more symmetric
# there is a possible issue: log_grow_rate is bimodal
# however, in the interest of time, let's not worry about that for now


# ---  scatterplots: response vs. predictors ----

# function to create scatterplot of log expenditure vs. log covariate given
# covariate name. also adds a scatter plot smooth
scatter_smooth_log <- function(var){
  x_var <- enquo(var)
  ggplot(ny, aes(x = !! x_var, y = log_expenditure)) +
    geom_point() +
    geom_smooth(method = loess, formula = y ~ x) # see also method = lm
}

scatter_smooth_log(log_wealth)
scatter_smooth_log(log_income)
scatter_smooth_log(log_pop)
scatter_smooth_log(log_perc_intergov)
scatter_smooth_log(log_grow_rate)

# all the above plots show an approximatley linear relationship 
# except log expenditure vs. log population
# in the following plot, it looks like if we might be able to
# split log population into 2 groups and get an approximately linear relation

scatter_smooth_log(log_pop) + 
  geom_vline(xintercept = 8.3)

# let's investigate further by splitting log population:
# log population below and above 8.3
ny_low <- ny %>%
  filter(log_pop <= 8.3)

ny_high <- ny %>%
  filter(log_pop > 8.3)

ggplot(ny_low, aes(x = log_pop, y = log_expenditure)) +
  geom_point() +
  geom_smooth(method = loess, formula = y ~ x)

ggplot(ny_high, aes(x = log_pop, y = log_expenditure)) +
  geom_point() +
  geom_smooth(method = loess, formula = y ~ x)

# based on the above plots, it appears that the relationship between
# log expenditure and log population is approximately piecewise linear

# NOTE: ask Prof. Levine:
# do we even need to worry about having a fit for log_pop < 8.3?
# the values of log pop for Warwick and Monroe all fall under
# the log_pop > 8.3 subset
# should we check all the other variables wealth, income, etc.
# to make sure they fall within the range of values in the subset?
pairs(ny) # scatterplot matrix
# see also argument log to pairs. and diag.panel 
# also look at 580 code to see how to add density plot
pairs(ny, log = TRUE)





# =======================================================
# ----------------- model building ----------------------
# =======================================================

# we will use stepwise regression for model selection
# with AIC as our criteria


for_mod_fit_high <- ny_high %>%
  dplyr::select(log_expenditure,
                log_wealth,
                log_income,
                log_pop,
                log_perc_intergov,
                log_grow_rate) %>%
  mutate(
    log_wealth_2 = log_wealth^2,
    log_income_2 = log_income^2,
    log_pop_2 = log_pop^2,
    log_perc_intergov_2 = log_perc_intergov^2,
    log_grow_rate_2 = log_grow_rate^2
  )

# fit model without quadratic terms
# best AIC is -611.3
fit_high <- lm(log_expenditure ~ log_wealth +
                      log_income +
                      log_pop +
                      log_perc_intergov +
                      log_grow_rate,
                    data = for_mod_fit_high)
best_fit_high <- stepAIC(fit_high)
best_fit_high

# fit model with quadratic terms
# best AIC is -622.91
# this model is not used because:
# there isn't much of an improvement in AIC compared to the simpler model above
# this model includes quadratic terms without their respective linear terms
#   we just can not justify such a model
full_fit <- lm(log_expenditure ~., data = for_mod_fit_high)
best_full_fit <- stepAIC(fit)
best_full_fit

final_fit <- lm(log_expenditure ~ 
                  log_wealth +
                  log_pop + 
                  log_perc_intergov +
                  log_grow_rate,
                data = ny_high)

# =======================================================
# ----------------- model diagnostics -------------------
# =======================================================

# all p-values (except intercept) are significant at the 0.05 level
# F test significant at 0.05 level
summary(final_fit)

# all VIF's are less than 1.2
vif(final_fit)  # (car package)

# constant variance test (car package)
ncvTest(final_fit)

# --------------- diagnostic plots ----------------

# add fitted values, residuals, etc. to orignal data frame used to fit the model
ny_high <- augment(final_fit) # (broom package)

# residuals vs. fitted
ggplot(ny_high, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## qq plot
ny_high %>%
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()

# histogram
ny_high %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 50)


# attempt at using studentized residuals
dist_pars = list(df = final_fit$df.residual)
ny_high %>%
  mutate(stud = rstudent(final_fit)) %>%
  ggplot(aes(sample = stud)) +
  stat_qq(distribution = qt, dparms = dist_pars["df"]) +
  stat_qq_line(distribution = qt, dparms = dist_pars["df"])


