
library(readr) # read in data
library(dplyr) # data manipulation
library(corrplot) # correlation plotting
library(ggplot2) # plotting
library(MASS) # stepwise regression
library(car)
library(broom) # tidy model output
library(here) # file path helper
library(xtable) # table formatting

ny <- read_csv(here("reports/ny_expenditure/ny_expend.csv"))

# =======================================================
# ----------------- EDA ------------------------
# =======================================================

# ---- prior to subsetting ----

# ------ numerical summaries --------
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

ny <- ny %>%
  dplyr::select(-pop_dens) # remove population density

# ---  histograms and density plots ----

# function to create histogram and density given variable name
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

lpop_smooth <- lowess(ny$log_pop, ny$log_expenditure)

ggplot(ny, aes(x = log_pop, y = log_expenditure)) +
  geom_point() +
  geom_line(aes(x = lpop_smooth$x, y = lpop_smooth$y), color = "#00BFC4") +
  geom_segment(aes(x = 8.3, xend = 8.3, y = 4, yend = 5.24), color = "#00BA38")


# ------ data subsetting ------
ny_low <- ny %>%
  filter(log_pop <= 8.3)

ny_high <- ny %>%
  filter(log_pop > 8.3)
dim(ny_high) # 266  12

ggplot(ny_low, aes(x = log_pop, y = log_expenditure)) +
  geom_point() +
  geom_smooth(method = loess, formula = y ~ x)

lpop_high_smooth <- lowess(ny_high$log_pop, ny_high$log_expenditure)

ggplot(ny_high, aes(x = log_pop, y = log_expenditure)) +
  geom_point() +
  geom_line(aes(x = lpop_high_smooth$x,
                y = lpop_high_smooth$y), color = "#00BFC4")


# ----- histograms and density plots -----------

hist_dens_high <- function(var){
  plot_var <- enquo(var)
  ggplot(ny_high, aes(x = !! plot_var, y = ..density..)) +
    geom_histogram(bins = 50) +
    geom_density(color = "#00BFC4") +
    theme_classic()
}

hist_dens_high(expenditure) 
hist_dens_high(wealth) 
hist_dens_high(income) 
hist_dens_high(pop) 
hist_dens_high(perc_intergov) 
hist_dens_high(grow_rate) 

# ----- log transformed ----
hist_dens_high(log_expenditure) 
hist_dens_high(log_wealth) 
hist_dens_high(log_income) 
hist_dens_high(log_pop) 
hist_dens_high(log_perc_intergov) 
hist_dens_high(log_grow_rate) 


# ----- scatterplots: response vs. predictors -----

scatter_smooth_log_high <- function(var){
  x_var <- enquo(var)
  ggplot(ny_high, aes(x = !! x_var, y = log_expenditure)) +
    geom_point() +
    ylab("Log Expenditure")
}

scatter_smooth_log_high(log_wealth) + xlab("Log Wealth")
scatter_smooth_log_high(log_income) + xlab("Log Income")
scatter_smooth_log_high(log_perc_intergov) + 
  xlab("Log % Intergov Funding")
scatter_smooth_log_high(log_grow_rate) + xlab("Log Growth Rate")




# based on the above plots, it appears that the relationship between
# log expenditure and log population is approximately piecewise linear

# =======================================================
# ----------------- model building ----------------------
# =======================================================

# we will use stepwise regression for model selection
# with AIC as our criteria
# we consider quadratic log terms in the model 
# mostly just to see what the results are
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
# we just can not justify such a model
full_fit <- lm(log_expenditure ~., data = for_mod_fit_high)
best_full_fit <- stepAIC(full_fit)
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


# --------------- diagnostic plots ----------------

# add fitted values, residuals, etc. to orignal data frame used to fit the model
ny_high <- augment(final_fit, data = ny_high) # (broom package)

# add studentized residuals to data frame
ny_high <- ny_high %>%
  mutate(stud_res = rstudent(final_fit))

# studentized residuals vs. fitted
ggplot(ny_high, aes(x = .fitted, y = stud_res)) +
  geom_point() +
  geom_hline(yintercept = 0)

# qq plot studentized residuals vs. t distribution
dist_pars = list(df = final_fit$df.residual)
ny_high %>%
  mutate(stud = rstudent(final_fit)) %>%
  ggplot(aes(sample = stud)) +
  stat_qq(distribution = qt, dparams = dist_pars[["df"]]) +
  stat_qq_line(distribution = qt, dparams = dist_pars[["df"]])

# row with smallest studentized residual
ny_high %>%
  filter(stud_res == min(stud_res))

ny_high %>%
  filter(stud_res == min(stud_res)) %>%
  dplyr::select(stud_res)

# ----- regression summaries -------

# all p-values (except intercept) are significant at the 0.05 level
# F test significant at 0.05 level
summary(final_fit)

# all VIF's are less than 1.2
vif(final_fit)  # (car package)


# the following code is mostly just formatting
ci_bounds = formatC(signif(confint(final_fit), digits = 6),
                   digits = 2, format = "f", flag = "#")

regress_tbl <- tidy(final_fit) %>% # from broom
  dplyr::select(-statistic) %>%
  bind_cols(`95% CI` = paste("(", ci_bounds[,1], ",", ci_bounds[,2], ")")) %>%
  mutate(
    Estimate = formatC(signif(estimate, digits = 6),
                       digits = 2, format = "f", flag = "#"),
    SE = formatC(signif(std.error, digits = 6),
                 digits = 2, format = "f", flag = "#"),
    `p-value` = formatC(signif(p.value, digits = 6),
                        digits = 3, format = "f", flag = "#")
    
  ) %>%
  dplyr::select(Estimate, SE, `p-value`, `95% CI`) %>%
  as.data.frame()

rownames(regress_tbl) = c("Intercept", "Log Wealth", "Log Population",
                         "Log % Intergov Funding", "Log Growth Rate")

regress_tbl
# convert to latex table
xtable(regress_tbl, label = "tbl:regress", align = "|l|rrrr|")

# =======================================================
# ----------------- predictions -------------------------
# =======================================================

# projected values for Warwick and Monroe for 2005 & 2025
projected <- data.frame(
  pop = c(20442L, 31033L, 10496L, 13913L),
  wealth = c(85000L, 89000L, 58000L, 60000L),
  perc_intergov = c(24.7, 26.0, 8.8, 10.1),
  grow_rate = c(35.0, 40.0, 35.0, 35.0)
)

log_projected <- projected %>%
  mutate(
    log_pop = log(pop),
    log_wealth = log(wealth),
    log_perc_intergov = log(perc_intergov),
    log_grow_rate = ifelse(grow_rate > 0,
                           log(grow_rate + 0.15),
                           -log(-grow_rate + 0.15))
  ) %>%
  dplyr::select(-(pop:grow_rate))

sd_fit <- sd(final_fit$resid)

pred <- exp(predict(final_fit,
                    newdata = log_projected,
                    interval = "prediction") + sd_fit^2/2)
pred <- formatC(signif(pred, digits = 6), format = "d", flag = "#")

# format for latex output via xtable
pred_tbl <- projected %>%
  mutate(
    perc_intergov = formatC(signif(perc_intergov, digits = 2),
                            digits = 1, format = "f", flag = "#"),
    grow_rate = formatC(signif(grow_rate, digits = 2),
                        digits = 1, format = "f", flag = "#")
  ) %>%
  rename(
    Population = pop,
    Wealth = wealth,
    `% Intergov Funding` = perc_intergov,
    `Growth Rate` = grow_rate
  ) %>%
  bind_cols(Estimate = pred[,1]) %>%
  bind_cols(`95 % PI` = paste("(", pred[,2], ",", pred[,3], ")"))
rownames(pred_tbl) <- c("Warwick-2005", "Warwick-2025 ",
                        "Monroe-2005", "Monroe-2025")

xtable(pred_tbl, align = "|l|rrrrrr|", label = "tbl:pred",
       caption = "Projections for Warwick and Monroe along with predicted
       expenditures and prediction intervals.")

