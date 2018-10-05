
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)

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
  ggplot(ny, aes(x = !!plot_var, y = ..density..)) +
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



pairs(ny) # scatterplot matrix
# see also argument log to pairs. and diag.panel 
# also look at 580 code to see how to add density plot
pairs(ny, log = TRUE)








# =======================================================
