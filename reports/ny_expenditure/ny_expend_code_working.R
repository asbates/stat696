
library(readr)
library(dplyr)
library(corrplot)

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
summary(ny)
ny <- filter(ny, !is.na(expenditure))
dim(ny) # 914 x 7
summary(ny)
corr_mat <- cor(ny)
corrplot(corr_mat, type = "upper", diag = FALSE) # correlation matrix plot
pairs(ny) # scatterplot matrix
# see also argument log to pairs. and diag.panel 
# also look at 580 code to see how to add density plot









# =======================================================
