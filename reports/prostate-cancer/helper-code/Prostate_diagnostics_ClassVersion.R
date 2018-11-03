# Read-in data
Prostate = read.table("prostate.txt", header=T)
dim(Prostate)
#[1] 380 8
sum(is.na(Prostate)) # 3 in race
Prostate<-na.omit(Prostate); dim(Prostate) # 377 8
Prostate$dcaps = as.factor(Prostate$dcaps)
Prostate$dpros = as.factor(Prostate$dpros)
Prostate$race = as.factor(Prostate$race)
attach(Prostate)

head(Prostate)
summary(Prostate)


# Residual plots
# Load-in required functions
one.fourth.root=function(x){
  x^0.25
}
source("examine.logistic.reg.R")

# Consider model of PSA, Gleason score, and Detection of capsular involvement
dat.glm <- glm(capsule ~ psa+gleason+dcaps, family = binomial, data = Prostate)
dat.mf <- model.frame(dat.glm)
## Covariate pattern: too many EVPs!
w <- aggregate(formula = capsule ~ psa+gleason+dcaps, data = Prostate, FUN = sum)
n <- aggregate(formula = capsule ~ psa+gleason+dcaps, data = Prostate, FUN = length)
w.n <- data.frame(w, trials = n$capsule, prop = round(w$capsule/n$capsule,2))
dim(w.n)
#[1] 301 6

# Create EVPs by binning continuous covariates
g = 5 # number of categories
psa_interval = cut(psa, quantile(psa, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
levels(psa_interval)

# Diagnostic plots
w <- aggregate(formula = capsule ~ psa_interval+gleason+dcaps, data = Prostate, FUN = sum)
n <- aggregate(formula = capsule ~ psa_interval+gleason+dcaps, data = Prostate, FUN = length)
w.n <- data.frame(w, trials = n$capsule, prop = round(w$capsule/n$capsule,2))
mod.prelim1 <- glm(formula = capsule/trials ~ psa_interval+gleason+dcaps,
                   family = binomial(link = logit), data = w.n, weights = trials)
save1=examine.logistic.reg(mod.prelim1, identify.points=T, scale.n=one.fourth.root, scale.cookd=sqrt)

# Evaluation of EVPs for potential outlying sets of points
w.n.diag1=data.frame(w.n, pi.hat=round(save1$pi.hat, 2), std.res=round(save1$stand.resid, 2), 
                     cookd=round(save1$cookd, 2), h=round(save1$h, 2))
p=length(mod.prelim1$coef) # number of parameters in model (# coefficients)
ck.out=abs(w.n.diag1$std.res)>2 | w.n.diag1$cookd>4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
extract.EVPs=w.n.diag1[ck.out, ]
extract.EVPs

# Note: EVPs purely for diagnostics, akin to histogram binning to assess
# distribution shape.  The analysis does not use this EVP binning.

