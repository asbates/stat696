HLTest = function(obj, g) {

 # first, check to see if we fed in the right kind of object

 stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")

 y = obj$model[[1]]

 trials = rep(1, times = nrow(obj$model))

 if(any(colnames(obj$model) == "(weights)")) 

  trials <- obj$model[[ncol(obj$model)]]

 # the double bracket (above) gets the index of items within an object

 if (is.factor(y)) 

  y = as.numeric(y) == 2  # Converts 1-2 factor levels to logical 0/1 values

 yhat = obj$fitted.values 

 interval = cut(yhat, quantile(yhat, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g

 Y1 <- trials*y

 Y0 <- trials - Y1

 Y1hat <- trials*yhat

 Y0hat <- trials - Y1hat

 obs = xtabs(formula = cbind(Y0, Y1) ~ interval)

 expect = xtabs(formula = cbind(Y0hat, Y1hat) ~ interval)

 if (any(expect < 5))

  warning("Some expected counts are less than 5. Use smaller number of groups")

 pear <- (obs - expect)/sqrt(expect)

 chisq = sum(pear^2)

 P = 1 - pchisq(chisq, g - 2)

 # by returning an object of class "htest", the function will perform like the 

 # built-in hypothesis tests

 return(structure(list(

  method = c(paste("Hosmer and Lemeshow goodness-of-fit test with", g, "bins", sep = " ")),

  data.name = deparse(substitute(obj)),

  statistic = c(X2 = chisq),

  parameter = c(df = g-2),

  p.value = P,

  pear.resid = pear,

  expect = expect,

  observed = obs

 ), class = 'htest'))

}