### Data generation


## Objects generated from CENIC-2 RCT data. Data is not currently available
## to general public
# cenic_X <- readRDS("cenic_X.Rds")
# h_rf <- readRDS("h_rf.Rds")

#' @param g0 Function of X giving the CATE model
#' @param h0 Function of X giving expected outcome under control
#' @param f0 Targeted f-squared value
#' @param sd Error term standard deviation
dg0 <- function(g0 = "g2", h0 = "h2", f0 = 0.15, sd = 4) {
  
  
  cal <- match.call()
  g <- get(g0, mode = "function", envir = parent.frame())
  h <- get(h0, mode = "function", envir = parent.frame())
  
  n <- 1000
  p <- 100
  
  # Covariates
  if (h0 == "h3") {
    # If using randomforest fit to CENIC2 for main effects, take a bootstrap
    # sample of covariates from the CENIC2 data.
    X <- cenic_X[sample(1:nrow(cenic_X), size = n, replace = TRUE), ]
  } else {
    X <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = diag(p))
  }
  
  
  # Potential outcome under control 
  y0 <- h(X)
  
  # (unscaled) Treatment effects ( g(X) )
  z <- g(X)
  # Errors
  epsilon <- rnorm(n = n, mean = 0, sd = sd)
  # Treatment assignment
  Trt <- rep(c(0, 1), times = 1, each = n/2)
  
  # Calculate f2 and scale CATE accordingly
  s <- scale_cate(f0 = f0, y0 = y0, z = z, epsilon = epsilon, Trt = Trt)
  f2 <- calc_f2(s = s, y0, z, epsilon, Trt)
  
  # Potential outcome under treatment
  y1 <- y0 + s * z
  
  temp0 <- y0
  temp1 <- y1
  
  Y <- rbind(cbind(Trt=0, y0 + epsilon)[1:(n/2),] , cbind(Trt=1, y1 + epsilon)[((n/2)+1):n,])
  reg <- as.data.frame(cbind(Y, X))
  names(reg) <- c("Trt", "Y", paste("V", 1:ncol(X), sep=""))
  
  #Benefit (ITE > 0)
  benefit <- (temp1-temp0)>0
  
  return(list(reg = reg, bene=benefit, temp0=temp0, temp1=temp1, f2 = f2, s = s, call = cal))
}

calc_f2 <- function(s, y0, z, epsilon, Trt) {
  # s scaling factor on CATE to be set to get a desired f-squared
  
  y1 <- y0 + s * z
  # Observed outcome
  y <- y0 * (1 - Trt) + y1 * Trt 
  # Mean treatment effect
  d <- mean(y1) - mean(y0) 
  
  ## R-squared under null model (only main effect for treatment)
  r20 <- 1 - var(y - y0 + Trt * d + epsilon) / var(y + epsilon)
  
  ## R-squared under full treatment effect model
  r21 <- 1 - var(epsilon) / var(y + epsilon)
  
  f2 <- (r21 - r20) / (1 - r21) 
  f2
}

scale_cate <- function(f0, y0, z, epsilon, Trt) {
  # f0 = targeted f-squared
  # According to Cohen (1988) 0.02 :: small, 0.15 :: medium, 0.35
  s_grid <- seq(0.01, 50, by = 0.01)
  f2 <- sapply(s_grid, calc_f2, y0 = y0, z = z, epsilon = epsilon, Trt = Trt)
  distance <- f2 - f0

  s_grid[which.min(abs(distance))]
}



### CATE ====
# True tree
g1 <- function(X) {
  gx <- ifelse(
    X[, 1] > 0, 
    ifelse(
      X[, 2] > 0, -4, -2
    ),
    ifelse(
      X[, 3] > 0, 2, 4
    )
  )
  as.numeric(scale(gx))
  
}

# Polynomial function of one covariate 
gp <- function(x) {
  x/2 + x^2 + x^3 
}
# Additive polynomial
g2 <- function(X) {
  gx <- gp(X[, 1]) + 1/2 * gp(X[, 2]) + 1/4 * gp(X[, 3])
  as.numeric(scale(gx))
}

# sigmoid
g3 <- function(X) {
  x0 <- g2(X)
  gx <- exp(x0) / (1 + exp(x0))
  gx - mean(gx)
}

### MAIN EFFECTS ====
# Linear
h1 <- function(X) {
  # 15 main effects, 12 continuous, 3 binary
  y0 <- X %*% c(rep(1,15), rep(0,ncol(X)-15) ) 
  drop(y0)
}

# Nonlinear
h2 <- function(X) {
    # 15 main effects
    y0 <- X[, 1] + X[, 2] + 
      0.5 * X[, 1] * X[, 2] +
      -1 * (X[, 13]>0) * X[, 1] + #Continuous - binary interaction
      1.25 * (
        (X[, 3])^2 + (X[, 4])^2 +
          (X[, 5])^2 + (X[, 6])^2 +
          (X[, 7])^2 + 
          (X[, 8]) + (X[, 9]) + 
          (X[, 10]) + (X[, 11]) +
          (X[, 12])
      ) +
      X[, 13] + X[, 14] + X[, 15]
    
  drop(y0)
}

# Data-based (CENIC2)
h3 <- function(X) {
  y0 <- predict(h_rf, newdata = X)$predicted
  as.vector(unlist(y0))
}

####################################### VT Step 1 #######################################
#Get individual treatment effects 

#LASSO models for trt/ctrl arms, errors "nope" when lasso model is too sparse
i.las <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt))), 
                  y = dat0$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt))), 
                  y = dat1$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  p0 <- coef(m0, s="lambda.1se")
  p1 <- coef(m1, s="lambda.1se")
  # ifelse(length(p0@i)<2 | length(p1@i)<2, stop("nope"), x <- 1)
  pred0 <- predict(m0, newx = data.matrix(subset(dat, select = -c(Y, Trt))), s = "lambda.1se")
  pred1 <- predict(m1, newx = data.matrix(subset(dat, select = -c(Y, Trt))), s = "lambda.1se")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#RF with RandomforestSRC
i.rf <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)

  m0 <- tune(Y~. , data = subset(dat0, select = -Trt), doBest = TRUE) #automatically tunes forest
  m1 <- tune(Y~. , data = subset(dat1, select = -Trt), doBest = TRUE)
  
  m0_rf <- rfsrc(Y ~ ., data = subset(dat0, select = -Trt),
                 nodesize = m0$optimal[1], mtry = m0$optimal[2])
  
  m1_rf <- rfsrc(Y ~ ., data = subset(dat1, select = -Trt),
                 nodesize = m1$optimal[1], mtry = m1$optimal[2])
  
  pred0 <- predict(m0_rf, subset(dat, select = -c(Y, Trt)))$predicted
  pred1 <- predict(m1_rf, subset(dat, select = -c(Y, Trt)))$predicted
  
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

i.rf.fast <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  
  m0 <- rfsrc(Y ~ ., data = subset(dat0, select = -Trt))
  
  m1 <- rfsrc(Y ~ ., data = subset(dat1, select = -Trt))
  
  pred0 <- predict(m0, subset(dat, select = -c(Y, Trt)))$predicted
  pred1 <- predict(m1, subset(dat, select = -c(Y, Trt)))$predicted
  
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with MARS
i.mars <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  m0 <- earth(Y~., data = subset(dat0, select = -Trt))
  m1 <- earth(Y~., data = subset(dat1, select = -Trt))
  pred0 <- predict(m0, newdata = subset(dat, select = -c(Y, Trt)), type = "response")
  pred1 <- predict(m1, newdata = subset(dat, select = -c(Y, Trt)), type = "response")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}


#Piecewise model with Superlearner
i.super <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  SL.earth.def = function(...) { #changing to earth package defaults
    SL.earth(..., degree = 1, penalty = 2)
  }
  slmethods <- c("SL.glmnet", "SL.randomForest","SL.earth.def") 
  m0 <- SuperLearner(Y = dat0$Y, 
                     X = as.data.frame(subset(dat0, select = -c(Y, Trt))), 
                     family = gaussian(), 
                     SL.library = slmethods)
  m1 <- SuperLearner(Y = dat1$Y, 
                     X = as.data.frame(subset(dat1, select = -c(Y, Trt))), 
                     family = gaussian(), 
                     SL.library = slmethods)
  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

# Modified superlearner with faster ensemble methods and fewer CV folds
i.super.fast <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  
  slmethods <- c("SL.glmnet.fast", "SL.randomForest.fast","SL.earth.def") 
  m0 <- SuperLearner.fast(
    Y = dat0$Y, 
    X = as.data.frame(subset(dat0, select = -c(Y, Trt))), 
    family = gaussian(), 
    SL.library = slmethods)
  m1 <- SuperLearner.fast(
    Y = dat1$Y, 
    X = as.data.frame(subset(dat1, select = -c(Y, Trt))), 
    family = gaussian(), 
    SL.library = slmethods)
  
  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

SL.glmnet.fast <- function(...) {
  SL.glmnet(...)
}

SL.randomForest.fast <- function(...) { # Default ntree is 1000, cut this down
  SL.randomForest(..., ntree = 250)
}

SL.earth.def = function(...) { #changing to earth package defaults
  SL.earth(..., degree = 1, penalty = 2)
}

SuperLearner.fast <- function(...) { # Change 10 folds to 3 folds
  SuperLearner(..., cvControl = SuperLearner::SuperLearner.CV.control(V = 3L))
}

SL.gam.fast <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, 
                         ...)  {
  if (!require("gam")) {
    stop("SL.gam requires the gam package, but it isn't available")
  }
  if ("mgcv" %in% loadedNamespaces()) 
    warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+"), 
                                  "+", paste(colnames(X[, !cts.x, drop = FALSE]), 
                                             collapse = "+")))
  }
  else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+")))
  }
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                              collapse = "+"), sep = ""))
  }
  fit.gam <- gam::gam(gam.model, data = X, family = family, 
                      control = gam::gam.control(maxit = 25, bf.maxit = 25), 
                      weights = obsWeights)
  # if (packageVersion("gam") >= 1.15) {
  pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
  # }
  # else {
  #   stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  # }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}

# Modified superlearner with faster ensemble methods and fewer CV folds
# AND a GAM in the ensemble
i.super.fast2 <- function(train, test, vt1_est = list()){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  
  slmethods <- c("SL.glmnet.fast", "SL.randomForest.fast","SL.earth.def", "SL.gam.fast") 
  m0 <- SuperLearner.fast(
    Y = dat0$Y, 
    X = as.data.frame(subset(dat0, select = -c(Y, Trt))), 
    family = gaussian(), 
    SL.library = slmethods)
  m1 <- SuperLearner.fast(
    Y = dat1$Y, 
    X = as.data.frame(subset(dat1, select = -c(Y, Trt))), 
    family = gaussian(), 
    SL.library = slmethods)
  
  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

####################################### VT Step 2 #######################################
#make trees using test set, run the estimated test set treatment effects through to get classification 

#not doing a step 2, just using est from step 1 to get trt assignment
c.none <- function(dat, est){
  preds <- est$Z
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  return(list(nwg=sum(wg), mse=mse))
}

#using a tree to get number of misclassified, and grabbing the predictors
c.tree <- function(dat, est){
  test <- dat$reg
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)),
                       method = "rpart2",
                       tuneLength = 3, 
                       trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  #getting predictors from tree: 
  vars <- unique(tfit$finalModel$frame$var)
  tvars <- vars[!vars=="<leaf>"]
  return(list(nwg=sum(wg), mse=mse, vars=tvars, nvars = length(tvars)))
}


# CHANGED 2021-07-26 !!! Now fit Lasso as-is
#linear model for step 2
c.lin <- function(dat, est){
  test <- dat$reg
  test$Z <- est$Z
  m <- cv.glmnet(x = data.matrix(subset(test, select = -c(Y, Trt, Z))), 
                 y = data.matrix(est$Z), 
                 family = "gaussian", alpha = 1, standardize = TRUE)
  
  lasso.coefs <- coef(m, s = "lambda.min")
  lvars <- rownames(lasso.coefs)[which(lasso.coefs != 0)]
  lvars <- setdiff(lvars, "(Intercept)")
  
  preds <- predict(m, newx = data.matrix(subset(test, select = -c(Y, Trt, Z))), s = "lambda.min")
  wg <- (preds>0)!=dat$bene
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  return(list(nwg=sum(wg), mse=mse, vars=lvars))
}

#using a conditional inference tree
c.ctree <- function(dat, est){
  test <- dat$reg
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)), 
                       method = 'ctree2', 
                       trControl = trainControl(method = "repeatedcv", number=10, repeats = 3),
                       tuneGrid = expand.grid(maxdepth = c(1:3), mincriterion=0.95),
                       metric='RMSE')
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  #getting predictors from tree: 
  raw <- capture.output(tfit$finalModel@tree)
  vars <- unique(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
  vars <- vars[!is.na(vars)]
  return(list(nwg=sum(wg), mse=mse, vars=vars))
}



