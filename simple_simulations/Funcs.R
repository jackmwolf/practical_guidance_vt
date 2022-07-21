#Functions for Data Simulations

# source('C:/Users/Chuyu/Desktop/Research/Genentech/Packs.R')


####################################### Linear Data Generation #######################################


regDG <- function(N){
  n <- 2*N #N is 1000, 200 or 80
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(reg), (nrow(reg)/2))
  train <- reg[-test.i,]
  test <- reg[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}


corDG <- function(N){
  n <- 2*N
  p <- 96; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #correlated variables
  r <- 0.70; a <- 4
  mat <- matrix(r,a,a); diag(mat) <- 1
  corV <- mvrnorm(n=n, mu=m[1:4], Sigma=mat, empirical=TRUE)
  #continuous variables
  V <- cbind(corV,matrix(rnorm(n*p, m[5:100], 1),ncol=p, byrow = TRUE))
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  cordat <- as.data.frame(cbind(Y,V,C))
  names(cordat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(cordat), (nrow(cordat)/2))
  train <- cordat[-test.i,]
  test <- cordat[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

sbDG <- function(N){
  n <- 3*N
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  sbdat <- as.data.frame(cbind(Y,V,C))
  names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating nonrandom training and random test samples
  test.i <- sample(nrow(sbdat), N)
  test <- sbdat[test.i,]
  Rsum <- rowSums(sbdat[-test.i ,c("V15", "V16", "V17","V18")]) ######both predictive and prog covariates
  tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[N],]
  bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[N],]
  train <- rbind(sample_n(tbl = tophalf, size = N*0.75), sample_n(tbl = bothalf, size = N/4))
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}


####################################### Nonlinear Data Generation #######################################

treeregDG <- function(N){
  n <- 2*N #N is 1000, 200 or 80
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(reg), (nrow(reg)/2))
  train <- reg[-test.i,]
  test <- reg[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

treecorDG <- function(N){
  n <- 2*N
  p <- 96; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #correlated variables
  r <- 0.70; a <- 4
  mat <- matrix(r,a,a); diag(mat) <- 1
  corV <- mvrnorm(n=n, mu=m[1:4], Sigma=mat, empirical=TRUE)
  #continuous variables
  V <- cbind(corV,matrix(rnorm(n*p, m[5:100], 1),ncol=p, byrow = TRUE))
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  cordat <- as.data.frame(cbind(Y,V,C))
  names(cordat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(cordat), (nrow(cordat)/2))
  train <- cordat[-test.i,]
  test <- cordat[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

treesbDG <- function(N){
  n <- 3*N
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  sbdat <- as.data.frame(cbind(Y, V,C))
  names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating nonrandom training and random test samples
  test.i <- sample(nrow(sbdat), N)
  test <- sbdat[test.i,]
  Rsum <- rowSums(sbdat[-test.i ,c("V1", "V5")]) ###
  tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[N],]
  bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[N],]
  train <- rbind(sample_n(tbl = tophalf, size = N*0.75), sample_n(tbl = bothalf, size = N/4))
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

####################################### VT Step 1 #######################################
#Get individual treatment effects 

#LASSO models for trt/ctrl arms, errors "nope" when lasso model is too sparse
i.las <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt))), 
                  y = dat0$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt))), 
                  y = dat1$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  p0 <- coef(m0, s="lambda.1se")
  p1 <- coef(m1, s="lambda.1se")
  ifelse(length(p0@i)<2 | length(p1@i)<2, stop("nope"), x <- 1)
  pred0 <- predict(m0, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
  pred1 <- predict(m1, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#RF with RandomforestSRC
i.rf <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- tune(Y~. , data = subset(dat0, select = -Trt), doBest = TRUE) #automatically tunes forest
  m1 <- tune(Y~. , data = subset(dat1, select = -Trt), doBest = TRUE)
  pred0 <- predict(m0$rf, subset(test, select = -c(Y, Trt)))$predicted
  pred1 <- predict(m1$rf, subset(test, select = -c(Y, Trt)))$predicted
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with MARS
i.mars <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- earth(Y~., data = subset(dat0, select = -Trt))
  m1 <- earth(Y~., data = subset(dat1, select = -Trt))
  pred0 <- predict(m0, newdata = subset(test, select = -c(Y, Trt)), type = "response")
  pred1 <- predict(m1, newdata = subset(test, select = -c(Y, Trt)), type = "response")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#SVM using kernlab inside of caret
i.svm <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- caret::train(Y~., data = subset(dat0, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3, 
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  m1 <- caret::train(Y~., data = subset(dat1, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3, 
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3)) 
  pred0 <- predict(m0$finalModel, newdata=subset(test, select = -c(Y, Trt)))
  pred1 <- predict(m1$finalModel, newdata=subset(test, select = -c(Y, Trt)))  
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with Superlearner
i.super <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
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
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  return(list(nwg=sum(wg), mse=mse))
}

#using a tree to get number of misclassified, and grabbing the predictors
c.tree <- function(dat, est){
  test <- dat$test
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)),
                       method = "rpart2",
                       tuneLength = 3, 
                       trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  #getting predictors from tree: 
  vars <- levels(tfit$finalModel$frame$var)
  tvars <- vars[!vars=="<leaf>"]
  return(list(nwg=sum(wg), mse=mse, vars=tvars, nvars = length(tvars)))
}


#linear model for step 2, and using top predictors (same # as tree)
c.lin <- function(dat, est, top){
  test <- dat$test
  test$Z <- est$Z
  m <- cv.glmnet(x = data.matrix(subset(dat$test, select = -c(Y, Trt))), 
                    y = data.matrix(est$Z), 
                    family = "gaussian", alpha = 1, standardize = TRUE)
  #getting top predictors: 
  p <- coef(m, s="lambda.min") %>% as.matrix()
  pv <- p[-1]
  vars <- rownames(p)[abs(p)>=sort(abs(pv), decreasing = T)[top] & p!=0]
  lvars <- vars[!vars=="(Intercept)"]
  #fitting the linear model with top predictors
  fit <- lm(paste("Z~", paste(lvars, collapse = "+")), data = test)
  preds <- predict(fit, newdata = subset(test, select = -c(Z, Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  return(list(nwg=sum(wg), mse=mse, vars=lvars))
}

#using a conditional inference tree
c.ctree <- function(dat, est){
  test <- dat$test
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)), 
                      method = 'ctree2', 
                      trControl = trainControl(method = "repeatedcv", number=10, repeats = 3),
                      tuneGrid = expand.grid(maxdepth = c(1:3), mincriterion=0.95),
                      metric='RMSE')
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  #getting predictors from tree: 
  raw <- capture.output(tfit$finalModel@tree)
  vars <- unique(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
  vars <- vars[!is.na(vars)]
  return(list(nwg=sum(wg), mse=mse, vars=vars))
}




# ####################################### Imbalanced Arms & MARS DG #######################################
# imbDG <- function(N){
#   n <- 2*N
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
#   #outcome variable
#   temp0 <- cbind(V,C) %*% c(rep(0,2), rep(1,15), rep(0,83), rep(1,2), rep(0,8) )
#   temp1 <- cbind(V,C) %*% c(rep(0,2), rep(1,20), rep(0,78), rep(1,5), rep(0,5) )
#   offset <- mean(temp1) - mean(temp0) ##
#   temp1 <- temp1-offset ##
#   Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(n, mean = temp1, sd=3)
#   #prob(T=0)~Y
#   Rsum <- rowSums(cbind(V[,c(10, 16)],C[,c(1,2)]))
#   Trt <- rep(3,n); Trt[sort(Rsum)[(n/4)]>=Rsum] <- 0; Trt[Rsum>sort(Rsum)[(3*n/4)]] <- 1
#   Trt[Trt==3] <- c(rep(0,n/4), rep(1,n/4))
#   Y <- ifelse(Trt==0, Y0, Y1)
#   #making the data frame (110 covars)
#   imbdat <- as.data.frame(cbind(Trt,Y,V,C))
#   names(imbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   #Creating training and test samples
#   test.i <- sample(nrow(imbdat), (nrow(imbdat)/2))
#   train <- imbdat[-test.i,]
#   test <- imbdat[test.i,]
#   #Benefit in test group (benefit=T if trt raised outcome)
#   benefit <- (temp1-temp0)>0 ##
#   return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
# }
# 
# 
# treeimbDG <- function(N){
#   n <- 2*N #N is 1000, 200 or 80
#   p <- 100; c <- 10
#   m <- rnorm(100,0,3) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
#   #outcome variable
#   temp0 <- temp1 <- c()
#   for(i in 1:n){
#     if(V[i,1]>m[1]){
#       if(V[i,2]>m[2]){temp0[i] <- 20; temp1[i] <- 22
#       }else{temp0[i] <- 23; temp1[i] <- 20}
#     }else{
#       if(V[i,5]>m[5]){temp0[i] <- 25; temp1[i] <- 25
#       }else{temp0[i] <- 22; temp1[i] <- 23}
#     }
#   }
#   Y0 <- rnorm(n, mean = temp0, sd=1)
#   Y1 <- rnorm(n, mean = temp1, sd=1)
#   #prob(T=0)~Y
#   Rsum <- rowSums(cbind(V[,c(1, 2, 5)])) ###
#   Trt <- rep(3,n); Trt[sort(Rsum)[(n/4)]>=Rsum] <- 0; Trt[Rsum>sort(Rsum)[(3*n/4)]] <- 1
#   Trt[Trt==3] <- c(rep(0,n/4), rep(1,n/4))
#   Y <- ifelse(Trt==0, Y0, Y1)
#   #making the data frame (110 covars)
#   imbdat <- as.data.frame(cbind(Trt,Y,V,C))
#   names(imbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   #Creating training and test samples
#   test.i <- sample(nrow(imbdat), (nrow(imbdat)/2))
#   train <- imbdat[-test.i,]
#   test <- imbdat[test.i,]
#   #Benefit in test group (benefit=T if trt raised outcome)
#   benefit <- (temp1-temp0)>0 ##
#   return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
# }
# 
# 
# #method 1: joining 2 linear pieces at knot
# pieregDG <- function(N){
#   n <- 2*N #N is 1000, 200 or 80
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
#   #outcome variable for control
#   temp0 <- cbind(V,C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) )
#   #outcome variable for trt
#   temp2 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(0.5,5), rep(0,5) )
#   temp2 <- temp2-mean(temp2)+mean(temp0)
#   temp3 <- cbind(V,C) %*% c(rep(5,20), rep(0,80), rep(5,5), rep(0,5) )
#   temp1 <- ifelse(temp2>mean(temp2), temp3-mean(temp3)+mean(temp2), temp2)
#   #plot(x=rowSums(V[,1:20]), y=temp1); points(x=rowSums(V[,1:20]), y=temp0, col="red")
#   #putting together the Y
#   Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(n, mean = temp1, sd=3)
#   #plot(x=rowSums(V[,1:20]), y=Y1); points(x=rowSums(V[,1:20]), y=Y0, col="red")
#   Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
#   #making the data frame (110 covars)
#   reg <- as.data.frame(cbind(Y, V,C))
#   names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   #mod <- lm(Y~., data=reg)
#   #summary(mod)
#   #Creating training and test samples
#   test.i <- sample(nrow(reg), (nrow(reg)/2))
#   train <- reg[-test.i,]
#   test <- reg[test.i,]
#   #Benefit in test group (benefit=T if trt raised outcome)
#   benefit <- (temp1-temp0)>0 ##
#   return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
# }
# 
# #method 2: using a MARS generation
# pieregDG2 <- function(N){
#   n <- 2*N #N is 1000, 200 or 80
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
#   #outcome variable for control
#   temp0 <- cbind(V, C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) )
#   #outcome variable for trt
#   xs <- c()
#   for(i in 1:20){xs <- cbind(xs, pmax(0,V[,i]-m[i]), pmax(0,m[i]-V[,i]))}
#   temp1 <- cbind(1, xs, C) %*% c(1, rep(c(5,1),20), rep(1,5), rep(0,5))
#   offset <- mean(temp1) - mean(temp0) ##
#   temp1 <- temp1-offset ##
#   #plot(x=rowSums(V[,1:20]), y=temp1); points(x=rowSums(V[,1:20]), y=temp0, col="red")
#   #putting together the Y
#   Y0 <- rnorm(n, mean = temp0, sd=2) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(n, mean = temp1, sd=2)
#   #plot(x=rowSums(V[,1:20]), y=Y1); points(x=rowSums(V[,1:20]), y=Y0, col="red")
#   Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
#   #making the data frame (110 covars)
#   reg <- as.data.frame(cbind(Y, V,C))
#   names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   #mod <- lm(Y~., data=reg[reg$Trt==1,]); summary(mod)
#   #Creating training and test samples
#   test.i <- sample(nrow(reg), (nrow(reg)/2))
#   train <- reg[-test.i,]
#   test <- reg[test.i,]
#   #Benefit in test group (benefit=T if trt raised outcome)
#   benefit <- (temp1-temp0)>0 ##
#   return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
# }
# 
# pieregDG3 <- function(N){
#   n <- 2*N #N is 1000, 200 or 80
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
#   #outcome variable for control
#   temp4 <- cbind(V,C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) )
#   temp5 <- cbind(V,C) %*% c(rep(-2,15), rep(0,85), rep(-2,2), rep(0,8) )
#   temp0 <- ifelse(temp4>mean(temp4), temp5-mean(temp5)+mean(temp4), temp4)
#   #outcome variable for trt
#   temp2 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(0.5,5), rep(0,5) )
#   temp2 <- temp2-mean(temp2)+mean(temp0)
#   temp3 <- cbind(V,C) %*% c(rep(5,20), rep(0,80), rep(5,5), rep(0,5) )
#   temp1 <- ifelse(temp2>mean(temp2), temp3-mean(temp3)+mean(temp2), temp2)
#   #plot(x=rowSums(V[,1:20]), y=temp1); points(x=rowSums(V[,1:20]), y=temp0, col="red")
#   #putting together the Y
#   Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(n, mean = temp1, sd=3)
#   #plot(x=rowSums(V[,1:20]), y=Y1); points(x=rowSums(V[,1:20]), y=Y0, col="red")
#   Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
#   #making the data frame (110 covars)
#   reg <- as.data.frame(cbind(Y, V,C))
#   names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   #mod <- lm(Y~., data=reg)
#   #summary(mod)
#   #Creating training and test samples
#   test.i <- sample(nrow(reg), (nrow(reg)/2))
#   train <- reg[-test.i,]
#   test <- reg[test.i,]
#   #Benefit in test group (benefit=T if trt raised outcome)
#   benefit <- (temp1-temp0)>0 ##
#   return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
# }


# ####################################### Older stuff from Genentech #######################################
# 
# #Simulation Wrappers for ATE & Predictors
# Cond0 <- function(N){
#   dat <- regDG(N)
#   train <- dat$train; test <- dat$test
#   
#   #Naive difference btw trt:
#   NZ <- mean(test$Y[test$Trt==1])-mean(test$Y[test$Trt==0])
#   #ATE with LASSO:
#   reg.est <- iATE(train, test)
#   VTZ <- mean(reg.est$Z)
#   
#   #ATE with RandomforestSRC
#   out0 <- tune(Y~. , data = train[train$Trt==0,], doBest = TRUE) #automatically tunes forest
#   out1 <- tune(Y~. , data = train[train$Trt==1,], doBest = TRUE)
#   pred0 <- predict(out0$rf, subset(test, select = -Y))$predicted
#   pred1 <- predict(out1$rf, subset(test, select = -Y))$predicted
#   RFZ <- mean(pred1-pred0)
#   
#   #predictors for LASSO
#   #Run Zs through regression tree and 3 layers of predictors:
#   l.tvars <- rtree(reg.est$Z, test, 3)
#   #Run Zs through CV lasso and getting top 5 predictors:
#   l.lvars <- plasso(reg.est$Z, test, 5)
#   #Run Zs through random forest and getting top 5 predictors
#   l.fvars <- rforest(reg.est$Z, test, 5)
#   
#   #predictors for RF
#   #Run Zs through regression tree and 3 layers of predictors:
#   rf.tvars <- rtree(as.vector(pred1-pred0), test, 3)
#   #Run Zs through CV lasso and getting top 5 predictors:
#   rf.lvars <- plasso(as.vector(pred1-pred0), test, 5)
#   #Run Zs through random forest and getting top 5 predictors
#   rf.fvars <- rforest(as.vector(pred1-pred0), test, 5)
#   
#   return(list(ATE=c(TZ=dat$TZ, NZ=NZ, LZ=mean(reg.est$Z), RFZ=RFZ), 
#               l.tvars=l.tvars, l.lvars=l.lvars, l.fvars=l.fvars, 
#               rf.tvars=rf.tvars, rf.lvars=rf.lvars, rf.fvars=rf.fvars))
# }
# 
# 
# 
# #Benefit Analysis
# #gives how many wrong benefit classifications VT and naive methods produce
# bene0 <- function(N){
#   n <- 2*N
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3))
#   #continuous variables
#   V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
#   V[V<0] <- 0
#   #categorical variables
#   C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE) 
#   #outcome variable
#   temp0 <- cbind(V,C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) ) 
#   temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) ) 
#   offset <- mean(temp1) - mean(temp0) ##
#   temp1 <- temp1-offset ##
#   Y0 <- rnorm(n, mean = temp0, sd=3) 
#   Y1 <- rnorm(n, mean = temp1, sd=3) 
#   Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
#   oreg <- as.data.frame(cbind(Y, V, C)) 
#   names(oreg) <- c("Trt", "Y", paste("V", 1:110, sep="")) 
#   benefit <- (temp1-temp0)>0 ## benefit if trt raised outcome
#   
#   #Creating training and test samples
#   test.i <- sample(nrow(oreg), (nrow(oreg)/2))
#   train <- oreg[-test.i,]
#   test <- oreg[test.i,]
#   
#   #VT step 1 with LASSO:
#   oreg.est <- iATE(train, test)
#   #VT step 1 with RF:
#   out0 <- tune(Y~. , data = train[train$Trt==0,], doBest = TRUE) #automatically tunes forest
#   out1 <- tune(Y~. , data = train[train$Trt==1,], doBest = TRUE)
#   pred0 <- predict(out0$rf, subset(test, select = -Y))$predicted
#   pred1 <- predict(out1$rf, subset(test, select = -Y))$predicted
#   RFZ <- pred1-pred0
#   
#   #ones in wrong group
#   wg.lasso <- (oreg.est$Z>0)!=benefit[test.i] 
#   wg.rf <- (RFZ>0)!=benefit[test.i] 
#   
#   #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
#   ben <- data.frame("l.p0"=as.vector(oreg.est$pred0), "l.p1"=as.vector(oreg.est$pred1), 
#                     "rf.p0"=as.vector(pred0), "rf.p1"=as.vector(pred1), 
#                     "trueb"=benefit[test.i], "l.estb"=as.vector(oreg.est$Z>0), "rf.estb"=(RFZ>0))
#   lse <- (ben[wg.lasso,"l.p0"]-ben[wg.lasso,"l.p1"])^2
#   rfse <- (ben[wg.rf,"rf.p0"]-ben[wg.rf,"rf.p1"])^2
#   
#   return(list(mc.l = sum(wg.lasso), mc.rf = sum(wg.rf), lmse=mean(lse),rfmse=mean(rfse)))
# }
# 
# 
# 
# 
# #IPW stuff from summer
# #ipwcond1 is for the predictive covariates only simulation
# #ipwcond2 is for prognostic covariates only
# ipwcond1 <- function(n){
#   n <- 1000
#   N <- 3*n
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,3)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(N*p, m, 1),ncol=p, byrow = TRUE)
#   V[V<0] <- 0
#   #categorical variables
#   C <- matrix(rbinom(c*N, size=1, prob=0.7), ncol = c, byrow = TRUE) 
#   #outcome variable
#   temp0 <- cbind(V,C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8))
#   temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5))
#   Y0 <- rnorm(N, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(N, mean = temp1, sd=3) 
#   Y <- rbind(cbind(Trt=0,Y0)[1:(N/2),] , cbind(Trt=1,Y1)[((N/2)+1):N,])
#   #making the data frame (110 covars)
#   sbdat <- as.data.frame(cbind(Y, V,C))
#   names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   
#   #Creating nonrandom training samples and random test samples
#   test.i <- sample(nrow(sbdat), n)
#   test <- sbdat[test.i,]
#   Rsum <- rowSums(sbdat[-test.i ,c("V16", "V17")]) ###########only predictive covariates
#   tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[n],]
#   bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[n],]
#   train <- rbind(sample_n(tbl = tophalf, size = n*0.75), sample_n(tbl = bothalf, size = n/4))
#   
#   #True ATE:
#   true0 <- c(m, rep(0.7,10)) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) )
#   true1 <- c(m, rep(0.7,10)) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
#   TZ <- true1-true0
#   
#   #Naive difference btw trt:
#   NZ <- mean(test$Y[test$Trt==1])-mean(test$Y[test$Trt==0])
#   
#   #VT ATE with different train and test:
#   sbdat.est <- iATE(train, test)
#   VTZ <- mean(sbdat.est$Z)
#   
#   #IPW: combine train and test sets together again, make weights
#   all <- rbind(cbind(pop=1, test), cbind(pop=0,train))
#   
#   # #####change to logistic ridge
#   # ipwd <- cv.glmnet(x = data.matrix(subset(all, select = -c(Y, pop))), 
#   #                   y = all$pop,
#   #                   family = "binomial", alpha = 0)
#   # ipwd.vals <- predict(ipwd, s = "lambda.min", newx = data.matrix(subset(all, select = -c(Y, pop))), type="response")
#   # toselect.x <- abs(coef(ipwd, ipwd$lambda.min)@x) > 0.5
#   # relevant.x <- coef(ipwd, ipwd$lambda.min)@Dimnames[[1]][toselect.x == FALSE] 
#   # sig.formula <- as.formula(paste("pop ~",paste(relevant.x[2:length(relevant.x)], collapse= "+")))
#   # ipwn <- cv.glmnet(x = data.matrix(subset(all, select = relevant.x[2:length(relevant.x)])), 
#   #                   y = all$pop,
#   #                   family = "binomial", alpha = 0)
#   # ipwn.vals <- predict(ipwn, s = "lambda.min", newx = data.matrix(subset(all, select = relevant.x[2:length(relevant.x)])), type = "response")
#   # all$ipw <- ifelse(all$pop==1, ipwn.vals/ipwd.vals, (1-ipwn.vals)/(1-ipwd.vals))
#   
#   #####original logistic
#   ipwd <- glm(pop~., data = subset(all, select = -Y), family = "binomial" )
#   toselect.x <- summary(ipwd)$coeff[-1,4] < 0.001
#   relevant.x <- names(toselect.x)[toselect.x == FALSE]
#   sig.formula <- as.formula(paste("pop ~",paste(relevant.x, collapse= "+")))
#   ipwn <- glm(sig.formula, data = subset(all, select = -Y), family = "binomial" )
#   all$ipw <- ifelse(all$pop==1, ipwn$fitted.values/ipwd$fitted.values, (1-ipwn$fitted.values)/(1-ipwd$fitted.values))
#   all$ipw <- ifelse(all$ipw >= 2, 2, all$ipw)
#   
#   #IPW with stabilized weights
#   dat0 <- subset(all, pop==0 & Trt==0)
#   dat1 <- subset(all, pop==0 & Trt==1)
#   m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt, pop, ipw))), 
#                   y = dat0$Y, 
#                   weights = dat0$ipw,
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt, pop, ipw))), 
#                   y = dat1$Y, 
#                   weights = dat1$ipw, 
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   p0 <- coef(m0, s="lambda.1se")
#   p1 <- coef(m1, s="lambda.1se")
#   ifelse(length(p0@i)<3 | length(p1@i)<3, stop("nope"), x <- 1)
#   pred0 <- pred1 <- rep(0, nrow(test))
#   pred0 <- predict(m0, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   pred1 <- predict(m1, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   Z <- pred1-pred0
#   
#   return(c(TZ=TZ, NZ=NZ, VTZ=VTZ, IPWZ=mean(Z)))
# }
# 
# 
# ipwcond2 <- function(n){
#   #n <- 1000
#   N <- 3*n
#   p <- 100; c <- 10
#   m <- c(rnorm(4,5,.5), rnorm(96,5,2)) #mean for each column
#   #continuous variables
#   V <- matrix(rnorm(N*p, m, 1),ncol=p, byrow = TRUE)
#   V[V<0] <- 0
#   #categorical variables
#   C <- matrix(rbinom(c*N, size=1, prob=0.7), ncol = c, byrow = TRUE) 
#   #outcome variable
#   temp0 <- cbind(V,C) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8))
#   temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5))
#   Y0 <- rnorm(N, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
#   Y1 <- rnorm(N, mean = temp1, sd=3) 
#   Y <- rbind(cbind(Trt=0,Y0)[1:(N/2),] , cbind(Trt=1,Y1)[((N/2)+1):N,])
#   #making the data frame (110 covars)
#   sbdat <- as.data.frame(cbind(Y, V,C))
#   names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
#   
#   #Creating nonrandom training samples and random test samples
#   test.i <- sample(nrow(sbdat), n)
#   test <- sbdat[test.i,]
#   Rsum <- rowSums(sbdat[-test.i ,c("V13","V14", "V15")])
#   tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[n],]
#   bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[n],]
#   train <- rbind(sample_n(tbl = tophalf, size = n*0.75), sample_n(tbl = bothalf, size = n/4))
#   
#   #True ATE:
#   true0 <- c(m, rep(0.7,10)) %*% c(rep(1,15), rep(0,85), rep(1,2), rep(0,8) )
#   true1 <- c(m, rep(0.7,10)) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
#   TZ <- true1-true0
#   
#   #Naive difference btw trt:
#   NZ <- mean(test$Y[test$Trt==1])-mean(test$Y[test$Trt==0])
#   
#   #VT ATE with different train and test:
#   sbdat.est <- iATE(train, test)
#   VTZ <- mean(sbdat.est$Z)
#   
#   #IPW: combine train and test sets together again, make weights
#   all <- rbind(cbind(pop=1, test), cbind(pop=0,train))
#   ipwd <- glm(pop~., data = subset(all, select = -Y), family = "binomial" ) 
#   toselect.x <- summary(ipwd)$coeff[-1,4] < 0.001
#   relevant.x <- names(toselect.x)[toselect.x == FALSE] 
#   sig.formula <- as.formula(paste("pop ~",paste(relevant.x, collapse= "+")))
#   ipwn <- glm(sig.formula, data = subset(all, select = -Y), family = "binomial" ) 
#   all$ipw <- ifelse(all$pop==1, ipwn$fitted.values/ipwd$fitted.values, (1-ipwn$fitted.values)/(1-ipwd$fitted.values))
#   all$ipw <- ifelse(all$ipw >= 2, 2, all$ipw)
#   
#   #IPW with stabilized weights
#   dat0 <- subset(all, pop==0 & Trt==0)
#   dat1 <- subset(all, pop==0 & Trt==1)
#   m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt, pop, ipw))), 
#                   y = dat0$Y, 
#                   weights = dat0$ipw,
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt, pop, ipw))), 
#                   y = dat1$Y, 
#                   weights = dat1$ipw, 
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   p0 <- coef(m0, s="lambda.1se")
#   p1 <- coef(m1, s="lambda.1se")
#   ifelse(length(p0@i)<3 | length(p1@i)<3, stop("nope"), x <- 1)
#   pred0 <- pred1 <- rep(0, nrow(test))
#   pred0 <- predict(m0, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   pred1 <- predict(m1, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   Z <- pred1-pred0
#   
#   return(c(TZ=TZ, NZ=NZ, VTZ=VTZ, IPWZ=mean(Z)))
# }
# 
