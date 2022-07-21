#Packages for local version of Cond.R:
library(rpart)
library(rpart.plot)
library(glmnet)
library(dplyr)
library(randomForest)
library(randomForestSRC) 
library(purrr)
library(MASS)
library(earth)
library(SuperLearner)
library(caret)
library(party)
library(stringr)

# # using the CENIC data to generate simulation data
# p2 <- read.csv("C:/Users/Chuyu/Desktop/Research/CENIC1P2 data/cenicp2_data_forJeffB_20190925.csv", as.is = TRUE)
# var_map <- read.csv("C:/Users/Chuyu/Desktop/Research/CENIC1P2 data/variable_mapping.csv", na.strings = "", as.is = TRUE)
# to_replace <- names(p2) %in% var_map$p2
# old_names  <- names(p2)[to_replace]
# new_names  <- var_map$new_name[match(names(p2)[to_replace], var_map$p2, nomatch = NULL)] 
# names(p2)[to_replace] <- new_names
# #Trt: A = gradual, B = immediate, C = control
# p2 <- filter(p2, Treatment=="B" | Treatment=="C")
# p2$Treatment <- ifelse(p2$Treatment=="B", 1,0)
# #p2$gender <- factor(p2$gender, labels = c("Female", "Male"))
# p2$menthol <- ifelse(p2$menthol=="Menthol", 1, 0)
# p2$race_white <- ifelse(p2$race==1, 1, 0)
# p2$race_black <- ifelse(p2$race==2, 1, 0)
# p2$race_other <- ifelse(p2$race==3, 1, 0)
# #p2$edu <- factor(p2$edu, labels = c("HS or less", "HS Grad", "College or more"))
# cenic <- dplyr::select(p2, -ends_with("20"), -ends_with("8"), -c("studyid", "site", "tne_visit0", "ftnd_w_cpd", "race"), total_cpd_20)
# cenic <- cenic[complete.cases(cenic), ]
# cenic[,sapply(cenic, class)=="integer"] <- apply(cenic[,sapply(cenic, class)=="integer"], 2, function(x) as.double(x))
