pcks <-
  c(
    "crayon",
    "dplyr",
    "foreach",
    "glmnet",
    "MASS",
    "rpart",
    "rpart.plot",
    "randomForest",
    "randomForestSRC",
    "withr",
    "ggplot2",
    "caret",
    "ranger",
    "mvtnorm",
    "modeltools",
    "zoo",
    "sandwich",
    "strucchange",
    "party",
    "Formula",
    "plotrix",
    "TeachingDemos",
    "plotmo",
    "earth",
    "nnls",
    "kernlab",
    "SuperLearner",
    "purrr",
    "stringr",
    "glmnet",
    "gam"
)

# install.packages(pcks, lib = "RPackages/")
suppressMessages(lapply(pcks, require, character.only = TRUE, 
                        lib.loc = "/panfs/roc/groups/11/koopmein/wolfx681/RPackages")
                 )

# suppressMessages(lapply(pcks, require, character.only = TRUE))

# library(crayon, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for dyplyr
# library(dplyr, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(foreach, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for glmnet
# library(glmnet, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(MASS, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(rpart, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(rpart.plot, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(randomForest, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(randomForestSRC, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(withr, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for ggplot2
# library(ggplot2, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for caret
# library(caret, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(ranger, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(mvtnorm, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for party
# library(modeltools, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for party
# library(zoo, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for strucchange
# library(sandwich, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for strucchange
# library(strucchange, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for party
# library(party, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(Formula, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for earth
# library(plotrix, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for plotmo
# library(TeachingDemos, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for plotmo
# library(plotmo, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for earth
# library(earth, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(nnls, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") #needed for SuperLearner
# library(kernlab, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R") 
# library(SuperLearner, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(purrr, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")
# library(stringr, lib.loc = "/panfs/roc/groups/5/vock/dengx361/R")

# # using the CENIC data to generate simulation data
# p2 <- read.csv("/panfs/roc/groups/5/vock/dengx361/VTSims/Rcode/cenicp2_data_forJeffB_20190925.csv", as.is = TRUE)
# var_map <- read.csv("/panfs/roc/groups/5/vock/dengx361/VTSims/Rcode/variable_mapping.csv", na.strings = "", as.is = TRUE)
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
