#MSI version of VTSims code 11/11/19
#source("/panfs/roc/groups/5/vock/dengx361/VTSims/Rcode/Packages.R")
#source("/panfs/roc/groups/5/vock/dengx361/VTSims/Rcode/Funcs.R")
#args=(commandArgs(TRUE))
#for(i in 1:(length(args)-1)){eval(parse(text=args[[i]]))}

source('C:/Users/Chuyu/Desktop/Research/Genentech/Packs.R') #loading all the packages (and CENIC data)
source('C:/Users/Chuyu/Desktop/Research/Genentech/Funcs.R')

### job= 1 to 50
### dg= regDG, corDG, sbDG, imbDG, nlregDG, piecorDG, piesbDG, pieimbDG
### vt1= i.las, i.rf, i.mars, i.super, i.svm
job <- 1
dg <- regDG
vt1 <- i.rf
N <- 1000

sims <- 2 # 20 here gives 1000 iterations total
set.seed(as.numeric(job))
seeds <- round(runif(n = sims, min = 1, max = 10000))

#create simulation wrapper  
Cond <- function(N, seed){
  set.seed(seed)
  #generate data based on script input for dg
  dat <- dg(N)
  #VT1: getting individual treatment effects based on script input for vt1
  est <- vt1(dat$train, dat$test)
  #VT2: getting results from all 3 variations
  b.none <- c.none(dat, est)
  b.tree <- c.tree(dat, est)
  b.lin <- c.lin(dat, est, b.tree$nvars)
  b.ctree <- c.ctree(dat, est)
  #putting all the results together
  nwgs <- data.frame(none=b.none$nwg, lin=b.lin$nwg, tree=b.tree$nwg, ctree=b.ctree$nwg)
  mses <- data.frame(none=b.none$mse, lin=b.lin$mse, tree=b.tree$mse, ctree=b.ctree$mse)
  vars <- list(lin=b.lin$vars, tree=b.tree$vars, ctree=b.ctree$vars)
  return(list(nwgs=nwgs, mses=mses, vars=vars))
}

Cond <- possibly(eval(Cond), otherwise = list("error", "error", "error"))

#Sample size of 1000:
list1 <- mapply(Cond, 1000, seeds)
#Sample size of 200:
list2 <- mapply(Cond, 200, seeds)
#Sample size of 80:
list3 <- mapply(Cond, 80, seeds)

res <- list(list1, list2, list3)
saveRDS(res, paste0("sol.", job, ".rds"))
