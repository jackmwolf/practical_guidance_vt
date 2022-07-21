
source("Packages.R") #loading packages and CENIC data
source("Funcs_Complex.R")

args=(commandArgs(TRUE))
for(i in 1:length(args)){eval(parse(text=args[[i]]))}
job = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#for testing the code:
### vt1= i.las, i.rf.fast, i.mars, i.super.fast
# dg <- dg0(g0 = "g2", h0 = "h1", f0 = 0.35)

set.seed(as.numeric(job))
seeds <- round(runif(n = sims, min = 1, max = 10000))

#create simulation wrapper  
Cond <- function(vt1, seed){
  set.seed(seed)
  #generate data based on script input for dg
  dat <- dg0(g0, h0, f0)
  #VT1: getting individual treatment effects based on script input for vt1
  vt1 <- get(vt1, mode = "function", envir = parent.frame())
  est <- vt1(dat$reg)
  #VT2: getting results from all 3 variations
  b.none <- c.none(dat, est)
  b.tree <- c.tree(dat, est)
  b.lin <- c.lin(dat, est)
  b.ctree <- c.ctree(dat, est)
  
  #putting all the results together
  models <- list(none = b.none, lin = b.lin, tree = b.tree, ctree = b.ctree)
 
  nwgs <- sapply(models, function(.x) .x$nwg, simplify = TRUE)
  mses <- sapply(models, function(.x) .x$mse, simplify = TRUE)
  vars <- sapply(models, function(.x) .x$vars, simplify = TRUE)
  nvars <- sapply(vars, length, simplify = TRUE)
  vars <- sapply(vars, function(.x) paste(sort(.x), collapse = ", "), simplify = TRUE)
  
  re <- data.frame(nwgs = unname(nwgs), mses = unname(mses), 
                   vars = unname(vars), nvars = unname(nvars))
  re$Stage2 <- names(models)
  
  return(re)
  
}

Cond <- possibly(eval(Cond), 
                 otherwise = data.frame(nwgs = rep("error", 4), 
                                        mses = rep("error", 4), 
                                        vars = rep("error", 4),
                                        nvars = rep("error", 4),
                                        Stage2 = rep("error")))

#random forest:
list_rf <- mapply(Cond, "i.rf", seeds, SIMPLIFY = FALSE)
#super learner:
list_sl <- mapply(Cond, "i.super", seeds, SIMPLIFY = FALSE)
#lasso:
list_lasso <- mapply(Cond, "i.las", seeds, SIMPLIFY = FALSE)
#mars:
list_mars <- mapply(Cond, "i.mars", seeds, SIMPLIFY = FALSE)

res <- list(lasso = list_lasso, mars = list_mars, rf = list_rf, sl = list_sl) 
saveRDS(res, paste0("sol.", job, ".rds"))
