# ANALYZE SIMULATIONS FOR CHUYU'S PAPER
package_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/RPackages"
library(dplyr, lib.loc = package_dir)
library(tidyr, lib.loc = package_dir)
library(readr)
library(stringr)


cnums <- expand.grid("V", 1:3, 1:3, 1:3) %>% 
  do.call(what = str_c, args = .)

# Reference table to convert R#### to to settings
cnum_lookup <- tibble(cnum = cnums) %>% 
  mutate(.g = str_sub(cnum, 2, 2),
         .h = str_sub(cnum, 3, 3),
         f = str_sub(cnum, 4, 4),
         # h = ifelse(.h == "1", "Linear", "Nonlinear"),
         h = case_when(
           .h == "1" ~ "Linear",
           .h == "2" ~ "Nonlinear",
           .h == "3" ~ "CENIC2"
         ),
         g = case_when(
           .g == "1" ~ "Tree",
           .g == "2" ~ "Polynomial",
           .g == "3" ~ "Logistic"
         )
  ) %>% 
  select(-starts_with("."))


# test_string <- "V1, V14, V18, V6, V7"
get_n_hits <- function(x, hits) {
  sapply(x, function(x) {
    x <- str_split(x, pattern = ", ")[[1]]
    x <- x %in% hits
    return(sum(x))
  })
}

# Read in a directory of simulation results and aggregate them into one tibble
compact_results <- function(cnum, work_dir = file.path(), verbose = TRUE) {
  
  dir <- file.path(work_dir, cnum)
  
  if (verbose) {
    message(paste("Compacting simulations in", dir))
  }
  files <- list.files(dir, pattern = "*.rds$") 
  re <- lapply(files, function(.x) readRDS(file.path(dir, .x)))
  
  re_long <- 
    lapply(re, function(.x) {
    lapply(.x, function(.y) {
      # un-simplify output if .y is NOT a list of data frames
      if(is.matrix(.y)) {
        .y <- apply(.y, 2, as.data.frame)
      }
      .y <- lapply(.y, function(.z) mutate_if(.z, is.factor, as.character))
      .y <- do.call(rbind, .y)
      .y
    })
  })
  
  re_long <- apply(do.call(rbind, re_long), 2, function(.x) do.call(rbind, .x))
  for (i in 1:length(re_long)) {
    .name <- names(re_long)[[i]]
    vt1 <- .name
    re_long[[i]]$Stage1 <- vt1
  }

  re_out <- do.call(rbind, re_long)
  re_out$cnum <- str_extract(dir, "V\\d\\d\\d$")
  re_out <- mutate_at(re_out, c("nwgs", "mses", "nvars"), as.numeric)
  
  info <- cnum_lookup[cnum_lookup$cnum == cnum, ]
  
  # Variables with TEH

  hits <- paste0("V", c(1, 2, 3))
  P <- ifelse(info$h == "CENIC2", 40, 100)
  
  n_teh <- length(hits)
  re_out <- re_out %>% 
    mutate(n_hits = get_n_hits(vars, hits),
           prop_correct_marginal = n_hits / n_teh,
           prop_correct_conditional = ifelse(nvars > 0, n_hits / nvars, NA), #If no variables are selected, return NA
           sensitivity = n_hits / n_teh,
           specificity = 1 - (nvars - n_hits) / (P - n_teh)
    )
    
  return(re_out)
}



# READ RESULTS =================================================================

# Safe to ignore warnings of the form:  
#   NAs introduced by coercion 
#   Input `nvars` is `.Primitive("as.double")(nvars)`
# This just happens when convering the character "error" to NA

work_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu"
re <- lapply(cnums, compact_results, work_dir = work_dir)


# Put all c#### objects into one tibble
re.df <- do.call(bind_rows, re)

# Remove rows with "error"
re.df <- re.df %>% 
  filter(Stage2 != "error")

re.df <- re.df %>% 
  left_join(cnum_lookup, by = "cnum") %>% 
  mutate(N = 1000)

saveRDS(re.df, file.path(table_dir, paste0("vt_results_chuyu_full_", Sys.Date(), ".Rds")))


# TABLES =======================================================================
table_dir <- "/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/Results"

# Function to summarise based on grouping variables
summarise_vt <- function(x) {
  summarise(x, 
            Mean_X = mean(nvars),
            Pr_Any_X = mean(nvars != 0 ),
            sensitivity = mean(sensitivity),
            specificity = mean(specificity),
            Pr_Hit_Marginal = mean(prop_correct_marginal),
            Pr_Hit_Conditional = mean(prop_correct_conditional, na.rm = TRUE),
            wg = mean(nwgs/N),
            mse = mean(mses, na.rm = TRUE),
            N_Sims = n()
            )
}

summarize_vt <- summarise_vt # Accept both spellings of summarise

# Save results as a Rds
re.df %>% 
  group_by(Stage1, Stage2, g, h, f, cnum) %>% 
  summarise_vt %>% 
  saveRDS(file.path(table_dir, paste0("vt_results_chuyu1_", Sys.Date(), ".Rds")))