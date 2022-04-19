#setting up paths
wd = getwd()
code_root_path = strsplit(wd, split="ubelix/mod_perf_comparison")[[1]][1]
print(code_root_path)

#load R files
source("../../R/000_initialize.R")

#date and time when simulations were run
date = "20220405"
run = get_mod4_ubelix_arrays(date)

#save table
saveRDS(run, file = paste0(wd,"/results/table_comp-",date,".rds"))
