##########################################
#load arguments from ubelix array

#sets of parameters
solver= c("rk45","bdf","adams")
atol = c(1e-5, 1e-6)
rtol = c(1e-5, 1e-6)
warmup_iter = c(100,500,1000)
sampling_iter = c(100,500,1000)
max_num_steps = c(1000)

#args=c(1,1,1,1,1,1,1,1)
args=(commandArgs(TRUE))
args=as.numeric(unlist(args))
print("print args")
print(args)

solver= solver[args[1]]
atol = atol[args[2]]
rtol = rtol[args[3]]
warmup_iter = warmup_iter[args[4]]
sampling_iter = sampling_iter[args[5]]
max_num_steps = max_num_steps[args[6]]

##########################################
#Load data and compile model

# Initialize
code_root_path = getwd()
source("R/000_initialize.R")
print("print working dir")
print(code_root_path)

# 1)  data formatting
I_t_sim = readRDS("data/mod1_I_t_sim.RDS")
print("print I_t_sim")
print(I_t_sim)
I_t_sim = as.numeric(I_t_sim)
#2) compile model
mod1 = cmdstan_model(stan_file = paste0("stan/mod1_",solver,".stan"))

##########################################
#Run model
samples_mod1 =  mod1$sample(data = list(
  num_t = 45,
  ts = 1:45,
  popsize = 1000,
  beta_sim = 0.1,
  I0_raw_sim = 1,
  p_I0 = c(5,1),
  p_beta = c(1,1),
  incubation_period = 5,
  infectious_period = 2,
  contact = 11,
  inference = 1,
  rtol = atol,
  atol = rtol,
  max_num_steps = max_num_steps,
  I_t_sim = I_t_sim), 
  chains = 4, parallel_chains = 4,
  iter_warmup = warmup_iter, iter_sampling = sampling_iter)

##########################################
#Save results

#save the chains
samples_mod1$save_output_files(dir = paste0(code_root_path,"/stan_results/stan_chains"),
                               basename = paste0("chain-mod1-",paste(args[1:6],collapse="")))

#Save chain paths, time and diagnose
chain_paths =  sapply(samples_mod1$output_files(), function(x) sapply(strsplit(x,"usb_efficient_seir/"), getElement, 2)) %>% unname()
time = samples_mod1$time()$total
diagnose = samples_mod1$cmdstan_diagnose()
divergence = samples_mod1$sampler_diagnostics()[,,"divergent__"] %>% as.vector() %>% sum()
save(chain_paths, time, diagnose, divergence, file = paste0(paste0(code_root_path,"/stan_results/info-mod1-",paste(args[1:6],collapse="")),"-", format(Sys.time(), "%Y%m%d%H%M"),".RData"))

#Save estimates
res <- samples_mod1$summary(c("beta","I0_raw"))
print(res)
saveRDS(res, file = paste0(paste0(code_root_path,"/stan_results/est-mod1-",paste(args[1:6],collapse="")),"-", format(Sys.time(), "%Y%m%d%H%M"),".rds"))

I_t_predicted <- samples_mod1$summary(c("I_t_predicted"))
saveRDS(I_t_predicted, file = paste0(paste0(code_root_path,"/stan_results/pred-mod1-",paste(args[1:6],collapse="")),"-", format(Sys.time(), "%Y%m%d%H%M"),".rds"))
