##########################################
#load arguments from ubelix array

#sets of parameters
solver = c("ckrk","rk45","bdf","adams")
atol = c(1e-2, 1e-4, 1e-6)
rtol = c(1e-2, 1e-4, 1e-6)
warmup_iter = c(100,500,1000)
sampling_iter = c(100,500,1000)
max_num_steps = c(1000)

#args=c(3,1,1,1,1,1,1,1)
args=(commandArgs(TRUE))
args=as.numeric(unlist(args[1:8]))
print("print args")
print(args)
date=args[8]

solver= solver[args[1]]
atol = atol[args[2]]
rtol = rtol[args[3]]
warmup_iter = warmup_iter[args[4]]
sampling_iter = sampling_iter[args[5]]
max_num_steps = max_num_steps[args[6]]

##########################################
#Load data and compile model

# Initialize
code_root_path = strsplit(getwd(),split="ubelix/mod_perf_comparison/temp")[[1]][1]
ubelix_path = strsplit(getwd(),split="temp")[[1]][1]
source("../../../R/000_initialize.R")
print("print working dir")
print(code_root_path)

# 1)  load data that needs to be fitted
I_t_sim = readRDS("../../../data/mod4_I_t_sim.RDS")
print("print I_t_sim")
print(I_t_sim)

#define fixed parameters
contact_survey = contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 20, 35, 50,70),symmetric=TRUE)
age_dist = contact_survey$demography[,"proportion"][[1]]
L = contact_survey$matrix
mean.contact = weighted.mean(apply(L,1,sum),age_dist)
sex_dist = c(0.5,0.5)
M = matrix(c(1,1,1,1),nrow=2)
M = M/weighted.mean(apply(M,1,sum),sex_dist) * mean.contact
pop_dist = matrix(rep(age_dist,length(sex_dist)),ncol=length(sex_dist)) * matrix(rep(sex_dist,length(age_dist)),ncol=length(sex_dist),byrow=TRUE)
pop_dist = c(pop_dist)
K = kronecker(L,M)/mean.contact

#2) compile model
mod4 = cmdstan_model(stan_file = paste0("../../../stan/mod4_",solver,".stan"))

##########################################
#Run model
samples_mod4 =  mod4$sample(data = list(
  num_t = 45,
  ts = 1:45,
  popsize = 1000,
  num_age = 5,
  num_sex = 2,
  popdist = pop_dist, 
  num_knots = 4,
  knots = c(0,15,30,45),
  beta_sim = 0.1,
  I0_raw_sim = 5,
  rho_sim = c(0.2,2.5),
  p_I0 = c(5,1),
  p_beta = c(1,1),
  p_rho = 1,
  incubation_period = 5,
  infectious_period = 2,
  contact = K,
  inference = 1,
  #init = 0,
  rtol = atol,
  atol = rtol,
  max_num_steps = max_num_steps,
  I_t_sim = structure(I_t_sim, dim=c(45,10))), 
  chains = 4, parallel_chains = 4,
  iter_warmup = warmup_iter, iter_sampling = sampling_iter)

##########################################
#Save results

#save the chains
samples_mod4$save_output_files(dir = paste0(ubelix_path,"results/stan_chains"),
                               basename = paste0("chain-mod4-",paste(args[1:7],collapse="")))

#Save chain paths, time and diagnose
chain_paths =  sapply(samples_mod4$output_files(), function(x) sapply(strsplit(x,"usb_efficient_seir/"), getElement, 2)) %>% unname()
time = samples_mod4$time()$total
diagnose = samples_mod4$cmdstan_diagnose()
divergence = samples_mod4$sampler_diagnostics()[,,"divergent__"] %>% as.vector() %>% sum()
save(chain_paths, time, diagnose, divergence, file = paste0(paste0(ubelix_path,"results/info-mod4-",paste(args[1:7],collapse="")),"-", date,".RData"))

#Save estimates
res <- samples_mod4$summary(c("beta","I0_raw","rho"))
print(res)
saveRDS(res, file = paste0(paste0(ubelix_path,"results/est-mod4-",paste(args[1:7],collapse="")),"-", date,".rds"))

I_t_predicted <- samples_mod4$summary(c("I_t_predicted"))
saveRDS(I_t_predicted, file = paste0(paste0(ubelix_path,"results/pred-mod4-",paste(args[1:7],collapse="")),"-", date,".rds"))
