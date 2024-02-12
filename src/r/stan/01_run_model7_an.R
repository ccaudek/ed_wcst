# Script name: 011_run_model_classification.R
# Project: WCST
# Script purpose: run the Stan model for the WCST data by using only the HC and
# AN participants, without distinguishing between groups.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Feb  7 08:43:25 2024
# Last Modified Date: Wed Feb  7 08:43:25 2024
#
# 👉 

library("tidyverse")
library("cmdstanr")
library("posterior")
library("bayesplot")
library("recipes")
library("MCMCpack")
library("bayestestR")
library("insight")


# Get the data of the three groups.
stan_data <- readRDS(
  here::here(
    "data", "processed", "wcst", "stanlist_an.RDS"
  )
)

# chose model
# file <- file.path("src", "wcst", "models", "stan", "01_AU_rpdi_bis.stan") 
# file <- file.path("src", "wcst", "models", "stan", "02_AU_1pd1.stan") 
# file <- file.path("src", "wcst", "models", "stan", "03_MBRL.stan") 
# file <- file.path("src", "wcst", "models", "stan", "04_PRL.stan") 
# file <- file.path("src", "wcst", "models", "stan", "05_MBRL_without_inertia.stan") #
# file <- file.path("src", "wcst", "models", "stan", "06_PRL_without_inertia.stan")
file <- file.path("src", "r", "stan", "stan_models", "07_PRL_weighting.stan") 
# file <- file.path("src", "wcst", "models", "stan", "07bis_PRL_weighting.stan") 
# file <- file.path("src", "wcst", "models", "stan", "08_PRL_weighting_without_inertia.stan") 


# Parmeters of interest
# params_mod1 <- c("mu_r", "mu_p", "mu_d", "mu_i", "r", "p", "d", "i")
# 
# params_mod2 <- c("mu_p", "mu_d", "p", "d")
# 
# params_mod3 <- c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_temp",
#                  "MB_Arew", "MB_Apun", "MB_gamma","temp","log_lik","y_pred")
# 
# params_mod5 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_temp",
#                   "MB_Arew", "MB_Apun", "temp","log_lik","y_pred")  #### PRL
# 
# params_mod4 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
#                   "mu_MF_Apun", "mu_MF_gamma","mu_temp", "MB_Arew", "MB_Apun", 
#                   "MB_gamma","MF_Arew", "MF_Apun", "MF_gamma","temp","log_lik",
#                   "y_pred")
#   
# params_mod6 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MF_Arew", "mu_MF_Apun", 
#                   "mu_temp","MB_Arew", "MB_Apun", "MF_Arew", "MF_Apun", "temp",
#                   "log_lik","y_pred")

params_mod7 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MB_gamma", "mu_MF_Arew", 
                  "mu_MF_Apun", "mu_MF_gamma","mu_temp","mu_w", "MB_Arew", 
                  "MB_Apun", "MB_gamma","MF_Arew", "MF_Apun", "MF_gamma","temp",
                  "w","log_lik","y_pred")

# params_mod7bis <- c(
#   "mu_MB_Arew[1]", "mu_MB_Apun[1]", "mu_MB_gamma[1]", 
#   "mu_MF_Arew[1]", "mu_MF_Apun[1]", "mu_MF_gamma[1]", 
#   "mu_temp[1]", "mu_w[1]",
#   "mu_MB_Arew[2]", "mu_MB_Apun[2]", "mu_MB_gamma[2]", 
#   "mu_MF_Arew[2]", "mu_MF_Apun[2]", "mu_MF_gamma[2]", 
#   "mu_temp[2]", "mu_w[2]",
#   "mu_MB_Arew[3]", "mu_MB_Apun[3]", "mu_MB_gamma[3]", 
#   "mu_MF_Arew[3]", "mu_MF_Apun[3]", "mu_MF_gamma[3]", 
#   "mu_temp[3]", "mu_w[3]",
#   "MB_Arew", "MB_Apun", "MB_gamma", 
#   "MF_Arew", "MF_Apun", "MF_gamma", 
#   "temp", "w", 
#   "log_lik", "y_pred"
# )
# 
# params_mod8 <-  c("mu_MB_Arew", "mu_MB_Apun", "mu_MF_Arew", "mu_MF_Apun",
#                   "mu_temp","mu_w", "MB_Arew", "MB_Apun", "MF_Arew", "MF_Apun", 
#                   "temp","log_lik","y_pred")


# Fix old syntax ---------------------------------------------------------------
# The original stan files are written with an old syntax.
# To fix them, follow the instructions here:
# https://mc-stan.org/cmdstanr/reference/model-method-format.html

# set compile=FALSE then call format to fix old syntax
mod <- cmdstan_model(file, compile = FALSE)
# mod$format(canonicalize = list("deprecations"))
# overwrite the original file instead of just printing it
mod$format(canonicalize = list("deprecations"), overwrite_file = TRUE)

# compile model
mod <- cmdstan_model(file)

# mod$print()
# mod$exe_file()


# AN group ---------------------------------------------------------------------

fit_pf <- mod$pathfinder(data = stan_data, seed = 123)

mu_df <- fit_pf$summary()[19:818, 1:2]

mu_df_split <- mu_df %>%
  # Separate into two columns at the '[' but keep the brackets for further adjustment
  separate(variable, into = c("parameter", "id"), sep = "\\[", remove = TRUE, extra = "merge") %>%
  # Remove closing bracket from 'id' and convert to integer
  mutate(id = as.integer(sub("\\]", "", id))) |> 
  # remove the intermediate parameters used in the non-centered parameterization.
  dplyr::filter(!str_detect(parameter, "_pr")) 

unique(mu_df_split$parameter)

mu_df_wide <- pivot_wider(
  mu_df_split, 
  names_from = parameter, 
  values_from = mean, 
  values_fill = list(mean = NA)
)

# hist(mu_df_wide$w)

rio::export(
  mu_df_wide,
  here::here("data", "processed", "wcst", "mod7_params_an.csv")
)


# eof ---

# 
# fit_pf$summary()[819:826, ]
# 
# 
# 
# 
# 
# draws <- fit_pf$draws(variables = params_mod7, format = "data.frame")
# # Trasforma i dati in un formato lungo per facilitare il calcolo delle medie per soggetto
# draws_long <- draws %>%
#   pivot_longer(cols = starts_with("mu_"), names_to = "parameter", values_to = "value")
# 
# # Estrai l'indice del soggetto da ogni parametro e calcola la media
# draws_long <- draws_long %>%
#   mutate(subject = stringr::str_extract(parameter, "\\[\\d+\\]") %>% stringr::str_remove_all("\\[|\\]"),
#          parameter = stringr::str_remove(parameter, "\\[\\d+\\]")) %>%
#   group_by(subject, parameter) %>%
#   summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
# 
# print(draws_long)
# 
# dim(draws_long)
# 
# 
# fit_mcmc <- mod$sample(
#   data = stan_data,
#   seed = 123,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 50 # print update every 500 iters
# )
# 
# draws <- fit_mcmc$draws(variables = params_mod7, format = "data.frame")
# 
# 
# # Trasforma i dati in un formato lungo per facilitare il calcolo delle medie per soggetto
# draws_long <- draws %>%
#   pivot_longer(cols = starts_with("mu_"), names_to = "parameter", values_to = "value")
# 
# 
# # Estrai l'indice del soggetto da ogni parametro e calcola la media
# draws_long <- draws_long %>%
#   mutate(subject = stringr::str_extract(parameter, "\\[\\d+\\]") %>% stringr::str_remove_all("\\[|\\]"),
#          parameter = stringr::str_remove(parameter, "\\[\\d+\\]")) %>%
#   group_by(subject, parameter) %>%
#   summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
# 
# print(draws_long)
# 
# dim(draws_long)
# 
# 
# # fit_mcmc <- readRDS(
# #   here::here("src", "traces", "fit_wcst_mcmc.rds")
# # )
# 
# # Get names parameters
# params <- names(mod$variables()$parameters)
# 
# fit_mcmc$summary(
#   variables = params,
#   posterior::default_summary_measures(),
#   extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
# )
# 
# 
# # Model 8, patients
# # variable   mean median    sd    mad      q5    q95   q2.75  q97.5
# # 1 mu_p[1]   2.48   2.43  0.492 0.463   1.77    3.37   1.67    3.61 
# # 2 mu_p[2]  -3.11  -3.05  0.417 0.394  -3.86   -2.51  -4.03   -2.43 
# # 3 mu_p[3]  -3.85  -3.81  0.347 0.316  -4.50   -3.37  -4.65   -3.30 
# # 4 mu_p[4]  -3.13  -3.06  0.372 0.351  -3.85   -2.64  -4.00   -2.57 
# # 5 mu_p[5]  -2.01  -1.99  0.108 0.0978 -2.21   -1.86  -2.27   -1.85 
# # 6 mu_p[6]  -1.54  -1.52  0.140 0.133  -1.80   -1.35  -1.86   -1.32 
# 
# 
# # Obtain a posterior mode (penalized maximum likelihood) estimate.
# # fit_mle <- mod$optimize(data = data_list, seed = 123)
# 
# 
# 
# params_mod <- params_mod7 # <------------ Change this for each of the 8 models.
# 
# res_mcmc <- fit_mcmc$summary(params_mod) 
# traces_df <- res_mcmc %>% as.data.frame()
# 
# traces_df <- traces_df[!grepl("y_pred", traces_df$variable), ]
# traces_df <- traces_df[!grepl("log_lik", traces_df$variable), ]
# 
# traces_clean_df <- traces_df |> 
#   dplyr::select(variable, mean)
# 
# 
# traces_clean_df <- traces_clean_df[-c(1:8), ]
# 
# # Splitting the variable column into two
# traces_clean_df <- traces_clean_df %>%
#   separate(variable, into = c("variable_name", "index"), sep = "\\[") %>%
#   mutate(index = gsub("\\]", "", index), # Remove the closing bracket
#          index = as.integer(index)) # Convert index to integer
# 
# 
# traces_wide_df <- traces_clean_df %>%
#   pivot_wider(names_from = variable_name, values_from = mean, id_cols = index)
# 
# rio::export(traces_wide_df, "wcst_params_classification.csv")
# 
# 
# 
# # eof ----
# 
# 
# 
# 
# max(res_mcmc$rhat)
# # [1] 1.011177
# 
# mean(res_mcmc$rhat)
# # [1] 1.000409
# 
# 
# # Group 1: patients
# # Group 2: hc
# # group 3: ri
# 
# # Extract posterior samples for mu_MB_Arew -------------------------------------
# mu_MB_Arew_1_samples <- fit_mcmc$draws("mu_MB_Arew[1]")
# mu_MB_Arew_2_samples <- fit_mcmc$draws("mu_MB_Arew[2]")
# mu_MB_Arew_3_samples <- fit_mcmc$draws("mu_MB_Arew[3]")
# 
# mean(mu_MB_Arew_1_samples > mu_MB_Arew_2_samples)
# # [1] 0.4685
# mean(mu_MB_Arew_1_samples > mu_MB_Arew_3_samples)
# # [1] 0.414
# 
# # Extract posterior samples for mu_MB_Apun -------------------------------------
# mu_MB_Apun_1_samples <- fit_mcmc$draws("mu_MB_Apun[1]")
# mu_MB_Apun_2_samples <- fit_mcmc$draws("mu_MB_Apun[2]")
# mu_MB_Apun_3_samples <- fit_mcmc$draws("mu_MB_Apun[3]")
# 
# mean(mu_MB_Apun_1_samples > mu_MB_Apun_2_samples)
# # [1] 0.4635
# mean(mu_MB_Apun_1_samples > mu_MB_Apun_3_samples)
# # [1] 0.3405
# 
# # Extract posterior samples for mu_MB_gamma ------------------------------------
# mu_MB_gamma_1_samples <- fit_mcmc$draws("mu_MB_gamma[1]")
# mu_MB_gamma_2_samples <- fit_mcmc$draws("mu_MB_gamma[2]")
# mu_MB_gamma_3_samples <- fit_mcmc$draws("mu_MB_gamma[3]")
# 
# mean(mu_MB_gamma_1_samples > mu_MB_gamma_2_samples)
# # [1] 0.67925
# mean(mu_MB_gamma_1_samples > mu_MB_gamma_3_samples)
# # [1] 0.36075
# 
# 
# # Extract posterior samples for MF_Arew ----------------------------------------
# mu_MF_Arew_1_samples <- fit_mcmc$draws("mu_MF_Arew[1]")
# mu_MF_Arew_2_samples <- fit_mcmc$draws("mu_MF_Arew[2]")
# mu_MF_Arew_3_samples <- fit_mcmc$draws("mu_MF_Arew[3]")
# 
# mean(mu_MF_Arew_1_samples > mu_MF_Arew_2_samples)
# # [1] 0.484
# mean(mu_MF_Arew_1_samples > mu_MF_Arew_3_samples)
# # [1] 0.51225
# 
# # Extract posterior samples for MF_Arew ----------------------------------------
# mu_MF_Apun_1_samples <- fit_mcmc$draws("mu_MF_Apun[1]")
# mu_MF_Apun_2_samples <- fit_mcmc$draws("mu_MF_Apun[2]")
# mu_MF_Apun_3_samples <- fit_mcmc$draws("mu_MF_Apun[3]")
# 
# mean(mu_MF_Apun_1_samples > mu_MF_Apun_2_samples)
# # [1] 0.454
# mean(mu_MF_Apun_1_samples > mu_MF_Apun_3_samples)
# # [1] 0.321
# 
# # Extract posterior samples for mu_MF_gamma ------------------------------------
# mu_MF_gamma_1_samples <- fit_mcmc$draws("mu_MF_gamma[1]")
# mu_MF_gamma_2_samples <- fit_mcmc$draws("mu_MF_gamma[2]")
# mu_MF_gamma_3_samples <- fit_mcmc$draws("mu_MF_gamma[3]")
# 
# mean(mu_MF_gamma_1_samples > mu_MF_gamma_2_samples)
# # [1] 0.5575
# mean(mu_MF_gamma_1_samples > mu_MF_gamma_3_samples)
# # [1] 0.312
# 
# # Extract posterior samples for mu_MF_temp ------------------------------------
# mu_temp_1_samples <- fit_mcmc$draws("mu_temp[1]")
# mu_temp_2_samples <- fit_mcmc$draws("mu_temp[2]")
# mu_temp_3_samples <- fit_mcmc$draws("mu_temp[3]")
# 
# mean(mu_temp_1_samples > mu_temp_2_samples)
# # [1] 0.487
# mean(mu_temp_1_samples > mu_temp_3_samples)
# # [1] 0.31925
# 
# # Extract posterior samples for mu_MF_temp ------------------------------------
# mu_w_1_samples <- fit_mcmc$draws("mu_w[1]")
# mu_w_2_samples <- fit_mcmc$draws("mu_w[2]")
# mu_w_3_samples <- fit_mcmc$draws("mu_w[3]")
# 
# mean(mu_w_1_samples > mu_w_2_samples)
# # [1] 0.546
# mean(mu_w_1_samples > mu_w_3_samples)
# # [1] 0.50025


# eof ----


