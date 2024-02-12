# Script name: 02_run_model_hc.R
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
    "data", "processed", "wcst", "stanlist_hc.RDS"
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


# HC group ---------------------------------------------------------------------

fit_pf <- mod$pathfinder(data = stan_data, seed = 123)
mu_df <- fit_pf$summary()[19:818, 1:2]

mu_df_split <- mu_df %>%
  # Separate into two columns at the '[' but keep the brackets for further adjustment
  separate(variable, into = c("parameter", "id"), sep = "\\[", remove = TRUE, extra = "merge") %>%
  # Remove closing bracket from 'id' and convert to integer
  mutate(id = as.integer(sub("\\]", "", id))) |> 
  # remove the intermediate parameters used in the non-centered parameterization.
  dplyr::filter(!str_detect(parameter, "_pr")) 

# unique(mu_df_split$parameter)

mu_df_wide <- pivot_wider(
  mu_df_split, 
  names_from = parameter, 
  values_from = mean, 
  values_fill = list(mean = NA)
)

# hist(mu_df_wide$w)


rio::export(
  mu_df_wide,
  here::here("data", "processed", "wcst", "mod7_params_hc.csv")
)


# eof ---

