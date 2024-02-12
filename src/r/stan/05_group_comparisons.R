
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(brms)
  library(cmdstanr)
})

params_an_df <- rio::import(
  here::here(
    "data", "processed", "wcst", "mod7_params_an.csv"
  )
)
params_an_df$group <- "an"

params_hc_df <- rio::import(
  here::here(
    "data", "processed", "wcst", "mod7_params_hc.csv"
  )
)
params_hc_df$group <- "hc"

params_ri_df <- rio::import(
  here::here(
    "data", "processed", "wcst", "mod7_params_ri.csv"
  )
)
params_ri_df$group <- "ri"


params_df <- rbind(params_an_df, params_hc_df, params_ri_df)

hist(params_df$MB_Arew)
m1 <- brm(
  MB_Arew ~ group,
  data = params_df,
  family = student(),
  backend = "cmdstanr"
)
pp_check(m1)
summary(m1)




