# Script name: 12_recoding_for_stan_ri.R
# Project: EDs and WCST
# Script purpose: Generate list for PRL Stan model.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed May 25 05:57:16 2022
# Last Modified Date: Mon Feb 12 09:50:12 2024
#
# 👉 This script generates the input data for Steinke's algorithm of the WCST.

# rule_choice : which category is rewarded.
#   color  : 1
#   shape  : 2
#   number : 3

# resp_choice rew resp_color resp_shape resp_number

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("stringi")
})



# Source functions -------------------------------------------------------------
source(here::here("src", "r", "helper_functions", "funs_wcst.R"))

# get_one_subj_data_for_stan()
source(here::here("src", "r", "helper_functions", "funs_input_for_stan_wcst.R"))


# Generate RDS raw data for patients or controls -------------------------------

# Select group.
GROUP <- "controls"  # "controls" "patients"

dir <- here("data", "raw", "wcst", GROUP)

if (GROUP == "patients") {
  file_names <- as.character(list.files(path=dir, pattern="wcst_pazienti"))
} else {
  file_names <- as.character(list.files(path=dir, pattern="wcst_eds1"))
}

n_files <- length(file_names)
# n_files

d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(
    here("data", "raw", "wcst", GROUP, file_names[i]), 
    header = FALSE
  )
  
  d$subj_name <- file_names[i]
  d$block <- rep(1:6, each = 10)
  
  d$card_shown <- d$V1
  d$correct_card <- d$V2
  d$card_chosen_if_perseveration <- d$V3
  d$trial_in_a_sequence <- d$V4 
  d$name_of_task <- d$V5
  d$card_shape <- d$V6
  d$card_number_of_symbols <- d$V7
  d$card_color <- d$V8
  d$rt <- d$V9
  d$is_correct <- d$V10 # 1=correct, 2=wrong card, 3=too slow
  d$chosen_card <- d$V11 # a number between 1 and 4, or 0 if none clicked
  d$is_error <- d$V12 
  # If 1, this trial was an error (otherwise 0); 0 or 1
  d$is_perseverative_error <- d$V13 
  # If 1, this trial was a perseveration error (otherwise 0); 0, 1
  d$is_non_perseverative_error <- d$V14
  # If 1, this trial was not a perseveration error (otherwise 0); 0, 1
  
  d_list[[i]] <- d
}

# convert list into data.frame
df0 <- do.call(rbind.data.frame, d_list)

# length(unique(df0$subj_name))

# Examine accuracy.
bysubj_acc <- df0 %>% 
  group_by(subj_name) %>% 
  summarise(
    error_rate = mean(is_error)
  ) %>% as.data.frame()

# hist(bysubj_acc$error_rate)

bad_subj_df <- bysubj_acc %>%
  dplyr::filter(error_rate > 0.5)
 
bad_subjects <- bad_subj_df$subj_name
# bad_subjects
# [1] "wcst_eds1.2020-12-04-1510.data.0befcae9-ff0f-402b-863d-be488b1089fd.txt"
# [2] "wcst_eds1.2021-01-20-1227.data.e1b90ccd-0c30-4d40-ab46-85ea3960534a.txt"
# [3] "wcst_eds1.2021-01-27-1954.data.445af47b-f619-41c6-a3a0-39805e44eabc.txt"
# [4] "wcst_eds1.2021-03-16-1135.data.b95075e7-4ec7-480c-82c4-6e8285efeb71.txt"
# [5] "wcst_eds1.2021-03-16-1300.data.fb5d2835-323c-41b2-917f-7cd07eb7f716.txt"
# [6] "wcst_eds1.2021-03-16-1547.data.c9cb97a0-f25d-4e3a-908b-258910a1096e.txt"
# [7] "wcst_eds1.2021-03-17-1726.data.74712e43-1965-4b7e-b4fa-8a7397d204e4.txt"
# [8] "wcst_eds1.2021-03-20-1816.data.6a22eb3a-2e4a-4063-b068-a0eba27f1d2a.txt"
# [9] "wcst_eds1.2021-03-21-1043.data.2e23dd2c-5ac7-4908-857f-14258a06e1c1.txt"
# [10] "wcst_eds1.2021-05-21-1502.data.4727f714-3733-4358-96fd-b573a293cc82.txt"

# # Remove bad subjects.
df1 <- df0[!(df0$subj_name %in% bad_subjects), ]
# length(unique(df1$subj_name))

df <- df1[!is.na(df1$chosen_card), ]

# Recoding as required by Steinke's algorithm. 
df <- df %>% 
  dplyr::rename(
    card_number = card_number_of_symbols
  )

# length(unique(df$subj_name))

if (GROUP == "patients") {
  saveRDS(
    df,
    here::here("data", "interim", "wcst", "raw_input_for_stan_an.RDS")
  )
} else {
  saveRDS(
    df,
    here::here("data", "interim", "wcst", "raw_input_for_stan_hc.RDS")
  )
}


# Read subj_name for each of the three groups ----------------------------------

# I need to use the same subjects that were used in the PRL task. 
# The list "participants_list" shows the subj_name for each of the three 
# groups that were used in the PRL task, with this structure:
# participants_list[[1]] <- patients
# participants_list[[2]] <- hc
# participants_list[[3]] <- ri
source(here::here("src", "r", "list_participants.R"))


# Generate the input list for cmdstan

# # AN ---------------------------------------------------------------------------
# 
# # subj_code of patients who completed the PRL task
# patients_from_prl <- participants_list[[1]]
# 
# # we have 41 patients who completed the WCST and also the PRL task.
# # These patients will be included in the WCST sample.
# patients_wcst_look_up_tbl <- gen_correspondence_table_codes("patients")
# 
# patients_keep <- 
#   patients_wcst_look_up_tbl[
#     patients_wcst_look_up_tbl$subj_name %in% patients_from_prl, ]
# 
# # 19 patients who completed the WCST but did not complete the PRL task.
# patients_not_keep <- 
#   patients_wcst_look_up_tbl[
#     !(patients_wcst_look_up_tbl$subj_name %in% patients_from_prl), ]
# 
# # 4 patients that we can add to the 41.
# names_patients_to_add_to_wcst <- c(
#   "de_sc_1992_07_02_116_f", 
#   "ma_be_1997_09_01_726_f", 
#   "gr_ma_1995_09_05_060_f", 
#   "ca_fa_1996_03_26_092_f",
#   "chiara_benazzi_1990_12_20_153_f",
#   "ch_ma_2001_10_27_331_f",
#   "st_sa_1996_06_05_556_f",
#   "fe_sa_2002_05_09_08_f",
#   "ma_ba_1995_05_25_321_f",
#   "emma_orsucci_2003_01_02_101_f",
#   "ma_be_1997_09_01_726_f",
#   "la_al_1996_06_14_190_f",
#   "fr_ma_1996_02_16_959_f"
# )
# 
# patients_new_code_psytoolkit <- patients_not_keep[
#   patients_not_keep$subj_name %in% names_patients_to_add_to_wcst, 
# ]$code_psytoolkit
# 
# patients_old_code_psytoolkit <- patients_keep$code_psytoolkit
# 
# patients_code_psytoolkit_for_wcst <- c(
#   patients_new_code_psytoolkit, patients_old_code_psytoolkit
# )
# length(patients_code_psytoolkit_for_wcst)
# # 45
# 
# # Read the raw data.
# df_patients <- readRDS(
#   here::here("data", "interim", "wcst", "raw_input_for_stan_patients.RDS")
# )
# 
# # Select the 45 patients from the raw data.
# df_patients_clean <- df_patients[
#   df_patients$subj_name %in% patients_code_psytoolkit_for_wcst, ]
# 
# length(unique(df_patients_clean$subj_name))
# 
# df_patients_clean$group <- "an"
# 
# stan_input_patients <- compile_data_for_stan(df_patients_clean)
# 
# saveRDS(
#   stan_input_patients,
#   here::here("data", "processed", "wcst", "stanlist_patients.RDS")
# )


# # HC ---------------------------------------------------------------------------
# 
# # 50 subj_code of HC who completed the PRL task
# hc_from_prl <- participants_list[[2]]
# 
# hc_wcst_look_up_tbl <- gen_correspondence_table_codes("controls")
# 
# 
# # 47 HC who completed the WCST and are included in the 50 who completed 
# # the PRL task
# hc_code_psytoolkit_for_wcst <- hc_wcst_look_up_tbl[
#   hc_wcst_look_up_tbl$subj_name %in% hc_from_prl, 
# ]$code_psytoolkit
# 
# df_hc <- readRDS(
#   here::here("data", "interim", "wcst", "input_stan_controls.RDS")
# )
# 
# df_hc_clean <- df_hc[
#   df_hc$subj_name %in% hc_code_psytoolkit_for_wcst, ]
# 
# length(unique(df_hc_clean$subj_name))
# 
# hc_code_psytoolkit_not_in_the_45 <- hc_wcst_look_up_tbl[
#   !(hc_wcst_look_up_tbl$code_psytoolkit %in% df_hc_clean$subj_name), 
# ]$code_psytoolkit[1:5] 
# 
# hc_code_psytoolkit_for_wcst <- c(
#   hc_code_psytoolkit_not_in_the_45, unique(df_hc_clean$subj_name)
# )
# 
# df_hc_new <- df_hc[df_hc$subj_name %in% hc_code_psytoolkit_for_wcst, ]
# 
# length(unique(df_hc_new$subj_name))
# 
# saveRDS(
#   get_one_subj_data_for_stan(df_hc_new),
#   here::here("data", "processed", "wcst", "stanlist_controls.RDS")
# )


# RI ---------------------------------------------------------------------------

# 24 subj_code of HC who completed the PRL task
ri_from_prl <- participants_list[[3]]

hc_wcst_look_up_tbl <- gen_correspondence_table_codes("controls")

ri_code_psytoolkit_for_wcst <- hc_wcst_look_up_tbl[
  hc_wcst_look_up_tbl$subj_name %in% ri_from_prl, 
]$code_psytoolkit

df_hc <- readRDS(
  here::here("data", "interim", "wcst", "raw_input_for_stan_hc.RDS")
)

df_ri_clean <- df_hc[
  df_hc$subj_name %in% ri_code_psytoolkit_for_wcst, ]

# length(unique(df_ri_clean$subj_name))
# 24

df_ri_clean$group <- "ri"

stan_input_ri <- compile_data_for_stan(df_ri_clean)


saveRDS(
  stan_input_ri,
  here::here("data", "processed", "wcst", "stanlist_ri.RDS")
)


# 
# # Combine the select subject of each group in a single data frame --------------
# 
# # Add group
# df_patients_clean$group <- "an"
# df_hc_new$group <- "hc"
# df_ri_clean$group <- "ri"
# 
# temp <- rbind(df_patients_clean, df_hc_new)
# temp1 <- rbind(temp, df_ri_clean)
# 
# temp1 |> 
#   group_by(group) |> 
#   summarize(
#     n = n_distinct(subj_name)
#   )
# 
# all_groups_df <- temp1 |> 
#   dplyr::select(
#     !starts_with("V")
#   )
# 
# 
# # Use this function to prepare your data
# stan_data <- compile_data_for_stan(all_groups_df)


# eof ----

