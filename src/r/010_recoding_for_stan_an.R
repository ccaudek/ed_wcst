# Script name: 01_recoding_for_stan_an.R
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



# Source functions
source(here::here("src", "r", "helper_functions", "funs_wcst.R"))
# get_one_subj_data_for_stan()
source(here::here("src", "r", "helper_functions", "funs_input_for_stan_wcst.R"))


# Generate RDS raw data for patients or controls -------------------------------

# Select group.
GROUP <- "patients"  # "controls" "patients"

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
# [1] "wcst_pazienti.2021-01-29-1540.data.ac4753cb-6fee-40d2-b612-1d2ed30ebf95.txt"
# [2] "wcst_pazienti.2021-05-05-1829.data.1bde0f3a-bced-4e52-ad41-216a3d245f69.txt"
# [3] "wcst_pazienti.2021-06-01-1529.data.7c4e8f61-1fcd-4b54-ac64-4b6dbe87746b.txt"
# [4] "wcst_pazienti.2021-07-08-1741.data.9117ab46-6270-4b8d-9625-038ce18f05ff.txt"

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
    here::here("data", "interim", "wcst", "raw_input_for_stan_patients.RDS")
  )
} else {
  saveRDS(
    df,
    here::here("data", "interim", "wcst", "raw_input_for_stan_controls.RDS")
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

# AN ---------------------------------------------------------------------------

# subj_code of patients who completed the PRL task
patients_from_prl <- participants_list[[1]]

# we have 41 patients who completed the WCST and also the PRL task.
# These patients will be included in the WCST sample.
patients_wcst_look_up_tbl <- gen_correspondence_table_codes("patients")

patients_keep <- 
  patients_wcst_look_up_tbl[
    patients_wcst_look_up_tbl$subj_name %in% patients_from_prl, ]

# 19 patients who completed the WCST but did not complete the PRL task.
patients_not_keep <- 
  patients_wcst_look_up_tbl[
    !(patients_wcst_look_up_tbl$subj_name %in% patients_from_prl), ]

# 4 patients that we can add to the 41.
names_patients_to_add_to_wcst <- c(
  "de_sc_1992_07_02_116_f", 
  "ma_be_1997_09_01_726_f", 
  "gr_ma_1995_09_05_060_f", 
  "ca_fa_1996_03_26_092_f",
  "chiara_benazzi_1990_12_20_153_f",
  "ch_ma_2001_10_27_331_f",
  "st_sa_1996_06_05_556_f",
  "fe_sa_2002_05_09_08_f",
  "ma_ba_1995_05_25_321_f",
  "emma_orsucci_2003_01_02_101_f",
  "ma_be_1997_09_01_726_f",
  "la_al_1996_06_14_190_f",
  "fr_ma_1996_02_16_959_f"
)

patients_new_code_psytoolkit <- patients_not_keep[
  patients_not_keep$subj_name %in% names_patients_to_add_to_wcst, 
]$code_psytoolkit

patients_old_code_psytoolkit <- patients_keep$code_psytoolkit

patients_code_psytoolkit_for_wcst <- c(
  patients_new_code_psytoolkit, patients_old_code_psytoolkit
)

# length(patients_code_psytoolkit_for_wcst)
# 45

# Read the raw data.
df_patients <- readRDS(
  here::here("data", "interim", "wcst", "raw_input_for_stan_patients.RDS")
)

# Select the 45 patients from the raw data.
df_patients_clean <- df_patients[
  df_patients$subj_name %in% patients_code_psytoolkit_for_wcst, ]

# length(unique(df_patients_clean$subj_name))

df_patients_clean$group <- "an"

stan_input_patients <- compile_data_for_stan(df_patients_clean)

saveRDS(
  stan_input_patients,
  here::here("data", "processed", "wcst", "stanlist_an.RDS")
)


# eof ----

