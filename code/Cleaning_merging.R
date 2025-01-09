## Code to generate a compiled dataset for Rhea's MS Thesis 

#### set up ####

# load libraries
library(haven)
library(dplyr)

#load_all("../dataprepr") 

# load REACH data
reach_intake <- read.delim("data/Reach/intake.tsv", na.strings = "n/a", quote="")
#or 
reach_intake <- read.csv("data/Reach/intake.tsv",  sep = "\t", na.strings = "n/a", quote="")
reach_cebq <- read.csv("data/Reach/cebq.tsv",sep = "\t", na.strings = "n/a")
reach_bisbas <- read.csv("data/Reach/bisbas.tsv", sep = "\t", na.strings = "n/a")
reach_participants <- read.csv("data/Reach/participants.tsv", sep = "\t", na.strings = "n/a")
reach_anthro <- read.csv("data/Reach/anthropometrics.tsv", sep = "\t", na.strings = "n/a")
reach_demo <- read.csv("data/Reach/demographics.tsv",sep = "\t", na.strings = "n/a") 

# I am using read.csv here

# load food and brain data
fb_intake <- read_sav("data/Food Brain/intake_data.sav")
fb_eatbeh <- read_sav("data/Food Brain/qs_eatbeh_bodyimage.sav")
compiled_data_correct <- read.csv("data/Food Brain/compiled_correct_bisbas.csv") #Had to use this file as it has updated bis bas scores for food and brain
new <- compiled_data_correct[compiled_data_correct$study == "food_brain", ]
fb_bisbas <- data.frame(new[, c("participant_id", "bis", "bas", "bas_funseeking", "bas_drive", "bas_rewardresp")])
names(fb_bisbas)[names(fb_bisbas) == "participant_id"] <- "id"
fb_v1 <- read_sav("data/Food Brain/visit1_data.sav")

#### compile REACH ####

# subset intake data -- include V3 data only, select freddy and consumption cols (for individual items and overall)
reach_intake_v3 <- reach_intake[reach_intake$visit_protocol == 3, c("participant_id", "pre_meal_fullness", "pre_eah_fullness", grep("consumed", names(reach_intake), value = TRUE))]

# rename individual food items -- append vars with fb_[meal or eah]_
reach_meal_items <- c("grilled_cheese", "tender", "carrot", "chips", "fruit", "water", "ranch", "ketchup")
reach_eah_items <- c("brownie", "corn_chip", "kiss", "ice_cream", "oreo", "popcorn", "pretzel", "skittle", "starburst", "water_eah")
reach_meal_vars <- grep(paste0(do.call(c, list(paste0(reach_meal_items, "_kcal_consumed"), paste0(reach_meal_items, "_grams_consumed"))), collapse = "|"), names(reach_intake_v3), value = TRUE)
reach_eah_vars <- grep(paste0(do.call(c, list(paste0(reach_eah_items, "_kcal_consumed"), paste0(reach_eah_items, "_grams_consumed"))), collapse = "|"), names(reach_intake_v3), value = TRUE)
names(reach_intake_v3)[grep(paste(reach_meal_vars, collapse="|"), x = names(reach_intake_v3))] <- paste0("reach_meal_", reach_meal_vars)
names(reach_intake_v3)[grep(paste(reach_eah_vars, collapse="|"), x = names(reach_intake_v3))] <- paste0("reach_eah_", reach_eah_vars)


# merge data
compiled_reach <- merge(reach_participants[c("participant_id", "sex", "child_protocol_3_age", "risk_status_maternal")], reach_anthro[reach_anthro$session_id == "ses-1", c("participant_id", "child_bmi_z", "child_bmi", "child_bmi_p", "maternal_anthro_method", "parent1_sex")], by=c("participant_id"))
compiled_reach <- merge(compiled_reach, reach_demo[reach_demo$session_id == "ses-1", c("participant_id", "demo_income", "ethnicity", "race")], by=c("participant_id")) # add ses-1 income
compiled_reach <- merge(compiled_reach, reach_cebq[reach_cebq$session_id == "ses-1", c("participant_id", "cebq_sr", "cebq_fr", "cebq_ff", "cebq_avoid", "cebq_eue", "cebq_se")], by=c("participant_id")) # cebq SR from session 1 only
compiled_reach <- merge(compiled_reach, reach_bisbas[,c("participant_id","bis","bas","bas_funseeking","bas_drive","bas_rewardresp")], by=c("participant_id"))
compiled_reach <- merge(compiled_reach, reach_intake_v3, by=c("participant_id"))


# rename columns
names(compiled_reach)[names(compiled_reach) == "child_protocol_3_age"] <- "age_yr"
#or
colnames(compiled_reach)[colnames(compiled_reach) == "child_protocol_3_age"] <- "age_yr"
names(compiled_reach)[names(compiled_reach) == "eah_grams_consumed"] <- "eah_grams_consumed_foodonly"
names(compiled_reach)[names(compiled_reach) == "demo_income"] <- "income"

# remove maternal_anthro_method
compiled_reach <- subset(compiled_reach, select=-c(maternal_anthro_method))

# add study col
compiled_reach$study <- "reach"

# replace factor values with labels that match food and brain 
compiled_reach$ethnicity<-factor(compiled_reach$ethnicity,levels=0:1,labels = c("NOT Hispanic or Latino","Hispanic or Latino"))
compiled_reach$race<-factor(compiled_reach$race,levels=0:5,labels = c("American Indian/Alaskan Native","Asian", "Black or African American",  "White", "Hawaiian/Pacific Islander","Other"))
compiled_reach$sex <- recode(compiled_reach$sex, "male" = "Male", "female" = "Female")
compiled_reach$parent1_sex <- recode(compiled_reach$parent1_sex, "male" = "Male", "female" = "Female")

#### compile Food and Brain ####


# subset intake data
fb_intake_v1 <- fb_intake[c("id","sex","age_yr","measured_parent","bmi","bmi_percentile", "bmi_z","race", "ethnicity","income",
                            "risk_status_mom", "v1_freddy_pre_meal", "v1_eah_total_kcal","v1_meal_total_g","v1_meal_total_kcal","v1_freddy_pre_eah",
                            grep("^v1_consumed", names(fb_intake), value = TRUE)
)]

# rename individual food items -- append vars with fb_[meal or eah]
fb_eah_items <- c("brownies", "cornchips", "hersheys", "icecream", "oreos", "popcorn", "pretzels", "skittles", "starbursts", "water")
fb_meal_items <- c("applesauce", "carrot", "cheese_sndwch", "cookies", "ham_sndwch", "milk", "pbj_sndwch", "potatochip", "turkey_sndwch", "ketchup", "mayo", "mustard")
fb_eah_vars <- grep(paste(fb_eah_items, collapse="|"), names(fb_intake_v1), value = TRUE)
fb_meal_vars <- grep(paste(fb_meal_items, collapse="|"), names(fb_intake_v1), value = TRUE)
names(fb_intake_v1)[grep(paste(fb_meal_items, collapse="|"), x = names(fb_intake_v1))] <- gsub("v1_", "fb_meal_", fb_meal_vars)
names(fb_intake_v1)[grep(paste(fb_eah_items, collapse="|"), x = names(fb_intake_v1))] <- gsub("v1_", "fb_eah_", fb_eah_vars)


# make lists of eah item consumption variables
eah_foods_g_vars <- c("fb_eah_consumed_brownies_g", "fb_eah_consumed_cornchips_g", "fb_eah_consumed_hersheys_g", "fb_eah_consumed_icecream_g", "fb_eah_consumed_oreos_g", "fb_eah_consumed_popcorn_g", "fb_eah_consumed_pretzels_g", "fb_eah_consumed_skittles_g", "fb_eah_consumed_starbursts_g")
eah_items_g_vars <- c(eah_foods_g_vars, "fb_eah_consumed_water_g")

# compute eah_grams_consumed_foodonly
fb_intake_v1$eah_grams_consumed_foodonly <- fb_intake_v1 %>%
  dplyr::select(dplyr::all_of(eah_foods_g_vars)) %>%
  rowSums(na.rm = FALSE)

# compute v1_eah_grams_consumed_incwater
fb_intake_v1$eah_grams_consumed_inc_water <- fb_intake_v1 %>%
  dplyr::select(dplyr::all_of(eah_items_g_vars)) %>%
  rowSums(na.rm = FALSE)

# formatting and merge data
fb_bisbas <- fb_bisbas[order(as.numeric(fb_bisbas$id)), ]
all(fb_bisbas$id %in% fb_intake_v1$id)
all(fb_intake_v1$id %in% fb_bisbas$id)
fb_intake_v1$id <- trimws(as.character(fb_intake_v1$id))
fb_bisbas$id <- trimws(as.character(fb_bisbas$id))
compiled_fb <- merge(fb_intake_v1, fb_bisbas, by=c("id"))

# rename columns to match compiled_reach
names(compiled_fb)[names(compiled_fb) == "id"] <- "participant_id"
names(compiled_fb)[names(compiled_fb) == "bmi_percentile"] <- "child_bmi_p"
names(compiled_fb)[names(compiled_fb) == "bmi_z"] <- "child_bmi_z"
names(compiled_fb)[names(compiled_fb) == "bmi"] <- "child_bmi"
names(compiled_fb)[names(compiled_fb) == "risk_status_mom"] <- "risk_status_maternal"
names(compiled_fb)[names(compiled_fb) == "measured_parent"] <- "parent1_sex"
names(compiled_fb)[names(compiled_fb) == "v1_meal_total_g"] <- "meal_grams_consumed"
names(compiled_fb)[names(compiled_fb) == "v1_meal_total_kcal"] <- "meal_kcal_consumed"
names(compiled_fb)[names(compiled_fb) == "v1_eah_total_kcal"] <- "eah_kcal_consumed"
names(compiled_fb)[names(compiled_fb) == "v1_freddy_pre_eah"] <- "pre_eah_fullness"
names(compiled_fb)[names(compiled_fb) == "v1_freddy_pre_meal"] <- "pre_meal_fullness"

# convert columns
compiled_fb$participant_id <- as.character(compiled_fb$participant_id)
compiled_fb$sex<-factor(compiled_fb$sex,levels=0:1,labels = c("Male","Female"))
compiled_fb$risk_status_maternal<-factor(compiled_fb$risk_status_maternal,levels=0:1,labels = c("low-risk","high-risk"))
compiled_fb$parent1_sex<-factor(compiled_fb$parent1_sex,levels=0:1,labels = c("Female","Male"))
compiled_fb$ethnicity<-factor(compiled_fb$ethnicity,levels=0:1,labels = c("NOT Hispanic or Latino","Hispanic or Latino"))
compiled_fb$race<-factor(compiled_fb$race,levels=0:2,labels = c("White","American Indian/Alaskan Native","Asian"))

# add study col
compiled_fb$study <- "food_brain"

#Merge both reach and Food brain
compiled <- dplyr::bind_rows(compiled_reach, compiled_fb)


# make study first column 
compiled <-
  compiled %>%
  dplyr::relocate("study")

# move reach individual food items to end of dataframe
reach_item_vars <- grep("^reach", names(compiled), value = TRUE)
compiled <- compiled[, c(setdiff(names(compiled), reach_item_vars), reach_item_vars)]

#### Export data ####
write.csv(compiled,"data/data_compiled.csv", row.names = FALSE)

# export json
source("code/Cleaning_merging.R")
filename_json <- "data/data_compiled.json"
write(json_ssib(), filename_json)
