library(shiny)
library(shinythemes)
library(tidyverse)
library(expss)
library(gtsummary)
library(labelled)
library(DT)
library(shinydashboard)

# Read the datasets
###import all data sets
screening_form <- read_csv("screening_form.csv")
outcome <- read_csv("outcome.csv")
follow_qst <- read_csv("follow_qst.csv")
commodity_tracker <- read_csv("commodity_tracker.csv")
follow_abs <- read_csv("follow_abs.csv")
enrol_qst <- read_csv("enrol_qst.csv")
phq <- read_csv("phq.csv")
missed_visit <- read_csv("missed_visit.csv")
enrol_abs <- read_csv("enrol_abs.csv")
enrol_abs <- mutate(enrol_abs,age=enrol_dt-q02_dob)
enrol_abs <- mutate(enrol_abs,age=(enrol_dt-q02_dob)/365)
enrol_abs <- mutate(enrol_abs,
                    # Create categories
                    age_group = case_when(
                      age < 5  ~ "<5",
                      age >=5 & age <15 ~ "5-14",
                      age >=15 ~ ">=15"))

###screening form label
screening_form <- apply_labels(screening_form,
                               district = c("Leribe"=1, "Mohale's Hoek"=2, "Qacha's Nek"=3, "Thaba Tseka"=4), facility = c("Motebang HP"=10, "Maputsoe SDA HC"=11, "Maputsoe Filter Clinic"=12, "Emmanuel HC"=13, "Pontmain HC"=14, "St. Monica HC"=15, "Ntsekhe HP"=16, "Holy Cross HC"=17, "Mofumahali Oa Rosari HC"=18, "Ha Tsepo HC"=19, "Machabeng HP"=20, "Sacred Heart HC"=21, "Hermitage HC"=22,"Sehlaba-Thebe HC"=23, "Paray HP"=24, "Manamaneng HC"=25, "Semenanyane HC"=26, "St Theresa HC"=27, "Thaba Tseka Health Division HC"=28, "Katse HC"=29), 
                               q03_cohort = c("pre"=1, "post"=2), q04_screen_category = c("New Patient"=1, "Previously lost patient returning to care"=2, "Patient on ART with unsuppressed viral load"=3),
                               q05_who_stage = c("I"=1, "II"=2, "III"=3, "IV"=4),
                               q06_cd4_count = c("<200 cells/ml"=1, ">200 cells/ml"=0, "Not done"=2),
                               q07_emancipated_minor = c("Yes"=1, "No"=0),
                               q08_written_consent = c("Yes"=1, "No"=0),
                               q09_caregiver_consent = c("Yes"=1, "No"=0),
                               q10_verbal_assent = c("Yes"=1, "No"=0),
                               q11_screening_outcome = c("Eligible/enrolled (Abstraction)"=1, "Eligible/enrolled (Abrastration & interviews)"=2))

# Set labels from dictionary
var_description <- tibble::tribble(
  ~ name,                    ~ label,
  "q03_cohort",             "cohort" ,
  "q05_who_stage",          "WHO clinical stage",
  "q06_cd4_count",          "Cd4 count",
  "q04_screen_category",    "Patient category"
)

# create list of var labels
var_labels <- setNames(as.list(var_description$label), var_description$name)

# label the variables
screening_form <- screening_form %>% 
  set_variable_labels(.labels = var_labels, .strict = TRUE)

###Enrolment abstraction form labels

enrol_abs <- apply_labels(enrol_abs,
                          #district = c("Leribe"=1, "Mohale's Hoek"=2, "Qacha's Nek"=3, "Thaba Tseka"=4),
                          facility = c("Motebang HP"=10, "Maputsoe SDA HC"=11, "Maputsoe Filter Clinic"=12,   
                                       "Emmanuel HC"=13, "Pontmain HC"=14, "St. Monica HC"=15, "Ntsekhe HP"=16,
                                       "Holy Cross HC"=17, "Mofumahali Oa Rosari HC"=18, "Ha Tsepo HC"=19, 
                                       "Machabeng HP"=20, "Sacred Heart HC"=21, "Hermitage HC"=22, 
                                       "Sehlaba-Thebe HC"=23, "Paray HP"=24, "Manamaneng HC"=25, "Semenanyane 
                                HC"=26, "St Theresa HC"=27, "Thaba Tseka Health Division HC"=28, "Katse 
                                HC"=29),
                          
                          g01_gender = c("male"=1, "female"=2, "other"=77),
                          q04_hiv_dt_avail=c("Yes"=1, "No"=0),
                          q05_hiv_entry = c("Adolescent corner"=1, "General OPD"=2, "Men's Clinic"=3, "ART 
                                    clinic"=4, "MCH clinic"=5, "TB Clinic"=6, "Wards (inpatient)"=7, 
                                            "ANC/PNC clinics"=8, "Community outreach"=9),
                          q10_cd4_test = c("Yes"=1, "No"=0),
                          q11_cd4_dt_avail=c("Yes"=1, "No"=0),
                          q14_clinical_condition=c("Was receiving inpatient care"=1, "Recently (within 3 months)                                          discharged from hospital inpatient care"=2, "Stable, receiving                                          outpatient care (did not need admission)"=3, "Unknown"=99),
                          q15_on_art = c("Yes"=1, "No"=0),
                          q19_art_changes = c("Yes"=1, "No"=0),
                          q20_art_changes_reasons = c("Side effects"=1, "Treatment failure"=2, "Guideline     
                                              change"=3, "Drug stock out"=4, "Other"=77),
                          q21_tb_screened = c("Yes"=1, "No"=0, "unknown"=99),
                          q22_tbscreen_results = c("Presumptive TB"=1, "No TB"=0, "On TB Treatment"=2),
                          q23_tb_symptoms=c("Cough"=1, "Night Sweats"=2, "Fever"=3, "Weight Loss"=4,  
                                            "Other"=77),
                          q24_tb_investigations =c("Xpert"=1, "X-ray"=2, "Microscopy"=3, "Urine TB LAM"=4,   
                                                   "None"=0),
                          q25_tb_final =c("Confirmed TB"=1, "No TB"=0),
                          q26_type_tb =c("Pulmonary"=1, "Extrapulmonary"=2, "Not Specified"=3),
                          #q27_initiated_tbtx=c("Yes"=1, "No"=0, "unknown"=99),
                          q29_tpt=c("Yes"=1, "No"=0, "unknown"=99),
                          q31_tpt_regimen=c("6H"=1, "3HP"=2, "3RH"=3, "INH"=4),
                          q32_tpt_complete=c("Yes"=1, "No"=0, "On TPT at time of clinic visit"=2),
                          q34_cotri=c("Yes"=1, "No"=0, "unknown"=99),
                          #q36_scrag_done=c("Yes"=1, "N/A > 200 cells/mm"=2, "No"=0, "Unknown"=99),
                          #q37_scrag_result=c("positive"=1, "negative"=0),
                          #q38_csf_done=c("Yes"=1, "No"=0),
                          #q39_csf_result=c("positive"=1, "negative"=0),
                          q40_flucanazole1=c("Yes"=1, "No"=0),
                          q41_crypt_meningitis=c("Yes"=1, "No"=0, "unknown"=99),
                          q43_crypt_meningitis_medic=c("Amphotericin B"=1, "Flucytosine"=2, "Fluconazole"=3,  
                                                       "Other "=77),
                          q44_flucanazole2=c("Yes"=1, "No"=0, "Unknown"=2, "N/A"=99),
                          q46_mental_disorder =c("Yes"=1, "No"=0, "Unknown"=99),
                          q47_disorder_type=c("Anxiety Disorder"=1, "Schizophrenia"=2, "Depression"=3, "Substance    
                                Abuse"=4, "Bipolar Disorder"=5, "Other" =77),
                          q48_disorder_treated=c("Yes"=1, "No"=0),
                          #lab_xperttb=c("positive"=1, "negative"=0, "not done"=9),
                          lab_sputum=c("positive"=1, "negative"=0, "not done"=9),
                          lab_lam=c("positive"=1, "negative"=0, "not done"=9),
                          lab_serum=c("positive"=1, "negative"=0, "not done"=9),
                          #lab_csf=c("positive"=1, "negative"=0, "not done"=9),
                          lab_ccs=c("positive"=1, "negative"=0, "not done"=9),
                          lab_preg=c("positive"=1, "negative"=0, "not done"=9),
                          oth_tests=c("None"=0, "Hemoglobin"=1, "CD4"=2, "Viral Load"=3, "Serum Creatinine"=4, "Random Blood Glucose"=5)
)

# Set labels from dictionary
var_description1 <- tibble::tribble(
  ~ name,                    ~ label,
  "g01_gender",             "sex"         
)

# create list of var labels
var_labels1 <- setNames(as.list(var_description1$label), var_description1$name)

# label the variables in enrol_abs
enrol_abs <- enrol_abs %>% 
  set_variable_labels(.labels = var_labels1, .strict = TRUE)

phq <- apply_labels(phq,
                    period = c("Enrolment"=1,"Month-3"=2,"Month-6"=3),
                    q01_interest_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q02_depressed_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q03_asleep_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q04_tired_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q05_appetite_name = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q06_bad_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q07_concentrate_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q08_moving_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3),
                    q09_thoughts_value = c("Never"=0, "Several days"=1, "More than half the days"=2, "Nearly every day"=3)
)

# Set labels from dictionary
var_description2 <- tibble::tribble(
  ~ name,                    ~ label,
  "period",             "follow up period"         
)

# create list of var labels
var_labels2 <- setNames(as.list(var_description2$label), var_description2$name)

# label the variables in phq
phq <- phq %>% 
  set_variable_labels(.labels = var_labels2, .strict = TRUE)

#####Outcome form labels

outcome <- outcome %>% 
  mutate(q02_outcome_6months=replace(q02_outcome_6months,q02_outcome_6months==4,5))

outcome <- apply_labels(outcome,
                        q02_outcome_6months = c("Participant Alive and In Care"=1, "Participant Transferred Out"=2, "Participant has died"=3, "Lost follow up"=4, "Lost follow up(LTFU)"=5, "Stopped treatment"=6, "Other"=77),
                        q02_death_report = c("Spouse"=1, "Parent/Primary Caregiver"=2, "Community Health Worker"=3, "clinical health worker"=4, "Neighbor/friend"=5, "Not documented"=99, "Other"=77),
                        q03_death_cause =c("Tuberculosis"=1, "Neuromeningeal cryptococcosis"=2, "Other infectious and parasitic diseases"=3, "other hiv illness causing death"=4, "Accidental causes"=5, "Cancer"=6, "Other natural causes not HIV-related)"=7, "Violence"=8, "Diarrhea"=9, "Malnutrition"=10, "Anemia"=11, "Kidney disease"=12, "Stroke"=13, "Other natural causes (not HIV-related)"=14, "Diabetes"=15, "Side effects of medications"=16, "Malaria"=17, "other"=77, "unknown"=98, "Not documented"=99),
                        q04_death_location=c("health centre"=1, "Home"=2, "From a traditional healer"=3, "In transit for care"=4, "unknown"=99, "other"=77),
                        q05_medical_death =c("no"=0,"yes"=1,"unknown"=99),
                        q06_tx_where =c("This installation (study site)"=1, "Other health establishment"=2, "Traditional healer"=3,"The patient did not seek treatment"=4,"other"=77,"unknown"=99),
                        termination_raison=c("The parent or guardian has withdrawn consent"=1,"Responsible person deceased"=2, "other"=77)
)


# Set labels from dictionary
var_description3 <- tibble::tribble(
  ~ name,                    ~ label,
  "q02_outcome_6months",   "six months outcome"         
)

# create list of var labels
var_labels3 <- setNames(as.list(var_description3$label), var_description3$name)

# label the variables in outcome
outcome <- outcome %>% 
  set_variable_labels(.labels = var_labels3, .strict = TRUE)

####Enrolment questionnaire labels
enrol_qst <- apply_labels(enrol_qst,
                          district = c("Leribe"=1, "Mohale's Hoek"=2, "Qacha's Nek"=3, "Thaba Tseka"=4),
                          facility = c("Motebang HP"=10, "Maputsoe SDA HC"=11, "Maputsoe Filter Clinic"=12,   
                                       "Emmanuel HC"=13, "Pontmain HC"=14, "St. Monica HC"=15, "Ntsekhe HP"=16,
                                       "Holy Cross HC"=17, "Mofumahali Oa Rosari HC"=18, "Ha Tsepo HC"=19, 
                                       "Machabeng HP"=20, "Sacred Heart HC"=21, "Hermitage HC"=22, 
                                       "Sehlaba-Thebe HC"=23, "Paray HP"=24, "Manamaneng HC"=25, "Semenanyane 
                                HC"=26, "St Theresa HC"=27, "Thaba Tseka Health Division HC"=28, "Katse 
                                HC"=29),
                          q01_education= c("Primary"=1,"Secondary"=2,"Tertiary"=3,"None"=0),
                          q02_employed= c("Yes"=1,"No"=0,"n/a"=99),
                          q03_occupation= c("Factory worker"=1, "Mine worker"=2,"Driver"=3,"Farmer"=4, "Soldier/ Policeman"=5, "Professional"=6, "Casual laborer"=7, "Sales/ service worker"=8, "Unemployed"=9),
                          q04_livingwith= c("Yes"=1,"No"=0,"n/a"=99),
                          q06_pregnant=c("Yes"=1,"No"=0),
                          q07_hiv_known=c("Yes"=1,"No"=0),
                          q08_ART=c("Yes"=1,"No"=0),
                          q09_ART_why=c("I did not want people to know that I have HIV"=1, "I did not have any symptoms and did not think I needed treatment"=2, "I feared the medicine would make me sick"=3, "I did not believe the results"=4, "I did not trust the healthcare workers"=5, "I dislike taking medicine"=6,"Other reason"=77),
                          q11_missed_arv= c("Yes"=1,"No"=0,"n/a"=99),
                          q14_meds_prob=c("I forgot"=1,"I did not have any symptoms and did not think I needed treatment"=2,"The medicine makes me sick"=3,"I do not believe I have HIV"=4,"I had problems with the healthcare workers who were treating me"=5, "I felt weak, couldn't return to the facility on time"=6, "I got tired of taking medicine"=7, "I travelled and did not carry the medicine"=8, "I did not want to be seen taking medicine"=9,"Ran out of drugs"=10,"Other reason"=99),
                          q15_meds_major_prob=c("I fear other people will get to know that I have HIV"=1,"I don't have any symptoms and I don't think I need treatment"=2, "The medicine makes me sick"=3,"I don't believe that I have HIV"=4,"The healthcare workers are rude and criticize me"=5, "I get tired of taking medicine everyday"=6,"I may move to a new place far from my clinic"=7, "Difficulty getting to the health facility for medication refills"=8,"I have no problem"=9,"Other reason (specify)"=77),
                          q16_counseling=c("Yes"=1,"No"=0,"n/a"=99),
                          q17_meds_by_day=c("Yes"=1,"No"=0,"dont know"=99),
                          q18_meds_notaken=c("I will become ill and die"=1,"Nothing bad will happen to me"=2, "I don't know"=3,"Other,specify"=77), 
                          q19_meds_reminder=c("Yes"=1,"No"=0),
                          q20_meds_reminder_who=c("Spouse/partner"=1,"Father or Mother"=2,"Sibling"=3,"Other relatives/friends"=4),
                          q21_partner_know=c("Yes"=1,"No"=0,"dont know"=88,"n/a"=99),
                          q23_alcohol=c("Yes"=1,"No"=0),
                          q24_alcohol_often=c("Never had alcohol"=0,"At least once a day"=1, "At least once a week"=2, "Less than once a week"=3),
                          q25_peer=c("Yes"=1,"No"=0)
)


# Set labels from dictionary
var_description4 <- tibble::tribble(
  ~ name,                    ~ label,
  "q01_education",   "Education level"         
)

# create list of var labels
var_labels4 <- setNames(as.list(var_description4$label), var_description4$name)

# label the variables in outcome
enrol_qst <- enrol_qst %>% 
  set_variable_labels(.labels = var_labels4, .strict = TRUE)

##obtain expected dates of return at month 3 and month 6
enrol_qst <- mutate(enrol_qst,month3=enrol_dt+90)
enrol_qst <- mutate(enrol_qst,month6=enrol_dt+180)

pre <- filter(screening_form,q03_cohort == 1) %>% set_variable_labels(.labels = var_labels, .strict = TRUE)
post <- filter(screening_form,q03_cohort == 2) %>% set_variable_labels(.labels = var_labels, .strict = TRUE)

diagnosed <- filter(screening_form,q11_screening_outcome == 1 | q11_screening_outcome == 2 )
diagnosed_pre <- filter(diagnosed,q03_cohort==1) %>% set_variable_labels(.labels = var_labels, .strict = TRUE)
diagnosed_post <- filter(diagnosed,q03_cohort==2) %>% set_variable_labels(.labels = var_labels, .strict = TRUE)

# Summaries for the Shiny app
##flow chart information for screening and enrollment updates
#Total number eligible for AHD screening 
# Summaries for the Shiny app
##flow chart information for screening and enrollment updates
#Total number eligible for AHD screening 
Total_AHD_screening <- select(screening_form, q04_screen_category) %>%
  tbl_summary(missing = "no")
Total_AHD_screeningN <- Total_AHD_screening$N

#Eligible for AHD screening(pre and post)
Eligible_AHD_pre_post <- select(screening_form, q03_cohort) %>%
  tbl_summary(missing = "no")
Eligible_AHD_pre_postN <- Eligible_AHD_pre_post$N

Pre_Eligible_AHD_pre_postN <- Eligible_AHD_pre_post$table_body$n[1]
Post_Eligible_AHD_pre_postN <- Eligible_AHD_pre_post$table_body$n[2]


#Eligible for AHD screening
Eligible_AHD <- select(screening_form, q03_cohort, q04_screen_category) %>%
  tbl_summary(q03_cohort, missing = "no") %>%
  add_overall()
Eligible_AHDN <- Eligible_AHD$N
Pre_Eligible_AHDn <- Eligible_AHD$df_by$n[1]
Post_Eligible_AHDn <- Eligible_AHD$df_by$n[2]

#Total number diagnosed with AHD
Total_diag_AHD <- select(diagnosed, q03_cohort) %>%
  tbl_summary(missing = "no")
Total_diag_AHDN <- Total_diag_AHD$N

#Diagnosed with AHD in pre and post
Diag_AHD_pre_post <- select(diagnosed, q03_cohort, q04_screen_category) %>%
  tbl_summary(q03_cohort, missing = "no") %>%
  add_overall()

Pre_Diag_AHD_pre_postn <- Diag_AHD_pre_post$df_by$n[1]
Post_Diag_AHD_pre_postn <- Diag_AHD_pre_post$df_by$n[2]

#Not eligible for prospective cohort
Not_elig_pros_coh <- sum(enrol_abs$enrol_dt < '2023-01-10',na.rm=TRUE)
Not_elig_pros_coh <- data.frame(Not_elig_pros_coh)
#text_expand()

#Eligible for consent
Elig_consent <- sum(enrol_abs$enrol_dt > '2023-09-30',na.rm=TRUE)
Elig_consent <- data.frame(Elig_consent)
#text_expand()

#Prospective clients
Prospective_clients <- sum(enrol_qst$study_id !="",na.rm=TRUE)
Prospective_clients <- data.frame(Prospective_clients)
#text_expand()

#Total entries
Total_entries <- dim(enrol_qst)
#text_expand()
Total_entries <- data.frame(Total_entries[1])

###Data abstraction status 
#Number of AHD Clients screened and diagnosed
Diag <- select(diagnosed,q03_cohort) %>%
  tbl_summary(missing = "no")

#Number enrolled in pre cohort
Sum_Enl_Pre <- sum(enrol_abs$enrol_dt < '2023-01-01',na.rm=TRUE) 

#number enrolled in post cohort
Sum_Enl_Post <-sum(enrol_abs$enrol_dt > '2023-01-01',na.rm=TRUE) 

####Followup in the prospective cohort
#Eligible for consent / prospective cohort
Eligible_for_consent <- sum(enrol_abs$enrol_dt > '2023-09-30',na.rm=TRUE)

#Interviewed and enrolled in prospective cohort
Interviewed_Enr <- sum(enrol_qst$study_id !="",na.rm=TRUE)

#Expected at month three
Expected_at_3 <- sum(enrol_qst$month3 < now(),na.rm=TRUE)

#Interviewed at month three
Interviewed_at_3 <- sum(follow_qst$visit_type ==1,na.rm=TRUE)

#Expected at month six
Expected_at_6 <- sum(enrol_qst$month6 < now(),na.rm=TRUE)

#Interviewed at month six
Interviewed_at_6 <- sum(follow_qst$visit_type ==2,na.rm=TRUE)

##Distribution by CD4 and WHO

#Distribution by CD4 and WHO stage at screening(PRE)
Distribution_CD4_PRE <- select(pre,q05_who_stage,q06_cd4_count) %>%
  tbl_summary(q06_cd4_count,
              missing = "no")

#Distribution by CD4 and WHO stage at screening(POST)
Distribution_CD4_Post <-select(post,q05_who_stage,q06_cd4_count) %>%
  tbl_summary(q06_cd4_count,
              missing = "no")

#Matrix function will be used in the main function

create_matrix <- function(vector) {
  # Calculate the number of rows based on the vector length
  rows <- length(vector) / 2
  
  # Check if the length of vector is even
  if (length(vector) %% 2 != 0) {
    stop("Length of vector must be even to create a 2-column matrix")
  }
  
  # Create the matrix by row-wise filling from the vector
  matrix_from_vector <- matrix(vector, nrow = rows, byrow = TRUE)
  
  # Convert the matrix to a data frame
  matrix_as_dataframe <- as.data.frame(matrix_from_vector)
  
  return(matrix_as_dataframe)
}

#Function with tbl_summary
create_side_by_side_bar_plot <- function(x) {
  # Load required packages
  library(ggplot2)
  library(reshape2)
  
  # Convert tbl_summary object to data frame if needed
  if (inherits(x, "tbl_summary")) {
    x <- as.data.frame(x$table_body)
  }
  
  # Extract labels and values
  Label <- x$label[-1]
  Value <- x$stat_0[-1]
  
  # Extract numeric values
  Extract_Value <- as.numeric(gsub(",", "", unlist(regmatches(Value, gregexpr("\\d+(?:,\\d+)*", Value, perl=TRUE)))))
  
  # Separate Values from percentage and arrange them in a matrix form
  # First convert the results into vectors
  Extract_vector <- c(Extract_Value)
  
  # Create the matrix by specifying the elements for each row
  #Extract_matrix <- matrix(c(Extract_vector[1:2], Extract_vector[3:4], Extract_vector[5:6]), nrow=3, byrow=TRUE)
  #Extract_matrix <- data.frame(Extract_matrix)
  
  Extract_vector <- create_matrix (Extract_vector)
  
  # Name the matrix
  names(Extract_matrix) <- c("Value", "Percent")
  
  # Combine Label with Values
  Final_Data <- data.frame(label = Label, Extract_matrix)
  
  # Reshape the data for side-by-side bar plot
  data_melt <- melt(Final_Data, id.vars = "label")
  
  # Create the side-by-side bar plot
  ggplot(data_melt, aes(x = label, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Side-by-Side Bar Plot", x = "Label", y = "Value", fill = "Metric") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example of how to call the function (assuming x is defined)

# create_side_by_side_bar_plot(x)
#Shiny dashboard
library(shiny)
library(shinydashboard)
library(gtsummary)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "LESOTHO AHD"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .sidebar-menu li a {
          background-color: white !important;
          color: #0073B7 !important;
        }
        .sidebar-menu li a:hover {
          background-color: #005A9C !important;
          color: white !important;
        }
        .sidebar-menu li.active a, .sidebar-menu li.active a:hover {
          background-color: #0073B7 !important;
          color: white !important;
        }
        .guideline-box {
          font-size: 20px;
        }
        .box-title {
          font-size: 20px;
        }
        table {
          font-size: 18px;
        }
      "))
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Summary", tabName = "summary", icon = icon("")),
      menuItem("Screening and Enrollment Updates", tabName = "screening_enrollment", icon = icon("table")),
      menuItem("Data Abstraction Status", tabName = "data_abstraction", icon = icon("database")),
      menuItem("Followup in Prospective Cohort", tabName = "followup_cohort", icon = icon("users")),
      menuItem("Distribution by CD4 and WHO", tabName = "cd4_who_distribution", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                infoBox("Total AHD Screening ", Total_AHD_screeningN, icon = icon(""), color = "green"),
                infoBox("Eligible for AHD Screening pre & post", Eligible_AHD_pre_postN, icon = icon(""), color = "green"),
                infoBox("Eligible for AHD Screening", Eligible_AHDN, icon = icon(""), color = "green"),
                infoBox("Pre Eligible for AHD Screening", Pre_Eligible_AHDn, icon = icon(""), color = "blue"),
                infoBox("Post Eligible for AHD Screening", Post_Eligible_AHDn, icon = icon(""), color = "blue"),
                infoBox("Total Diagnosed with AHD", Total_diag_AHDN, icon = icon(""), color = "blue"),
                infoBox("Pre Diagnosed with AHD", Pre_Diag_AHD_pre_postn, icon = icon(""), color = "red"),
                infoBox("Post Diagnosed with AHD", Post_Diag_AHD_pre_postn, icon = icon(""), color = "red"),
                infoBox("Not eligible for prospective", Not_elig_pros_coh, icon = icon(""), color = "red"),
                infoBox("Eligible for prospective", Elig_consent, icon = icon(""), color = "yellow"),
                infoBox("Prospective_clients", Prospective_clients, icon = icon(""), color = "yellow"),
                infoBox("Total entries", Total_entries, icon = icon(""), color = "yellow")
              )
      ),
      tabItem(tabName = "screening_enrollment",
              fluidRow(
                box(title = "Total number eligible for AHD screening", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    splitLayout(
                      cellWidths = c("50%", "50%"),
                      tableOutput("table1"),
                      plotOutput("plot1")
                    )
                )
              ),
              fluidRow(
                box(title = "Eligible for AHD Screening (pre and post)", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    splitLayout(
                      cellWidths = c("50%", "50%"),
                      tableOutput("table2"),
                      plotOutput("plot2")
                    )
                )
              ),
              fluidRow(
                box(title = "Eligible for AHD Screening and Total number diagnosed with AHD", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    splitLayout(
                      cellWidths = c("50%", "50%"),
                      tableOutput("table3"),
                      tableOutput("table4")
                    )
                )
              ),
              fluidRow(
                box(title = "Diagnosed with AHD in pre and post", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    splitLayout(
                      cellWidths = c("50%", "50%"),
                      tableOutput("table5"),
                      plotOutput("plot3")
                    )
                )
              )  
              ),
      tabItem(tabName = "data_abstraction",
              fluidRow(
                box(title = "Number of AHD Clients screened and diagnosed", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table10")),
                box(title = "Number enrolled in pre cohort", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table11")),
                box(title = "Number enrolled in post cohort", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table12"))
              )
      ),
      tabItem(tabName = "followup_cohort",
              fluidRow(
                box(title = "Eligible for consent / prospective cohort", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table13")),
                box(title = "Interviewed and enrolled in prospective cohort", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table14")),
                box(title = "Expected at month three", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table15")),
                box(title = "Interviewed at month three", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table16")),
                box(title = "Expected at month six", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table17")),
                box(title = "Interviewed at month six", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table18"))
              )
      ),
      tabItem(tabName = "cd4_who_distribution",
              fluidRow(
                box(title = "Distribution by CD4 and WHO stage at screening (PRE)", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table19")),
                box(title = "Distribution by CD4 and WHO stage at screening (POST)", status = "primary", solidHeader = TRUE, width = 12, class = "box-title",
                    tableOutput("table20"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Function to clean and convert gtsummary tables to data frames
  clean_table <- function(tbl) {
    tbl %>%
      as_tibble() %>%
      mutate(across(everything(), ~gsub("\\**", "", .))) %>%
      replace(is.na(.), "")
  }
  
  # Render tables for Screening and Enrollment Updates
  output$table1 <- renderTable({
    Total_AHD_screening
  })
  
  output$plot1 <- renderPlot({
    create_side_by_side_bar_plot(Total_AHD_screening)
  })
  
  output$table2 <- renderTable({
  Eligible_AHD_pre_pos
  })
  
  output$plot2 <- renderPlot({
  create_side_by_side_bar_plot(Eligible_AHD_pre_pos)
  })
  
  output$table3 <- renderTable({
    Eligible_AHD
  })
  
  output$table4 <- renderTable({
    sample_data
  })
  
  output$table5 <- renderTable({
    sample_data
  })
  
  output$plot3 <- renderPlot({
    ggplot(sample_data, aes(x = Category)) +
      geom_point(aes(y = Pre), color = "blue", size = 4) +
      geom_point(aes(y = Post), color = "red", size = 4) +
      geom_line(aes(y = Pre), color = "blue") +
      geom_line(aes(y = Post), color = "red") +
      labs(title = "Diagnosed with AHD in pre and post", y = "Number Diagnosed")
  })
}

  output$table6 <- renderTable({
    Not_elig_pros_coh
  })
  
  output$table7 <- renderTable({
    Elig_consent
  })
  
  output$table8 <- renderTable({
    Prospective_clients
  })
  
  output$table9 <- renderTable({
    Total_entries
  })
  
  # Render tables for Data abstraction status
  output$table10 <- renderTable({
    clean_table(Diag)
  })
  
  output$table11 <- renderTable({
    Sum_Enl_Pre
  })
  
  output$table12 <- renderTable({
    Sum_Enl_Post
  })
  
  # Followup in the prospective cohort
  output$table13 <- renderTable({
    Eligible_for_consent
  })
  
  output$table14 <- renderTable({
    Interviewed_Enr
  })
  
  output$table15 <- renderTable({
    Expected_at_3
  })
  
  output$table16 <- renderTable({
    Interviewed_at_3
  })
  
  output$table17 <- renderTable({
    Expected_at_6
  })
  
  output$table18 <- renderTable({
    Interviewed_at_6
  })
  
  # Distribution by CD4 and WHO
  output$table19 <- renderTable({
    clean_table(Distribution_CD4_PRE)
  }) 
  
  output$table20 <- renderTable({
    clean_table(Distribution_CD4_Post)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
