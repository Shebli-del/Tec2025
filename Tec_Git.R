
# Starting with packages --------------------------------------------------

library(readxl)
library(dplyr)
library(gtsummary)
library(AER)
library(survival)
library(lubridate)
library(ggplot2)
library(survminer)
library(tidyr)
library(broom)
library(patchwork)
library(prodlim)
#library(bstfun)
#devtools::install_github("MSKCC-Epi-Bio/bstfun")

# Load data ---------------------------------------------------------------

Tec <- read_excel("~/Desktop/AbdallahMeera/2025/Teclistamab/Teclistamab USMIRC Master.xlsx")
TecT <- read_excel("~/Desktop/AbdallahMeera/2025/Teclistamab/Teclistamab USMIRC Master.xlsx", 
                   sheet = "Toxicity ")
Tec %>%
  mutate(Dosing= 
           case_when( `Every_2_week_dosing?` == "Yes"~ "Biweekly",
                      Every_4_week_dosing_== "yes" ~ "Monthly",
                      `Every_2_week_dosing?` == "No"& Every_4_week_dosing_ =="No" ~ "Weekly"
           )
  ) -> Tec
TecT$Dosing <- Tec$Dosing
TecT$best_response <- Tec$best_response
TecWB <- subset(Tec, Tec$Race != "H" & Tec$Race != "Asian")
TecTWB <- subset(TecT, TecT$Race != "H" & Tec$Race != "Asian")
TecRes <- TecRes %>%
  mutate(Dosing_frequency = 
           ifelse (TecRes$Continuous_Fixed == "Fixed", "Fixed", 
                   ifelse(
                     `Every_2_week_dosing?` == "Yes","2_weeks",
                     ifelse(
                       `Every_4_week_dosing_` == "yes","4_weeks",
                       "weekly")))
  )

Tec <- subset(Tec, Tec$Center != "MUSC") #remove MUSC
TecT <- subset(TecT, TecT$Center != "MUSC")  #remove MUSC

Tec$Date_of_initial_diagnosis <- as.Date(Tec$Date_of_initial_diagnosis,
                                         origin = "1899-12-31")
as.Date(Tec$Date_of_initial_diagnosis, "%m/%d/%y")

Tec$PS <- as.factor(Tec$PS)


Tec$`#_of_LOT_(=<4:1,_>4:2)` <- as.factor(Tec$`#_of_LOT_(=<4:1,_>4:2)`)

TecT$Onset_of_CRS <- as.numeric(TecT$Onset_of_CRS)
TecT$Onset_of_ICANS <- as.numeric(TecT$Onset_of_ICANS)
TecT$`Duration_of_CRS_(days)` <- as.numeric(TecT$`Duration_of_CRS_(days)`)
TecT$`Duration_of_ICANS_(days)` <- as.numeric(TecT$`Duration_of_ICANS_(days)`)
TecT$`Onset_of_NT_(days)_` <- as.numeric(TecT$`Onset_of_NT_(days)_`)
TecT$`No_of_Toci_doses` <- as.numeric(TecT$`No_of_Toci_doses`)
TecT$day_of_infection_ <- as.numeric(TecT$day_of_infection_)

TecTRes$Dosing_frequency <- TecRes$Dosing_frequency
TecTRes$IgG_at_D90 <- as.numeric(TecTRes$IgG_at_D90)
TecTRes$Continuous_Fixed  <- TecRes$Continuous_Fixed
TecTRes$`Duration_of_CRS_(days)` <- as.numeric(TecTRes$`Duration_of_CRS_(days)`)
TecTRes$Dosing_frequency <- TecRes$Dosing_frequency
TecTRes$IgG_at_D90 <- as.numeric(TecTRes$IgG_at_D90)

# Variable names ----------------------------------------------------------

`PT_ID`,`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
`Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
`Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
`1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
`#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
`#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
`Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
`Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
`Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
`Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
`Commercial_vs_Trial`,`Name_of_BCMA_therapy`,`ADC_(Blenrep)`,
`CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
`Most_Recent_BCMA_agent`,`Date_of_first_exposure_to_prior_BCMA_agent_(C1D1)`,
`Date_of_last_exposure_to_prior_BCMA_agent`,
`Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
`___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
`___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
`No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
`Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
`Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
`non_secretory_MM`,`Type_of_EMD`,`Step_up_dosing_1,4,7_or_1,3,5?`,
`Duration_of_hospitalization_for_ramp_up_(days)`,
`ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
`Every_4_week_dosing_`,`ORR`,`Start_treatment`,`End_of_treatment`,
`best_response`,`time_to_1st_response_(days)`,`time_to_best_response_(months)`,
`Date_from_dx_till_Teclisitamab_(months)_`,`Progression_(or_death_if_first)`,
`date_of_progession`,`Date_of_D/C`,`Reason_for_D/C_`,
`Therapy_after_Tec_failure`,`Did_patients_passed?`,`Death_date`,
`Cause_of_expiration`,`TRM_(Y/N)`,`Last_office_visit_`,
`Duration_from_dx_till_LOV_or_expiration_(months)`,
`ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
`PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
`AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
`Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
`Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
`Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
`Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
`ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
`prior_BCMA_exposure`,`Reason_for_not_meeting_criteria`


# Variable Tox ------------------------------------------------------------

`PT_ID`,`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
`Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
`GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
`Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
`Symptoms_resolved_`,`ICANS`,`Description`,`Onset_of_ICANS`,
`Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
`Symptoms_resolved_1`,`Neurological_Abn`,`Onset_of_NT_(days)_`,
`Symptoms_Resolved_`,`Toci_treatment`,`No_of_Toci_doses`,
`_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
`Hospitalization_(subsequent_does)_`,`Cause_of_Hospitalization_`,
`Cause_of_Hospitalization_(abbreviated)`,`Neuropathy_`,`Dose_delayed`,
`Cause_of_dose_delayed`,`Reason_for_D/C__(ORR)`,`Infection`,`Details`,
`day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
`Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
`Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
`Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
`Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
`IgG_at_D30`,`IgG_at_D90`


# Demographics -------------------------------------------------------------


Tec %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,`ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n()


# Demographics for responders ---------------------------------------------



TecRes %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,`ORR`, `Continuous_Fixed`, `Dosing_frequency`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n()%>%
  as_hux_xlsx("Demographic of responders.xlsx")



# Demographics for response by dosing -------------------------------------


TecRes %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,Dosing_frequency,`Continuous_Fixed`, `ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    by = Dosing_frequency,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Dosing frequency**") |>
  modify_caption("**Table x. Patient Characteristics based on dosing frequency **") |>
  bold_labels() %>%
  bold_p() %>%
  as_hux_xlsx("Demographic of responders by dosing.xlsx")


# Demo Fixed vs. continuous  ----------------------------------------------

TecRes %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,Dosing_frequency, `Continuous_Fixed`, `ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    by = `Continuous_Fixed`,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Dosing frequency**") |>
  modify_caption("**Table x. Patient Characteristics based on dosing frequency **") |>
  bold_labels() %>%
  bold_p() %>%
  as_hux_xlsx("Demographic of responders by fixed vs. continuous treatment.xlsx")

# Reason for ineligibility ------------------------------------------------


Tec %>%
  select(`Did_patient_meet_MAJESTEC-1_criteria`, `ECOG_>_or_=_2?_(Yes,_No)`, `On_HD_(Yes/No)_`, 
         CNS_inovolvement, `PCL,_POEMS,_AL`, `Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         ASCT_within_12_weeks_) %>%
  tbl_summary(
    by = `Did_patient_meet_MAJESTEC-1_criteria`
  ) %>%
  modify_caption("**Table X. Reason for not meeting MAJESTEC-1_criteria eligibility**")%>%
  as_hux_xlsx("MAJESTEC-1_criteria.xlsx")

# Demo By prior BCMA exposure --------------------------------------------------

Tec %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,`ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    by = `prior_BCMA_exposure`,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**BCMA Received**") |>
  modify_caption("**Table 3. Patient Characteristics based on prior BCMA exposure**") |>
  bold_labels() %>% 
  as_hux_xlsx("By_BCMA_summary")

# Demo By Race -----------------------------------------------------------------

Tec %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,`ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    by = `Race`,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Race**") |>
  modify_caption("**Table 5. Patient Characteristics based on race **") |>
  bold_labels() %>%
  bold_p()


# Demo_WB -----------------------------------------------------------------

TecWB %>%
  select(`Center`,`Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
         `Type_of_myeloma_`,`Ig_Type`,`Kappa/Lambda`,`Date_of_initial_diagnosis`,
         `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
         `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
         `#_of_ASCT_prior_Teclistimab_`,`#_of_LOT_prior_Teclistimab_`,
         `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
         `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_exposure`,`PI_refractory`,
         `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
         `Pomalidomide_refractory`,`imid_exp`,`imid_refra`,`Daratumumab_exposure_`,
         `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
         `Commercial_vs_Trial`,`ADC_(Blenrep)`,
         `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
         `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
         `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
         `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
         `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
         `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
         `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
         `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
         `Duration_of_hospitalization_for_ramp_up_(days)`,
         `ICU_admission_during_ramp_up?`,`Every_2_week_dosing?`,
         `Every_4_week_dosing_`,`ORR`,
         `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
         `PCL,_POEMS,_AL`,`CNS_inovolvement`,`LVEF_<45%_(Yes,_No)`,
         `AST/ALT_>3.0_ULN_(Yes,_No)`,`Total_Bilirubin_>2.0_ULN_(Yes,_No)`,
         `Corrected_serum_calcium__>_13.5_mg/dL_(Yes,_No)`,`ANC_<1000/uL_(Yes,_No)`,
         `Hemoglobin_<_8_g/dL_(Yes,_No)`,`Platelets_<_75K_(Yes,_No)`,`Any_cytopenia`,
         `Uncontrolled_systemic_fungal,_bacterial,_viral_or_other_infection_(Yes,_No)`,
         `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
         `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
         `prior_BCMA_exposure`) %>%
  tbl_summary(
    by = `Race`,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list (), 
    type = list()
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Race**") |>
  modify_caption("**Table 7. Patient Characteristics based on race (AA vs. White) **") |>
  bold_labels() %>%
  bold_p()



# Tox_Summary -------------------------------------------------------------


TecT %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,`ICANS`,`Onset_of_ICANS`,
         `Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
         `Symptoms_resolved_`,`Onset_of_NT_(days)_`,
         `Symptoms_Resolved`,`Toci_treatment`,`No_of_Toci_doses`,
         `_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`,
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`) %>%
  tbl_summary(
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `Onset_of_CRS` ~ "{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~ "{mean} ({min}, {max})",
      `Duration_of_ICANS_(days)` ~"{mean} ({min}, {max})",
      `Onset_of_ICANS` ~"{mean} ({min}, {max})",
      `Onset_of_NT_(days)_` ~"{mean} ({min}, {max})",
      `No_of_Toci_doses` ~"{mean} ({min}, {max})",
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `Onset_of_CRS`~"continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Duration_of_ICANS_(days)` ~"continuous",
      `Onset_of_NT_(days)_` ~"continuous",
      `No_of_Toci_doses` ~"continuous",
      `day_of_infection_` ~"continuous"
    )
  ) %>%
  add_n()


# Tox_Dosing --------------------------------------------------------------

TecT %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,`ICANS`,`Onset_of_ICANS`,
         `Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
         `Symptoms_resolved_`,`Onset_of_NT_(days)_`,
         `Symptoms_Resolved`,`Toci_treatment`,`No_of_Toci_doses`,
         `_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`,
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`, Dosing) %>%
  tbl_summary(
    by= Dosing,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `Onset_of_CRS` ~ "{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~ "{mean} ({min}, {max})",
      `Duration_of_ICANS_(days)` ~"{mean} ({min}, {max})",
      `Onset_of_ICANS` ~"{mean} ({min}, {max})",
      `Onset_of_NT_(days)_` ~"{mean} ({min}, {max})",
      `No_of_Toci_doses` ~"{mean} ({min}, {max})",
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `Onset_of_CRS`~"continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Duration_of_ICANS_(days)` ~"continuous",
      `Onset_of_NT_(days)_` ~"continuous",
      `No_of_Toci_doses` ~"continuous",
      `day_of_infection_` ~"continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Teclistamab dosing**") |>
  modify_caption("**Table 4. Patient toxicity based on teclistamab dosing **") |>
  bold_labels() %>%
  bold_p()


# Toxicity by dosing frequency for responders --------------------------------------------



TecTRes %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`, 
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`, Dosing_frequency) %>%
  tbl_summary(
    by= Dosing_frequency,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `day_of_infection_` ~"continuous",
      `IgG_at_D90` ~ "continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_3") ~ 
                           "**Teclistamab dosing frequency**") |>
  modify_caption("**Table x. Patient toxicity based on teclistamab dosing frequency **") |>
  bold_labels() %>%
  bold_p()%>%
  as_hux_xlsx("Toxicity of teclistamab responders by frequency of dosing.xlsx")


# Toxicity for responders by Fixed dosing ---------------------------------

TecTRes$Onset_of_CRS <- as.numeric(TecTRes$Onset_of_CRS)

TecTRes %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`, 
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`, Continuous_Fixed) %>%
  tbl_summary(
    by= Continuous_Fixed,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `day_of_infection_` ~"{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~"{median} ({p25}, {p75})"
    ), 
    type = list(
      `day_of_infection_` ~"continuous",
      `IgG_at_D90` ~ "continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Onset_of_CRS` ~"continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ 
                           "**Teclistamab dosing frequency**") |>
  modify_caption(
    "**Table x. Toxicity of teclistamab responders by fixed vs. continuous dosing **") |>
  bold_labels() %>%
  bold_p()%>%
  as_hux_xlsx("Toxicity of teclistamab responders by fixed vs. continuous dosing.xlsx")

# Tox_BCMA ----------------------------------------------------------------


TecT %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,`ICANS`,`Onset_of_ICANS`,
         `Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
         `Symptoms_resolved_`,`Onset_of_NT_(days)_`,
         `Symptoms_Resolved`,`Toci_treatment`,`No_of_Toci_doses`,
         `_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`,
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`, prior_BCMA_exposure) %>%
  tbl_summary(
    by= prior_BCMA_exposure ,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `Onset_of_CRS` ~ "{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~ "{mean} ({min}, {max})",
      `Duration_of_ICANS_(days)` ~"{mean} ({min}, {max})",
      `Onset_of_ICANS` ~"{mean} ({min}, {max})",
      `Onset_of_NT_(days)_` ~"{mean} ({min}, {max})",
      `No_of_Toci_doses` ~"{mean} ({min}, {max})",
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `Onset_of_CRS`~"continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Duration_of_ICANS_(days)` ~"continuous",
      `Onset_of_NT_(days)_` ~"continuous",
      `No_of_Toci_doses` ~"continuous",
      `day_of_infection_` ~"continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Prior BCMA exposure**") |>
  modify_caption("**Table 4. Patient toxicity based on prior BCMA exposure **") |>
  bold_labels() %>%
  bold_p()




# Tox_Race ----------------------------------------------------------------


TecT %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,`ICANS`,`Onset_of_ICANS`,
         `Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
         `Symptoms_resolved_`,`Onset_of_NT_(days)_`,
         `Symptoms_Resolved`,`Toci_treatment`,`No_of_Toci_doses`,
         `_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`,
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`,Race) %>%
  tbl_summary(
    by= Race ,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `Onset_of_CRS` ~ "{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~ "{mean} ({min}, {max})",
      `Duration_of_ICANS_(days)` ~"{mean} ({min}, {max})",
      `Onset_of_ICANS` ~"{mean} ({min}, {max})",
      `Onset_of_NT_(days)_` ~"{mean} ({min}, {max})",
      `No_of_Toci_doses` ~"{mean} ({min}, {max})",
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `Onset_of_CRS`~"continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Duration_of_ICANS_(days)` ~"continuous",
      `Onset_of_NT_(days)_` ~"continuous",
      `No_of_Toci_doses` ~"continuous",
      `day_of_infection_` ~"continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Race**") |>
  modify_caption("**Table 6. Patient toxicity based on race **") |>
  bold_labels() %>%
  bold_p()


# Tox_WB ------------------------------------------------------------------

TecTWB %>%
  select(`Center`,`ICU_admission_during_ramp_up?`,`Hospitalization_first_dose`,
         `Start_treatment`,`Schedule_(1,3,5)_or_(1,4,7)`,`Liver_`,
         `GI_toxicity_`,`Neutropenic_fever`,`CRS_grade`,`Onset_of_CRS`,
         `Developed_CRS_after_2nd_step_up_dose1`,`Duration_of_CRS_(days)`,
         `Symptoms_resolved_1`,`ICANS`,`Onset_of_ICANS`,
         `Developed_CRS_after_2nd_step_up_dose`,`Duration_of_ICANS_(days)`,
         `Symptoms_resolved_`,`Onset_of_NT_(days)_`,
         `Symptoms_Resolved`,`Toci_treatment`,`No_of_Toci_doses`,
         `_Recurrent_CRS_after_Toci`,`Dexamethasone_treatment`,
         `Hospitalization_(subsequent_does)_`,
         `Neuropathy_`,`Dose_delayed`,
         `Infection`,
         `day_of_infection_`,`severe_infection_(yes,_no)`,`Use_of_GCSF`,
         `Use_of_TPO_agonists`,`Use_of_IVIG`,`PRBC_tranfusion`,
         `Platelet_transfusion`,`Leukopenia_Gr_at_D30`,`Anemia_Gr_at_D30`,
         `Neutropenia_Gr_at_D30`,`Thrombocytopenia_Gr_at_D30`,`Leukopenia_grade_at_D90`,
         `Anemia_grade_at_D90`,`Neutropenia_at_D90`,`Thrombocytopenia_at_D90`,
         `IgG_at_D30`,`IgG_at_D90`,Race) %>%
  tbl_summary(
    by= Race ,
    label =  list ( ),
    percent ="column",
    digits =list (),
    statistic = list ( 
      `Onset_of_CRS` ~ "{mean} ({min}, {max})",
      `Duration_of_CRS_(days)` ~ "{mean} ({min}, {max})",
      `Duration_of_ICANS_(days)` ~"{mean} ({min}, {max})",
      `Onset_of_ICANS` ~"{mean} ({min}, {max})",
      `Onset_of_NT_(days)_` ~"{mean} ({min}, {max})",
      `No_of_Toci_doses` ~"{mean} ({min}, {max})",
      `day_of_infection_` ~"{mean} ({min}, {max})"
    ), 
    type = list(
      `Onset_of_CRS`~"continuous",
      `Duration_of_CRS_(days)` ~"continuous",
      `Duration_of_ICANS_(days)` ~"continuous",
      `Onset_of_NT_(days)_` ~"continuous",
      `No_of_Toci_doses` ~"continuous",
      `day_of_infection_` ~"continuous"
    )
  ) %>%
  add_n() %>%
  add_p() %>%
  add_q %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Race**") |>
  modify_caption("**Table 8. Patient toxicity based on race (AA vs. White)**") |>
  bold_labels() %>%
  bold_p()



# PFS Univariate analysis -------------------------------------------------

my_theme <- theme_gtsummary_journal("jama")

tec_PFS <-
  Tec %>%
  select (PFS,Progressed, `Age`,`Gender`,`Race`,`PS`,`Stage_R-ISS`,
          `Ig_Type`,`Kappa/Lambda`,
          `Cytogenetics`,`Del_17p_(yeses,_no)`,`t(4;14)_(yes,_no)`,`t(14;16)_(yes,_no)`,
          `1q_gaino/Amp_(yes,_no,_specifyes)`,`Prior_ASCT__(yes/no)`,`prior_Allo?`,
          `#_of_LOT_prior_Teclistimab_`,
          `#_of_LOT_(=<4:1,_>4:2)`,`Velcade_exposure_`,`Velcade_refractory`,
          `Carfilzomib_exposure`,`Carfilzomib_refractory`,`PI_refractory`,
          `Lenoalidomide_exposure`,`Lenoalidomide_refractory`,`Pomalidomide_exposure`,
          `Pomalidomide_refractory`,
          `imid_refra`,`Daratumumab_exposure_`,
          `Daratumumab_refractory_`,`BCMA_exposure_`,`BCMA_refractory_`,
          `ADC_(Blenrep)`,
          `CART_ABECMA,_CILTA,_OTHER`,`T-cell_engagers_`,`>=2_BCMA_Rx`,
          `Best_response_to_most_recent_prior_BCMA_(CR,_VGPR,_PR,_SD,_PD)`,
          `___________________Exposure_to_BCMA_<3_months_prior_to_Tec`,
          `___________________Exposure_to_BCMA_<6_months_prior_to_Tec`,
          `No_of_LOT_b/w_most_recent_BCMA_and_Tec_0,_1,_2,_3,3+`,`Double_refractory`,
          `Triple_exposed`,`Triple_RRMM`,`Penta_exposed`,`Penta-refractory`,
          `Extramedullary_disease_`,`Plasma_cell_leukemia`,`Amyeloidosis`,
          `non_secretory_MM`,`Step_up_dosing_1,4,7_or_1,3,5?`,
          `Duration_of_hospitalization_for_ramp_up_(days)`,
          `ICU_admission_during_ramp_up?` , Dosing ,`ORR`,
          `ASCT_within_12_weeks_`,`Prior_Allo_SCT_=<6_mo_(Yes,_No)`,
          `PCL,_POEMS,_AL`,`CNS_inovolvement`,
          `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
          `ECOG_>_or_=_2?_(Yes,_No)`,`Did_patient_meet_MAJESTEC-1_criteria`,
          `prior_BCMA_exposure`
  ) %>%
  tbl_uvregression(
    method=coxph ,
    y=Surv(PFS ,  Progressed) ,
    exponentiate = TRUE
  ) %>%
  add_n(location="level")%>%
  add_global_p()%>%
  bold_labels() %>%
  bold_p() %>%
  add_q %>%
  bold_labels() %>%
  bold_p()

tec_PFS
Tec %>%
  select (`ICU_admission_during_ramp_up?` , Dosing 
  ) %>%
  tbl_summary(
    by= Dosing
  ) %>%
  add_p() %>%
  add_q

?tbl_2way_summary()

# PFS multivariate --------------------------------------------------------

tbl_Multi_PFS <-
  coxph( 
    Surv(PFS,Progressed)~ Dosing+ 
      `#_of_LOT_(=<4:1,_>4:2)`+ 
      `Extramedullary_disease_`, 
    data=Tec
  ) %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  add_n(location ="level") %>%
  add_global_p()%>%
  bold_labels() %>%
  bold_p() %>%
  add_q()


tbl_Multi_PFS 

as_forest_plot(tbl_Multi_PFS, col_names = c("stat_n", "estimate",  "p.value"))


tbl_merge(list(tec_PFS,tbl_Multi_PFS),
          tab_spanner = c("**PFS Univariate**","**PFS Multivariate**"))


# MVA by Prior BCMA -------------------------------------------------------

#Stratified univariate:
tbl_strata_ex1 <-
  Tec %>%
  select (PFS,Progressed, `Gender`,`Race`,`Stage_R-ISS`, Age,
          `Extramedullary_disease_`,
          `Renal_insuffieciy_CrCl_<_40_mL/min_(Yes,_No)`,`On_HD_(Yes/No)_`,
          `prior_BCMA_exposure`) %>%
  tbl_strata(
    strata = `prior_BCMA_exposure`,
    .tbl_fun =
      ~ .x %>%
      tbl_uvregression(
        method=coxph ,
        y=Surv(PFS, Progressed) ,
        exponentiate = TRUE
      ) %>%
      add_global_p() %>%
      add_n(location = "level") ,
    .header = "**{strata}**, N = {n}"
  ) %>%
  as_forest_plot()

tbl_strata_ex1




# Survival Function -------------------------------------------------------


ShebliSurv <- function(fitmodel, data,coxmodel, ylocation=0.85, xlocation=25, maintitle="", submaintitle="", xlimit=46, xlab="Months",
                       ylab1="probability of overall survival", legend1="No BCMA-targeted", legendsize=5,
                       tablab1="Median with BCMA") {
  par(bg = gray(.9),mar=c(2,2,1,2))
  margin(t = 2, r = 5, b = 1, l = 1, unit = "pt")
  p2 <- ggsurvplot(fitmodel, data = data, risk.table = TRUE, palette = "jco",
                   main = maintitle, surv.median.line = "hv", 
                   submain = submaintitle , break.x.by=5,
                   caption = "", xlim = c(0,xlimit), ylim = c(0, 1.01), xlab=xlab,fun=NULL,
                   axes.offset=FALSE,pval.method = F, conf.int=F, 
                   ylab= ylab1,
                   legend.title = "",
                   legend.labs = c(legend1))
  Xjust <- 0
  p2$plot<- p2$plot + ggplot2::annotate("text", 
                                        x = c(xlocation-Xjust,xlocation+Xjust+2,xlocation+Xjust+4,
                                              xlocation-Xjust,xlocation+Xjust+2,xlocation+Xjust+3, xlocation+Xjust+6,xlocation+Xjust+8),
                                        y = c(rep(ylocation,3),rep(ylocation-0.09,5)), # x and y coordinates of the text
                                        label = c(tablab1, round(surv_median(fitmodel)$median,2),"Months",
                                                  "95% CI (",
                                                  round(surv_median(fitmodel)$lower,2), ",",
                                                  round(surv_median(fitmodel)$upper,2),")"),
                                        size = legendsize)
  
  combined_plot <- p2$plot / p2$table + plot_layout(heights = c(10,0.8))
  combined_plot
}

ShebliSurv2 <- function(fitmodel, data,coxmodel, ylocation=0.85, xlocation=25, maintitle="", submaintitle="", xlimit=46, xlab="Months",
                        ylab1="probability of overall survival", legend1="No BCMA-targeted", legend2="BCMA-targeted", legendsize=5,
                        tablab1="Median with BCMA", tablab2="Median with out BCMA") {
  par(bg = gray(.9),mar=c(2,2,1,2))
  margin(t = 2, r = 5, b = 1, l = 1, unit = "pt")
  p2 <- ggsurvplot(fitmodel, data = data, risk.table = TRUE,pval=T,palette = "jco",
                   main = maintitle, surv.median.line = "hv", pval.coord=c(xlocation-5,0.6),
                   submain = submaintitle , break.x.by=5,
                   caption = "", xlim = c(0,xlimit), ylim = c(0, 1.01), xlab=xlab,fun=NULL,
                   axes.offset=FALSE,pval.method = F, conf.int=F, 
                   ylab= ylab1,
                   legend.title = "",
                   legend.labs = c(legend1,legend2))
  p2$plot<- p2$plot + ggplot2::annotate("text", 
                                        x = c(rep(c(xlocation,xlocation+8,xlocation+12,xlocation+14,xlocation+17,xlocation+18.5,xlocation+20,xlocation+22),2),
                                              xlocation-4,xlocation,xlocation+6,xlocation+10,xlocation+11.5,xlocation+13,xlocation+15), 
                                        y = c(rep(ylocation,8),rep(ylocation-0.09,8),rep(ylocation-0.18,7)), # x and y coordinates of the text
                                        label = c(tablab1, round(surv_median(fitmodel)$median[1],2),"Months",
                                                  "(",round(surv_median(fitmodel)$lower[1],2), ",",
                                                  round(surv_median(fitmodel)$upper[1],2),")",
                                                  tablab2, round(surv_median(fitmodel)$median[2],2),"Months",
                                                  "(",round(surv_median(fitmodel)$lower[2],2), ",",
                                                  round(surv_median(fitmodel)$upper[2],2),")",
                                                  "HR is",round(summary(coxmodel)$coefficients[2],2),"95% CI (",
                                                  round(exp(confint(coxmodel)[1]),2), ",",
                                                  round(exp(confint(coxmodel)[2]),2),")"),
                                        size = legendsize) + 
    theme(legend.text=element_text(size=legendsize+3))
  
  combined_plot <- p2$plot / p2$table + plot_layout(heights = c(3,0.8))
  
  combined_plot
}



# Survival calculations ---------------------------------------------------

ftb1<- survfit(Surv(Tec$PFS,Tec$Progressed)~1); ftb1
ftb2<- survfit(Surv(Tec$OS,Tec$Died)~1); ftb2
quantile(
  prodlim(
    Hist(OS,Died)~1,data=Tec,reverse=T
  )
) # do not use data$OSm just OSm
ftb3<- survfit(Surv(Tec$PFS,Tec$Progressed)~Tec$`#_of_LOT_(=<4:1,_>4:2)`); ftb3
PR.cox3 <- coxph(Surv(Tec$PFS,Tec$Progressed)~Tec$`#_of_LOT_(=<4:1,_>4:2)`);summary(PR.cox3)

ftb4<- survfit(Surv(Tec$PFS,Tec$Progressed)~Tec$BCMA_exposure_); ftb4
PR.cox4 <- coxph(Surv(Tec$PFS,Tec$Progressed)~Tec$BCMA_exposure_);summary(PR.cox4)

ftb5<- survfit(Surv(TecRes$PFS,TecRes$Progressed)~
                 TecRes$Continuous_Fixed); ftb5
PR.cox5 <- coxph(Surv(TecRes$PFS,TecRes$Progressed)~
                   TecRes$Continuous_Fixed);summary(PR.cox5)

ftb6<- survfit(Surv(TecRes$OS,TecRes$Died)~
                 TecRes$Continuous_Fixed); ftb6
PR.cox6 <- coxph(Surv(TecRes$OS,TecRes$Died)~
                   TecRes$Continuous_Fixed);summary(PR.cox6)


# Kaplan Meier plots ------------------------------------------------------

ShebliSurv(fitmodel= ftb1, data= Tec, coxmodel= PR.cox, ylocation=0.85,
           xlocation=10, maintitle="Hello", submaintitle="KM for progression free
           survival", xlimit=21,
           xlab="Months",
           ylab1="Probability of progression-free survival",
           legend1="All patients", legendsize=5,
           tablab1="Median")
ShebliSurv(fitmodel= ftb2, data= Tec, coxmodel= PR.cox, ylocation=0.85,
           xlocation=10, maintitle="Hello", submaintitle="KM for overall survival", xlimit=21,
           xlab="Months",
           ylab1="Probability of overall survival",
           legend1="All patients", legendsize=5,
           tablab1="Median    Not reached")
ShebliSurv2(fitmodel= ftb3, data= Tec, coxmodel= PR.cox3, ylocation=0.85,
            xlocation=23, maintitle="",
            submaintitle="KM for PFS by prior lines of treatment", xlimit=46,
            xlab="Months",
            ylab1="probability of progression free survival", legend1="4 or less",
            legend2="More than 4", legendsize=5,
            tablab1="4 or less", tablab2="More than 4")

ShebliSurv2(fitmodel= ftb4, data= Tec, coxmodel= PR.cox4, ylocation=0.85,
            xlocation=23, maintitle="",
            submaintitle="KM for PFS by prior BCMA targeted therapy exposure", xlimit=46,
            xlab="Months",
            ylab1="probability of progression free survival", legend1="No prior BCMA exposure",
            legend2="Prior BCMA exposure", legendsize=5,
            tablab1="No prior BCMA exposure", tablab2="Prior BCMA exposure")

ppll1= ShebliSurv2(fitmodel= ftb5, data= TecRes, coxmodel= PR.cox5, ylocation=0.85,
            xlocation=23, maintitle="",
            submaintitle="KM for PFS by continous vs. fixed treatment", xlimit=46,
            xlab="Months",
            ylab1="Probability of progression free survival", legend1="Continuous",
            legend2="Fixed", legendsize=5,
            tablab1="Continuous", tablab2="Fixed")

png(file = "file.png",   # The directory you want to save the file in
    width = 20000, # The width of the plot in inches
    height = 20000,
    res       = 2200,
    pointsize = 2) # The height of the plot in inches

# Step 2: Create the plot with R code
ppll1

# Step 3: Run dev.off() to create the file!
dev.off()

ppll2= ShebliSurv2(fitmodel= ftb6, data= TecRes, coxmodel= PR.cox6, ylocation=0.85,
                   xlocation=23, maintitle="",
                   submaintitle="KM for OS by continous vs. fixed treatment", xlimit=46,
                   xlab="Months",
                   ylab1="Probability of overall survival", legend1="Continuous",
                   legend2="Fixed", legendsize=5,
                   tablab1="Continuous", tablab2="Fixed")
ppll2

png(file = "fileOS.png",   # The directory you want to save the file in
    width = 20000, # The width of the plot in inches
    height = 20000,
    res       = 2200,
    pointsize = 2) # The height of the plot in inches

# Step 2: Create the plot with R code
ppll1

# Step 3: Run dev.off() to create the file!
dev.off()


# End ---------------------------------------------------------------------

tbl <- tbl_uvregression(
  data = mtcars,
  method = glm,
  method.args = list(family = binomial),
  y = am
) %>%
  as_forest_plot(col_names = c("stat_n", "estimate",  "p.value"))

# Extract results
results <- tbl %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
tbl$x$meta_data
