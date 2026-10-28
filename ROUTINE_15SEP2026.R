###########################################################
# ROUTINE STUDY - SEP 2026 ANALYSIS
###########################################################


###########################################################
# Load libraries
###########################################################

library(dplyr)
library(tidyr)
library(REDCapR)



###########################################################
# Load data from R
###########################################################

redcap_uri <- "https://redcap.cheori.org/api/"
token <- "024128A9598BF81FD61AE63459BFDE93"


result <- redcap_read(
  redcap_uri = redcap_uri,
  token = token
)

data <- result$data




###########################################################
# Remove test records
###########################################################

z <- data

z <- z %>%
  mutate(siteid_extracted = gsub("([0-9]+)-([0-9]+)","\\1",record_id))

## remove invalid record IDs
  z <- z %>% filter(!record_id=="1")   #test record from Sainte-Justine for 4131-009, correct 4131-009 already in dataset
  z <- z %>% filter(!record_id=="2")   #blank record
  z <- z %>% filter(!record_id=="3")   #test record from BC for 5421-001 per ROUTINE study tracker
  z <- z %>% filter(!record_id=="4")   #test record from BC for 5421-006, correct 5421-006 already in dataset
  z <- z %>% filter(!record_id=="5")   #test record from BC for 5421-007, correct 5421-007 already in dataset
  z <- z %>% filter(!record_id=="6")   #test record from BC for 5421-007, correct 5421-007 already in dataset
  z <- z %>% filter(!record_id=="7")   #test record from BC for 5421-007, correct 5421-007 already in dataset
  
  any(duplicated(z$record_id))

## fix site IDs
  z <- z %>%
    mutate(siteid = substr(siteid, 1, 4))

  z$site_id <- NULL




###########################################################
# Identify eligible patients 
###########################################################

z <- z %>%
  
  mutate(
    # Meets inclusion 1
    incl_1_age = case_when(
      incl_1_age==1 ~ "Yes",
      is.na(incl_1_age) ~ NA,
      TRUE ~ "No"),
   
    # Meets inclusion 2
      incl_2_comp = case_when(
      incl_2_a==1 ~ "Yes",
      incl_2_b==1 ~ "Yes",
      is.na(incl_2_a) & is.na(incl_2_b) ~ NA,
      TRUE ~ "No"),
    
    # Meets inclusion 3
      incl_3_ddx = case_when(
      incl_3_ddx==1 ~ "Yes",
      is.na(incl_3_ddx) ~ NA,
      TRUE ~ "No"),
    
    # Meets inclusion 4
      incl_4_fever = case_when(
      incl_4_fever==1 ~ "Yes",
      is.na(incl_4_fever) ~ NA,
      TRUE ~ "No"),
    
    # Meets inclusion 5
      incl_5_comp = case_when(
      incl_5a==1 ~ "Yes",
      incl_5b==1 ~ "Yes",
      is.na(incl_5a) & is.na(incl_5b) ~ NA,
      TRUE ~ "No"),
    
    # Meets inclusion 6
      incl_6_comp = case_when(
      incl_6a==1 ~ "Yes",
      incl_6b==1 ~ "Yes",
      incl_6c==1 ~ "Yes",
      incl_6d==1 ~ "Yes",
      incl_6e==1 ~ "Yes",
      is.na(incl_6a) & is.na(incl_6b) & is.na(incl_6c) & is.na(incl_6d) & is.na(incl_6e) ~ NA,
      TRUE ~ "0"),
    
    passed_incl = case_when(
      incl_1_age=="Yes" & incl_2_comp=="Yes" & incl_3_ddx=='Yes' & incl_4_fever=="Yes" & incl_5_comp=="Yes" & incl_6_comp=="Yes" ~ "Yes",
      TRUE ~ "No"),
    
    failed_incl = case_when(
      incl_1_age=="No"|incl_2_comp=="No"|incl_3_ddx=="No"|incl_4_fever=="No"|incl_5_comp=="No" |incl_6_comp=="No" ~ "Yes",
      incl_1_age=="Yes" & incl_2_comp=="Yes" & incl_3_ddx=="Yes" & incl_4_fever=="Yes" & incl_5_comp=="Yes" ~ "No",
      TRUE ~ "Test"),
    
    # Meets exclusion 1
    excl_1 = case_when(
      excl_1==1 ~ "Yes",
      is.na(excl_1) ~ NA,
      TRUE ~ "No"),
    
    # Meets exclusion 2
    excl_2 = case_when(
      excl_2==1 ~ "Yes",
      is.na(excl_2) ~ NA,
      TRUE ~ "No"),
    
    # Meets exclusion 3
    excl_3 = case_when(
      excl_3==1 ~ "Yes",
      is.na(excl_3) ~ NA,
      TRUE ~ "No"),
    
    # Meets exclusion 4
    excl_4 = case_when(
      excl_4==1 ~ "Yes",
      is.na(excl_4) ~ NA,
      TRUE ~ "No"),
    
    eligible = case_when(
      incl_1_age=="Yes" & incl_2_comp=="Yes" & incl_3_ddx=="Yes" & incl_4_fever=="Yes" & incl_5_comp=="Yes" & incl_6_comp=="Yes" &
      excl_1=="No" & excl_2=="No" & excl_3=="No" & excl_4=="No" ~ "Yes",
      TRUE ~ "No")
    
    )

notEligible <- z %>% filter(eligible == "No")
Eligible <- z %>% filter(eligible == "Yes")




###########################################################
# Identify enrolled patients 
###########################################################

  # To be included in the analysis data, participants must:
  
  # 1. Be eligible
  # 2. Consented
  # 3. Enrolled (where all "reason_missed" variables = "Unchecked")

w <- Eligible %>% 
  
  mutate(
    
  # Patient missed reason 1 (patient missed)
    reason_missed___1 = case_when(
    reason_missed___1==1 ~ "Yes",
    TRUE ~ "No"),
    
  # Patient missed reason 2 (parents declined consent)
    reason_missed___2 = case_when(
    reason_missed___2==1 ~ "Yes",
    TRUE ~ "No"),
  
  # Patient missed reason 3 (clinician denied patient for participation)
    reason_missed___3 = case_when(
    reason_missed___3==1 ~ "Yes",
    TRUE ~ "No"),

  # Patient missed reason 4 (parents unable to complete questionnaires)
    reason_missed___4 = case_when(
    reason_missed___4==1 ~ "Yes",
    TRUE ~ "No"),

  # Patient missed reason 5 (other)
    reason_missed___5 = case_when(
    reason_missed___5==1 ~ "Yes",
    TRUE ~ "No"),
  
  # Consented
    consent = case_when(
    consent==1 ~ "Yes",
    is.na(consent) ~ NA,
    TRUE ~ "No"),
  
  # Enrolled
    enrolled = case_when(
    reason_missed___1=="No" & reason_missed___2=="No" & reason_missed___3=="No" & reason_missed___4=="No" & reason_missed___5=="No"
    & consent =="Yes" ~ "Yes",
    TRUE ~ "No")
  
  )
  
Enrolled <- w %>% filter(enrolled == "Yes")




###########################################################
# CONSORT Flow Diagram
###########################################################









###########################################################
# Table 1 - Baseline Characteristics
###########################################################

## combine EN and FR variables

x <- Enrolled %>% 
  
  mutate(
    
    age = ifelse(
      is.na(age_days), fr_age, age_days),
    
    age_cat = case_when(
      age < 60  ~ "birth to <2 months",
      age >=60 & age < 183 ~ "2 months to <6 months",
      age >= 183  ~ "6 months to <2 years",
      TRUE ~ NA),
    
    sex = ifelse(
      is.na(birthsex), fr_sex, birthsex),
    
    preterm = ifelse(
      is.na(preterm_yesno), fr_gestage, preterm_yesno),
    
    race_eth = ifelse(
      is.na(ethnicity), fr_q2_3, ethnicity),
    
    inc = ifelse(
      is.na(income), fr_q2_6, income),
  
    education = ifelse(
      is.na(parent_school), fr_q2_4, parent_school),
    
    weight = ifelse(
      is.na(weight), fr_weight, weight),
    
    comorbids_any = case_when(
      rowSums(across(
        c(
          comorbids___1, comorbids___2, comorbids___3, comorbids___4,
          comorbids___5, comorbids___6, comorbids___7, comorbids___8,
          comorbids___9, comorbids___10, comorbids___11,
          fr_comorbid___1, fr_comorbid___2, fr_comorbid___3, fr_comorbid___4,
          fr_comorbid___5, fr_comorbid___6, fr_comorbid___7, fr_comorbid___8,
          fr_comorbid___9, fr_comorbid___10, fr_comorbid___11
        ),
        ~ as.numeric(as.character(.))
      ), na.rm = TRUE) > 0 ~ 1,
      comorbids___12 == 1 | fr_comorbid___12 == 1 ~ 0,
      TRUE ~ 0
    ),
    
    fr_q2_10 = case_when(
      fr_q2_10==1 ~ 1,
      fr_q2_10==2 ~ 0,
      fr_q2_10==3 ~ 2, 
      TRUE ~ fr_q2_10),
    
    circumcision = ifelse(
      is.na(circumsized), fr_q2_10, circumsized),
  
    kidney_hx = ifelse(
      is.na(hx_kidney), fr_q2_13, hx_kidney),

    vcreflux_hx = ifelse(
      is.na(hx_vcreflux), fr_q2_14, hx_vcreflux),
    
    fr_q2_16 = case_when(
      fr_q2_16==3 ~ 2, TRUE ~ fr_q2_16),
    
    prenatal_us_completed = ifelse(
      is.na(prenatal_us), fr_q2_16, prenatal_us),
    
    fr_q2_19 = case_when(
      fr_q2_19==3 ~ 2, TRUE ~ fr_q2_19),

    prenatal_abnormality = ifelse(
      is.na(prenatal_us_result), fr_q2_19, prenatal_us_result)
    
  )

## completeness check

  # test <- x %>% filter(is.na(age))
  # test <- x %>% filter(is.na(sex))
  # test <- x %>% filter(is.na(preterm))
  # test <- x %>% filter(is.na(race_eth))
  # test <- x %>% filter(is.na(inc))
  # test <- x %>% filter(is.na(education))
  # test <- x %>% filter(is.na(weight))
  # test <- x %>% filter(is.na(comorbids_any))
  # test <- x %>% filter(is.na(circumcision))
  # test <- x %>% filter(is.na(kidney_hx))
  # test <- x %>% filter(is.na(vcreflux_hx))
  # test <- x %>% filter(is.na(prenatal_us_completed))
  # test <- x %>% filter(is.na(prenatal_abnormality))


  ### circumcision male/female inconsistencies
  # test <- x %>% filter(sex == 2 & circumcision == 1) ## sex = female, circumcision = yes
  # test <- x %>% filter(sex == 1 & circumcision == 2) ## sex = male, circumcision = N/A female



###########################################################
# Table 2 - UTI Diagnosis and Microbiology Results
###########################################################

### method of urine collection
  test <- x %>% filter(is.na(q4_1_urine)) # missing method
  test <- x %>% filter(q4_1_urine == 4) # selected "Not Available"


### pyuria

  ## based on screening form
    x <- x %>% 
      mutate(
        pyuria_positive_screening = case_when(
          incl_6a==1 ~ 1,
          incl_6b==1 ~ 1,
          incl_6c==1 ~ 1,
          is.na(incl_6a) & is.na(incl_6b) & is.na(incl_6c) & is.na(incl_6d) & is.na(incl_6e) ~ NA,
          TRUE ~ 0))

    table(x$pyuria_positive_screening)

  ## based on urinalysis leukocytes
    test <- x %>% filter(is.na(q4_a_leukocyte))
    test <- x %>% filter(q4_a_leukocyte == 6) # 6 = Not available
    test <- x %>% filter(q4_a_leukocyte == 1) # 1 = Negative
    
    x <- x %>% 
      mutate(
        pyuria_positive_leukocytes = case_when(
          q4_a_leukocyte == 2 ~ 1,
          q4_a_leukocyte == 3 ~ 1,
          q4_a_leukocyte == 4 ~ 1,
          q4_a_leukocyte == 5 ~ 1,
          q4_a_leukocyte == 7 ~ 1,
          q4_a_leukocyte == 1 ~ 0,
          TRUE ~ NA))
    
    table(x$pyuria_positive_leukocytes)
    
  ## overall pyuria - screening form and urinalysis leukocytes
    x <- x %>% 
      mutate(
        pyuria = case_when(
          pyuria_positive_leukocytes == 1  ~ 1,
          pyuria_positive_screening == 1 ~ 1,
          (pyuria_positive_leukocytes == 0 & pyuria_positive_screening == 0) ~ 0,
          (is.na(pyuria_positive_leukocytes) & pyuria_positive_screening == 0) ~ 0,
          TRUE ~ NA))

    table(x$pyuria)
    test <- x %>% filter(is.na(x$pyuria))
    test <- x %>% filter(pyuria == 0)
    


### nitrites
    
    ## based on screening form
    x <- x %>% 
      mutate(
        nitrites_positive_screening = case_when(
          incl_6e==1 ~ 1,
          is.na(incl_6a) & is.na(incl_6b) & is.na(incl_6c) & is.na(incl_6d) & is.na(incl_6e) ~ NA,
          TRUE ~ 0))
    
    table(x$nitrites_positive_screening)

    ## based on urinalysis nitrites
    table(x$q4_b_nitrites)
    test <- x %>% filter(is.na(x$q4_b_nitrites))

    x <- x %>% 
      mutate(
        nitrites_positive = case_when(
          q4_b_nitrites == 1 ~ 0,
          q4_b_nitrites == 2 ~ 1,
          TRUE ~ NA))
    
    ## overall nitrites
    x <- x %>% 
      mutate(
        nitrites = case_when(
          nitrites_positive_screening == 1  ~ 1,
          nitrites_positive == 1 ~ 1,
          (nitrites_positive == 0 & nitrites_positive_screening == 0) ~ 0,
          (is.na(nitrites_positive) & nitrites_positive_screening == 0) ~ 0,
          TRUE ~ NA))
    
    table(x$nitrites)
    test <- x %>% filter(is.na(x$nitrites))
    


### bacteriuria
    x <- x %>% 
      mutate(
        bacteriuria_positive_screening = case_when(
          incl_6d==1 ~ 1,
          is.na(incl_6a) & is.na(incl_6b) & is.na(incl_6c) & is.na(incl_6d) & is.na(incl_6e) ~ NA,
          TRUE ~ 0)
      )
    
    table(x$bacteriuria_positive_screening)
    

    
### WBC count
  table(x$q5_1_yesno)
  test <- x %>% filter(q5_1_yesno == 1 & is.na(q5_1_firstwbc)) ## WBC count exists = yes, missing first WBC count
  
  summary(x$q5_1_firstwbc)

  
  ### check WBC date plausibility
  test <- x %>% filter(q5_1_yesno == 1 & is.na(q5_1_date)) ## WBC count exists = yes, missing date
  test <- x %>% filter(is.na(q4_1_date))
  
  x$urinalysis_to_WBC <- x$q4_1_date - x$q5_1_date ## FR version doesn't have admission date, using urinalysis date instead
  summary(x$urinalysis_to_WBC)
  
  test <- x %>% filter(urinalysis_to_WBC > 30 | urinalysis_to_WBC < -30)

  
### neutrophil count
  table(x$q5_2_neutro_unavail___1)
  sum(is.na(x$q5_2_neutrophil))
  
  test <- x %>% filter(q5_1_yesno == 1 & q5_2_neutro_unavail___1 == 0 & is.na(q5_2_neutrophil)) 
  
  summary(x$q5_2_neutrophil)
  
  
### band count
  table(x$q5_2_band_unavail___1)
  sum(is.na(x$q5_2_bandcount))
  
  test <- x %>% filter(q5_1_yesno == 1 & q5_2_band_unavail___1 == 1 & is.na(q5_2_bandcount)) 
  
  summary(x$q5_2_bandcount)


### CRP
table(x$q5_3_yesno)
test <- x %>% filter(q5_3_yesno == 1 & is.na(q5_3_crp)) ## CRP result exists but missing
  
  table(x$crp_unit)
  
  x <- x %>% 
    mutate(
      crp_mg_l = case_when(
        crp_unit == 2 ~ q5_3_crp,
        crp_unit == 1 ~ q5_3_crp, ## implement conversion from nmol/L to mg/L
        TRUE ~ NA))
  
  summary(x$q5_3_crp)
  
  ### check CRP date plausibility
  test <- x %>% filter(q5_3_yesno == 1 & is.na(q5_3_date)) ## CRP result exists, missing date

  x$urinalysis_to_CRP <- x$q4_1_date - x$q5_3_date ## FR version doesn't have admission date, using urinalysis date instead
  summary(x$urinalysis_to_CRP)
  
  test <- x %>% filter(urinalysis_to_CRP > 30 | urinalysis_to_CRP < -30)
  
  

  
### creatinine
table(x$q5_4_yesno)
test <- x %>% filter(q5_4_yesno == 1 & is.na(q5_4_creatinine)) ## Cr result exists but missing
test <- x %>% filter(q5_4_yesno == 1 & is.na(q5_4_creat_unit)) ## Cr result exists but unit missing


  ## convert to same units
  table(x$q5_4_creat_unit)
  
    x <- x %>% 
      mutate(
        cr_umol_l = case_when(
          q5_4_creat_unit == 1 ~ q5_4_creatinine,
          q5_4_creat_unit == 2 ~ q5_4_creatinine * 88.4,
        TRUE ~ NA))
    
  
  summary(x$cr_umol_l)
  test <- x %>% filter(cr_umol_l > 30 | cr_umol_l < -30)
  

### urea

  
  
### urine CFU
test <- x %>% filter(is.na(q4_2_cfu))
test <- x %>% filter(is.na(q4_2_unit))
test <- x %>% filter(is.na(q4_2_growth))

table(x$q4_2_unit_test)
table(x$q4_2_growth)
table(x$q4_2_cfu)


x <- x %>% 
  mutate(q4_2_growth_test = case_when(
    q4_2_growth == 1 ~ "= equal to",
    q4_2_growth == 2 ~ "> greater than",
    q4_2_growth == 3 ~ "≥ greater than or equal to",
    q4_2_growth == 4 ~ "Not available",
    TRUE ~ NA))


x <- x %>% 
  mutate(q4_2_unit_test = case_when(
    q4_2_unit == 1 ~ "mL",
    q4_2_unit == 2 ~ "L",
    TRUE ~ NA))


  x <- x %>% 
    mutate(cfu = ifelse(q4_2_unit_test == "L",q4_2_cfu /1000,q4_2_cfu),
           new_unit = ifelse(!is.na(cfu),"mL",NA_character_)) %>% 
    mutate(
      cfu_cat = case_when(
        cfu < 10000 ~ "< 10,000 CFU/mL",
        cfu == 10000 & q4_2_growth_test %in% c("= equal to","> greater than","≥ greater than or equal to") ~ "10,000-49,999 CFU/mL",
        20000<=cfu & cfu<= 40000 & q4_2_growth_test == "= equal to" ~ "10,000-49,999 CFU/mL",
        50000<=cfu ~ "≥ 50,000 CFU/mL",
        TRUE ~ NA_character_
      ),
      cfu_cat = factor(cfu_cat, levels=c(
        "< 10,000 CFU/mL","10,000-49,999 CFU/mL","≥ 50,000 CFU/mL")))

    table(x$cfu_cat)
    test <- x %>% filter(is.na(cfu_cat))
    
  

### causative bacteria
    table(x$q4_3_organism)
    test <- x %>% filter(is.na(q4_3_organism))
    test <- x %>% filter(q4_3_organism == 14)
    
    
    table(x$q4_3_organism_2)
    test <- x %>% filter(is.na(q4_3_organism_2))
    
   
    #########################################
    ### recode "Other" bacteria
        # test <- x %>% filter(q4_3_organism == 13)
        # print(test$q4_3_other)
        # 
        # test <- x %>% filter(q4_3_organism_2 == 13)
        # print(test$q4_3_other_2)
        # 
        # x <- x %>% 
        #   mutate(q4_3_organism = case_when(
        #     q4_3_other == "Klebsiella Oxytoca" ~ 3,
        #     q4_3_other == "klebsiella oxytoca" ~ 3,
        #     q4_3_other == "Enterobacter hormaechei" ~ 4,
        #     q4_3_other == "Enterococcus faecalis (Enlfaeca)" ~ 5,
        #     q4_3_other == "Citrobacter koseri" ~ 1,
        #     q4_3_other == "Citrobacter koseri" ~ 1,
        #     q4_3_other == "Methicillin-Resistant Staphylococcus aureus" ~ 10,
        #     q4_3_other == "ESBL E. Coli" ~ 2,
        #     q4_3_other == "ESBL Escherichia coli" ~ 2,
        #     TRUE ~ q4_3_organism))
        # 
        # x <- x %>% 
        #   mutate(q4_3_organism_2 = case_when(
        #     q4_3_other_2 == "Klebsielle oxytoca" ~ 3,
        #     q4_3_other_2 == "Klebsiella pneumoniae ssp pneumoniae" ~ 3,
        #     q4_3_other_2 == "citrobacter Koseri" ~ 1,
        #     q4_3_other_2 == "Streptococcus mitis group" ~ 13,
        #     q4_3_other_2 == "Streptococcus dysgalactiae (Group C&G)" ~ 13,
        #     q4_3_other_2 == "Raoultella species (formerly Klebsiella species) Abnormal" ~ 13,
        #     TRUE ~ q4_3_organism_2
        #   ))
      ########################################
    
    for (i in 1:14) {
      x[[paste0("bacteria_", i)]] <- as.integer(
        coalesce(x$q4_3_organism == i, FALSE) |
          coalesce(x$q4_3_organism_2 == i, FALSE))
    }

    bacteria_vars <- c(
      "bacteria_1", "bacteria_2", "bacteria_3", "bacteria_4", "bacteria_5", "bacteria_6", "bacteria_7",
      "bacteria_8", "bacteria_9", "bacteria_10", "bacteria_11", "bacteria_12", "bacteria_13", "bacteria_14")
    
    bacteria_table <- x %>%
      summarise(across(all_of(bacteria_vars), ~sum(. == 1, na.rm = TRUE))) %>%
      pivot_longer(everything(),
                   names_to = "Bacteria",
                   values_to = "N") %>%
      mutate(
        Percent = round(100 * N / 452, 1), #NTS: update 452 if total number of participants changes
        `N (%)` = sprintf("%d (%.1f%%)", N, Percent))
    
    bacteria_table
    `
    
  
# Blood culture positivity
table(x$blood_culture)
    
    ## check plausibility of blood culture date
    test <- x %>% filter(blood_culture == 1 & is.na(blood_culture_date)) ## blood culture done, missing date
    
    x$urinalysis_to_bloodculture <- x$q4_1_date - x$blood_culture_date ## FR version doesn't have admission date, using urinalysis date instead
    summary(x$urinalysis_to_bloodculture)
    
    test <- x %>% filter(urinalysis_to_bloodculture > 30 | urinalysis_to_bloodculture < -30)
    summary(test$urinalysis_to_bloodculture)
    
  
  table(x$blood_culture_result)
 
    
    
# Blood culture organism
  table(x$culture_organism)
  table(x$culture_organism_2)

  for (i in 1:14) {
    x[[paste0("culture_bacteria_", i)]] <- as.integer(
      coalesce(x$culture_organism == i, FALSE) |
        coalesce(x$culture_organism_2 == i, FALSE))
  }
  
  culture_bacteria_vars <- c(
    "culture_bacteria_1", "culture_bacteria_2", "culture_bacteria_3", "culture_bacteria_4", "culture_bacteria_5", "culture_bacteria_6", "culture_bacteria_7",
    "culture_bacteria_8", "culture_bacteria_9", "culture_bacteria_10", "culture_bacteria_11", "culture_bacteria_12", "culture_bacteria_13", "culture_bacteria_14")
  
  culture_bacteria_table <- x %>%
    summarise(across(all_of(culture_bacteria_vars), ~sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(),
                 names_to = "Bacteria",
                 values_to = "N") %>%
    mutate(
      Percent = round(100 * N / 77, 1), #NTS: update 77 if total number of participants with positive blood cultures changes
      "N (%)" = sprintf("%d (%.1f%%)", N, Percent))
  
  culture_bacteria_table
  
  
# CSF specimen obtained
  table(x$csf_sample)

  
# CSF WBC count	
  test <- x %>% filter(csf_sample == 1 & csf_wbc_unavail___1 == 0 & csf_wbc_quantity != 3)
  test <- x %>% filter(csf_sample == 1 & csf_wbc_unavail___1 == 0)
  
  test <- x %>% filter(csf_sample == 1 & csf_wbc_quantity == 3)
  test <- x %>% filter(csf_sample == 1 & csf_wbc_unavail___1 == 1 & csf_wbc_quantity != 3)


  test <- x %>% filter(is.na(csf_wbc))
  test <- test %>% filter(csf_sample == 1)
  test <- test %>% filter(csf_wbc_unavail___1 == 0)

  table(test$csf_wbc_quantity)  
  
  
  test <- x %>% filter(csf_wbc>=0)
  table(test$csf_wbc_quantity)
  summary(test$csf_wbc)
  hist(test$csf_wbc)
  
  test <- test %>% filter(csf_wbc > 100) ## flagging outliers in case error or different units

  
  
# CSF culture positive
  table(x$csf_culture)
  
  
# antibiotic susceptibility
  
  

  

###########################################################
# Table 3 - UTI Treatment
###########################################################
  
## length of hospital stay--- NTS: come back to this once definition final
  table(x$q9_1_yesno) ## does ED triage time exist
  test <- x %>% filter(is.na(q9_1_date))
  
  table(x$q9_2_yesno) ## was patient admitted to hospital
  table(x$q9_3_yesno) ## does discharge info exist
  
  
  table(x$q9_0_admit_date) 
  table(x$q9_0_other_disc_date) 
  
  
## antibiotic treatment
  
  ## any abx given?
  table(x$q6_1_abx_given)
  test <- x %>% filter(is.na(q6_1_abx_given))
  table(x$q6_0_1)
  table(x$q6_5)
  table(x$q6_8)
  
  test <- x %>% filter(is.na(q6_1_firstantib))
  test <- x %>% filter(is.na(q6_2_lastantib))
  
  
  ## abx duration
  
    ## in-hospital abx duration
    x <- x %>% 
      mutate(hosp_abx_duration = q6_2_lastantib - q6_1_firstantib)
    summary(x$hosp_abx_duration)
    
    ## at home abx duration
    x <- x %>% 
      mutate(home_abx_duration = q6_2_1 - q6_1_1)
    summary(x$home_abx_duration)
    
    test <- x %>% filter(home_abx_duration >0 & is.na(hosp_abx_duration))
    
    ## overall abx duration
    x <- x %>% 
      mutate(total_abx_duration = case_when(
        hosp_abx_duration>=0 ~ hosp_abx_duration,
        home_abx_duration>=0 ~ home_abx_duration,
        TRUE ~ NA))
    summary(x$total_abx_duration)
    

  test <- x %>% filter(is.na(total_abx_duration))
  test <- x %>% filter(total_abx_duration > 30)
  
  
  
  table(x$q6_8)
  
  table(x$q6_8_days)
  summary(x$q6_8_days)
  
  
  table(x$q6_9)
  test <- x %>% filter(is.na(q6_9))
  
  
  
  

  
###########################################################
# Table 4 - US Classification
###########################################################
  
  ## ultrasound completion
    ## date 
    ## check plausibility
    ## reports pasted
  
    ## study team classifications
  
  
  ###########################################################
  ## Create final classification variables per adjudication
  ###########################################################
  
  x <- x %>%
    mutate(
      q1_0_final = case_when(
        
        ## errors in both initial reports, corrections made on "Final" form
        final_adjudication == 1 ~ q1_0_final,
        
        ## if ""Final" form completed, use that by default
        !is.na(q1_0_final) ~ q1_0_final,
        
        ## both initial reports agreed, use VF
        final_adjudication == 2 & is.na(q1_0_final) ~ q1_0_vf,
        
        ## flagged to use VF, use VF
        final_adjudication == 3 & is.na(q1_0_final) ~ q1_0_vf,
        
        ## flagged to use GL, use SM
        final_adjudication == 4 & is.na(q1_0_final) ~ q1_0_sm,
        
        TRUE ~ NA

      )
    )
      
  
  x <- x %>%
    mutate(
      q1_0_final = case_when(
        q1_0_final==1 ~ "Normal",
        q1_0_final==2 ~ "A",
        q1_0_final==3 ~ "B",
        q1_0_final==4 ~ "C",
        TRUE ~ NA)
    )
  
  table(x$q1_0_final)
  
  combined <- x %>%
    select(record_id, q1_0_vf, q1_0_sm, q1_0_final, final_adjudication) %>%
    left_join(
      dr %>% select(record_id, q1_0_final),
      by = "record_id"
    )

  test <- combined %>% filter(q1_0_final.x != q1_0_final.y)
  
  table(x$q1_0_final, dr$q1_0_final)
  
  
  
  
  test <- combined %>% filter(q1_0_final.x != q1_0_final.y)
  
  
  ###########################################################
  ## Create Class A, B, C Abnormality Variable
  ###########################################################
  
  table(x$q1_0_final)
  test <- x %>% filter(is.na(q1_0_final))

  test <- test %>%
    select(
      record_id,
      q1_0_vf,
      q1_0_sm,
      q1_0_final,
      q7_3,
      q11_0)  ## all missing classifications are US that were not completed or done elsewhere and unavailable
  
 
  ## create flag for records with unavailable ultrasound
  x <- x %>% 
    mutate(
      us_unavail = case_when(
        is.na(q1_0_fin) ~ 1,
        TRUE ~ 0
      )
    )

  
  ###########################################################################

  
  x <- x %>%
    mutate(
      
      right_csd_fin = case_when(
        q1_0_r_csd_n_fin___1== 1 ~ "Normal",
        q1_0_r_csd_a_fin___1 == 1 ~ "A",
        q1_0_r_csd_a_fin___2 == 1 ~ "A",
        q1_0_r_csd_a_fin___3 == 1 ~ "A",
        q1_0_r_csd_a_fin___4 == 1 ~ "A",
        q1_0_r_csd_b_fin___1 == 1 ~ "B",
        q1_0_r_csd_b_fin___2 == 1 ~ "B",
        q1_0_r_csd_b_fin___3 == 1 ~ "B",
        q1_0_r_csd_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      right_ureter_fin = case_when(
        q1_0_r_ureter_n_fin___1 == 1 ~ "Normal",
        q1_0_r_ureter_a_fin___1 == 1 ~ "A",
        q1_0_r_ureter_b_fin___1 == 1 ~ "B",
        q1_0_r_ureter_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      right_ut_fin = case_when(
        q1_0_r_ut_n_fin___1 == 1 ~ "Normal",
        q1_0_r_ut_a_fin___1 == 1 ~ "A",
        q1_0_r_ut_b_fin___1 == 1 ~ "B",
        q1_0_r_ut_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      right_par_fin = case_when(
        q1_0_r_par_n_fin___1 == 1 ~ "Normal",
        q1_0_r_par_a_fin___1 == 1 ~ "A",
        q1_0_r_par_b_fin___1 == 1 ~ "B",
        q1_0_r_par_b_fin___2 == 1 ~ "B",
        q1_0_r_par_b_fin___3 == 1 ~ "B",
        q1_0_r_par_b_fin___4 == 1 ~ "B",
        q1_0_r_par_c_fin___1 == 1 ~ "C",
        q1_0_r_par_c_fin___2 == 1 ~ "C",
        q1_0_r_par_c_fin___3 == 1 ~ "C",
        q1_0_r_par_c_fin___4 == 1 ~ "C",
        q1_0_r_par_c_fin___5 == 1 ~ "C",
        q1_0_r_par_c_fin___6 == 1 ~ "C",
        q1_0_r_par_c_fin___7 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      left_csd_fin = case_when(
        q1_0_l_csd_n_fin___1 == 1 ~ "Normal",
        q1_0_l_csd_a_fin___1 == 1 ~ "A",
        q1_0_l_csd_a_fin___2 == 1 ~ "A",
        q1_0_l_csd_a_fin___3 == 1 ~ "A",
        q1_0_l_csd_a_fin___4 == 1 ~ "A",
        q1_0_l_csd_b_fin___1 == 1 ~ "B",
        q1_0_l_csd_b_fin___2 == 1 ~ "B",
        q1_0_l_csd_b_fin___3 == 1 ~ "B",
        q1_0_l_csd_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      left_ureter_fin = case_when(
        q1_0_l_ureter_n_fin___1 == 1 ~ "Normal",
        q1_0_l_ureter_a_fin___1 == 1 ~ "A",
        q1_0_l_ureter_b_fin___1 == 1 ~ "B",
        q1_0_l_ureter_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      left_ut_fin = case_when(
        q1_0_l_ut_n_fin___1 == 1 ~ "Normal",
        q1_0_l_ut_a_fin___1 == 1 ~ "A",
        q1_0_l_ut_b_fin___1 == 1 ~ "B",
        q1_0_l_ut_c_fin___1 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      left_par_fin = case_when(
        q1_0_l_par_n_fin___1 == 1 ~ "Normal",
        q1_0_l_par_a_fin___1 == 1 ~ "A",
        q1_0_l_par_b_fin___1 == 1 ~ "B",
        q1_0_l_par_b_fin___2 == 1 ~ "B",
        q1_0_l_par_b_fin___3 == 1 ~ "B",
        q1_0_l_par_b_fin___4 == 1 ~ "B",
        q1_0_l_par_c_fin___1 == 1 ~ "C",
        q1_0_l_par_c_fin___2 == 1 ~ "C",
        q1_0_l_par_c_fin___3 == 1 ~ "C",
        q1_0_l_par_c_fin___4 == 1 ~ "C",
        q1_0_l_par_c_fin___5 == 1 ~ "C",
        q1_0_l_par_c_fin___6 == 1 ~ "C",
        q1_0_l_par_c_fin___7 == 1 ~ "C",
        TRUE ~ NA_character_
      ),
      
      bladder_fin = case_when(
        q1_0_l_blad_n_fin___1 == 1 ~ "Normal",
        q1_0_l_blad_a_fin___1 == 1 ~ "A",
        q1_0_l_blad_b_fin___1 == 1 ~ "B",
        q1_0_l_blad_b_fin___2 == 1 ~ "B",
        q1_0_l_blad_c_fin___1 == 1 ~ "C",
        q1_0_l_blad_c_fin___2 == 1 ~ "C",
        q1_0_l_blad_c_fin___3 == 1 ~ "C",
        TRUE ~ NA_character_
      )
      
    )
  
  x <- x %>%
    mutate(
      across(
        c(
          right_csd_fin,
          right_ureter_fin,
          right_ut_fin,
          right_par_fin,
          left_csd_fin,
          left_ureter_fin,
          left_ut_fin,
          left_par_fin,
          bladder_fin
        ),
        ~ if_else(us_unavail == 1, NA_character_, .)
      )
    )
  
  
  ## check for sanjay
  
  q1_0_r_par_b_final___3
  
  
  x <- x %>%
    mutate(
      r_kidney_size_disc = case_when(
        
        ## errors in both initial reports, corrections made on "Final" form
        final_adjudication == 1 ~ q1_0_r_par_b_final___3,
        
        ## if ""Final" form completed, use that by default
        !is.na(q1_0_final) ~ q1_0_r_par_b_final___3,
        
        ## both initial reports agreed, use VF
        final_adjudication == 2 ~ q1_0_r_par_b_vf___3,
        
        ## flagged to use VF, use VF
        final_adjudication == 3 ~ q1_0_r_par_b_vf___3,
        
        ## flagged to use GL, use SM
        final_adjudication == 4 ~ q1_0_r_par_b_sm___3,
        
        TRUE ~ NA
        
      )
    )
  
  x <- x %>%
    mutate(
      l_kidney_size_disc = case_when(
        
        ## errors in both initial reports, corrections made on "Final" form
        final_adjudication == 1 ~ q1_0_l_par_b_final___3,
        
        ## if ""Final" form completed, use that by default
        !is.na(q1_0_final) ~ q1_0_l_par_b_final___3,
        
        ## both initial reports agreed, use VF
        final_adjudication == 2 ~ q1_0_l_par_b_vf___3,
        
        ## flagged to use VF, use VF
        final_adjudication == 3 ~ q1_0_l_par_b_vf___3,
        
        ## flagged to use GL, use SM
        final_adjudication == 4 ~ q1_0_l_par_b_sm___3,
        
        TRUE ~ NA
        
      )
    )
  table(x$l_kidney_size_disc)
  table(x$r_kidney_size_disc)
  
  test <- x %>% filter(l_kidney_size_disc==1 | r_kidney_size_disc==1)
  
  test <- test %>%
    select(
      record_id,
      q1_0_l_par_b_vf___3,
      q1_0_l_par_b_sm___3,
      q1_0_l_par_b_final___3,
      final_adjudication,
      r_kidney_size_disc,
      l_kidney_size_disc)
  
  write.csv(test, "C:/Users/Sadia Akbar/Desktop/ROUTINE/kidneysize_discrepancies.csv", row.names = FALSE)
  
  #######################################################################
  ### reverting to complex - should still work 
  #########################################################################
  
 vf_vars <- names(x)[grepl("_vf($|_)", names(x))]
 vf_vars <- setdiff(
  vf_vars, c("use_this_vf", "rater_vf") )
  
 for(vf in vf_vars) {
      sm <- gsub("_vf($|_)", "_sm\\1", vf)
        if(sm %in% names(x)) {
          final <- gsub("_vf($|_)", "_final\\1", vf)
          fin   <- gsub("_vf($|_)", "_fin\\1", vf)
           
          x <- x %>%
            mutate(
              !!fin := case_when(
                
                final_adjudication == 1 ~ .data[[final]],

                final_adjudication == 2 ~ .data[[vf]],
                
                final_adjudication == 3 ~ .data[[vf]],
                
                final_adjudication == 4 ~ .data[[sm]],
                TRUE ~ NA
                         )
                     )
                    }
 }
 
 table(x$q1_0_fin)
 test <- x %>% filter(is.na(q1_0_fin))
 
  
  
  ########################################################################
  ########################################################################
  
  
  ####### check missingness ####### ####### ####### ####### #######
 test <- x %>% filter(is.na(x$right_csd_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$right_ureter_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$right_ut_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$right_par_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 
 test <- x %>% filter(is.na(x$left_csd_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$left_ureter_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$left_ut_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 test <- x %>% filter(is.na(x$left_par_fin) & us_unavail == 0 & q1_0_csd_fin == 0)
 
 test <- x %>% filter(is.na(x$bladder_fin) & us_unavail == 0 & q1_0_csd_fin == 0)

 
 test <- x %>% filter(is.na(x$q2_1_yn_fin) & us_unavail == 0)
 test <- x %>% filter(is.na(x$q2_2_yn_fin) & us_unavail == 0)
 
 test <- test %>%
   select(
     record_id)
 
 
 test <- x %>% filter(q2_1_yn_fin == 1)
 
   test <- test %>%
    select(
      record_id,
      q2_1_r_ra_fin,
      q2_1_r_rh_fin,
      q2_1_r_rd_fin,
      q2_1_r_mdk_fin,
      q2_1_r_pk_fin,
      q2_1_r_hk_fin,
      q2_1_r_epk_fin,
      q2_1_r_vur_fin,
      q2_1_r_dcs_fin,
      q2_1_r_mu_fin,
      q2_1_r_other_fin,
      q2_1_l_ra_fin,
      q2_1_l_rh_fin,
      q2_1_l_rd_fin,
      q2_1_l_mdk_fin,
      q2_1_l_pk_fin,
      q2_1_l_hk_fin,
      q2_1_l_epk_fin,
      q2_1_l_vur_fin,
      q2_1_l_dcs_fin,
      q2_1_l_mu_fin,
      q2_1_l_other_fin)
  
   
   
   test <- x %>% filter(q2_2_yn_fin == 1)
   
   test <- test %>%
     select(
       record_id,
       q2_1_r_ra_12m_fin,
       q2_1_r_rh_12m_fin,
       q2_1_r_rd_12m_fin,
       q2_1_r_mdk_12m_fin,
       q2_1_r_pk_12m_fin,
       q2_1_r_hk_12m_fin,
       q2_1_r_epk_12m_fin,
       q2_1_r_vur_12m_fin,
       q2_1_r_dcs_12m_fin,
       q2_1_r_mu_12m_fin,
       q2_1_r_other_12m_fin,
       q2_1_l_ra_12m_fin,
       q2_1_l_rh_12m_fin,
       q2_1_l_rd_12m_fin,
       q2_1_l_mdk_12m_fin,
       q2_1_l_pk_12m_fin,
       q2_1_l_hk_12m_fin,
       q2_1_l_epk_12m_fin,
       q2_1_l_vur_12m_fin,
       q2_1_l_dcs_12m_fin,
       q2_1_l_mu_12m_fin,
       q2_1_l_other_12m_fin)  
   
   
   
  ####### ####### ####### ####### ####### ####### ####### #######
  
        
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################### cakut classifications ################### 
 
  table(x$q2_1_yn_vf)
  table(x$q2_1_yn_sm)
  table(x$q2_1_yn_final)
  
  test2 <- x %>% filter(is.na(q2_1_yn_vf) & is.na(q2_1_yn_sm) & is.na(q2_1_yn_final))
  
  anti_join(test2, test, by = "record_id")
  
  
  ################### radiologist classifications ################### 
    
  table(x$q1_0_csd_sm)
  table(x$q1_0_csd_vf)
  table(x$q1_0_csd_final)
  table(x$q1_0_csd_flag)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
###########################################################
# Table 5 - OUTCOMES
###########################################################
 
## another febrile UTI 
  table(x$q8_1)
  test <- x %>% filter(is.na(q8_1) & consent_date < "2025-09-24")
  
  table(x$q8_1_num)
  test <- x %>% filter((q8_1 == 1) & is.na(q8_1_num))
  
  table(x$q8_2)
  test <- x %>% filter((q8_1 == 1) & is.na(q8_2))
  
  test <- x %>% filter((q8_1 == 1) & is.na(q8_3a))
  test <- x %>% filter((q8_1 == 1) & is.na(q8_3b))
  
  table(x$q8_3c)
  test <- x %>% filter((q8_1 == 1) & is.na(q8_3c))
  
  table(x$q8_4)
  test <- x %>% filter((q8_1 == 1) & is.na(q8_4))
  
  
## repeat renal US
  table(x$q8_9_yesno)
  test <- x %>% filter(is.na(q8_9_yesno) & consent_date < "2025-09-24")
  
  x$time_to_repeat_us <- x$q8_9_date - x$q7_1
  summary(x$time_to_repeat_us)
  
  test <- x %>% filter(time_to_repeat_us < 0)
  
  test <- x %>% filter((q8_9_yesno == 1) & is.na(q8_9_report))
  
  test <- x %>% filter((us_result_yesno_2 == 1) & is.na(q8_9_report_2))
  

  test <- x %>% filter(q8_9_yesno == 1)

  
  ## create report of repeat renal US needing classification
  test <- x %>% filter(q8_9_yesno == 1)
  
  test <- test[, c("record_id", "q7_1", "q7_3", "q7_7",
                   "final_adjudication", "q1_0_final",
                   "q8_9_yesno", "q8_9_num", 
                   "q8_9_date", "q8_9_report", "q8_9_classification",
                   "q8_9_date_2", "q8_9_report_2", "q8_9_classification_2",
                   "q8_9_date_3", "q8_9_report_3", "q8_9_classification_3",
                   "q8_9_date_4", "q8_9_report_4", "q8_9_classification_4",
                   "q8_9_date_5", "q8_9_report_5", "q8_9_classification_5"
  )]

  test <- test %>% filter (!is.na(q7_7))
  
  test <- test[, c("record_id","q7_3", "q7_7")]
  
## vcug
  table(x$q8_10_yesno)
  test <- x %>% filter(is.na(q8_10_yesno) & consent_date < "2025-09-24")
  
  x$time_to_vcug <- x$q8_10_date - x$q7_1
  summary(x$time_to_vcug)
  
  test <- x %>% filter(time_to_vcug < 0)

  test <- x %>% filter(q8_10_yesno == 1) & )
  

  test <- x %>% filter((is.na(q8_10_yesno) & q8_10_classification>=0))
  test <- x %>% filter(q8_10_yesno == 0 & q8_10_classification>=0)


  table(x$q8_10_classification)
  

  table(x$q8_10_classification)
  table(x$q8_10_vur)
  
  test <- x %>% filter(q8_10_classification == 1 & is.na(q8_10_vur))
  
  table(x$q8_10_num)
  
  x$time_to_vcug2 <- x$q8_10_date_2 - x$q7_1
  summary(x$time_to_vcug2)
  
  table(x$q8_10_classification_2)
  
  
  
## dmsa
  table(x$q8_11_yesno)
  test <- x %>% filter(is.na(q8_11_yesno) & consent_date < "2025-09-24")
  
  x$time_to_dmsa <- x$q8_11_date - x$q7_1
  summary(x$time_to_dmsa)
  
  test <- x %>% filter(q8_11_yesno == 1 & is.na(q8_11_date))
 
   table(x$q8_11_classification)

   
## CT
   table(x$q8_12_yesno)
   test <- x %>% filter(is.na(q8_12_yesno) & consent_date < "2025-09-24")
   
   table(x$q8_12_num)
   
   x$time_to_ct <- x$q8_12_date - x$q7_1
   summary(x$time_to_ct)
   
   
  ## MRI
   table(x$q8_13_yesno)
   test <- x %>% filter(is.na(q8_13_yesno) & consent_date < "2025-09-24")
   
   table(x$q8_13_num)
   
   x$time_to_mri <- x$q8_13_date - x$q7_1
   summary(x$time_to_mri)
   
   test <- x %>% filter(q8_13_yesno == 1 & is.na(q8_13_date))
   
   table(x$q8_13_classification)
   
   test <- x %>% filter(q8_13_yesno == 1 & is.na(q8_13_classification))
   
  
   
## surgery
   table(x$q8_14_yesno)
   test <- x %>% filter(is.na(q8_14_yesno) & consent_date < "2025-09-24")
   
   table(x$num_surgeries)
   
   x$time_to_surg <- x$q8_14_date - x$q7_1
   summary(x$time_to_surg)
   
   test <- x %>% filter(q8_14_yesno == 1 & is.na(q8_14_date))
   
   table(x$q8_14_sxtype)
   table(x$q8_14_sxtype_2)
   
   
## abx prophylaxis
   table(x$q8_15_yesno)
   test <- x %>% filter(is.na(q8_15_yesno) & consent_date < "2025-09-24")
   

   x$time_to_prophy <- x$q8_15_date - x$q7_1
   summary(x$time_to_prophy)

   test <- x %>% filter(q8_15_yesno == 1 & is.na(q8_15_date))
   
   
## uti outpatient visit
   table(x$q8_17_yesno)
   test <- x %>% filter(is.na(q8_17_yesno) & consent_date < "2025-09-24")
   
   
   table(x$num_outpt_visits)
   
   table(x$q8_17_visittype)
   
   
## specialist visit
   table(x$q8_18_yesno)
   test <- x %>% filter(is.na(q8_18_yesno) & consent_date < "2025-09-24")
   table(x$q8_18_visittype)
   
   
   
   
   ## CHECK OTHER IMAGING NEEDING CLASSIFICATION
   
   ## VCUG 
   outcomes_complete <- x %>% filter(outcomes_emr_complete == 2 | outcomes_emr_complete == 1)
   
   
   VCUG <- outcomes_complete %>% filter(q8_10_yesno == 1)
   
   VCUG <- VCUG[, c("record_id", "final_adjudication", "q1_0_final",
    "q8_10_yesno", "q8_10_num", 
    "q8_10_date", "q8_10_report", "q8_10_classification", "q8_10_abnormal", "q8_10_vur",
    "q8_10_date_2", "q8_10_report_2", "q8_10_classification_2", "q8_10_abnormal_2", "q8_10_vur_2",
    "q8_10_date_3", "q8_10_report_3", "q8_10_classification_3", "q8_10_abnormal_3", "q8_10_vur_3",
    "q8_10_date_4", "q8_10_report_4", "q8_10_classification_4", "q8_10_abnormal_4", "q8_10_vur_4",
    "q8_10_date_5", "q8_10_report_5", "q8_10_classification_5", "q8_10_abnormal_5", "q8_10_vur_5")]
    
    
   
    ## DMSA
    DMSA <- outcomes_complete %>% filter(q8_11_yesno == 1)
    DMSA <- DMSA[, c("record_id","final_adjudication", "q1_0_final",
    "q8_11_yesno", "q8_11_num",
    "q8_11_date", "q8_11_report", "q8_11_classification", "q8_11_abnormal", 
    "q8_11_date_2", "q8_11_report_2", "q8_11_classification_2", "q8_11_abnormal_2", 
    "q8_11_date_3", "q8_11_report_3", "q8_11_classification_3", "q8_11_abnormal_3", 
    "q8_11_date_4", "q8_11_report_4", "q8_11_classification_4", "q8_11_abnormal_4", 
    "q8_11_date_5", "q8_11_report_5", "q8_11_classification_5", "q8_11_abnormal_5")]
    
    

    ## CT
    CT <- outcomes_complete %>% filter(q8_12_yesno == 1)
    CT <- CT[, c("record_id","final_adjudication", "q1_0_final",
    "q8_12_yesno", "q8_12_num",
    "q8_12_date", "q8_12_report", "q8_12_classification", "q8_12_abnormal", 
    "q8_12_date_2", "q8_12_report_2", "q8_12_classification_2", "q8_12_abnormal_2", 
    "q8_12_date_3", "q8_12_report_3", "q8_12_classification_3", "q8_12_abnormal_3", 
    "q8_12_date_4", "q8_12_report_4", "q8_12_classification_4", "q8_12_abnormal_4", 
    "q8_12_date_5", "q8_12_report_5", "q8_12_classification_5", "q8_12_abnormal_5")]
    
    
    ## MRI
    MRI <- outcomes_complete %>% filter(q8_13_yesno == 1)
    MRI <- MRI[, c("record_id","final_adjudication", "q1_0_final",
    "q8_13_yesno", "q8_13_num",
    "q8_13_date", "q8_13_report", "q8_13_classification", "q8_13_abnormal", 
    "q8_13_date_2", "q8_13_report_2", "q8_13_classification_2", "q8_13_abnormal_2", 
    "q8_13_date_3", "q8_13_report_3", "q8_13_classification_3", "q8_13_abnormal_3", 
    "q8_13_date_4", "q8_13_report_4", "q8_13_classification_4", "q8_13_abnormal_4", 
    "q8_13_date_5", "q8_13_report_5", "q8_13_classification_5", "q8_13_abnormal_5")]
    