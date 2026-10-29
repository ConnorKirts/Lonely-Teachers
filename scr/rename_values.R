rename_values <- function(data) {
  
  # --- Helpers for multi-select items (Role, LvlTaught, TeachAct) -------------
  # Multiple answers are stored as comma separated codes, e.g. "1,2,3,6"
  
  # How many options selected? 
  count_selected <- function(x) {
    ifelse(is.na(x), NA_integer_, lengths(strsplit(as.character(x), ",")))
  }
  
  # Turn "1,2,3,6" into "Label 1 / Label 2 / Label 3 / Label 6"
  recode_multi <- function(x, lookup) {
    sapply(as.character(x), function(v) {
      if (is.na(v)) return(NA_character_)
      codes  <- trimws(strsplit(v, ",")[[1]])
      labels <- unname(lookup[codes])
      labels <- ifelse(is.na(labels), codes, labels)
      paste(labels, collapse = " / ")
    }, USE.NAMES = FALSE)
  }
  
  # --- EmpCount --------------------------------------
  country_lookup <- c(
    "Australia "              = "Australia",
    "Canafa"                  = "Canada",
    "canada"                  = "Canada",
    "france"                  = "France",
    "FRANCE"                  = "France",
    "France "                 = "France",
    "Germany "                = "Germany",
    "germany"                 = "Germany",
    "Italia"                  = "Italy",
    "Morocco "                = "Morocco",
    "Nederland"               = "Netherlands",
    "The Netherlands"         = "Netherlands",
    "spain"                   = "Spain",
    "España"                  = "Spain",
    "South African "          = "South Africa",
    "South Africa "           = "South Africa",
    "United Arab Emirates"    = "UAE",
    "United Kingdom "         = "United Kingdom",
    "united kingdom"          = "United Kingdom",
    "UK"                      = "United Kingdom",
    "U.K."                    = "United Kingdom",
    "England"                 = "United Kingdom",
    "England "                = "United Kingdom",
    "USA"                     = "United States",
    "usa"                     = "United States",
    "united states"           = "United States",
    "United States "          = "United States",
    "United States of America"= "United States",
    "Teacher "                = "Unknown",
    "Teacher"                = "Unknown"
  )
  data$EmpCount <- dplyr::recode(trimws(data$EmpCount), !!!country_lookup)
  
  # --- YearTaught -----------------------------------
  year_lookup <- c(
    "1,5"             = "1",
    "10 years"        = "10",
    "8 years"         = "8",
    "8years"          = "8",
    "20 YEARS"        = "20",
    "20+"             = "20"
  )
  
  # Keep the cleaned numeric years, then group them
  data <- data %>%
    dplyr::mutate(
      YearTaught_num = as.numeric(dplyr::recode(trimws(YearTaught), !!!year_lookup)),
      
      YearTaught = dplyr::case_when(
        is.na(YearTaught_num) ~ NA_character_,   # keep missing as NA (not a group)
        YearTaught_num <= 7   ~ "1-7 years",
        YearTaught_num <= 10  ~ "8-10 years",
        YearTaught_num <= 17  ~ "11-17 years",
        TRUE                  ~ "18-40 years"
      ),
      
      YearTaught = factor(YearTaught,
                          levels = c("1-7 years", "8-10 years",
                                     "11-17 years", "18-40 years"))
    )
  
  # Check distribution
  # data %>% count(YearTaught)
  
  # Gender ----------------------------------------------------------------
  data$Gender <- dplyr::case_when(
    data$Gender == 1 ~ "Male",
    data$Gender == 2 ~ "Female",
    data$Gender == 3 ~ "Non-binary / third gender",
    data$Gender == 4 ~ "Prefer not to say",
    TRUE             ~ as.character(data$Gender)
  )
  
  # Role ------------------------------------------------------------------
  role_lookup <- c(
    "1" = "Classroom teacher",
    "2" = "Subject Specialist",
    "3" = "Support/Special",
    "4" = "Teaching Assistant",
    "5" = "Principal/Coordinator",
    "6" = "Other"
  )
  data$N_Role <- count_selected(data$Role)
  data$Role <- recode_multi(data$Role, role_lookup)
  
  # LvlTaught -------------------------------------------------------------
  lvl_lookup <- c(
    "1" = "Early Childhood",
    "2" = "Primary",
    "3" = "Lower secondary",
    "4" = "Upper secondary",
    "5" = "Further Ed./Adult",
    "6" = "Higher Ed.",
    "7" = "Other"
  )
  data$N_LvlTaught <- count_selected(data$LvlTaught)  
  data$LvlTaught <- recode_multi(data$LvlTaught, lvl_lookup)
  
  # EmpStatus -------------------------------------------------------------
  data$EmpStatus <- dplyr::case_when(
    data$EmpStatus == 1 ~ "Permanent, Full",
    data$EmpStatus == 2 ~ "Permanent, Part",
    data$EmpStatus == 3 ~ "Fixed term/Temp.",
    data$EmpStatus == 4 ~ "Freelance/Self employed",
    data$EmpStatus == 5 ~ "Other",
    TRUE                ~ as.character(data$EmpStatus)
  )
  
  # TeachAct --------------------------------------------------------------
  act_lookup <- c(
    "1" = "Classroom-based",
    "2" = "One-to-one",
    "3" = "Small group",
    "4" = "Online/Blended",
    "5" = "Practical/Lab based",
    "6" = "Other"
  )
  data$N_TeachAct <- count_selected(data$TeachAct)   
  data$TeachAct <- recode_multi(data$TeachAct, act_lookup)
  
  # OftenTeach ------------------------------------------------------------
  data$OftenTeach <- dplyr::case_when(
    data$OftenTeach == 1 ~ "Daily (+5 days a week)", 
    data$OftenTeach == 2 ~ "Several times per week",
    data$OftenTeach == 3 ~ "Once per week",
    data$OftenTeach == 4 ~ "Less than once per week",
    data$OftenTeach == 5 ~ "Irregular/variable schedule",
    TRUE                 ~ as.character(data$OftenTeach)
  )
  
  # Qualified -------------------------------------------------------------
  data$Qualified <- dplyr::case_when(
    data$Qualified == 1 ~ "Bachelor's degree",
    data$Qualified == 2 ~ "Master's degree",
    data$Qualified == 3 ~ "Doctorate",
    data$Qualified == 4 ~ "Professional Qualification",
    data$Qualified == 5 ~ "Other",
    TRUE                ~ as.character(data$Qualified)
  )
  
  # Formal ----------------------------------------------------------------
  data$Formal <- dplyr::case_when(
    data$Formal == 1 ~ "No",  
    data$Formal == 2 ~ "Yes",
    data$Formal == 3 ~ "In progress",
    TRUE             ~ as.character(data$Formal)
  )
  
  return(data)
}
