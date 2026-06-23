rename_values <- function(data) {
  
  # translate a single numeric code to its label using a named vector
  recode_one <- function(code, lookup) {
    code   <- trimws(as.character(code))
    result <- lookup[code]
    if (is.na(result)) code else unname(result)  # leave unmapped values as-is
  }
  
  # multi-select columns where a response may be a single
  # value ("1") or several comma-separated values ("1,2,4").
  recode_multiselect <- function(x, lookup) {
    sapply(x, function(val) {
      if (is.na(val)) return(NA_character_)
      codes  <- strsplit(as.character(val), ",")[[1]]
      labels <- sapply(trimws(codes), recode_one, lookup = lookup)
      paste(labels, collapse = " / ")
    }, USE.NAMES = FALSE)
  }
  
  # EmpCount (Country of Employment) --------------------------------------
  country_lookup <- c(
    "Australia "               = "Australia",
    "Canafa"                   = "Canada",
    "canada"                   = "Canada",
    "france"                   = "France",
    "FRANCE"                   = "France",
    "France "                  = "France",
    "Germany "                 = "Germany",
    "germany"                  = "Germany",
    "Italia"                   = "Italy",
    "Morocco "                 = "Morocco",
    "Nederland"                = "Netherlands",
    "The Netherlands"          = "Netherlands",
    "spain"                    = "Spain",
    "España"                   = "Spain",
    "South African "           = "South Africa",
    "South Africa "            = "South Africa",
    "United Arab Emirates"     = "UAE",
    "United Kingdom "          = "United Kingdom",
    "united kingdom"           = "United Kingdom",
    "UK"                       = "United Kingdom",
    "U.K."                     = "United Kingdom",
    "England"                  = "United Kingdom",
    "England "                 = "United Kingdom",
    "USA"                      = "United States",
    "usa"                      = "United States",
    "united states"            = "United States",
    "United States "           = "United States",
    "United States of America" = "United States",
    "Teacher "                 = "Unknown"
  )
  data$EmpCount <- dplyr::recode(trimws(data$EmpCount), !!!country_lookup)
  
  # YearTaught (How many years taught) --------------------------------------
  year_lookup <- c(
    "1,5"           = "< 10 years",
    "2"             = "< 10 years",
    "3"             = "< 10 years",
    "4"             = "< 10 years",
    "5"             = "< 10 years",
    "6"             = "< 10 years",
    "7"             = "< 10 years",
    "8"             = "< 10 years",
    "8 years"       = "< 10 years",
    "8years"        = "< 10 years",
    "9"             = "< 10 years",
    "10"            = ">= 10 years",
    "10 years"      = ">= 10 years",
    "11"            = ">= 10 years",
    "12"            = ">= 10 years",
    "13"            = ">= 10 years",
    "14"            = ">= 10 years",
    "15"            = ">= 10 years",
    "16"            = ">= 10 years",
    "17"            = ">= 10 years",
    "18"            = ">= 10 years",
    "19"            = ">= 10 years",
    "20"            = ">= 20 years",
    "20 YEARS"      = ">= 20 years",
    "20+"           = ">= 20 years",
    "21"            = ">= 20 years",
    "22"            = ">= 20 years",
    "23"            = ">= 20 years",
    "24"            = ">= 20 years",
    "25"            = ">= 20 years",
    "26"            = ">= 20 years",
    "27"            = ">= 20 years",
    "28"            = ">= 20 years",
    "29"            = ">= 20 years",
    "30"            = ">= 30 years",
    "31"            = ">= 30 years",
    "34"            = ">= 30 years",
    "35"            = ">= 30 years",
    "40"            = ">= 30 years"
  )
  YearTaught <- data$YearTaught
  data$YearTaught <- dplyr::recode(trimws(data$YearTaught), !!!year_lookup)
  
  
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
  data$Role <- recode_multiselect(data$Role, role_lookup)
  
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
  data$LvlTaught <- recode_multiselect(data$LvlTaught, lvl_lookup)
  
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
  data$TeachAct <- recode_multiselect(data$TeachAct, act_lookup)
  
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
