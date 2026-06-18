rename_values <- function(data, col = "EmpCount") {
  
  # Build a named vector
  lookup <- c(
    # Australia
    "Australia "        = "Australia",
    # Canada
    "Canafa"            = "Canada",
    "canada"            = "Canada",
    # France
    "france"            = "France",
    "FRANCE"            = "France",
    "France "           = "France",
    # Germany
    "Germany "          = "Germany",
    "germany"           = "Germany",
    # Italy
    "Italia"            = "Italy",
    # Morocco
    "Morocco "          = "Morocco",
    # Netherlands
    "Nederland"         = "Netherlands",
    "The Netherlands"   = "Netherlands",
    # Spain
    "spain"             = "Spain",
    "España"            = "Spain",
    # South Africa
    "South African "    = "South Africa",
    "South Africa "     = "South Africa",
    # UAE
    "United Arab Emirates" = "UAE",
    # United Kingdom
    "United Kingdom "   = "United Kingdom",
    "united kingdom"    = "United Kingdom",
    "UK"                = "United Kingdom",
    "U.K."              = "United Kingdom",
    "England"           = "United Kingdom",
    "England "          = "United Kingdom",
    # United States
    "USA"               = "United States",
    "usa"               = "United States",
    "united states"     = "United States",
    "United States "    = "United States",
    "United States of America" = "United States",
    # Unknown
    "Teacher "          = "Unknown"
  )
  
  # Renaming values ----------------------------------
 # df$Gender <- case_when(d$Gender == 1 ~ "Male", 
 #                        d$Gender == 2 ~ "Female")
 # 
 # df$Role <- case_when(d$Role == 1 ~ "", 
 #                      d$Role == 2 ~ "", 
 #                      d$Role == 3 ~ "", 
 #                      d$Role == 4 ~ "", 
 #                      d$Role == 5 ~ "", 
 #                      d$Role == 6 ~ "")
 # 
 # df$LvlTaught <- case_when(d$LvlTaught == 1 ~ "", 
 #                           d$LvlTaught == 2 ~ "", 
 #                           d$LvlTaught == 3 ~ "", 
 #                           d$LvlTaught == 4 ~ "", 
 #                           d$LvlTaught == 5 ~ "", 
 #                           d$LvlTaught == 6 ~ "",
 #                           d$LvlTaught == 7 ~ "")
 # 
 # df$EmpStatus <- case_when(d$EmpStatus == 1 ~ "", 
 #                           d$EmpStatus == 2 ~ "", 
 #                           d$EmpStatus == 3 ~ "", 
 #                           d$EmpStatus == 4 ~ "",
 #                           d$EmpStatus == 5 ~ "")
 # 
 # df$TeachAct <- case_when(d$TeachAct == 1 ~ "", 
 #                          d$TeachAct == 2 ~ "", 
 #                          d$TeachAct == 3 ~ "", 
 #                          d$TeachAct == 4 ~ "", 
 #                          d$TeachAct == 5 ~ "")
 # 
 # df$OftenTeach <- case_when(d$OftenTeach == 1 ~ "", 
 #                            d$OftenTeach == 2 ~ "", 
 #                            d$OftenTeach == 3 ~ "", 
 #                            d$OftenTeach == 4 ~ "", 
 #                            d$OftenTeach == 5 ~ "")
 # 
 # df$Qualified <- case_when(d$Qualified == 1 ~ "", 
 #                           d$Qualified == 2 ~ "", 
 #                           d$Qualified == 3 ~ "", 
 #                           d$Qualified == 4 ~ "", 
 #                           d$Qualified == 5 ~ "")
 # 
 # df$Formal <- case_when(d$Formal == 1 ~ "", 
 #                        d$Formal == 2 ~ "", 
 #                        d$Formal == 3 ~ "")
  
  # Apply the lookup; unmatched values are left as-is
  data[[col]] <- dplyr::recode(data[[col]], !!!lookup)
  
  return(data)
}



