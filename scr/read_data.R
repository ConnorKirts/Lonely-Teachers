read_data <- function(filename=NULL, controlQ=NULL) {
  
  print(filename); print(controlQ)
  
  tmp <- read.csv(filename, header = TRUE, na.strings = "")
  names(tmp) <- gsub("\\.", "", names(tmp)) # remove full stops from the column names
  tmp <- tmp[3:nrow(tmp), ]     # delete first two rows (metadata)
  
  # need to change the column names...
  
  col_rename <- c(
    Age           = 'Q2',
    Gender        = 'Q3',
    EmpCount      = 'Q4',         # Country of Employment 
    Role          = 'Q5',         # multi-select number of teaching role
    RoleQual      = 'Q5_6_TEXT',  # free response text of role
    Subject       = 'Q6',         # free response text of subject taught
    LvlTaught     = 'Q7',         # multi-select number of Ed. lvl taught
    LvlTaughtQual = 'Q7_7_TEXT',  # free response text of lvl taught
    YearTaught    = 'Q8',         # number of years taught 
    EmpStatus     = 'Q9',         # Employment status
    EmpStatusQual = 'Q9_5_TEXT',  # Employment status
    TeachAct      = 'Q10',        # multi-select number of teaching activities
    TeachActQual  = 'Q10_6_TEXT', # Employment status
    OftenTeach    = 'Q11',        # how often do you teach
    Qualified     = 'Q12',        # highest lvl education
    QualifiedQual = 'Q12_5_TEXT', # free responce ed. lvl. obtained
    AcadBack      = 'Q13',        # field / academic background
    Formal        = 'Q14'         # formal teaching training
  )
  
  # Making sure values are numeric --------------------
  tmp <- tmp %>% mutate(across(starts_with("Prog"), as.numeric))
  tmp <- tmp %>% mutate(across(starts_with("Dur"), as.numeric))
  tmp$Q2 <- as.numeric(tmp$Q2)
  
  tmp <- dplyr::filter(tmp, Q2 > 18) # keep participants above 18 
  
  print(paste("Participants after consent/age filter:", nrow(tmp)))
  
  # Participant check ---------------------------------
  tmp2 <- dplyr::filter(tmp,
                        Progress == 100,
                        DistributionChannel != "preview",
                        Q2 != 99)
  
  print(paste("Participants after complete/preview/age-99 filter:", nrow(tmp2)))
  
  # Eliminate unnecessary columns --------------------
  drop_cols <- c('StartDate', 'EndDate', 'Status', 'Progress', 'Durationinseconds', 
                 'Finished', 'RecordedDate', 'DistributionChannel', 'UserLanguage', 
                 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 
                 'RecipientEmail', 'ExternalReference', 
                 'CreateNewFieldorChooseFromDropdown', 'Q29')
  
  d <- dplyr::select(tmp2, -any_of(drop_cols))
  
  # rename columns left
  d <- dplyr::rename(d, any_of(col_rename))
  
  # rename responses to EmpCount
  
  country <- c(
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
    "South African"     = "South Africa",
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
  d$EmpCount <- trimws(d$EmpCount)
  d$EmpCount <- dplyr::recode(d$EmpCount, !!!country)
  
  # Check control question ---------------------------
  # Q asked participants to write 'teacher'
  
  if (length(controlQ) > 0) {
    c_id <- which(names(d) %in% controlQ)
    inattentive <- !grepl("^teacher$", d[, c_id], ignore.case = TRUE)
    INATTENTIVE <- data.frame(inattentive)
    if (ncol(INATTENTIVE) > 1) {
      inattentive <- rowSums(inattentive) > 1
    }
    
    print(paste("Inattentive total:", sum(inattentive)))
    
    if (sum(inattentive) > 0) {
      print("IDs of inattentive participants:")
      print(d$PROLIFIC_PID[inattentive])
      df <- d[!inattentive, ]
    } else {
      df <- d
    }
    
    df <- dplyr::select(df, -all_of(controlQ))
    
  } else {
    df <- d
  }
  
  print(paste("Number of accepted participants:", nrow(df)))
  return(df)
  
}