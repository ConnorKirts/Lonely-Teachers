demographicV <- function(d, label) {
  N <- length(unique(d$ID))
  
  age     <- d$Age[!duplicated(d$ID)]
  age     <- age[!is.na(age)]
  Mean    <- round(mean(age), 1)
  Median  <- round(median(age), 1)
  SD      <- round(sd(age), 2)
  Min     <- min(age)
  Max     <- max(age)
  
  gender_counts <- table(d$Gender[!duplicated(d$ID)])
  Gender <- paste(
    names(gender_counts),
    gender_counts,
    sep = ": ",
    collapse = "; "
  )
  
  emp_counts <- dplyr::count(d, EmpCount, sort = TRUE)
  
  data.frame(
    Dataset      = label,
    N            = N,
    `Mean Age`   = Mean,
    `Median Age` = Median,
    `SD Age`     = SD,
    `Age Range`  = paste0("[", Min, ", ", Max, "]"),
    Gender       = Gender,
    Country      = emp_counts,
    row.names    = NULL,
    check.names  = FALSE
  )
}