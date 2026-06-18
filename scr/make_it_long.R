make_it_long <- function(d = NULL, instruments = NULL, verbose = TRUE){
  if(is.null(d)){
    stop("Data is NULL. Please provide a data frame.")
  }
  
  # Remove unnecessary columns
  
  x <- c('Age', 'Gender', 'EmpCount', 'Role', 'RoleQual', 'Subject', 'LvlTaught', 'LvlTaughtQual', 'YearTaught', 'EmpStatus', 'EmpStatusQual', 'TeachAct', 'TeachActQual', 'OftenTeach', 'Qualified', 'QualifiedQual', 'AcadBack', 'Formal')
  
  d <- dplyr::select(d, -any_of(x))
  
  # making it longer than wider
  
  item_ID <- str_detect(names(d), "_") & names(d) != "PROLIFIC_PID" # find the item names for all instruments
  
  df <- pivot_longer(d, cols = names(d)[item_ID], names_to = "item", values_to = "value")
  df <- df %>% mutate(across(value, as.numeric))
  
  # Add instrument name if lookup provided
  if (!is.null(instruments)) {
    instruments <- Filter(length, instruments)  # drop any empty vectors
    instrument_map <- stack(instruments) %>%
      dplyr::rename(item = values, instrument = ind)
    df <- dplyr::left_join(df, instrument_map, by = "item")
  }
  
  if(verbose) {
    print(paste("Number of rows in the long data:", nrow(df)))
    print(paste("Number of unique participants in the long data:", length(unique(df$PROLIFIC_PID))))
    print(paste("Number of unique items in the long data:", length(unique(df$item))))
    if (!is.null(instruments)) {
      print(paste("Instruments found:", paste(names(instruments), collapse = ", ")))
    }
  }

  return(df)

}