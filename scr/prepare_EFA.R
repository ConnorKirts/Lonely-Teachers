prepare_EFA <- function(d = NULL, instrument_name = NULL) {
  if (is.null(d)) {
    stop("Data is NULL. Please provide a data frame.")
  }
  if (is.null(instrument_name)) {
    stop("instrument_name is NULL. Please specify an instrument (e.g., 'Michele').")
  }
  
  # Filter to the specified instrument only
  tmp <- dplyr::filter(d, instrument == instrument_name)
  
  # Pivot back to wide so each item is a column (required for EFA)
  tmp_w <- tidyr::pivot_wider(
    tmp,
    id_cols    = PROLIFIC_PID,
    names_from = item,
    values_from = value
  )
  tmp_w <- data.frame(tmp_w)
  
  # Set row names to participant ID then drop the ID column
  rownames(tmp_w) <- tmp_w$PROLIFIC_PID
  tmp_w <- dplyr::select(tmp_w, -PROLIFIC_PID)
  
  return(tmp_w)
}