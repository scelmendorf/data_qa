
# Function if a variable in a data frame is non-numeric
# @param df data frame
# @param var variable to check
# @return data frame with non-numeric values
check_non_numeric <- function(df, var) {
  row_probs <- which(!is.na(df[[var]]) & is.na(suppressWarnings(as.numeric(df[[var]]))))
  if (length(row_probs > 0)) {
    warning(paste("Check file for", var, "\n"))
    return(df[row_probs, ])
  } else {
    cat(paste("all values are numeric for ", var, "\n"))
    return(NULL)
  }
}