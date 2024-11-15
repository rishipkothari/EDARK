# data processing functions

# check if a vector needs to be converted to a numeric type
# numbers, logicals, dates all make it to numbers when casted
# type CHAR that contains all numbers will return false

check_numeric <- function(x) (
  if(class(x) %in% c("int","num")){
    T
  }
  else if (class(x) %in% c("factor", "logical")) F
  else {
    sum(is.na(as.numeric(x))) == sum(is.na(x))
  }
)

# Define a mode function as it's not built-in in R
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
