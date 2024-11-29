#generate a test dataset for testing trending feature

filename <- paste0(getwd(),"/Datasets/lqi_data_clarity_2020-12-18.csv")
df_test <- read_csv(filename, na = "NULL")
df_test %<>%
  mutate(num_rnd1 = round(runif(nrow(df_test),1,10),1),
         logical_rnd1 = as.logical(round(runif(nrow(df_test), 0, 1), 0)),
         col_factor1 = as.factor(sample(c("red", "green", "blue"), nrow(df_test), replace = TRUE)),
         col_factor2 = as.factor(sample(c("dog", "cat", "elephant"), nrow(df_test), replace = TRUE)),
         )

str(df_test)

# Create a sample dataset with random data
sample_dataset <- tibble(
  col_num1 = round(runif(100, 1, 10), 1),
  col_num2 = round(runif(100, 1, 10), 1),
  col_num3 = round(runif(100, 1, 10), 1),
  col_factor1 = sample(c("red", "green", "blue"), 100, replace = TRUE),
  col_factor2 = sample(c("dog", "cat", "elephant"), 100, replace = TRUE),
  col_factor3 = sample(c("New York", "London", "Tokyo"), 100, replace = TRUE),
  col_logical1 = sample(c(TRUE, FALSE), 100, replace = TRUE),
  col_logical2 = sample(c(TRUE, FALSE), 100, replace = TRUE),
  col_month1 = sample(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 month"), 100, replace = TRUE),
  col_date2 = sample(seq(as.POSIXct("2021-01-01", tz = "UTC"), as.POSIXct("2021-12-31", tz = "UTC"), by = "1 day"), 100)
)

# add a column to this dataset called logical_rnd1 that is a random logical value


df_test %>% str

variable_names <- names(df_test)

for(variable_name in variable_names){
  print(paste("Variable name:",variable_name,"\t Class:",class(df_test[[variable_name]]),"- check_numeric:",check_numeric(df_test[[variable_name]])))
}
