# COVID-19-cases-prediction
JUMP-DROP-ADJUSTED PREDICTION OF CUMULATIVE CASES OF COVID-19 USING MODIFIED SIS MODEL

# Installation
This library can be intatalled using the following command:
library(devtools)
install_github("RashiMohta/COVID-19-cases-prediction")

This library can be imported using:
library(adjustedPred)

Additional libraries to be imported:
library(pracma)
library(ggplot2)

# Define constants
name_of_state = "DL"
ub_for_adjustment = 5
bound_metric = "C3_1day"
population = 18710922
gamma = 1 / 14.0
start_date = "2020-3-13"
cur_date = "2021-4-12"
cur_day = as.numeric(difftime(cur_date, start_date, units = "days"))
cur_day  = 400
last_n_day = 20
last_limit = 30
next_n_days = 20

# Read data
file_name <- "/home/rashi/BTP/btp/state_wise_daily.csv"
data <- read.csv(file_name)
state_data = data[, c("Status", "Date", name_of_state)]

# Format the date according to the dataset
for (i in 1:nrow(state_data)) {
  date <- state_data$Date[i]
  lst <- unlist(strsplit(as.character(date) , '-'))
  if (lst[2] == "Sept")
    state_data[i, 2] <-
    strcat(strcat(lst[1], "Sep", '-'), lst[3], '-')
}
colnames(state_data) <- c("Status", "Date", "Count")
date <- vector()
num <- vector()
num_adj <- vector()
for (i in 1:nrow(state_data)) {
  row <- state_data[i,]
  status <- row$Status
  if( status == "Confirmed" ){
    date <- append(date,toString(row$Date))
    num <- append(num, row$Count)      #store original values
    num_adj <- append(num_adj, row$Count) #store adjusted values
  }
}

# Obtain results
writeLines("without adjustment")
output_original <-sisd_cummulative(population, gamma, cur_day, last_n_day,last_limit, next_n_days, state_data, 0L,num,num_adj)
writeLines("\nwith adjustment")
num_adj = threshold(ub_for_adjustment, bound_metric, num, num_adj);
output_adjusted <-sisd_cummulative(population, gamma, cur_day, last_n_day,last_limit, next_n_days, state_data,1L, num,num_adj);
plot_adjustment(date, num, num_adj)
print(plot_cumulative(output_original, output_adjusted))
final = comparing_results(output_original,output_adjusted)
print(final[1])
print(paste("Validation period MSE: ", final[3]))
print(paste("Prediction period MSE: ", final[2]))

