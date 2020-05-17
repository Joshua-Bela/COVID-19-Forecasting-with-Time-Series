# Set directories for input and output file
input_directory = 'C:/Users/jgbel/Desktop/data/us-counties.csv'
output_directory = 'C:/Users/jgbel/Desktop/data/us-counties0.csv'
###########################################################################################
# Import data
data = read.csv(input_directory)
# Import libraries
library(magrittr)
library(aTSA)
library(plyr)
library(forecast)

# Create new data frame, summarized as total cases and deaths by day
data0 = ddply(
  data,
  .(date),
  summarize,
  cases_sum = sum(cases),
  deaths_sum = sum(deaths)
)

# Create data frame where each row has the three ARIMA parameters
# and the MSE (mean squared error) for the corresponding ARIMA model.
# The first row is just 0's.  It will be deleted later.
results = data.frame(
  p = 0,
  d = 0,
  q = 0,
  MSE = 0
)
# Perform a grid search for the three parameters values to find the optimal model.
for(p in 0:4){
  for(d in 0:2){
    for(q in 0:4){
      fit = try(
        {arima(data0$cases_sum, order = c(p, d, q))},
        silent = T
      )
      if(class(fit) == "try-error")
        MSE0 = NA
      else
        MSE0 = ((fit %>% residuals())^2) %>% mean()
      results = results %>% rbind(c(p, d, q, MSE0))
      print(results)
    }
  }
}
# Delete the first row of the grid search results.
results = results[-1,]
# Sort the rows so that the best models (models with the lowest MSE) are at the top.
results = results[order(results$MSE),]
# Print out information for the top 7 models.
results[1:7,]

###########################################################################################
# Write the new, engineered data frame to a CSV in the output directory.
write.csv(data0, output_directory, row.names = F)