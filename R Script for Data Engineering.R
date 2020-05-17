input_directory = 'C:/Users/jgbel/Desktop/data/us-counties.csv'
output_directory = 'C:/Users/jgbel/Desktop/data/us-counties0.csv'
###########################################################################################
data = read.csv(input_directory)
library(magrittr)
library(aTSA)
library(plyr)
library(forecast)

data0 = ddply(
  data,
  .(date),
  summarize,
  cases_sum = sum(cases),
  deaths_sum = sum(deaths)
)

results = data.frame(
  p = 0,
  d = 0,
  q = 0,
  MSE = 0
)
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
results = results[-1,]
results = results[order(results$MSE),]
results[1:7,]

###########################################################################################
write.csv(data0, output_directory, row.names = F)