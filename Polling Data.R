library("pollstR");
chart <- pollster_chart('2016-general-election-trump-vs-clinton')
Candidates <-  chart$estimates$choice[1]
Candidates[2] <-  chart$estimates$choice[2]
Dates <- chart$estimates_by_date$date
Actual_Dates <- vector(length = length(Dates)/4)
Clinton_Numbers <- vector(length = length(Dates)/4)
Trump_Numbers <- vector(length = length(Dates)/4)
side = 1

for (i in 0:length(Dates)/4) {
  Actual_Dates[i] <- Dates[(i*4) +1]
}
Actual_Dates <- as.Date(Actual_Dates)

for (i in 1:(length(chart$estimates_by_date$value)/4))
  {
  Trump_Numbers[i] <- chart$estimates_by_date$value[(i*4) + 1]
  Clinton_Numbers[i] <- chart$estimates_by_date$value[(i*4) + 2]
  }

Trump_Numbers
Clinton_Numbers

