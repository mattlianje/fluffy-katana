covid_data = read.csv(file.path(getwd(), "data", "covid_data", "covid_19_clean_complete.csv"), header = TRUE, stringsAsFactors = FALSE)
covid_data$Date <- as.Date(covid_data$Date, format="%m/%d/%y")
covid_data <- aggregate(Confirmed ~ Country.Region + Date, FUN = sum, data=covid_data)

forecast_confirmed <- function(country) {
  country_ts <- covid_data %>% filter(Country.Region == country) %>% select(c(Confirmed))
  country_ts <- ts(country_ts, start = as.Date("2020-1-22"), frequency = 365)
  fit <- auto.arima(country_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  fore <- forecast(fit, h = 30, level = 90)
  plot(fore, ylab = "Number of People", main = paste("Forecast for Number of Cases in", country), xlab = "Time", xaxt = "n")
}

forecast_confirmed("Canada")
