test <- data.frame(Year0 = 0, Year1 = -3.64, Year2 = 2.99, Year3 = 9.61, Year4 = 16.24, Year5 = 22.45)

Slope <- function(x) {
  TempDF <- data.frame(x, year = 0:5)
  lm(x ~ year, data <- TempDF)$coefficients[2]
}

TData = as.data.frame(t(test))
test$slope = sapply(TData, Slope)
test