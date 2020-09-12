pollutantmean <- function(directory, pollutant, 
                          id = 1:332) {
  dataList <- list.files(directory)
  valuePollutant <- c()
  for (i in id) {
    data <- read.csv(file.path(directory, dataList[i]))
    valuePollutant <- c(valuePollutant, 
                        data[[pollutant]])
  }
  meanAll = mean(valuePollutant, na.rm = TRUE)
  meanAll
}