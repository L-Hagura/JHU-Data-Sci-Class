source("complete.R")
corr <- function(directory, threshold = 0) {
  dataList <- list.files(directory)
  covariant <- numeric(0)
  completeDataFrame = complete(directory)
  for (i in 1: nrow(completeDataFrame)) {
    numCases <- completeDataFrame[i, "nobs"]
    if (numCases > threshold) {
      id <- completeDataFrame[i, "id"]
      data <- read.csv(file.path(directory, dataList[id]))
      clean <- complete.cases(data)
      cleanData <- data[clean, ]
      covariant <- c(covariant, 
                     cor(cleanData[["sulfate"]], cleanData[["nitrate"]]))
    }
    else {
      next
    }
  }
  covariant 
}