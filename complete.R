complete <- function(directory, id = 1:332) {
  dataList <- list.files(directory)
  completeDataFrame <- data.frame(matrix(ncol=2,nrow=0))
  for (i in id) {
    data <- read.csv(file.path(directory, dataList[i]))
    completeData <- data[complete.cases(data), ]
    rows <- c(i, nrow(completeData))
    completeDataFrame <- rbind(completeDataFrame, 
                               rows)
  }
  colnames(completeDataFrame) <- c("id", "nobs")
  completeDataFrame
}