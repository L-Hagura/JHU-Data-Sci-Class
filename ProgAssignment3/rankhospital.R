rankHospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% validOutcome)) {
    stop("invalid outcome")
  }
  stateData = data[which(data$State == state), ]
  if (num == "best") {
    num <- as.numeric(1)
  }
  else {
    num <- as.numeric(num)
  }
  result <- character()
  if (outcome == "heart attack") {
    desiredData <- as.numeric(stateData[ ,11])
    hospitalRank <- order(desiredData, stateData[, 2])
    if (is.na(num)) {
      num <- length(desiredData[!is.na(desiredData)])
    }
    else if (num > length(desiredData[!is.na(desiredData)])) {
      return(NA)
    }
    result <- append(result, stateData[hospitalRank[[num]], 2])
  } 
  else if (outcome == "heart failure") {
    desiredData <- as.numeric(stateData[ ,17])
    hospitalRank <- order(desiredData, stateData[, 2])
    if (is.na(num)) {
      num <- length(desiredData[!is.na(desiredData)])
    }
    else if (num > length(desiredData[!is.na(desiredData)])) {
      return(NA)
    }
    result <- append(result, stateData[hospitalRank[[num]], 2])
  } 
  else {
    desiredData <- as.numeric(stateData[ ,23])
    hospitalRank <- order(desiredData, stateData[, 2])
    if (is.na(num)) {
      num <- length(desiredData[!is.na(desiredData)])
    }
    else if (num > length(desiredData[!is.na(desiredData)])) {
      return(NA)
    }
    result <- append(result, stateData[hospitalRank[[num]], 2])
  }
  result
}