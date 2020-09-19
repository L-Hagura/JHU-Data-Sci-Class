rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% validOutcome)) {
    stop("invalid outcome")
  }
  stateName <- unique(data[7])
  stateName <- stateName[order(stateName),]
  if (num == "best") {
    num <- as.numeric(1)
  }
  else if (num == "worst") {
    num <- as.numeric(-1)
  }
  else {
    num <- as.numeric(num)
  }
  if (outcome == "heart attack") {
    retrieveData(data, num, 11, stateName)
  } 
  else if (outcome == "heart failure") {##17
    retrieveData(data, num, 17, stateName)
  } 
  else {##23
    retrieveData(data, num, 23, stateName)
  }
}

retrieveData <- function(data, num, outcomeNum, stateName) {
  flag <- TRUE
  check <- FALSE
  reset <- FALSE
  for (name in stateName) {
    desiredData <- data[which(data$State == name),]
    numData <- as.numeric(desiredData[ ,outcomeNum])
    hospitalRank <- order(numData, desiredData[ ,2])
    if (num == -1) {
      num <- length(numData[!is.na(numData)])
      reset <- TRUE
    }
    else if (num > length(numData[!is.na(numData)])) {
      check <- TRUE
    }
    if (flag) {
      if (check) {
        result <- as.data.frame(desiredData[1 ,c(2,7)])
        result[1,1] <- NA
        check <- FALSE
      }
      else {
        result <- as.data.frame(desiredData[hospitalRank[[num]],c(2,7)])
      }
      flag <- FALSE
    }
    else {
      if (check) {
        result <- rbind(result, c(NA, name))
        check <- FALSE
      }
      else {
        result <- rbind(result, as.data.frame(
          desiredData[hospitalRank[[num]],c(2,7)]))
      }
    }
    if (reset) {
      num <- -1
      reset <- FALSE
    }
  }
  result
}