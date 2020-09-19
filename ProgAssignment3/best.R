best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% validOutcome)) {
    stop("invalid outcome")
  }
  stateData = data[which(data$State == state), ]
  if (outcome == "heart attack") {
    desiredData <- as.numeric(stateData[ ,11])
    bestHospital <- stateData[which(as.numeric(stateData[,11]) 
                             == min(desiredData, na.rm = TRUE)),2]
    bestHospital[[1]]
  } 
  else if (outcome == "heart failure") {
    desiredData <- as.numeric(stateData[ ,17])
    bestHospital <- stateData[which(as.numeric(stateData[,17]) 
                                    == min(desiredData, na.rm = TRUE)),2]
    bestHospital[[1]]
  } 
  else {
    desiredData <- as.numeric(stateData[ ,23])
    bestHospital <- stateData[which(as.numeric(stateData[,23]) 
                                    == min(desiredData, na.rm = TRUE)),2]
    bestHospital[[1]]
  }
}