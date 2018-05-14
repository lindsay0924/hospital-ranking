outcome[, 11] <- as.numeric(outcome[, 11])
> ## You may get a warning about NAs being introduced; that is okay
> hist(outcome[, 11])
## Read outcome data
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that state and outcome are valid
x = state %in% data$State
outcomes <- c("heart attack", "heart failure", "pneumonia")
y = outcome %in% outcomes
if (x == FALSE){
  stop("Invalid State")
}
if (y == FALSE){
  stop("Invalid Outcome")
} 
## Return hospital name in that state with lowest 30-day a
a <- as.numeric(outcome[, 11])
b <- as.character(outcome[, 7])
k <- 1
for (i in 1:length(b)) {
  if (b(i) %in% b(i+1)) {
    k <- k+1} else {
      m <- min(a(i:i-1+k))
      n <- match (m, a)
      return(outcome[n,2])
      k <- 1
    }
  }
}