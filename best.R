best <- function (state, outcome){
   care_meas <- read.csv("outcome-of-care-measures.csv", colClasses="character")
   if (!any(state == state.abb)){ 
   stop ("invalid state")}
   if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
   stop ("invalid outcome")}
   ## the code above just checks to see whether the arguments for best are valid or not
   state_dat <- subset(care_meas, care_meas[,7]==state)
   state_dat[,11] <- suppressWarnings(as.numeric(state_dat[,11]))
   state_dat[,17] <- suppressWarnings(as.numeric (state_dat[,17]))
   state_dat[ ,23] <- suppressWarnings(as.numeric (state_dat[,23]))
   colnames(state_dat)[11] <- "heart attack"
   colnames(state_dat)[17] <- "heart failure"
   colnames(state_dat)[23] <- "pneumonia"
   x <- cbind(state_dat[2], state_dat[outcome])
   state_dat_1 <- na.omit(x)
   o <- order(state_dat_1[,outcome])
   res <- state_dat_1[o[1],]
   print (res[,1])
}