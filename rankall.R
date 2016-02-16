rankall <- function(outcome, num="best"){
    care_meas <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
        stop ("invalid outcome")}
    ## explicit coercion of outcome columns to numeric
   care_meas[,11] <- suppressWarnings(as.numeric(care_meas[,11]))
   care_meas[,17] <- suppressWarnings(as.numeric (care_meas[,17]))
   care_meas[ ,23] <- suppressWarnings(as.numeric (care_meas[,23]))
   ##rename column names as heart attack, heart failure and pneumonia. one of these is the outcome
   ##variable above
   colnames(care_meas)[11] <- "heart attack"
   colnames(care_meas)[17] <- "heart failure"
   colnames(care_meas)[23] <- "pneumonia"
   ##create state vector with factors from the state column 
   state <- factor(care_meas[,7])
   ##bind factor column to hospital and outcome column and remove na's, give each column a name
   x <- cbind(care_meas [2], state, care_meas[outcome])
   x <- na.omit(x)
   colnames(x) <- c("hospital", "state", "mort")
   ## split the dataframe by state, this gives you a list of smaller data frames
   z <- split(x, x$state)
   ## use lapply to sort each of the dataframes in the list by outcome
   zz <- lapply(z, function(x) x[order(x$mort, x$hospital),])
   ## below I tried to set up loops to deal with each of the function arguments provided 
   ## still maybe one or two issues that need to be fixed. 
   if (is.numeric(num)){
     resn <- lapply(zz, function(x) x[num,])
     resn <- do.call(rbind.data.frame, resn)
     as.data.frame (resn[, c(1,2)])
   }
   else if (num=="worst"){
       resw <- lapply (zz, function (x) tail(x, 1))
       resw <- do.call(rbind.data.frame, resw)
       print(resw[, c(1,2)]) 
   }
   if (num=="best") {
       res <- lapply(zz, function(x) head(x, 1)) 
       res <- do.call(rbind.data.frame, res)
       print(res[, c(1,2)])
   }          
}