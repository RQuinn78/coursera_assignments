rankhospital <- function (state, outcome, num= "best"){
    care_meas <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    if (!any(state == state.abb)){ 
        stop ("invalid state")}
    if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
        stop ("invalid outcome")}
    ## the code above just checks to see whether the arguments for best are valid or not
    state_dat <-subset(care_meas, care_meas[,7]==state)
    state_dat[,11] <- suppressWarnings(as.numeric(state_dat[,11]))
    state_dat[,17] <- suppressWarnings(as.numeric (state_dat[,17]))
    state_dat[ ,23] <- suppressWarnings(as.numeric (state_dat[,23]))
    colnames(state_dat)[11] <- "heart attack"
    colnames(state_dat)[17] <- "heart failure"
    colnames(state_dat)[23] <- "pneumonia"
    x <- cbind(state_dat[2], state_dat[7], state_dat[outcome])
    x <- na.omit(x)
    y <- x[order(x[,3], x$Hospital.Name),]
    z<- 1:nrow(x)
    ans <- cbind(y,z)
    colnames(ans)[4] <- "Rank"
    if (num == "best"){
        print (ans[1,1])
    }
    
    else if (num == "worst"){
        j <- tail(ans, 1)
        print (j[1,1])
    }
    else if (is.element(num, z)){
        k<- subset (ans, ans[["Rank"]]==num)
        print (k[1,1])
    }
    else if (num > tail(z, 1)){
    print ("NA")
    }
    
}
