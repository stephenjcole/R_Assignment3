best <- function (state, outcome) {
        ## read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # make a list of valid states
        state_list <- unique (data[,7])            
        
        # make a list of valid parameters for outcome
        valid_parms <- c("heart attack", "heart failure", "pneumonia")
        
        ## check validity of state and outcome parameters
        if(!(state %in% state_list)) stop("invalid state")
        if(!(outcome %in% valid_parms)) stop("invalid outcome")
        
        
        ## Return hospital name in the state with the lowest 30-day death rate
        
        # make analysis dataset for the selected state that contains
        # hospital name (2), heart attack (11), heart failure (17) and pneumonia (23)
        analysis_data <- subset(data, data[,7] == state, select =c(2, 11, 17, 23))
        colnames(analysis_data) <- c("name", "heart attack", "heart failure", 
                                     "pneumonia")
        
        # coerce the variables to be numeric
        for(i in 2:4) {
                analysis_data[,i] <- as.numeric(analysis_data[,i])
        }
        
        # order by outcome ascending, name ascending
        analysis_data <- analysis_data[order(analysis_data[,outcome], 
                                             analysis_data[,"name"]),]
        
        
        # return the hospital name in the first row
        analysis_data[1,"name"]
        
        
}