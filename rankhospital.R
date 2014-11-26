rankhospital <- function (state, outcome, rank = "best") {
        ## read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # make a list of valid states
        state_list <- unique (data[,7])            
        
        # make a list of valid parameters for outcome
        valid_parms <- c("heart attack", "heart failure", "pneumonia")
        valid_rank_words <- c("best", "worst")
        
        ## check validity of state and outcome parameters
        if(!(state %in% state_list)) stop("invalid state")
        if(!(outcome %in% valid_parms)) stop("invalid outcome")
        if(is.character(rank) & !(rank %in% valid_rank_words)) stop("invalid rank")
        
        
        # make analysis dataset for the selected state that contains
        # hospital name (2), heart attack (11), heart failure (17) and pneumonia (23)
        analysis_data <- subset(data, data[,7] == state, select =c(2, 11, 17, 23))
        colnames(analysis_data) <- c("name", "heart attack", "heart failure", 
                                     "pneumonia")
        
        # coerce the variables to be numeric
        for(i in 2:4) {
                analysis_data[,i] <- as.numeric(analysis_data[,i])
        }
        
        # drop NAs
        analysis_data <- subset(analysis_data, !is.na(analysis_data[,outcome]))
        
        
        # order by outcome ascending, name ascending
        
        analysis_data <- analysis_data[order(analysis_data[,outcome], 
                                             analysis_data[,"name"]),]
       
        
        
        # if rank is numeric, return the name of the hospital in that position
        # ?check less than or equal to number of rows?
        
        if(is.numeric(rank)) return (analysis_data[rank,"name"])
                else if(rank == "best") return (analysis_data[1,"name"])
                else if(rank == "worst") 
                        return (analysis_data[nrow(analysis_data), "name"])
                else return (NULL)
        
        
}