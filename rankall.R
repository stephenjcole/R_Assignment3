rankall <- function (outcome, rank = "best") {
        ## read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # make a list of valid states
        state_list <- as.matrix(unique (data[,7]))
        state_list <- state_list[order(state_list[,1]),]
        
        # make a list of valid parameters for outcome
        valid_parms <- c("heart attack", "heart failure", "pneumonia")
        valid_rank_words <- c("best", "worst")
        
        ## check validity of rank and outcome parameters
        if(!(outcome %in% valid_parms)) stop("invalid outcome")
        if(is.character(rank) & !(rank %in% valid_rank_words)) stop("invalid rank")
        
        # initialize the result data frame
        results <- data.frame(hospital = NULL, state = NULL, stringsAsFactors = F)
        
        # Loop over each state
        
        for (state in state_list) {
                
        
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
       
                # initialize the output name
                
                out_name <- NULL
        
                # if rank is numeric, set the output name to the corresponding row
                # otherwise, set it to best or worst
                # if all else fails, set it to NULL (shouldn't happen)
          
                if(is.numeric(rank)) out_name <- analysis_data[rank,"name"]
                        else if(rank == "best") out_name <- analysis_data[1,"name"]
                        else if(rank == "worst") out_name <- analysis_data[nrow(analysis_data), "name"]
                        else out_name <- NULL
                
                temp <- data.frame( hospital = out_name, state = state, stringsAsFactors = F)
                
                results <- rbind (results, temp)
        }
        
        # Return results
        
        return(results)
}