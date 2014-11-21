best <- function(state, outcome) {
        # --- Init loading outcome data
        outcomeDfr <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # --- Coerce character into numeric
        suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
        suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
        suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
        
        # --- Create a data frame of freq by state Remove row.names
        tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
                                                    length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
        rownames(tableDfr) <- NULL
        
        # --- Create a data frame of possible inputs and respective columns
        inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                               Col = c(11, 17, 23))
        
        # --- Check that state and outcome are valid
        if (nrow(tableDfr[tableDfr$State == state, ]) == 0) 
                stop("invalid state")
        if (nrow(inputDfr[inputDfr$Outcome == outcome, ]) == 0) 
                stop("invalid outcome")
        
        # --- Return hospital name in that state with lowest THIRTY(30)-day death
        # rate Create a data frame with given ONE (1) state Determine the relevant
        # row and column
        stateDfr <- outcomeDfr[outcomeDfr$State == state, ]
        colNum <- inputDfr[inputDfr$Outcome == outcome, 2]
        rowNum <- which.min(stateDfr[, colNum])
        return(stateDfr[rowNum, ]$Hospital.Name)
}