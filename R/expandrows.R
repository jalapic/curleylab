#' Expand rows of google forms
#'
#' @param df A dataframe
#' @return The dataframe with expanded rows, as well as a score variable.
#' @examples
#' expandrows(df)
#' @section Further details:
#' Expand rows that have a variable where 2 or more factors 
#' are split by commas into separate rows with
#' all other variables duplicated.
#' Make sure columns/variables are named "Actor" and "Recipient".
#' The score variable will be '1' for Actors that are clear winners
#' and a '0.5' for ties between Actors and Recipients.
#' @export

  
expandrows <- function(df){
    
    toDrop <- Actor <- Recipient <- id <- score <- NULL  ## not sure why this is needed
    
    temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                          "Actor", ",", "long"), 
                   "Recipient", ",", "long")
    
    
    ## Convert "Actor" and "Recipient" to numeric
    SD <- c("Actor", "Recipient")
    temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
    
    
    ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
    temp[, toDrop := duplicated(
      paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
        Actor == Recipient, by = id]
    
    ## Create your "score" column
    temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
    
    ## Subset and drop the irrelevant columns
    out <- temp[!temp[, toDrop, with = TRUE]][, toDrop := NULL]
    
    return(out)
  }

  
