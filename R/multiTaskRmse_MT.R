#' multiTaskRmse: calculate average (across tasks) RMSE for multi-label prediction problems
#' @param data A matrix including outcome vector/matrix and design matrix to test RMSE on
#' @param K An integer of number of studies/tasks
#' @param beta A matrix of estimated beta coefficients where each task is in a different column
#' @return Returns a scalar of average (across tasks) RMSE for predictions on data provided
#' @export

# multi task RMSE for multi-task (same design matrix)
multiTaskRmse_MT <- function(data, 
                             K = NA,
                             beta){
    
    if(is.na(K)){
        XY <- substr(colnames(data), 1, 1) # extract first letter and see which one is Y
        Yindx <- which(XY == "Y") 
        K <- length( Yindx )
        
    }
    
    trainSet <- vector(length = K) # list for indices
    
    for(j in 1:K){
        
        outcomeIndx <- Yindx[j]
        
        # rmse for jth study using jth beta (i.e., jth model)
        trainSet[j] <- sqrt( mean( 
            (  data[, outcomeIndx ] - cbind(1, as.matrix( data[, -seq(1, K) ] ) ) %*% beta[,j]  )^2 
        ) 
        )
        
    }
    
    
    return( mean(trainSet) )
    
}
