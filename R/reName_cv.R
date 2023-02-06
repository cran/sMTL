#' reName_cv: rename output from CV. For internal package use only.
#'
#' @param x A list (S3 class) supplied from internal sMTL functions 
#' @return A list (S3 class) with elements renamed. \item{best}{ A list (S3 class) with hyperparameters that achieve lowest average RMSE.} 
#' \item{best.1se}{ A list (S3 class) with hyperparameters associated with lowest sparsity level within 1 standard deviation of hyperparameters that achieve lowest average RMSE.}
#' \item{lambda_1}{ Numeric hyperparameter for L2 (ridge penalty).} \item{lambda_2}{ Numeric hyperparameter for betabar penalty.}
#' \item{rho}{ Integer specifying sparsity level (s).}
#' @export

reName_cv <- function(x){
    
    lambda1 <- lambda2 <- rho <- NULL # global variable declaration for CRAN checks
    
    # rename parameters for tuning function
    if(is.object(x$best)){
        x$best <- dplyr::rename(x$best, s = rho)
        if( "lambda1" %in% names(x$best) ){
            x$best <- dplyr::rename(x$best, lambda_1 = lambda1)
        }     
        if( "lambda2" %in% names(x$best) ){
            x$best <- dplyr::rename(x$best, lambda_2 = lambda2) 
        }  
    }
    
    # rename parameters for 1se function
    if(is.object(x$best.1se)){
        x$best.1se <- dplyr::rename(x$best.1se, s = rho)
        if( "lambda1" %in% names(x$best.1se) ){
            x$best.1se <- dplyr::rename(x$best.1se, lambda_1 = lambda1)
        }     
        if( "lambda2" %in% names(x$best.1se) ){
            x$best.1se <- dplyr::rename(x$best.1se, lambda_2 = lambda2) 
        }  
    }
    
    
    # rmse 
    if(is.object(x$rmse)){
        if( "lambda1" %in% rownames(x$rmse) )    rownames(x$rmse)[rownames(x$rmse) == "lambda1"] = "lambda_1"
        if( "lambda2" %in% rownames(x$rmse) )    rownames(x$rmse)[rownames(x$rmse) == "lambda2"] = "lambda_2"
        if( "rho" %in% rownames(x$rmse) )    rownames(x$rmse)[rownames(x$rmse) == "rho"] = "s"
    }
    
    
    # avg 
    if(is.object(x$avg)){
        if( "lambda1" %in% rownames(x$avg) )    rownames(x$avg)[rownames(x$avg) == "lambda1"] = "lambda_1"
        if( "lambda2" %in% rownames(x$avg) )    rownames(x$avg)[rownames(x$avg) == "lambda2"] = "lambda_2"
        if( "rho" %in% rownames(x$avg) )    rownames(x$avg)[rownames(x$avg) == "rho"] = "s"
    }
    
    
    return(x)
}

