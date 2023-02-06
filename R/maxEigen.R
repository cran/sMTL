#' maxEigen: maximum eigenvalue wrapper for Julia TSVD package. internal package use only
#'
#' @param X A matrix.
#' @param intercept A boolean.
#' @return A numeric scalar of the maximum eigenvalue of provided matrix, X.
#' @import JuliaConnectoR
#' @export


maxEigen = function(X, intercept = TRUE) {
    
    # find julia file
    smtl_path <- .libPaths("sMTL")
    smtl_path <- paste0(smtl_path, "/sMTL/julia/")
        
    maxEgn <- juliaCall("include", paste0(smtl_path, "eigenFn.jl") ) # max eigenvalue
    
    maxEgn(X = X, intercept = intercept)
    
}
