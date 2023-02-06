#' smtl_setup: setup Julia path and/or install Julia or Julia packages using functions based on external package JuliaCall::julia_setup().
#'
#' @param path A string
#' @param installJulia A boolean.
#' @param installPackages A boolean.
#' @return A message indicating either Julia language or package installation status or the path of Julia Binary on your computer. See vignette if you have problems
#' specifying the path of Julia binary correctly.
#' @examples
#' 
#' \dontrun{
#' 
#' if (identical(Sys.getenv("AUTO_JULIA_INSTALL"), "true")) { ## The examples are quite time consuming
#' ## Do initiation for and automatic installation if necessary
#' ##################################################################
#' # First Time Loading, Julia is Installed and Julia Path is Known 
#' ##################################################################
#' smtl_setup(path = "/Applications/Julia-1.5.app/Contents/Resources/julia/bin", 
#'            installJulia = FALSE, 
#'            installPackages = FALSE)
#' 
#' #####################################################################################
#' # If you have run smtl_setup() before, then path specification shouldn't be necessary
#' #####################################################################################
#' smtl_setup(path = NULL, installJulia = FALSE, installPackages = FALSE)
#' 
#' #####################################################################################
#' ##### First Time Loading, Julia is Not Installed   ######
#' #####################################################################################
#' smtl_setup(path = NULL, installJulia = TRUE, installPackages = FALSE)
#' 
#' #####################################################################################
#' ##### First Time Loading, Julia is Installed But Packages NEED INSTALLATION  ######
#' #####################################################################################
#' smtl_setup(path = "/Applications/Julia-1.5.app/Contents/Resources/julia/bin", 
#'            installJulia = TRUE, 
#'            installPackages = TRUE)
#'            
#'            }
#'            
#'  }
#'            
#' @import JuliaCall
#' @export

smtl_setup = function(path = NULL, installJulia = FALSE, installPackages = FALSE) {
  
  # use code from XRJulia to find Julia path
  if(!installJulia & is.null(path)){
    
    # look at path variable in julia_path folder in package for path
    
    # find path julia file
    # smtl_path <- paste0( .libPaths("sMTL"), "/sMTL/julia_path/" )
    # smtl_path <- smtl_path[length(smtl_path)] # use most recent path if there are multiple saved
    
    # read julia path
    julia_path <- Sys.getenv("JULIA_HOME") #as.character( utils::read.table(paste0(smtl_path, "julia.path.txt"),  stringsAsFactor = FALSE ) )
    
    if(julia_path == "NA" | julia_path == ""){
      # if no path given and julia is not to be installed and the path has not been updated then use code from XRJulia to find path
      findJulia <- function(test = FALSE) {
        ## See if a location for the Julia executable has been specified
        ## environment variables JULIA_BIN or JULIA_SRC
        envvar <- Sys.getenv("JULIA_BIN")
        if(!nzchar(envvar)) {
          src <- Sys.getenv("JULIA_SRC")
          if(nzchar(src)) {
            ## try either the standard Julia source, or the directory above bin, etc.
            trybin <- paste(src, "usr","bin","julia", sep=.Platform$file.sep)
            if(file.exists(trybin))
              envvar <- trybin
            else {
              trybin <- paste(src, "julia", sep=.Platform$file.sep)
              if(file.exists(trybin))
                envvar <- trybin
            }
          }
        } # if none of these succeeds, `which julia` used to find an executable version
        if(!nzchar(envvar)) {
          command <-if (.Platform$OS.type == "windows") "where" else "which"
          envvar <- tryCatch(system2(command, "julia", stdout = TRUE), warning = function(e) "")
          if(test)
            Sys.setenv(JULIA_BIN = envvar) # so next call finds this immediately
          else if(!nzchar(envvar))
            stop("No julia executable in search path and JULIA_BIN environment variable not set")
        }
        if(test)
          nzchar(envvar)
        else
          envvar
      }
      
      # julia_path <- findJulia() # use this to find path

      return( message(paste0("Julia binary path not specified. If Julia is not install then 
                                  1) Install Julia by reruning smtl_setup() and set installJulia = TRUE \n 
                                  OR 2) Rerun smtl_setup() and specify path: smtl_setup(installJulia = FALSE, path = <path>) \n
                                   --Run the following in Julia to find the path name: > println(Sys.BINDIR) and specify the path in 2)") ) )
    }else{
      
      # if path works
      Sys.setenv(JULIA_BINDIR = julia_path )
      
      return( message( paste0("Julia binary path Loaded: ", path, 
                              "If correct, proceed with package use! \n",
                              "\n  If this is not the path you intended, reset the Julia binary path to avoid having to specify with each sMTL package load: 
                            1) Open Julia and find the Julia binary path by executing the comamnd:  > println(Sys.BINDIR)
                            2) In R, execute R command:   > usethis::edit_r_environ() 
                            3) Add the line: > JULIA_HOME = <>    \n --where <> is string of Julia binary path 
                            4) Make sure there is a blank line at the end of .Renviorn file. Save and close") )  )
      
    }
    
  }
  
  # if install=TRUE, use JuliaCall to install
  if(installJulia){
    JuliaCall::julia_setup(installJulia = TRUE, JULIA_HOME = path) # which will invoke install_julia automatically if Julia is not found and also do initialization of JuliaCall.
    
    message("Julia Installed")
    
    # install packages
    JuliaCall::julia_install_package_if_needed("TSVD")
    JuliaCall::julia_install_package_if_needed("Statistics")
    JuliaCall::julia_install_package_if_needed("LinearAlgebra")
    
    message("Julia Packages Installed")
    
    # find the path for the future
    a <- JuliaCall::julia_command("a = Sys.BINDIR;")
    juliaPath <- JuliaCall::julia_eval("a")
    
    # find path of sMTL package
    smtl_path <- paste0( .libPaths("sMTL"), "/sMTL/julia_path/" )
    smtl_path <- smtl_path[length(smtl_path)] # use most recent path if there are multiple saved
    
    message( paste0("The Julia binary path is: ", path, 
            "\n  We recommend setting Julia binary path to avoid having to specify with each sMTL package load: \n
                            1) Execute R command:   > usethis::edit_r_environ() 
                            2) Add line: > JULIA_HOME = <>    \n --where <> is string of Julia binary path 
                            3) Make sure there is a blank line at the end of .Renviorn file. Save and close") ) 
    # save Julia path for the future in sMTL package directory for future use
    # fileConn <- file( paste0(smtl_path, "julia.path.txt") )
    # base::writeLines(juliaPath, fileConn) # save new julia path
    # close(fileConn)
    
    # close Julia session
    # detach("package:JuliaCall", unload=TRUE) # unload JuliaCall package
    Sys.setenv(JULIA_BINDIR = juliaPath ) # set path for JuliaConnectoR
    
  }else{
    
    # if Julia is not to be installed but packages are
    if(installPackages){
      
      message("Installing Julia Packages, Make Sure Path is Correctly Specified")
      
      JuliaCall::julia_setup(installJulia = FALSE, 
                             JULIA_HOME = path) # which will invoke install_julia automatically if Julia is not found and also do initialization of JuliaCall.
      
      # install packages
      JuliaCall::julia_install_package_if_needed("TSVD")
      JuliaCall::julia_install_package_if_needed("Statistics")
      JuliaCall::julia_install_package_if_needed("LinearAlgebra")
      
      message("Julia Packages Installed")
      
      # detach("package:JuliaCall", unload=TRUE) # unload JuliaCall package
      Sys.setenv(JULIA_BINDIR = path ) # set path for JuliaConnectoR
      
    }
  }
  
  # if path given
  if( !installJulia & !is.null(path) ){
    Sys.setenv(JULIA_BINDIR = path ) # set path for JuliaConnectoR
    
    # find path of sMTL package
    smtl_path <- paste0( .libPaths("sMTL"), "/sMTL/julia_path/" )
    smtl_path <- smtl_path[length(smtl_path)] # use most recent path if there are multiple saved
    
    # save Julia path for the future in sMTL package directory for future use
    # fileConn <- file( paste0(smtl_path, "julia.path.txt") )
    # base::writeLines(path, fileConn) # save new julia path
    # close(fileConn)
    message(paste0("Julia Path Loaded Successfully: No Installation of Julia Necessary \n
                            Julia Path: ", path) )
    
    
    return( message( paste0("Julia Path: ", path, 
                            "\n  We recommend setting Julia binary path to avoid having to specify with each sMTL package load: \n
                            1) Execute the R command:   usethis::edit_r_environ() 
                            2) Add the line: JULIA_HOME = <>    where <> is string of Julia binary path
                            3) Make sure there is a blank line at the end of .Renviorn file. Save and close") )  )
  }
  
}