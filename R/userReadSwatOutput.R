
#' User-defined output extraction
#'
#' @description
#' This is a dummy function. As SWAT/SWAT+ have many outputs, RSWAT only provides
#' some standard output extraction. Users want to extract other model outputs can
#' implement their own function and replace this function. For more detail see
#' RSWAT vignettes (userReadSwatOutput).
#'
#' @examples
#'
#' # Please see RSWAT vignettes for an example
#'
#' @export


   userReadSwatOutput <- function(workingDirectory,
                               coreNumber,
                               fileName,
                               output){
    
    
    filePath <- paste0("/scratch/s2110964/",workingDirectory, paste0("/TxtInOut_", coreNumber,"/"), fileName)
    output <- read.table(filePath, header = FALSE, sep = "", skip = 2)
    return(output)
}
  
