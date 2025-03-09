
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

library(dplyr)

userReadSwatOutput <- function(workingDirectory, coreNumber, fileName, output) {
  if (fileName == "basin_crop_yld_yr.txt") {
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    output <- read.table(filePath, header = TRUE, sep = "", skip = 1)
    return(output)
  } else {
    # Read irrigation file
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    Lines <- readLines(filePath)
    Lines <- Lines[-1]
    writeLines(Lines, filePath)
    irrigation <- read.table(filePath, header = TRUE, sep = "", skip = 1)
    irrigation <- irrigation %>% select(yr, unit, irr)
    
    # Read HRU file
    hru_file <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), "hru-data.hru")
    hru <- read.table(hru_file, header = TRUE, sep = "", skip = 1)
    
    # Process HRU data
    hru_filtered <- hru %>%
      select(id, lu_mgt) %>%
      filter(!lu_mgt %in% c("wetw_lum", "wetn_lum", "bsvg_lum", "urhd_lum", "swrn_lum"))
    
    # Match irrigation data with filtered HRU data
    output <- irrigation %>%
      inner_join(hru_filtered, by = c("unit" = "id")) %>%
      mutate(lu_mgt = gsub("_lum$", "", lu_mgt)) %>%
      select(unit, yr, irr, lu_mgt)
    
    return(output)
  }
}

  
