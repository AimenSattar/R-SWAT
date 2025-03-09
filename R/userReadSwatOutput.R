
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


userReadSwatOutput <- function(workingDirectory, coreNumber, fileName, output) {
  if (fileName == "basin_crop_yld_yr.txt") {
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    
    # Read directly using the simplified approach
    df <- read.table(filePath, header = FALSE, stringsAsFactors = FALSE, fill = TRUE, skip = 1)
    colnames(df) <- df[1,]
    df <- df[-1,]
    
    return(df)
  } else {
    # For irrigation file (lsunit_wb_yr.txt)
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    
    # Read irrigation file
    irrigation <- read.table(filePath, header = FALSE, stringsAsFactors = FALSE, fill = TRUE, skip = 1)
    colnames(irrigation) <- irrigation[1,]
    irrigation <- irrigation[-1,]
    
    # Select required columns using dplyr
    irrigation <- irrigation %>% 
      dplyr::select(yr, unit, irr)%>%
    mutate(unit= as.numeric(unit))
    
    # Read HRU file - simpler approach
    hru_file <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), "hru-data.hru")
    hru <- read.table(hru_file, header = TRUE, skip = 1)
    
    # Filter unwanted land use types using dplyr
    hru_filtered <- hru %>%
      dplyr::select(id, lu_mgt) %>%
      dplyr::filter(!lu_mgt %in% c("wetw_lum", "wetn_lum", "bsvg_lum", "urhd_lum", "swrn_lum"))%>%
    mutate(id= as.numeric(id))
    
    # Join irrigation and HRU data using dplyr inner_join
    output <- irrigation %>%
      dplyr::inner_join(hru_filtered, by = c("unit" = "id")) %>%
      dplyr::mutate(lu_mgt = gsub("_lum$", "", lu_mgt)) %>%
      dplyr::select(unit, yr, irr, lu_mgt)
    
    return(output)
  }
}
