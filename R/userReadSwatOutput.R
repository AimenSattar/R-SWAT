
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
    
    # Read the file lines to handle header/unit row issues
    lines <- readLines(filePath)
    
    # Skip the header line (first line)
    if (length(lines) > 2) {
      # Create a temporary file without the first line (SWAT+ header)
      tempFile <- tempfile()
      writeLines(lines[-1], tempFile)
      
      # Read with fill=TRUE to handle mismatched columns
      output <- try(read.table(tempFile, header = TRUE, fill = TRUE), silent = TRUE)
      unlink(tempFile)
      
      # Also remove the unit row (now the first row after reading)
      if (!inherits(output, "try-error") && nrow(output) > 0) {
        output <- output[-1, ]
      } else {
        warning(paste("Could not properly read", fileName))
        return(data.frame())
      }
    } else {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    return(output)
  } else {
    # Read irrigation file using the same robust approach
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    lines <- readLines(filePath)
    
    if (length(lines) > 2) {
      tempFile <- tempfile()
      writeLines(lines[-1], tempFile)
      irrigation <- try(read.table(tempFile, header = TRUE, fill = TRUE), silent = TRUE)
      unlink(tempFile)
      
      if (!inherits(irrigation, "try-error") && "yr" %in% colnames(irrigation) && 
          "unit" %in% colnames(irrigation) && "irr" %in% colnames(irrigation)) {
        irrigation <- irrigation[-1, ] # Remove unit row
        irrigation <- irrigation %>% select(yr, unit, irr)
      } else {
        warning(paste("Failed to correctly parse irrigation file:", fileName))
        return(data.frame())
      }
    } else {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    # Read HRU file with the same robust approach
    hru_file <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), "hru-data.hru")
    hru_lines <- readLines(hru_file)
    
    if (length(hru_lines) > 3) {
      tempFile <- tempfile()
      writeLines(hru_lines[-(1:2)], tempFile) # Skip first two rows
      hru <- try(read.table(tempFile, header = TRUE, fill = TRUE), silent = TRUE)
      unlink(tempFile)
      
      if (!inherits(hru, "try-error") && "id" %in% colnames(hru) && "lu_mgt" %in% colnames(hru)) {
        hru_filtered <- hru %>%
          select(id, lu_mgt) %>%
          filter(!lu_mgt %in% c("wetw_lum", "wetn_lum", "bsvg_lum", "urhd_lum", "swrn_lum"))
      } else {
        warning("Failed to correctly parse HRU file")
        return(data.frame())
      }
    } else {
      warning(paste("HRU file has insufficient content:", hru_file))
      return(data.frame())
    }
    
    # Match irrigation data with filtered HRU data
    output <- try({
      irrigation %>%
        inner_join(hru_filtered, by = c("unit" = "id")) %>%
        mutate(lu_mgt = gsub("_lum$", "", lu_mgt)) %>%
        select(unit, yr, irr, lu_mgt)
    }, silent = TRUE)
    
    if (inherits(output, "try-error")) {
      warning("Failed to join irrigation and HRU data")
      return(data.frame())
    }
    
    return(output)
  }
}

  
