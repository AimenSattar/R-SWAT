
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
  # Explicitly load required package
  library(dplyr)
  
  if (fileName == "basin_crop_yld_yr.txt") {
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    
    # Read all lines to handle SWAT+ header structure
    lines <- readLines(filePath)
    
    if (length(lines) > 2) {
      # Create a temporary file without header and unit rows
      tempFile <- tempfile()
      writeLines(lines[-1], tempFile)
      
      # Read data skipping the units row
      output <- read.table(tempFile, header = TRUE, skip = 1, fill = TRUE)
      unlink(tempFile)
    } else {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    return(output)
  } else {
    # For irrigation data (lsunit file)
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    
    # Read all lines
    lines <- readLines(filePath)
    
    if (length(lines) > 2) {
      # Create a temporary file without header
      tempFile <- tempfile()
      writeLines(lines[-1], tempFile)
      
      # Read data skipping the units row
      irrigation <- read.table(tempFile, header = TRUE, skip = 1, fill = TRUE)
      unlink(tempFile)
      
      # Select needed columns - using explicit namespace reference
      if (all(c("yr", "unit", "irr") %in% colnames(irrigation))) {
        irrigation <- dplyr::select(irrigation, yr, unit, irr)
      } else {
        warning(paste("Missing required columns in irrigation file:", 
                     paste(setdiff(c("yr", "unit", "irr"), colnames(irrigation)), collapse=", ")))
        return(data.frame())
      }
    } else {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    # Read HRU file
    hru_file <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), "hru-data.hru")
    hru_lines <- readLines(hru_file)
    
    if (length(hru_lines) > 3) {
      # Identify header line position - SWAT+ header can vary
      header_line_idx <- grep("id\\s+name", hru_lines)[1]
      if (is.na(header_line_idx)) {
        header_line_idx <- 2 # Default if not found
      }
      
      # Create a temporary file with just data and header
      tempFile <- tempfile()
      writeLines(hru_lines[c(header_line_idx, (header_line_idx+2):length(hru_lines))], tempFile)
      
      # Read HRU data
      hru <- read.table(tempFile, header = TRUE, fill = TRUE)
      unlink(tempFile)
      
      # Filter and select needed columns
      if (all(c("id", "lu_mgt") %in% colnames(hru))) {
        hru_filtered <- hru %>%
          dplyr::select(id, lu_mgt) %>%
          dplyr::filter(!lu_mgt %in% c("wetw_lum", "wetn_lum", "bsvg_lum", "urhd_lum", "swrn_lum"))
      } else {
        warning("Missing required columns in HRU file")
        return(data.frame())
      }
    } else {
      warning(paste("HRU file has insufficient content:", hru_file))
      return(data.frame())
    }
    
    # Match irrigation data with filtered HRU data
    output <- tryCatch({
      irrigation %>%
        dplyr::inner_join(hru_filtered, by = c("unit" = "id")) %>%
        dplyr::mutate(lu_mgt = gsub("_lum$", "", lu_mgt)) %>%
        dplyr::select(unit, yr, irr, lu_mgt)
    }, error = function(e) {
      warning(paste("Error joining irrigation and HRU data:", e$message))
      return(data.frame())
    })
    
    return(output)
  }
}
