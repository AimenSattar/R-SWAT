
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
    
    # Read all lines of the file
    lines <- readLines(filePath)
    
    if (length(lines) <= 2) {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    # Create temporary file skipping the header line
    tempFile <- tempfile()
    writeLines(lines[-1], tempFile)
    
    # Read the file with fill=TRUE to handle column mismatches
    # Skip the units row (which is now the first row in the temp file)
    df <- try(read.table(tempFile, header = TRUE, skip = 1, fill = TRUE), silent = TRUE)
    unlink(tempFile)
    
    if (inherits(df, "try-error")) {
      warning(paste("Failed to read file:", fileName))
      return(data.frame())
    }
    
    return(df)
  } else {
    # For irrigation file
    filePath <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), fileName)
    lines <- readLines(filePath)
    
    if (length(lines) <= 2) {
      warning(paste("File has insufficient content:", filePath))
      return(data.frame())
    }
    
    # Process lsunit file
    tempFile <- tempfile()
    writeLines(lines[-1], tempFile)
    
    # Skip the units row and use fill=TRUE
    irrigation <- try(read.table(tempFile, header = TRUE, skip = 1, fill = TRUE), silent = TRUE)
    unlink(tempFile)
    
    if (inherits(irrigation, "try-error") || 
        !all(c("yr", "unit", "irr") %in% colnames(irrigation))) {
      warning(paste("Failed to read irrigation data properly from:", fileName))
      return(data.frame())
    }
    
    # Select columns explicitly to avoid dplyr dependency issues
    irrigation <- irrigation[, c("yr", "unit", "irr")]
    
    # Process HRU file with the same approach
    hru_file <- paste0(workingDirectory, paste0("/TxtInOut_", coreNumber, "/"), "hru-data.hru")
    hru_lines <- readLines(hru_file)
    
    if (length(hru_lines) <= 3) {
      warning(paste("HRU file has insufficient content:", hru_file))
      return(data.frame())
    }
    
    # Find the header line
    header_line <- grep("id\\s+name", hru_lines)[1]
    if (is.na(header_line)) header_line <- 2 # Default if not found
    
    # Extract relevant lines
    hru_data_lines <- c(hru_lines[header_line], hru_lines[(header_line+2):length(hru_lines)])
    
    tempFile <- tempfile()
    writeLines(hru_data_lines, tempFile)
    
    hru <- try(read.table(tempFile, header = TRUE, fill = TRUE), silent = TRUE)
    unlink(tempFile)
    
    if (inherits(hru, "try-error") || 
        !all(c("id", "lu_mgt") %in% colnames(hru))) {
      warning("Failed to read HRU data properly")
      return(data.frame())
    }
    
    # Filter without dplyr
    hru_filtered <- hru[!(hru$lu_mgt %in% c("wetw_lum", "wetn_lum", "bsvg_lum", "urhd_lum", "swrn_lum")), 
                        c("id", "lu_mgt")]
    
    # Join data manually to avoid dplyr dependencies
    merged <- merge(irrigation, hru_filtered, by.x = "unit", by.y = "id")
    
    # Remove _lum suffix
    merged$lu_mgt <- gsub("_lum$", "", merged$lu_mgt)
    
    # Select final columns
    result <- merged[, c("unit", "yr", "irr", "lu_mgt")]
    
    return(result)
  }
}
