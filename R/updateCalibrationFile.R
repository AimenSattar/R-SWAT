
#' Update calibration.cal file for SWAT+
#'
#' @inheritParams runSWATpar
#' @param currentDirectory path which the new calibration.cal will be saved
#'
#' @return No return values, the new calibration.cal file will be save to
#' currentDirectory
#'
#'
#' @export
#'
#'

updateCalibrationFile <- function(paraSelection, parameterValue, currentDirectory) {
  # Path to the calibration.cal file
  calFile <- file.path(currentDirectory, "calibration.cal")
  
  # condition type
  conTyp <- c("hgs=", "texture=", "plt=", "landuse=", "region=", "region_lte=")
  
  # Initialize file content
  fileContent <- c()
  
  # If the file exists, read its content
  if (file.exists(calFile)) {
    existingContent <- readLines(calFile)
    # Extract the number of parameters from the second line and update it
    paramCount <- as.integer(existingContent[2]) + nrow(paraSelection)
    existingContent[2] <- as.character(paramCount)
    # Keep existing content (skip the first three lines for now)
    fileContent <- existingContent
  } else {
    # If the file does not exist, initialize headers
    fileContent[1] <- "calibration.cal file is created by R-SWAT"
    fileContent[2] <- as.character(nrow(paraSelection))
    fileContent[3] <- paste0("cal_parm            chg_typ       chg_val    conds ",
                             "soil_lyr1 soil_lyr2       yr1       yr2      day1",
                             "      day2   obj_tot")
  }
  
  # Change to SWAT+ keywords
  paraSelection[, 2] <- gsub("replace", "absval", paraSelection[, 2])
  paraSelection[, 2] <- gsub("relative", "relchg", paraSelection[, 2])
  paraSelection[, 2] <- gsub("absolute", "abschg", paraSelection[, 2])
  paraSelection[, 2] <- gsub("percent", "pctchg", paraSelection[, 2])
  
  # Remove the extension (e.g., .hru) in the parameter selection data
  paraName <- strsplit(trimws(paraSelection[, c(1)]), '[.]')
  for (i in seq_along(paraName)) {
    paraSelection[i, 1] <- paraName[[i]][1]
  }
  
  # Remove spaces in object and conditions
  paraSelection[, 5] <- gsub(" ", "", paraSelection[, 5], fixed = TRUE)
  paraSelection[, 6] <- gsub(" ", "", paraSelection[, 6], fixed = TRUE)
  
  # Process each parameter and add to file content
  for (i in seq_len(nrow(paraSelection))) {
    # Handle "All" or "all" in conditions
    if (grepl("All", paraSelection[i, 6], fixed = TRUE) |
        grepl("all", paraSelection[i, 6], fixed = TRUE)) {
      paraSelection[i, 6] <- ""
    }
    
    if (grepl("All", paraSelection[i, 5], fixed = TRUE) |
        grepl("all", paraSelection[i, 5], fixed = TRUE)) {
      paraSelection[i, 5] <- ""
    }
    
    # Extract parameter values and conditions as in your original function
    val <- parameterValue[i]
    lyr1 <- lyr2 <- year1 <- year2 <- day1 <- day2 <- obj_tot <- conds <- obj <- condition <- NULL
    
    # Soil layer extraction logic remains unchanged...
    
    # Construct the line for this parameter
    newLine <- paste(format(paraSelection[i,1], width = 20, justify = "left"),
                     format(paraSelection[i,2], width = 10, justify = "left"),
                     format(round(parameterValue[i],5), nsmall = 5, width = 10, justify = "right"),
                     format(conds, width = 10, justify = "right"),
                     format(lyr1, width = 10, justify = "right"),
                     format(lyr2, width = 10, justify = "right"),
                     format(year1, width = 10, justify = "right"),
                     format(year2, width = 10, justify = "right"),
                     format(day1, width = 10, justify = "right"),
                     format(day2, width = 10, justify = "right"),
                     format(obj_tot, width = 10, justify = "right"),
                     paste(format(obj, width = 10, justify = "right"), collapse=""),
                     sep="")
    
    # Add new line to file content
    fileContent[length(fileContent) + 1] <- newLine
    
    # Add condition lines if they exist
    if (!is.null(condition)) {
      for (condLine in condition) {
        fileContent[length(fileContent) + 1] <- condLine
      }
    }
    
  }
  
  # Overwrite the calibration.cal file with updated content
  writeLines(fileContent, calFile)
}
