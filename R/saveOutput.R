#' Save output of SWAT/SWAT+ after each model run
#'
#' @inherit runSWATpar return
#' @inheritParams runSWATpar
#' @inheritParams readOutputRchFile
#' @importFrom utils write.table
#' @param fileType character, type of the file that are being read to extract
#' output, possible values are:
#' "watout.dat" \cr
#' "output.rch" \cr
#' "output.sub" \cr
#' "output.hru" \cr
#' "channel_sd_day.txt" \cr
#' "channel_sd_mon.txt" \cr
#' "channel_sd_yr.txt" \cr
#' "channel_sdmorph_day.txt"  \cr
#' "channel_sdmorph_mon.txt" \cr
#' "channel_sdmorph_yr.txt" \cr
#' "lsunit_wb_day.txt"  \cr
#' "lsunit_wb_mon.txt" \cr
#' "lsunit_wb_yr.txt"  \cr
#' "basin_wb_day.txt" \cr
#' "basin_wb_mon.txt" \cr
#' "basin_wb_yr.txt" \cr
#'
#' @param simulationNumber integer, the simulation number (not the number of
#' simulations)
#'
#' @export
#'
saveOutput <- function(workingDirectory,
                       coreNumber,
                       fileName,
                       fileType,
                       fromToDate,
                       colNumber,
                       rchNumber,
                       fileCioInfo,
                       simulationNumber,
                       firstRun) {
  
  # STEP 1: Initialization and directory setup
  # Create output directory once
  outputDirectory <- file.path(workingDirectory, "Output", paste0("Core_", coreNumber))
  if (!dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  # SWAT+ output files
  swatPlusFiles <- c("channel_sd_day.txt", "channel_sd_mon.txt",
                     "channel_sd_yr.txt", "channel_sdmorph_day.txt",
                     "channel_sdmorph_mon.txt", "channel_sdmorph_yr.txt",
                     "lsunit_wb_day.txt", "lsunit_wb_mon.txt",
                     "lsunit_wb_yr.txt","basin_wb_day.txt",
                     "basin_wb_mon.txt", "basin_wb_yr.txt")

  # STEP 2: Process each file type
  for (i in 1:length(fileType)) {
    # Initialize output for this iteration
    output <- list()
    
    # Process current file type
    if (fileType[i] == "watout.dat") {
      output <- readWatoutFile(workingDirectory,
                               coreNumber,
                               fileName[i],
                               fromToDate,
                               as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                               fileCioInfo,
                               output)
                               
    } else if (fileType[i] %in% c("output.rch", "output.sub", "output.hru")) {
      output <- readOutputRchFile(workingDirectory,
                                  coreNumber,
                                  fileName[i],
                                  fromToDate,
                                  as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                                  fileCioInfo,
                                  getRchNumber(rchNumber[i]),
                                  output)
                                  
    } else if (fileType[i] %in% swatPlusFiles) {
      output <- readChannelFile(workingDirectory,
                                coreNumber,
                                fileName[i],
                                fromToDate,
                                as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                                fileCioInfo,
                                getRchNumber(rchNumber[i]),
                                output)
                                
    } else if (fileType[i] == "userReadSwatOutput") {
      output <- userReadSwatOutput(workingDirectory,
                                  coreNumber,
                                  fileName[i],
                                  output)
                                  
      # Handle custom output format for userReadSwatOutput
      if (fileName[i] == "basin_crop_yld_yr.txt") {
        outputFile <- file.path(outputDirectory, paste0('out_var_yield_', i, '.csv'))
      } else {
        outputFile <- file.path(outputDirectory, paste0('out_var_irrigation_', i, '.csv'))
      }
      
      file_exists <- file.exists(outputFile)
      
      # Write output as CSV
      if (firstRun || !file_exists) {
        write.csv(output, outputFile, row.names = FALSE)
      } else {
        write.table(output, outputFile, sep = ",", row.names = FALSE, 
                    col.names = FALSE, append = TRUE)
      }
      
      # Skip standard output processing for this file type
      next
    } else {
      warning(paste("Unknown output file type:", fileType[i]))
      next
    }
    
    # Standard output processing for non-userReadSwatOutput file types
    for (j in 1:length(output)) {
      outputFile <- file.path(outputDirectory, paste0('out_var_', j, '.txt'))
      
      if (firstRun) {
        file.create(outputFile)
      }
      
      # Write simulation number
      write.table(as.character(simulationNumber), outputFile, append = TRUE,
                  row.names = FALSE, col.names = FALSE)
      
      # Write simulated data
      write.table(output[[j]], outputFile, append = TRUE, sep = '\t',
                  row.names = FALSE, col.names = FALSE)
    }
  }
  
  # STEP 3: Finalization and status reporting
  # Write processing status to log file
  logFile <- file.path(workingDirectory, "Output", "ProcessingStatus.log")
  cat(paste0("Core ", coreNumber, ": Successfully processed simulation ", 
            simulationNumber, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
      file = logFile, append = TRUE)
  
  # Return invisible confirmation of success
  invisible(TRUE)
}
