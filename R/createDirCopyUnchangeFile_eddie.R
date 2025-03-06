
#' Create TxtInOut directories and copy unchanged files
#'
#' @inheritParams runSWATpar
#' @param TxtInOut path to original TxtInOut folder
#' @param exceptFiles character vector of files that do not need to copy
#' @param swatExe SWAT/SWAT+ executable files
#'
#' @return No return
#' 
#' @examples
#' \donttest{
#' # Create a directory and populate with TxtInOut of SWAT
#' extracExampleData(exampleData,"swatTxtInOut", tempdir())
#' TxtInOut <- file.path(tempdir(), "swatTxtInOut")
#'
#' # Create a directory to copy to this directory
#' dir.create(file.path(tempdir(), "workingDirectory"), showWarnings = FALSE)
#' workingDirectory <- file.path(tempdir(), "workingDirectory")
#'
#' # Except these files
#' exceptFiles <- exampleData$swatFiles[1:280]
#'
#' # Lets assume that this is the SWAT exe file
#' swatExe <- tempfile(pattern = "swatexe", tmpdir = tempdir(), fileext = ".exxxe")
#' file.create(swatExe)
#'
#' createDirCopyUnchangeFile(workingDirectory, 2, TxtInOut, exceptFiles, swatExe, TRUE)
#' }
#'
#' @export
#'
createDirCopyUnchangeFile_eddie <- function(workingDirectory, core,
                                      TxtInOut, exceptFiles, swatExe){

      dir <- file.path(workingDirectory, paste0('TxtInOut_', core))
      dir.create(dir, showWarnings = FALSE)
 

    # Copy all unchanged files
    copyAllExcept(TxtInOut, dir, exceptFiles)

    # Check if exe exist then delete
    temp <- strsplit(swatExe, split="/")[[1]]
    temp <- trimws(temp[length(temp)])
    if (file.exists(file.path(dir, temp))) {
      file.remove(file.path(dir, temp))
    }

    # Copy swat.exe file
    file.copy(swatExe, dir)
  

output_dir <- file.path(workingDirectory, 'Output')
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
}
