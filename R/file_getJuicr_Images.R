#' Reads a *_juicr.html report and extracts images into working directory.
#'
#' Reads a juicr generated report of image extractions contained within the
#' .html file, and saves all images (e.g., original, standardized,
#' and standardized with painted extractions).
#'
#' @param aFileName The file name and location of a *_juicr.html report.  Prompts
#'    for file name if none is explicitly called.  Must be .html format.
#' @param quiet When \code{"TRUE"}, does not display the file names of all images
#'    extracted and saved to the working directory.
#'
#' @return A vector of file names of each image extracted from the .html file.
#'
#' @examples \dontrun{
#'
#' file_getJuicr_Images("pretend_image_juicr.html")
#'
#'}
#'
#' @seealso \link{file_getJuicr_Extractions}
#'
#' @importFrom XML readHTMLTable htmlParse xpathSApply xmlAttrs
#' @importFrom RCurl base64Decode
#'
#' @export

file_getJuicr_Images <- function(aFileName = file.choose(),
                                 quiet = FALSE) {

  # collect tables from juicr .html file
  inputTables <- XML::readHTMLTable(aFileName)

  # collect standardized figures from juicr .html file
  juicrHTML = XML::htmlParse(aFileName)
  inputImages <- XML::xpathSApply(juicrHTML,
                                  "//table/tr/td/img",
                                  XML::xmlAttrs)["src", ]

  # re-create all image files
  savedFiles <- c()
  for(i in 1:length(inputTables$files$file_name)) {
    file.create(file.path(dirname(aFileName),
                          paste0("extracted_", inputTables$files$file_name[i])))
    tempImageFile <- file(file.path(dirname(aFileName),
                                    paste0("extracted_", inputTables$files$file_name[i])),
                          "wb")
    writeBin(RCurl::base64Decode(sub(".*,", "", inputImages[i]), mode = "raw"),
             tempImageFile, useBytes = TRUE)
    close(tempImageFile)
    if(!quiet) print("extracted = ",
                     file.path(dirname(aFileName),
                               paste0("extracted_", inputTables$files$file_name[i])))
    savedFiles <- c(savedFiles,
                    file.path(dirname(aFileName),
                              paste0("extracted_", inputTables$files$file_name[i])))
  }

  return(savedFiles)
}
