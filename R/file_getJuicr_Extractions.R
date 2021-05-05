#' Reads a *_juicr.html report and retrieves data extractions and history.
#'
#' Reads a juicr generated report of image extractions contained within the .html file.
#'
#' @param aFileName The file name and location of a *_juicr.html report.  Prompts
#'    for file name if none is explicitly called.  Must be .html format.
#' @param quiet When \code{"TRUE"}, does not display all extraction tables.
#'
#' @return A list of tables including all extractions, coordinates, image file
#'     names, and \code{juicr} GUI parameters.
#'
#' @examples \dontrun{
#'
#' theExtraction <- file_getJuicr_Extractions("pretend_image_juicr.html")
#' print(theExtraction$points)
#'
#'}
#'
#' @seealso \link{file_getJuicr_Images}
#'
#' @importFrom XML readHTMLTable
#'
#' @export

file_getJuicr_Extractions <- function(aFileName = file.choose(),
                                      quiet = FALSE) {

  dataTables <- XML::readHTMLTable(aFileName)
  if(!quiet) print(dataTables)
  return(dataTables)
}
