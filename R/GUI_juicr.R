#' A GUI screener to quickly code candidate studies for inclusion/exclusion into
#' a systematic review or meta-analysis.
#'
#' A GUI screener to help scan and evaluate the title and abstract of studies to
#' be included in a systematic review or meta-analysis. A description of GUI
#' options and layout is found here: \url{http://lajeunesse.myweb.usf.edu/juicr/juicr_basic_vignette_v0.1.html}.
#'
#' @param theFigureFile An optional file name and location of a .jpg, .png, or
#'    .tiff file containing the scientific image/plot/chart/figure to pre-load
#'    in the GUI. Within the GUI there is also a button to select the image file.
#'    Images in other formats should be converted to .png prior to using juicr.
#' @param theJuicrFile An optional file name and location of a *_juicr.html
#'    report containing extractions and images from a previous juicr
#'    session to pre-load into the GUI. Within the GUI there is also a button to
#'    select an .html file.
#' @param standardizeTheImage When \code{"TRUE"}, all large images are
#'    standardized to a common size with a width specified
#'    by \code{"standardSize"}. When \code{"FALSE"}, the image is unaltered
#'    in size.
#' @param standardSize The common width in pixels for standardizing large images;
#'    default is a width of 1000 pixels.
#' @param figureWindowSize Specifies the window size containing the image. By
#'    default, this image-viewer window will be 800 (width) by 600 (height)
#'    pixels, larger images will be scrollable to fit this window.
#' @param pointSize Changes the default size of a clickable data-point on the
#'   image. Size is the radius in pixels (default is 6).
#' @param animateDelay When \code{"TRUE"}, creates a very small pause when
#'    plotting individual automated extractions -- giving an animated effect.
#' @param groupNames A vector of the default eight names specifying the
#'    labels of each group. Default labels are fruit themed. Can be any size,
#'    but GUI will only print first 9 characters.
#' @param groupColors A vector of the default eight color names specifying the
#'    coloring of each group. Are in color-names format, but can also be HEX.
#'
#'
#' @return A console message of where saved .csv or *_juicr.html files are located.
#'
#' @examples \dontrun{
#'
#' GUI_juicr(system.file("images", "Kortum_and_Acymyan_2013_Fig4.jpg", package = "juicr"))
#'
#'}
#'
#' @note \strong{Installation and troubleshooting}\cr\cr For Mac OS users,
#'    installation is sometimes not straighforward as this GUI requires the
#'    Tcl/Tk GUI toolkit to be installed. You can get this toolkit by making sure
#'    the latest X11 application (xQuartz) is installed, see here:
#'    \url{https://www.xquartz.org/}. More information on
#'    installation is found in \code{juicrs}'s vignette.
#'
#' @import tcltk utils
#' @importFrom stats sd
#' @importFrom grDevices rgb col2rgb
#' @importFrom XML readHTMLTable htmlParse xpathSApply xmlAttrs
#' @importFrom RCurl base64Encode base64Decode
#'
#' @export GUI_juicr

GUI_juicr <- function(theFigureFile = "",
                      theJuicrFile = "",
                      standardizeTheImage = TRUE,
                      standardSize = 1000,
                      figureWindowSize = c(800, 600),
                      pointSize = 6,
                      animateDelay = TRUE,
                      groupNames = c("orangeGrp",
                                     "berryGrp",
                                     "cherryGrp",
                                     "plumGrp",
                                     "kiwiGrp",
                                     "bananaGrp",
                                     "grapeGrp",
                                     "pruneGrp"),
                      groupColors = c("dark orange",
                                    "turquoise3",
                                    "tomato3",
                                    "orchid",
                                    "yellow green",
                                    "goldenrod2",
                                    "plum4",
                                    "saddle brown")
                       ) {

  # if EBImage not installed, do it
  .juicrDependencies("EBImage")

  getIMG <- function(aFilename) return(system.file("images", aFilename, package = "juicr"))


  # checks if tcltk is available and can be loaded
  if(requireNamespace("tcltk", quietly = TRUE)) {

    juicrLogo <- tcltk::tcl("image", "create", "photo",
                     file = getIMG("juicr_hex_small_juicing2.png"))

    #############################################################################
    # START: ABOUT WINDOW: citation and authorship info

      aboutJuicrWindow <- function() {

        aboutWindow <- tcltk::tktoplevel()
        tcltk::tktitle(aboutWindow) <- "about juicr"
          aboutFrame <- tcltk::ttkframe(aboutWindow)
          juicrVignette <- tcltk::tkbutton(aboutFrame, text = "go to vignette for help",
                                    width = 180, compound = 'top',
                                    image = juicrLogo,
                                    command = function() utils::browseURL("http://lajeunesse.myweb.usf.edu/metagear/metagear_basic_vignette.html"))
          aboutText <- tcltk::tktext(aboutFrame,
                              font = "Consolas 10",
                              height = 20, width = 75,
                              tabs = "0.9i left")
          theText <- paste0(c("citation for 0.1 (beta):\n\n",
                              "  Lajeunesse M.J. (2021) juicr: extract data from images. v.0.1 R package\n",
                              "\n\nabout author:\n\n",
                              "  Marc J. Lajeunesse, Associate Professor\n",
                              "  Department of Integrative Biology\n",
                              "  University of South Florida, Tampa USA\n",
                              "  homepage: http://lajeunesse.myweb.usf.edu/\n",
                              "  email: lajeunesse@usf.edu\n",
                              "  twitter: @LajeunesseLab\n",
                              "  youtube: https://www.youtube.com/c/LajeunesseLab\n",
                              "\n\nacknowledgements:\n\n",
                              "  Citrus icons provided by: https://icons8.com"),
                            collapse = "")
          tcltk::tkinsert(aboutText, "1.0", theText)
          tcltk::tkgrid(juicrVignette, aboutText, padx = 5)
          tcltk::tkpack(aboutFrame)

      }

    # END: ABOUT WINDOW: citation and authorship info
    #################################################



    #############################################################################
    # START: DEBUG: totally unnecessary but necessary print
    #        function for within-GUI debugging

      debugGUI <- function(aTCLTKObject)
        message(paste0(as.character(aTCLTKObject), " "))

    # END: DEBUG: totally unnecessary but necessary print
    #      function for within-GUI debugging
    #################################################


    #############################################################################
    # START: GUI THEME & ICONS

      tcltk::.Tcl("ttk::style configure TNotebook -background white")
      tcltk::.Tcl("ttk::style configure TNotebook.Tab -background white")
      tcltk::.Tcl("ttk::style configure TNotebook.Tab -foreground grey")
      tcltk::.Tcl("ttk::style configure TNotebook -focuscolor grey")
      tcltk::.Tcl("ttk::style configure TFrame -background white")
      tcltk::.Tcl("ttk::style configure TLabelframe -background white")
      tcltk::.Tcl("ttk::style configure TLabelframe.Label -background white")
      tcltk::.Tcl("ttk::style configure TLabelframe.Label -foreground grey")
      tcltk::.Tcl("ttk::style configure TLabel -background white")
      tcltk::.Tcl("ttk::style configure TLabel -foreground grey")
      tcltk::.Tcl("ttk::style configure TCombobox -background white")
      tcltk::.Tcl("ttk::style configure TCombobox -foreground grey")
      tcltk::.Tcl("ttk::style configure TScrollbar -background white")
      tcltk::.Tcl("ttk::style configure TButton -foreground black")
      tcltk::.Tcl("ttk::style configure message.TButton -foreground orange")
      tcltk::.Tcl("ttk::style configure TButton -background white")
      tcltk::.Tcl("ttk::style map TButton -background [list active white]")
      tcltk::.Tcl("ttk::style map TButton -foreground [list active {green}]")

      imageScatter <- tcltk::tcl("image", "create", "photo", file = getIMG("scatterPlot_orange.png"))
      imageBarX <- tcltk::tcl("image", "create", "photo", file = getIMG("barPlotX_orange.png"))
      imageBarY <- tcltk::tcl("image", "create", "photo", file = getIMG("barPlotY_orange.png"))
      imageRegression <- tcltk::tcl("image", "create", "photo", file = getIMG("regressionPlot_orange.png"))
      imageLine <- tcltk::tcl("image", "create", "photo", file = getIMG("linePlot_orange.png"))
      orangeJuice <- tcltk::tcl("image", "create", "photo", file = getIMG("drinkjuice.png"))
      orangeJuiceSave <- tcltk::tcl("image", "create", "photo", file = getIMG("drinkjuice_nostraw.png"))
      juicrLogoJuicing <- tcltk::tcl("image", "create", "photo", file = getIMG("juicr_hex_small_juicing.png"))
      juiceBottle <- tcltk::tcl("image", "create", "photo", file = getIMG("juiceBottle.png"))
      circlePoint1 <- tcltk::tcl("image", "create", "photo", file = getIMG("pointCircle1.png"))
      circlePoint5 <- tcltk::tcl("image", "create", "photo", file = getIMG("pointCircle5.png"))
      circlePoint15 <- tcltk::tcl("image", "create", "photo", file = getIMG("pointCircle15.png"))
      circlePoint15Closed <-tcltk::tcl("image", "create", "photo", file = getIMG("pointCircleOpen.png"))
      diamondPoint15 <- tcltk::tcl("image", "create", "photo", file = getIMG("pointDiamond.png"))
      squarePoint15 <- tcltk::tcl("image", "create", "photo", file = getIMG("pointSquare.png"))
      lineQualityHigh <- tcltk::tcl("image", "create", "photo", file = getIMG("antialiasedLOW.png"))
      lineQualityLow <- tcltk::tcl("image", "create", "photo", file = getIMG("antialiasedHIGH.png"))
      barPoint1 <- tcltk::tcl("image", "create", "photo", file = getIMG("barShort5.png"))
      barPoint5 <- tcltk::tcl("image", "create", "photo", file = getIMG("barShort11.png"))
      barPoint15 <- tcltk::tcl("image", "create", "photo", file = getIMG("barShort19.png"))
      theOrange <- tcltk::tcl("image", "create", "photo", file = getIMG("orange_ico.png"))
      theOrangeGrey <- tcltk::tcl("image", "create", "photo", file = getIMG("orange_grey_ico_test.png"))
      autoPointImage <- tcltk::tcl("image", "create", "photo", file = getIMG("autoClustertest.png"))
      clusterPointImage <- tcltk::tcl("image", "create", "photo", file = getIMG("autoPointtest.png"))
      theBarImage <- tcltk::tcl("image", "create", "photo", file = getIMG("barLine11test.png"))
      leftArrowImage <- tcltk::tcl("image", "create", "photo", file = getIMG("left.png"))
      rightArrowImage <- tcltk::tcl("image", "create", "photo", file = getIMG("right.png"))
      hoverImage <- tcltk::tcl("image", "create", "photo", file = getIMG("hover2.png"))
      orangeJuiceFlip <- tcltk::tcl("image", "create", "photo")
      tcltk::tcl(orangeJuiceFlip, "copy", orangeJuice, "-subsample", -1, 1)
      juiceContainer <- tcltk::tcl("image", "create", "photo", file = getIMG("icons8-juice-bottle-96.png"))
      juiceContainerSmall <- tcltk::tcl("image", "create", "photo")
      tcltk::tcl(juiceContainerSmall, "copy", juiceContainer, "-subsample", 2, 2)
      juiceContainerSmall <- tcltk::tcl("image", "create", "photo")
      tcltk::tcl(juiceContainerSmall, "copy", juiceContainer, "-subsample", 2, 2)
      juicrLogoSmall <- tcltk::tcl("image", "create", "photo", file = getIMG("juicr_hex_small_juicing3.png"))


    # END: GUI THEME & ICONS
    ########################


      #############################################################################
      # START: juicr figure frame

      createJuicrFrame <- function(aJuicrWindow,
                                   theFigureFile,
                                   theStandardizedImageFile,
                                   theFigure,
                                   theFigureJuiced,
                                   animateDelay,
                                   openJuicrFile = "",
                                   aPointColor = groupColors[1],
                                   aTempPointColor = groupColors[1]) {

        # crate juicr environment to store globals
        juicr.env <- new.env()
        set_juicr <- function(aJuicrVar, aValue) assign(aJuicrVar, aValue, envir = juicr.env)
        get_juicr <- function(aJuicrVar) get(aJuicrVar, envir = juicr.env)

        set_juicr("pointColor", aPointColor)
        set_juicr("tempPointColor", aTempPointColor)

        #############################################################################
        # START: automated extractor functions

        asOdd <- function(aNum) return(ceiling(aNum) - ceiling(aNum) %% 2 + 1)

        autoX <- function(anEBImage,
                          binary_threshold = 0.6,
                          object_threshold = 0.2,
                          axis_length = 0.5,
                          asY = FALSE) {

          if(asY == TRUE) anEBImage <- EBImage::transpose(EBImage::flop(anEBImage))

          # convert to binary, remove where axis unlikely, extract
          aBinaryFigure <- 1 - (EBImage::channel(anEBImage, mode = "gray") > binary_threshold)
          aBinaryFigure[, 1:round(dim(aBinaryFigure)[2] * axis_length)] <- 0
          lineBrush <- EBImage::makeBrush(asOdd(dim(aBinaryFigure)[2] * axis_length), shape = "line", angle = 0)
          aPaintedPlot <- EBImage::opening(EBImage::distmap(aBinaryFigure), lineBrush)
          allDetectedX <- EBImage::watershed(EBImage::distmap(aPaintedPlot), tolerance = object_threshold, ext = 1)

          # if none found, repeat with alternative parameterization
          adjust <- 0.1
          while((max(allDetectedX) == 0) && (adjust != 0.5)) {
            aBinaryFigure <- 1 - (EBImage::channel(anEBImage, mode = "gray") > (binary_threshold + adjust))
            aBinaryFigure[, 1:round(dim(aBinaryFigure)[2] * (axis_length - adjust))] <- 0
            lineBrush <- EBImage::makeBrush(asOdd(dim(aBinaryFigure)[2] * (axis_length - adjust)), shape = "line", angle = 0)
            aPaintedPlot <- EBImage::opening(EBImage::distmap(aBinaryFigure), lineBrush)
            allDetectedX <- EBImage::watershed(EBImage::distmap(aPaintedPlot), tolerance = object_threshold, ext = 1)
            adjust <- adjust + 0.1
          }

          # eliminate all but the longest & lowermost
          if(max(allDetectedX) > 1) {
            allLines <- EBImage::computeFeatures.shape(allDetectedX)
            exclusionList <- which(allLines[, "s.area"] != max(allLines[, "s.area"]))
            allDetectedX <- EBImage::rmObjects(allDetectedX, exclusionList)
            theCoordinates <- EBImage::computeFeatures.moment(allDetectedX)
            exclusionList <- which(theCoordinates[, "m.cy"] != max(theCoordinates[, "m.cy"]))
            allDetectedX <- EBImage::rmObjects(allDetectedX, exclusionList)
          }

          if(max(allDetectedX) == 0) return(FALSE)
          if(asY == TRUE) return(EBImage::flop(EBImage::transpose(allDetectedX)))
          return(allDetectedX)
        }

        theAutoPointsAreEmpty <- FALSE
        theAutoPointsShape <- "disc"

        autoPoints <- function(anEBImage,
                               theX,
                               theY,
                               point_shape = "disc",
                               point_empty = FALSE,
                               point_size = 3,
                               point_tolerance = 2,
                               binary_threshold = 0.63) {

          aBinaryFigure <- 1 - (EBImage::channel(anEBImage, mode = "gray") > binary_threshold)

          # erase everything outside detected axis
          Xcontr <- EBImage::ocontour(theX)
          Xmax <- max(Xcontr[[1]][, 1]); Xmin <- min(Xcontr[[1]][, 1])
          aBinaryFigure[c(1:(Xmin + 3), Xmax:dim(aBinaryFigure)[1]), ] <- 0
          Ycontr <- EBImage::ocontour(theY)
          Ymax <- max(Ycontr[[1]][, 2]); Ymin <- min(Ycontr[[1]][, 2])
          aBinaryFigure[, c(1:(Ymin + 3), Ymax:dim(aBinaryFigure)[2]) ] <- 0

          if(point_empty == TRUE) {
            aBinaryFigure <- EBImage::fillHull(EBImage::watershed(EBImage::distmap(aBinaryFigure), tolerance = 2, ext = 1))
          }

          # paint candidate points with box, disc, or diamond brush with defined size
          pointBrush <- EBImage::makeBrush(size = asOdd(point_size), shape = point_shape, step = TRUE)
          aPaintedFigure <- EBImage::opening(EBImage::distmap(aBinaryFigure), pointBrush)
          detectedPoints <- EBImage::watershed(EBImage::distmap(aPaintedFigure), tolerance = point_tolerance, ext = 1)

          # if none found, repeat with alternative parameterization
          adjust <- 1
          while((max(detectedPoints) == 0) && (adjust != 11)) {
            pointBrush <- EBImage::makeBrush(size = asOdd(adjust), shape = point_shape, step = TRUE)
            aPaintedFigure <- EBImage::opening(EBImage::distmap(aBinaryFigure), pointBrush)
            detectedPoints <- EBImage::watershed(EBImage::distmap(aPaintedFigure), tolerance = point_tolerance, ext = 1)
            adjust <- adjust + 2
          }

          if(max(detectedPoints) == 0) return(FALSE)
          return(detectedPoints)
        }

        getClusters <- function(theDectedPoints) {
          isCluster <- mean(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"]) +
            stats::sd(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"])
          thenonClusters <- which(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"] < isCluster)
          return(EBImage::rmObjects(theDectedPoints, thenonClusters))
        }

        getNonClusters <- function(theDectedPoints) {
          isCluster <- mean(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"]) +
            stats::sd(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"])
          theClusters <- which(EBImage::computeFeatures.shape(theDectedPoints)[, "s.area"] >= isCluster)
          return(EBImage::rmObjects(theDectedPoints, theClusters))
        }

        getCoord_detectedAxis <- function(aDetectedImage) {
          theAxis <- EBImage::ocontour(aDetectedImage)
          coordX1 <- min(theAxis[[1]][, 1]); coordY1 <- min(theAxis[[1]][, 2]);
          coordX2 <- max(theAxis[[1]][, 1]); coordY2 <- max(theAxis[[1]][, 2]);
          return(c(coordX1, coordY1, coordX2, coordY2))
        }

        getCoord_detectedPoints <- function(aDetectedImage) {
          return(EBImage::computeFeatures.moment(aDetectedImage)[, 1:2])
        }

        resolve_crossedAxes <- function(theX, theY, asY = FALSE) {
          theCoordX <- getCoord_detectedAxis(theX)
          theCoordY <- getCoord_detectedAxis(theY)

          if(asY == TRUE) return(c(theCoordY[1], theCoordY[2], theCoordY[3], theCoordX[2]))
          return(c(theCoordY[3], theCoordX[2], theCoordX[3], theCoordX[4]))

        }

        autoBars <- function(anEBImage,
                             theX,
                             theY,
                             binary_threshold = 0.6,
                             object_threshold = 0.1,
                             bar_length = 9,
                             axis_length = 0.5,
                             asY = FALSE) {

          if(asY == TRUE) anEBImage <- EBImage::transpose(EBImage::flop(anEBImage))

          aBinaryFigure <- 1 - (EBImage::channel(anEBImage, mode = "gray") > binary_threshold)

          # erase everything outside detected axis
          Xcontr <- EBImage::ocontour(theX)
          Xmax <- max(Xcontr[[1]][, 1]); Xmin <- min(Xcontr[[1]][, 1])
          aBinaryFigure[c(1:(Xmin + 3), Xmax:dim(aBinaryFigure)[1]), ] <- 0
          Ycontr <- EBImage::ocontour(theY)
          Ymax <- max(Ycontr[[1]][, 2]); Ymin <- min(Ycontr[[1]][, 2])
          aBinaryFigure[, c(1:(Ymin + 3), Ymax:dim(aBinaryFigure)[2]) ] <- 0

          # detect all horizontal lines (the caps of column bars and error bars)
          lineBrush <- EBImage::makeBrush(bar_length, shape = "line", angle = 0)
          verticalLinesOnlyFigure <- EBImage::opening(EBImage::distmap(aBinaryFigure), lineBrush)
          extractedBars <- EBImage::watershed(EBImage::distmap(verticalLinesOnlyFigure), object_threshold)

          # clean up detections: exclude large lines detected, based on % X axis length
          theLines <- EBImage::computeFeatures.shape(extractedBars)
          exclusionList <- which(theLines[, "s.area"] >= dim(extractedBars)[1] * axis_length)
          extractedBars <- EBImage::rmObjects(extractedBars, exclusionList)

          ## clean up detections: outliers
          #extractedBars <- figure_removeOutlyingPoints(extractedBars, extractedXFigure, extractedYFigure)

          if(max(extractedBars) == 0) return(FALSE)
          if(asY == TRUE) return(EBImage::flop(EBImage::transpose(extractedBars)))
          return(extractedBars)

        }

        # END: automated extractor functions
        #######################################################



        #############################################################################
        # START: figure point vector and manipulation functions

        set_juicr("figurePoints", c())

        point_indexToPoint <- function(aPointIndex) return(as.numeric(gsub("pointID", "", aPointIndex)))
        point_pointToIndex <- function(aPoint) return(paste0("pointID", aPoint))

        point_add <- function() {
          allPoints <- get_juicr("figurePoints")
          newPoint <- ifelse(length(allPoints) == 0, 1, max(allPoints) + 1)
          set_juicr("figurePoints", c(allPoints, newPoint))
          return(newPoint)
        }

        point_delete <- function(aPoint) {
          allPoints <- get_juicr("figurePoints")
          set_juicr("figurePoints", allPoints[!allPoints %in% aPoint])
        }
        point_getTags <- function(aPointIndex) return(as.character(tcl(mainFigureCanvas, "gettags", aPointIndex)))
        point_getAll <- function() return(get_juicr("figurePoints"))
        point_getType <- function(aPointIndex) return(as.character(point_getTags(aPointIndex)[3]))
        point_getAuto <- function(aPointIndex) return(as.character(point_getTags(aPointIndex)[2]))

        point_getAllbyType <- function(pointType = "point") {
          allThePoints <- point_pointToIndex(point_getAll())
          theTags <- as.character(sapply(allThePoints, function(x) paste0(point_getType(x))))
          return(allThePoints[theTags == pointType])
        }

        point_getAllbyAuto <- function(pointType = "auto") {
          allThePoints <- point_pointToIndex(point_getAll())
          theTags <- as.character(sapply(allThePoints, function(x) paste0(point_getAuto(x))))
          return(allThePoints[theTags == pointType])
        }

        point_getCoordinates <- function(aPointIndex) {
          theCoord <- as.numeric(as.character(tkcoords(mainFigureCanvas, aPointIndex)))
          theType <- point_getType(aPointIndex)
          if(theType == "point") {
            if(point_getAuto(aPointIndex) == "autobar") {
              theCoordinates <- c(theCoord[1] + 8, theCoord[2] + 3)
            } else if(point_getAuto(aPointIndex) == "auto") {
              theCoordinates <- c(theCoord[1] + 8, theCoord[2] + 8)
            } else if(point_getAuto(aPointIndex) == "cluster") {
              theCoordinates <- c(theCoord[1] + 8, theCoord[2] + 8)
            } else {
              theCoordinates <- c(theCoord[1] + pointSize, theCoord[2] + pointSize)
            }
          } else if(theType == "error") {
            theCoordinates <- c(theCoord[1], theCoord[2], theCoord[3], theCoord[4])
          } else if (theType == "regression") {
            theCoordinates <- c(theCoord[1], theCoord[2], theCoord[3], theCoord[4])
          } else if (theType == "line") {
            theCoordinates <- theCoord
          }
          return(theCoordinates)
        }

        point_getCalibratedValue <- function(aPointIndex, theAxis = "x", coordinates = FALSE) {

          theCoord <- point_getCoordinates(aPointIndex)[ifelse(theAxis == "x", 1, 2)]
          if(coordinates == TRUE) return(theCoord)

          if(theAxis == "x") {
            xMaxValue <- as.numeric(text_get(figureXmaxDisplay))
            xMinValue <- as.numeric(text_get(figureXminDisplay))
            if(all(is.na(c(xMaxValue, xMinValue)))) return(theCoord)
          }

          if(theAxis == "y") {
            yMaxValue <- as.numeric(text_get(figureYmaxDisplay))
            yMinValue <- as.numeric(text_get(figureYminDisplay))
            if(all(is.na(c(yMaxValue, yMinValue)))) return(theCoord)
          }

          return(coordinate_calibrate(theCoord, theAxis))
        }

        isEmpty_calibrate <- function(theAxis = "x") {
          if(theAxis == "x") {
            if(text_get(figureXmaxDisplay) == "" || text_get(figureXminDisplay) == "") return(TRUE)
          } else {
            if(text_get(figureYmaxDisplay) == "" || text_get(figureYminDisplay) == "") return(TRUE)
          }
          return(FALSE)
        }

        coordinate_calibrate <- function(theCoor, theAxis = "x") {
          if(theAxis == "x") {
            maxValue <- as.numeric(text_get(figureXmaxDisplay))
            minValue <- as.numeric(text_get(figureXminDisplay))
            if(all(is.na(c(maxValue, minValue)))) return(NA)
            posLine <- as.numeric(tkcoords(mainFigureCanvas, x_calibrationLine))[c(1, 3)]
            calibrated <- (theCoor - min(posLine)) * ((maxValue - minValue)/(max(posLine) - min(posLine))) + minValue
          } else {
            maxValue <- as.numeric(text_get(figureYmaxDisplay))
            minValue <- as.numeric(text_get(figureYminDisplay))
            if(all(is.na(c(maxValue, minValue)))) return(NA)
            posLine <- as.numeric(tkcoords(mainFigureCanvas, y_calibrationLine))[c(2, 4)]
            calibrated <- (max(posLine) - theCoor) * ((maxValue - minValue)/(max(posLine) - min(posLine))) + minValue
          }
          return(calibrated)
        }

        point_pixelError <- function(theAxis = "x") {
          if(theAxis == "x") {
            maxValue <- as.numeric(text_get(figureXmaxDisplay))
            minValue <- as.numeric(text_get(figureXminDisplay))
            posLine <- as.numeric(tkcoords(mainFigureCanvas, x_calibrationLine))[c(1, 3)]
          } else {
            maxValue <- as.numeric(text_get(figureYmaxDisplay))
            minValue <- as.numeric(text_get(figureYminDisplay))
            posLine <- as.numeric(tkcoords(mainFigureCanvas, y_calibrationLine))[c(2, 4)]
          }
          return((maxValue - minValue)/(max(posLine) - min(posLine)))
        }

        text_get <- function(aTextIndex) paste(as.character(tcl(aTextIndex, "get", "1.0", "end")), collapse = " ")

        # END: figure point vector and manipulation functions
        #######################################################



        #############################################################################
        # START: text functions for data tabulation

        displayData <- function(tabDelimitedText, caption) {

          extractionWindow <- tcltk::tktoplevel()

          tcltk::tktitle(extractionWindow) <- paste0(caption, " via juicr")

          dataFrame <- tcltk::ttklabelframe(extractionWindow,
                                     text = caption,
                                     padding = 2)
          dataScroll <- tcltk::ttkscrollbar(extractionWindow, orient = "vertical",
                                     command = function(...) tcltk::tkyview(dataText, ...))
          dataText <- tcltk::tktext(dataFrame,
                             font = "Consolas 10",
                             height = 20, width = 160, tabs = "0.9i left",
                             yscrollcommand = function(...) tcltk::tkset(dataScroll, ...))

          aText <- tcltk::tkinsert(dataText, "1.0", tabDelimitedText)
          tcltk::tktag.add(dataText, "aTag1", "1.0", "1.end")
          tcltk::tktag.configure(dataText, "aTag1", font = "Consolas 10 bold")
          tcltk::tkgrid(dataText, dataScroll, sticky = "nsew")

          buttonFrame <- tcltk::ttkframe(dataFrame)
          clipboardButton <- tcltk::ttkbutton(buttonFrame, width = 12,
                                      text = " copy to\nclipboard",
                                      command = function() utils::writeClipboard(tabDelimitedText))
          removeFormatingButton <- tcltk::ttkbutton(buttonFrame,  width = 12,
                                            text = "  remove\nformatting",
                                            command = function() {
                                              tcltk::tkconfigure(dataText, tabs = "")
                                              tcltk::tktag.delete(dataText, "aTag1")
                                            })
          csvButton <- tcltk::ttkbutton(buttonFrame, width = 12,
                                            text = "save as\n  .csv",
                                            command = function() {
                                              fileContents <- switch(caption,
                                                                     "point/sample extractions" = "points",
                                                                     "bar extractions" = "bars",
                                                                     "axis line extractions" = "axes",
                                                                     "error bar extractions" = "error_bars",
                                                                     "regression line extractions" = "regressions",
                                                                     "line extractions" = "lines"
                                              )
                                              theNewFile <- paste0(tools::file_path_sans_ext(basename(theFigureFile)),
                                                                   "_juicr_extracted_",
                                                                   fileContents,
                                                                   ".csv")
                                              tcltk::tkconfigure(closeButton, text = paste0("SAVING AS:\n", theNewFile), style = "message.TButton")
                                              tcltk::tcl("update"); Sys.sleep(2);

                                              someTable <- read.table(text = tabDelimitedText,
                                                                      sep = "\t", header = TRUE)
                                              write.csv(someTable,
                                                        file = theNewFile,
                                                        row.names = FALSE)

                                              tcltk::tkconfigure(closeButton, text = "  close\nwindow", style = "TButton")

                                            })

          closeButton <- tcltk::ttkbutton(buttonFrame, width = 40,
                                text = "  close\nwindow",
                                command = function() tcltk::tkdestroy(extractionWindow))
          tcltk::tkgrid(removeFormatingButton, clipboardButton, csvButton, closeButton)
          tcltk::tkgrid(buttonFrame)
          tcltk::tkpack(dataFrame)

        }

        get_ExtractionList <- function() {

          fullNotes <- ""
          for(i in 1:(as.integer(tcltk::tclvalue(tcltk::tcl(theNotes, "index", "end"))) - 1)) {
            lineNotes <- tcltk::tcl(theNotes, "get", paste0(i, ".0"), paste0(i, ".end"))
            fullNotes <- paste0(fullNotes, paste0(lineNotes, collapse = " "), "\n")
          }

          allExtractions <- list("points" = getPointExtractions(sendToFile = TRUE),
                                 "bars" = getPointExtractions(sendToFile = TRUE),
                                 "axes" = getPointExtractions(sendToFile = TRUE),
                                 "error_bars" = getPointExtractions(sendToFile = TRUE),
                                 "regressions" = getPointExtractions(sendToFile = TRUE),
                                 "lines" = getPointExtractions(sendToFile = TRUE),
                                 "info" = data.frame("file" = theFigureFile,
                                                     "date" = Sys.Date(),
                                                     "notes" = fullNotes,
                                                     "figureXminDisplay" = as.character(text_get(figureXminDisplay)),
                                                     "figureXmaxDisplay" = as.character(text_get(figureXmaxDisplay)),
                                                     "figureXcaptionDisplay" = as.character(text_get(figureXcaptionDisplay)),
                                                     "figureXunitsDisplay" = as.character(text_get(figureXunitsDisplay)),
                                                     "figureYminDisplay" = as.character(text_get(figureYminDisplay)),
                                                     "figureYmaxDisplay" = as.character(text_get(figureYmaxDisplay)),
                                                     "figureYcaptionDisplay" = as.character(text_get(figureYcaptionDisplay)),
                                                     "figureYunitsDisplay" = as.character(text_get(figureYunitsDisplay))))
          return(allExtractions)

         }


        set_juicr("theSavedFile", "not saved this session")

        point_summary <- function() {
          #TO DO: OUT OF BOUNDS VALUES

          theNumberOfPoints <- length(point_getAll())
          theSummary <- "EXTRACTION SUMMARY\n---------------------------------\n"
          theSummary <- paste0(theSummary, "number of extractions = ", theNumberOfPoints, "\n")
          if(theNumberOfPoints == 0) return(theSummary)

          allThePoints <- point_pointToIndex(point_getAll())
          xMaxValue <- suppressWarnings(as.numeric(text_get(figureXmaxDisplay)))
          xMinValue <- suppressWarnings(as.numeric(text_get(figureXminDisplay)))
          yMaxValue <- suppressWarnings(as.numeric(text_get(figureYmaxDisplay)))
          yMinValue <- suppressWarnings(as.numeric(text_get(figureYminDisplay)))

          pointCoorX <- sapply(allThePoints, function(x) point_getCoordinates(x)[1])
          pointCoorY <- sapply(allThePoints, function(x) point_getCoordinates(x)[2])

          if(all(is.na(c(xMaxValue, yMaxValue, xMinValue, yMinValue)))) {
            xCalibrated <- signif(pointCoorX, 4)
            yCalibrated <- signif(pointCoorY, 4)
          } else {

            theSummary <- paste0(theSummary, "pixel error per extraction:\n")

            if(length(xMaxValue) == 0 && length(xMinValue) == 0) {
              xCalibrated <- NA
            } else {
              xCalibrated <- sapply(allThePoints, function(x) suppressWarnings(point_getCalibratedValue(x, theAxis = "x")))

              theSummary <- paste0(theSummary, " x = ",
                                   paste0(text_get(figureXcaptionDisplay), sep = " ", collapse = ""),
                                   paste0("(", text_get(figureXunitsDisplay),")", sep = " "),
                                   "+/- ", signif(point_pixelError("x"), 4), "\n")
            }

            if(length(yMaxValue) == 0 && length(yMinValue) == 0) {
              yCalibrated <- NA
            } else {
              yCalibrated <- sapply(allThePoints, function(x)suppressWarnings(point_getCalibratedValue(x, theAxis = "y")))
              theSummary <- paste0(theSummary, " y = ",
                                   paste0(text_get(figureYcaptionDisplay), sep = " ", collapse = ""),
                                   paste0("(", text_get(figureYunitsDisplay),")", sep = " "),
                                   "+/- ", signif(point_pixelError("y"), 4), "\n")
            }

          }
          theSummary <- paste0(theSummary, "saved in file =\n")
          theSummary <- paste0(theSummary, " ", get_juicr("theSavedFile"), "\n")
          theSummary <- paste0(theSummary, "---------------------------------\n")
          theSummary <- paste0(theSummary, "x\ty\ttype\tgroup\n")
          theSums <- paste0(signif(xCalibrated,4), "\t",
                            signif(yCalibrated,4), "\t",
                            sapply(allThePoints, function(x) paste0(abbreviate(point_getTags(x)[3], 3, dot = TRUE), "\t")),
                            sapply(allThePoints, function(x) paste0(point_getTags(x)[2], "\n")))

          return(paste0(c(theSummary, theSums), collapse = ""))
        }


        getPointExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE, coordinates = FALSE) {

          allThePoints <- point_getAllbyType("point")
          if(length(allThePoints) == 0) return(data.frame())
          xCoordinate <- sapply(allThePoints, function(x) suppressWarnings(point_getCalibratedValue(x, theAxis = "x", coordinates = TRUE)))
          yCoordinate <- sapply(allThePoints, function(x) suppressWarnings(point_getCalibratedValue(x, theAxis = "y", coordinates = TRUE)))
          xCalibrated <- sapply(allThePoints, function(x) suppressWarnings(point_getCalibratedValue(x, theAxis = "x")))
          yCalibrated <- sapply(allThePoints, function(x) suppressWarnings(point_getCalibratedValue(x, theAxis = "y")))

          theSummary <- paste0(c("x-calibrated\tx-label\tx-units\tx-coord\ty-calibrated\ty-label\ty-units\ty-coord\tgroup\n",
                                 paste0(
                                   signif(as.numeric(xCalibrated), 7), "\t",
                                   text_get(figureXcaptionDisplay), "\t",
                                   text_get(figureXunitsDisplay), "\t",
                                   as.numeric(xCoordinate), "\t",
                                   signif(as.numeric(yCalibrated), 7), "\t",
                                   text_get(figureYcaptionDisplay), "\t",
                                   text_get(figureYunitsDisplay), "\t",
                                   as.numeric(yCoordinate), "\t",
                                   sapply(allThePoints, function(x) paste0(point_getTags(x)[2], "\n"))
                                 )), collapse = "")

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "point/sample extractions")
          return(theSummary)
        }

        getBarExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE) {

          allThePoints <- point_getAllbyAuto("autobar")
          if(length(allThePoints) == 0) return(data.frame())

          allXCoords <- sapply(allThePoints, function(x) point_getCalibratedValue(x, theAxis = "x"))
          allYCoords <- sapply(allThePoints, function(x) point_getCalibratedValue(x, theAxis = "y"))

          numberBars = length(allXCoords) %/% 3
          if(max(allXCoords[1:3]) - min(allXCoords[1:3]) >= 3) numberBars = length(allXCoords) %/% 2

          theValues <- data.frame(matrix(allYCoords, nrow = numberBars, byrow = TRUE))
          for(i in 1:nrow(theValues))
            theValues[i, ] <- theValues[i, (sort(as.numeric(theValues[i,]), index.return = TRUE)$ix)]

          if(numberBars == length(allXCoords) %/% 3) {
            theSummary <- paste0(c("bar\tlower\tupper\tgroup\n",
                                   paste0(
                                     signif(as.numeric(theValues[, 2]), 7), "\t",
                                     signif(as.numeric(theValues[, 1]), 7), "\t",
                                     signif(as.numeric(theValues[, 3]), 7), "\t",
                                     paste0("autoBar", 1:nrow(theValues)), "\n"
                                   )), collapse = "")
          } else {
            theSummary <- paste0(c("bar\terror\tgroup\n",
                                   paste0(
                                     signif(as.numeric(theValues[, 1]), 7), "\t",
                                     signif(as.numeric(theValues[, 2]), 7), "\t",
                                     paste0("autoBar", 1:nrow(theValues)), "\n"
                                   )), collapse = "")
          }

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "bar extractions")
          return(theSummary)
        }

        getErrorExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE) {

          allThePoints <- point_getAllbyType("error")
          if(length(allThePoints) == 0) return(data.frame())

          errorCoords <- lapply(allThePoints, function(x) point_getCoordinates(x))

          theValues <- lapply(errorCoords,
                              function(x) {
                                if(x[1] == x[3]) {
                                  theMean <- suppressWarnings(coordinate_calibrate(x[2], "y"))
                                  theError <- suppressWarnings(abs(theMean - coordinate_calibrate(x[4], "y")))
                                  theType <- "y"
                                  meanX <- x[1]
                                  meanY <- x[2]
                                  errorX <- x[3]
                                  errorY <- x[4]
                                } else {
                                  theMean <- suppressWarnings(coordinate_calibrate(x[1], "x"))
                                  theError <- suppressWarnings(abs(theMean - coordinate_calibrate(x[3], "x")))
                                  theType <- "x"
                                  meanX <- x[1]
                                  meanY <- x[2]
                                  errorX <- x[3]
                                  errorY <- x[4]
                                }
                                return(c(mean = theMean,
                                         error = theError,
                                         type = theType,
                                         mx = meanX,
                                         my = meanY,
                                         ex = errorX,
                                         ey = errorY))
                              })

          theValues <- data.frame(matrix(unlist(theValues), nrow = length(theValues), byrow = TRUE))

          theSummary <- paste0(c("mean\terror\taxis\tgroup\tmean.x\tmean.y\terror.x\terror.y\n",
                                 paste0(
                                   signif(as.numeric(theValues[, 1]), 7), "\t",
                                   signif(as.numeric(theValues[, 2]), 7), "\t",
                                   theValues[, 3], "\t",
                                   sapply(allThePoints, function(x) paste0(point_getTags(x)[2], "\t")),
                                   theValues[, 4], "\t",
                                   theValues[, 5], "\t",
                                   theValues[, 6], "\t",
                                   theValues[, 7], "\n"
                                 )), collapse = "")

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "error bar extractions")
          return(theSummary)
        }

        getRegressionExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE) {

          allThePoints <- point_getAllbyType("regression")
          if(length(allThePoints) == 0) return(data.frame())

          regressionCoords <- lapply(allThePoints, function(x) point_getCoordinates(x))

          theValues <- lapply(regressionCoords,
                              function(x) {
                                x1 <- suppressWarnings(coordinate_calibrate(x[1], "x"))
                                y1 <- suppressWarnings(coordinate_calibrate(x[2], "y"))
                                x2 <- suppressWarnings(coordinate_calibrate(x[3], "x"))
                                y2 <- suppressWarnings(coordinate_calibrate(x[4], "y"))
                                slope <- (y2 - y1)/(x2 - x1)
                                intercept <- y1 - slope * x1
                                x1coord <- x[1]
                                y1coord <- x[2]
                                x2coord <- x[3]
                                y2coord <- x[4]
                                return(c(x1, y1, x2, y2, slope, intercept, x1coord, y1coord, x2coord, y2coord))
                              })

          theValues <- data.frame(matrix(unlist(theValues), nrow = length(theValues), byrow = TRUE))

          theSummary <- paste0(c("x1\ty1\tx2\ty2\tslope\tintercept\tx1.coord\ty1.coord\tx2.coord\ty2.coord\tgroup\n",
                                 paste0(
                                   signif(as.numeric(theValues[, 1]), 7), "\t",
                                   signif(as.numeric(theValues[, 2]), 7), "\t",
                                   signif(as.numeric(theValues[, 3]), 7), "\t",
                                   signif(as.numeric(theValues[, 4]), 7), "\t",
                                   signif(as.numeric(theValues[, 5]), 7), "\t",
                                   signif(as.numeric(theValues[, 6]), 7), "\t",
                                   theValues[, 7], "\t",
                                   theValues[, 8], "\t",
                                   theValues[, 9], "\t",
                                   theValues[, 10], "\t",
                                   sapply(allThePoints, function(x) paste0(point_getTags(x)[2], "\n"))
                                 )), collapse = "")

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "regression line extractions")
          return(theSummary)
        }

        getAxisExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE) {

          theSummary <- paste0(c("coord\tX.axis\tY.axis\n",
                                 paste0(
                                   c("y1", "x1", "y2", "x2"), "\t",
                                   as.numeric(tkcoords(mainFigureCanvas, x_calibrationLine)), "\t",
                                   as.numeric(tkcoords(mainFigureCanvas, y_calibrationLine)), "\n"
                                 )), collapse = "")

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "axis line extractions")
          return(theSummary)
        }

        getLineExtractions <- function(sendToWindow = FALSE, sendToFile = FALSE) {

          allThePoints <- point_getAllbyType("line")
          if(length(allThePoints) == 0) return(data.frame())

          lineCoords <- lapply(allThePoints, function(x) point_getCoordinates(x))

          allText <- data.frame()
          for(i in 1:length(lineCoords)) {
            coordMatrix <- matrix(lineCoords[[i]], ncol = 2, byrow = TRUE)
            allCoords <- split(coordMatrix, row(coordMatrix))
            theValues <- lapply(allCoords,
                                function(somePoint) {
                                  x <- suppressWarnings(coordinate_calibrate(somePoint[1], "x"))
                                  y <- suppressWarnings(coordinate_calibrate(somePoint[2], "y"))
                                  xcoord <- somePoint[1]
                                  ycoord <- somePoint[2]
                                  return(c(x, y, xcoord, ycoord))
                                })
            someText <- data.frame(matrix(unlist(theValues), nrow = length(theValues), byrow = TRUE),
                                   c(1:length(theValues)),
                                   i,
                                   point_getTags(allThePoints[i])[2])

            if(is.null(dim(allText))) allText <- someText
            else allText <- rbind(allText, someText)
          }

          theSummary <- paste0(c("x\ty\tx.coord\ty.coord\tlink\tset\tgroup\n",
                                 paste0(
                                   signif(as.numeric(allText[, 1]), 7), "\t",
                                   signif(as.numeric(allText[, 2]), 7), "\t",
                                   allText[, 3], "\t",
                                   allText[, 4], "\t",
                                   signif(as.numeric(allText[, 5]), 7), "\t",
                                   as.character(allText[, 6]), "\t",
                                   as.character(allText[, 7]), "\n"
                                 )), collapse = "")

          if(sendToFile == TRUE) return(read.table(text = theSummary, sep = "\t", header = TRUE))
          if(sendToWindow == TRUE) displayData(theSummary, "line extractions")
          return(theSummary)
        }

        # END: text functions for data tabulation
        #########################################


        ########################################
        ##   START: plot/figure/main image frame
        ########################################

        figureWindow <- tcltk::ttkframe(aJuicrWindow)

          mainFigureWidth <- as.integer(tcltk::tcl("image", "width", theFigure))
          mainFigureHeight <- as.integer(tcltk::tcl("image", "height", theFigure))
          mainFigureCanvas <- tcltk::tkcanvas(figureWindow, background = "grey95",
                                       width = figureWindowSize[1], height = figureWindowSize[2],
                                       "-scrollregion",
                                       paste(0, 0, mainFigureWidth + 20, mainFigureHeight + 50))
          mainFigure <- tcltk::tcl(mainFigureCanvas, "create", "image", 0,0,  image = theFigure, anchor = "nw")
          #mainFigureXscroll <- tkscrollbar(figureWindow, command = function(...) tcl(mainFigureCanvas, "xview", ...), orient = "horizontal")
          #mainFigureYscroll <- tkscrollbar(figureWindow, command = function(...) tcl(mainFigureCanvas, "yview", ...), orient = "vertical")
          mainFigureXscroll <- tcltk::tkscrollbar(figureWindow, command = function(...) tcltk::tkxview(mainFigureCanvas, ...), orient = "horizontal")
          mainFigureYscroll <- tcltk::tkscrollbar(figureWindow, command = function(...) tcltk::tkyview(mainFigureCanvas, ...), orient = "vertical")
          tcltk::tkconfigure(mainFigureCanvas, xscrollcommand = function(...) tcltk::tkset(mainFigureXscroll, ...))
          tcltk::tkconfigure(mainFigureCanvas, yscrollcommand = function(...) tcltk::tkset(mainFigureYscroll, ...))

          hoverText <- tcltk::tkcreate(mainFigureCanvas, "text", 0, 0, justify = "left", text = "", fill = "black", font = "Consolas 8")
          hoverShadow <- tcltk::tcl(mainFigureCanvas, "create", "image", 0, 0, image = "", anchor = "nw")

          epsButton <- tcltk::tkbutton(mainFigureCanvas, text = "save image as .eps", relief = "groove",
                                width = 16, command = function(){
                                  tcltk::tkitemconfigure(mainFigureCanvas, epsWindow, state = "hidden")
                                  tcltk::tkitemconfigure(mainFigureCanvas, clearWindow, state = "hidden")
                                  aEspFile <- tcltk::tkgetSaveFile(filetypes = "{{eps postscript files} {.eps}} {{All files} *}",
                                                            defaultextension = ".eps",
                                                            title = "juicr: save exact copy of image/extractions as postscript file")
                                  tcltk::tcl(mainFigureCanvas, "postscript", file = aEspFile)
                                  tcltk::tkitemconfigure(mainFigureCanvas, epsWindow, state = "normal")
                                  tcltk::tkitemconfigure(mainFigureCanvas, clearWindow, state = "normal")
                                })
          epsWindow <- tcltk::tkcreate(mainFigureCanvas, "window", mainFigureWidth, mainFigureHeight + 10, anchor = "ne", window = epsButton)

          clearButton <- tcltk::tkbutton(mainFigureCanvas, text = "hide extractions", relief = "groove",
                                  width = 13, command = function(){
                                    if(as.character(tcltk::tkcget(clearButton, "-relief")) == "sunken") {
                                      tcltk::tkconfigure(clearButton, relief = "groove")
                                      tcltk::tkitemconfigure(mainFigureCanvas, "extraction", state = "normal")
                                    } else {
                                      tcltk::tkconfigure(clearButton, relief = "sunken")
                                      tcltk::tkitemconfigure(mainFigureCanvas, "extraction", state = "hidden")
                                    }

                                  })

          clearWindow <- tcltk::tkcreate(mainFigureCanvas, "window", mainFigureWidth - 130, mainFigureHeight + 10, anchor = "ne", window = clearButton)

          tcltk::tkgrid(mainFigureCanvas, mainFigureYscroll, sticky = "news")
          tcltk::tkgrid(mainFigureXscroll, sticky = "ew")

        ########################################
        ##   END: plot/figure/main image frame
        ########################################



        #################################
        #####   START: options notebook
        #################################

        notebookFrame <- tcltk::ttknotebook(aJuicrWindow)

        ########################################
        ##### START: automated frame in notebook

        automatedWindow <- tcltk::ttkframe(notebookFrame)

        isBarPlot <- function(thePlot,
                              binary_threshold = 0.98,
                              object_threshold = 0.1,
                              bar_length = 0.05) {
          anEBImage <- EBImage::transpose(EBImage::flop(thePlot))
          aBinaryFigure <- 1 - (EBImage::channel(anEBImage, mode = "gray") > binary_threshold)

          aBinaryFigure[, round(dim(aBinaryFigure)[2] * 0.3):dim(aBinaryFigure)[2] ] <- 0
          aBinaryFigure[1:round(dim(aBinaryFigure)[1] * 0.6), ] <- 0
          lineBrush <- EBImage::makeBrush(bar_length * dim(aBinaryFigure)[1], shape = "line", angle = 0)
          verticalLinesOnlyFigure <- EBImage::opening(EBImage::distmap(aBinaryFigure), lineBrush)
          extractedBars <- EBImage::watershed(EBImage::distmap(verticalLinesOnlyFigure), 0.1)
          if(max(extractedBars) > 2) return(TRUE)
          return(FALSE)
        }

        update_X_axis <- function(y1, x1, y2, x2) {
          tcltk::tkcoords(mainFigureCanvas, x_calibrationLine, y1, x1, y2, x2)
        }

        update_Y_axis <- function(y1, x1, y2, x2) {
          tcltk::tkcoords(mainFigureCanvas, y_calibrationLine, y1, x1, y2, x2)
        }


        juiceItReset <- function() {

          tcltk::tkconfigure(juiceButton, image = juicrLogoJuicing); tcltk::tcl("update")

          if(length(c(as.character(tcltk::tkget(mainFigureCanvas, "autobar")), as.character(tcltk::tkget(mainFigureCanvas, "auto")))) != 0) {
            update_X_axis(1, 1, 1, 1); update_Y_axis(1, 1, 1, 1);
            tcltk::tkconfigure(xorangeLabel, image = theOrangeGrey)
            tcltk::tkconfigure(yorangeLabel, image = theOrangeGrey)
            tcltk::tkconfigure(dataOrangeLabel, image = theOrangeGrey); tcltk::tcl("update")
            allthePoints <- point_getAllbyType("point")
            for(i in 1:length(allthePoints)) {
              if((point_getTags(allthePoints[i])[2] == "autobar") || (point_getTags(allthePoints[i])[2] == "auto") || (point_getTags(allthePoints[i])[2] == "cluster")) {
                point_delete(point_indexToPoint(point_getTags(allthePoints[i])[1]))
                tcltk::tcl(mainFigureCanvas, "delete", point_getTags(allthePoints[i])[1])
              }
            }
            tcltk::tkitemconfigure(txtCanvas, theDataText, text = point_summary())
          }

        }


        animateAutodetection <- function() {
          if(animateDelay != FALSE) {Sys.sleep(0.01); tcltk::tcl("update");}
        }



        juiceIt <- function() {

          # reset all
          juiceItReset()

          # start axis detections
          detectedX <- autoX(theFigureJuiced, binary_threshold = as.numeric(text_get(qualityDisplay)))
          if(max(detectedX) == 1) {
            theCoordX <- getCoord_detectedAxis(detectedX)
            tcltk::tkitemconfigure(mainFigureCanvas, x_calibrationLine, width = 10, fill = "orange")

            update_X_axis(theCoordX[1], min(theCoordX[2], theCoordX[4]), theCoordX[3], min(theCoordX[2], theCoordX[4]))
            tcltk::tkconfigure(xorangeLabel, image = theOrange); tcltk::tcl("update")
          }

          detectedY <- autoX(theFigureJuiced, binary_threshold = as.numeric(text_get(qualityDisplay)), asY = TRUE)
          if(max(detectedY) == 1) {
            theCoordY <- getCoord_detectedAxis(detectedY)
            tcltk::tkitemconfigure(mainFigureCanvas, y_calibrationLine, width = 10, fill = "orange")
            update_Y_axis(max(theCoordY[1],theCoordY[3]), theCoordY[2], max(theCoordY[1],theCoordY[3]), theCoordY[4])
            tcltk::tkconfigure(yorangeLabel, image = theOrange); tcltk::tcl("update")
          }

          if((max(detectedX) == 1) && (max(detectedY) == 1)) {
            newXCoord <- resolve_crossedAxes(detectedX, detectedY)
            newYCoord <- resolve_crossedAxes(detectedX, detectedY, asY = TRUE)
            update_X_axis(newXCoord[1], min(newXCoord[2], newXCoord[4]), newXCoord[3], min(newXCoord[2], newXCoord[4]))
            update_Y_axis(max(newYCoord[1],newYCoord[3]), newYCoord[2], max(newYCoord[1],newYCoord[3]), newYCoord[4])
            tcltk::tkitemconfigure(mainFigureCanvas, y_calibrationLine, width = 5, fill = "tomato3")
            tcltk::tkitemconfigure(mainFigureCanvas, x_calibrationLine, width = 5, fill = "tomato")
          }

          # extract bar or scatter data
          if(isBarPlot(theFigureJuiced) == TRUE) {
            detectedBars <- autoBars(theFigureJuiced, detectedX, detectedY, binary_threshold = as.numeric(text_get(qualityDisplay)), bar_length = as.numeric(text_get(barSizeDisplay)))
            theCoords <- getCoord_detectedPoints(detectedBars)
            theCoords <- theCoords[order(theCoords[, 1]), ]

            if(!is.null(theCoords) && (length(theCoords) > 0)) {
              if(!is.null(nrow(theCoords))) {
                for(i in 1:nrow(theCoords)) {autoBar(theCoords[i, 1], theCoords[i, 2]); animateAutodetection();}
              } else {
                autoBar(theCoords[1], theCoords[2]);
              }
            }
            if(max(detectedBars) >= 1) tcltk::tkconfigure(dataOrangeLabel, image = theOrange); tcltk::tcl("update")

          } else {
            detectedPoints <- autoPoints(theFigureJuiced, detectedX, detectedY, point_empty = theAutoPointsAreEmpty, point_shape = theAutoPointsShape, point_size = as.numeric(text_get(circleSizeDisplay)))
            if(max(detectedPoints) >= 1) tcltk::tkconfigure(dataOrangeLabel, image = theOrange); tcltk::tcl("update")

            allAutoPoints <- getNonClusters(detectedPoints)
            theCoords <- getCoord_detectedPoints(allAutoPoints)

            if(!is.null(theCoords) && (length(theCoords) > 0)) {
              if(!is.null(nrow(theCoords))) {
                for(i in 1:nrow(theCoords)) {autoPoint(theCoords[i, 1], theCoords[i, 2]); animateAutodetection();}
              } else {
                autoPoint(theCoords[1], theCoords[2]);
              }
            }
            allAutoClusters <- getClusters(detectedPoints)
            theCoords <- getCoord_detectedPoints(allAutoClusters)

            if(!is.null(theCoords) && (length(theCoords) > 0)) {
              if(!is.null(nrow(theCoords))) {
                for(i in 1:nrow(theCoords)) {autoCluster(theCoords[i, 1], theCoords[i, 2]); animateAutodetection();}
              } else {
                autoCluster(theCoords[1], theCoords[2]);
              }

            }
          }

          tcltk::tkitemconfigure(txtCanvas, theDataText, text = point_summary())
          tcltk::tkconfigure(juiceButton, image = juicrLogo)
        }


        #### START: juicr automate button
        juiceItCanvas <- tcltk::ttkframe(automatedWindow)
        juiceButton <- tcltk::ttkbutton(juiceItCanvas, text = "juice image for data", width=33, compound = 'top', image = juicrLogo, command = function(){juiceIt();})
        tcltk::tkgrid(juiceButton, padx = 2, pady = 8)
        #### END: juicr automate button

        #### END: juicr progress frame
        progressCanvas <- tcltk::ttklabelframe(automatedWindow, text = "Extraction success", padding = 4)
        progressFrame <- tcltk::ttkframe(progressCanvas)
        xorangeLabel <- tcltk::ttklabel(progressFrame, text = "x-axis", compound = 'top', image = theOrangeGrey)
        yorangeLabel<- tcltk::ttklabel(progressFrame, text = "y-axis", compound = 'top', image = theOrangeGrey)
        dataOrangeLabel <- tcltk::ttklabel(progressFrame, text = "data", compound = 'top', image = theOrangeGrey)
        tcltk::tkgrid(xorangeLabel, yorangeLabel, dataOrangeLabel, padx = 7)
        detectionFrame <- tcltk::ttkframe(progressCanvas)
        autoPointLabel <- tcltk::ttklabel(detectionFrame, text = "= detected", compound = "left", image = autoPointImage)
        clusterPointLabel <- tcltk::ttklabel(detectionFrame, text = "= cluster", compound = "left", image = clusterPointImage)
        tcltk::tkgrid(autoPointLabel, clusterPointLabel, padx = 12, pady = 3)
        tcltk::tkgrid(detectionFrame)
        tcltk::tkgrid(progressFrame)
        #### END: juicr progress frame

        #### START: point options frame
        figureTypeCanvas <- tcltk::ttklabelframe(automatedWindow, text = "Point detection options", padding = 6)
        sizeFrame <- tcltk::ttkframe(figureTypeCanvas)
        circleSizeLabel <- tcltk::ttklabel(sizeFrame, text = "= size", width = 7)
        circleSmallButton <- tcltk::tkbutton(sizeFrame, text = "smallest", relief = "groove", image = circlePoint1, command = function(...) {tcltk::tkdelete(circleSizeDisplay, "0.0", "end"); tcltk::tkinsert(circleSizeDisplay, "1.0", as.character(1)); tcltk::tkconfigure(circleSmallButton, relief = "sunken"); tcltk::tkconfigure(circleMediumButton, relief = "groove"); tcltk::tkconfigure(circleBigButton, relief = "groove");} )
        circleMediumButton <- tcltk::tkbutton(sizeFrame, text = "medium", relief = "groove", image = circlePoint5, command = function(...) {tcltk::tkdelete(circleSizeDisplay, "0.0", "end"); tcltk::tkinsert(circleSizeDisplay, "1.0", as.character(5)); tcltk::tkconfigure(circleSmallButton, relief = "groove"); tcltk::tkconfigure(circleMediumButton, relief = "sunken"); tcltk::tkconfigure(circleBigButton, relief = "groove");} )
        tcltk::tkconfigure(circleMediumButton, relief = "sunken")
        circleBigButton <- tcltk::tkbutton(sizeFrame, text = "big", relief = "groove", image = circlePoint15, command = function(...) {tcltk::tkdelete(circleSizeDisplay, "0.0", "end"); tcltk::tkinsert(circleSizeDisplay, "1.0", as.character(15)); tcltk::tkconfigure(circleSmallButton, relief = "groove"); tcltk::tkconfigure(circleMediumButton, relief = "groove"); tcltk::tkconfigure(circleBigButton, relief = "sunken");} )
        circleSizeDisplay <- tcltk::tktext(sizeFrame, foreground = "tomato", height = 1, width = 4)
        tcltk::tkinsert(circleSizeDisplay, "1.0", as.character(5))
        tcltk::tkgrid(circleSmallButton, circleMediumButton, circleBigButton, circleSizeLabel, circleSizeDisplay, padx=3)
        shapeFrame <- tcltk::ttkframe(figureTypeCanvas)
        circleShapeLabel <- tcltk::ttklabel(shapeFrame, text = "= shape", width = 7)
        circleCircleButton <- tcltk::tkbutton(shapeFrame, text = "circle", relief = "groove", image = circlePoint15, command = function(...) {theAutoPointsShape <- "disc"; tcltk::tkconfigure(circleCircleButton, relief = "sunken"); tcltk::tkconfigure(circleDiamondButton, relief = "groove"); tcltk::tkconfigure(circleSquareButton, relief = "groove");})
        tcltk::tkconfigure(circleCircleButton, relief = "sunken")
        circleDiamondButton <- tcltk::tkbutton(shapeFrame, text = "diamond", relief = "groove", image = diamondPoint15, command = function(...) {theAutoPointsShape <- "diamond"; tcltk::tkconfigure(circleCircleButton, relief = "groove"); tcltk::tkconfigure(circleDiamondButton, relief = "sunken"); tcltk::tkconfigure(circleSquareButton, relief = "groove");})
        circleSquareButton <- tcltk::tkbutton(shapeFrame, text = "square", relief = "groove", image = squarePoint15, command = function(...) {theAutoPointsShape <- "box"; tcltk::tkconfigure(circleCircleButton, relief = "groove"); tcltk::tkconfigure(circleDiamondButton, relief = "groove"); tcltk::tkconfigure(circleSquareButton, relief = "sunken");})
        tcltk::tkgrid(circleCircleButton, circleDiamondButton, circleSquareButton, circleShapeLabel, padx=3, pady = 3)
        styleFrame <- tcltk::ttkframe(figureTypeCanvas)
        styleLabel <- tcltk::ttklabel(shapeFrame, text = "= style", width = 7)
        circleClosedButton <- tcltk::tkbutton(shapeFrame, text = "closed", relief = "groove", image = circlePoint15, command = function(...) {theAutoPointsAreEmpty <- FALSE; tcltk::tkconfigure(circleClosedButton, relief = "sunken"); tcltk::tkconfigure(circleOpenButton, relief = "groove");})
        connectLabel <- tcltk::ttklabel(shapeFrame, text = "or")
        tcltk::tkconfigure(circleClosedButton, relief = "sunken")
        circleOpenButton <- tcltk::tkbutton(shapeFrame, text = "open", relief = "groove", image = circlePoint15Closed, command = function(...) {theAutoPointsAreEmpty <- TRUE; tcltk::tkconfigure(circleClosedButton, relief = "groove"); tcltk::tkconfigure(circleOpenButton, relief = "sunken");})
        tcltk::tkgrid(circleClosedButton, connectLabel, circleOpenButton, styleLabel, padx=3)

        tcltk::tkgrid(shapeFrame, sticky = "w")
        tcltk::tkgrid(styleFrame, sticky = "w")
        tcltk::tkgrid(sizeFrame, sticky = "w")
        #tkgrid(clusterPointLabel, sticky = "w" )

        #### END: point options frame

        #### START: line options frame
        lineTypeCanvas <- tcltk::ttklabelframe(automatedWindow, text = "Axis detection options", padding = 6)
        lineFrame <- tcltk::ttkframe(lineTypeCanvas)
        lineQualityLabel <- tcltk::ttklabel(lineFrame, text = "= quality", width = 9)
        highQualityButton <- tcltk::tkbutton(lineFrame, text = "smallest", relief = "groove", width = 21, height = 21,  image = lineQualityHigh, command = function(...) {tcltk::tkdelete(qualityDisplay, "0.0", "end"); tcltk::tkinsert(qualityDisplay, "1.0", as.character(0.6)); tcltk::tkconfigure(highQualityButton, relief = "sunken"); tcltk::tkconfigure(lowQualityButton, relief = "groove");} )
        tcltk::tkconfigure(highQualityButton, relief = "sunken")
        lineConnectLabel <- tcltk::ttklabel(lineFrame, text = "or")
        lowQualityButton <- tcltk::tkbutton(lineFrame, text = "medium", relief = "groove", width = 21, height = 21, image = lineQualityLow, command = function(...) {tcltk::tkdelete(qualityDisplay, "0.0", "end"); tcltk::tkinsert(qualityDisplay, "1.0", as.character(0.4)); tcltk::tkconfigure(highQualityButton, relief = "groove"); tcltk::tkconfigure(lowQualityButton, relief = "sunken");} )
        qualityDisplay <- tcltk::tktext(lineFrame, foreground = "tomato", height = 1, width = 4)
        tcltk::tkinsert(qualityDisplay, "1.0", as.character(0.6))

        tcltk::tkgrid(highQualityButton, lineConnectLabel, lowQualityButton, lineQualityLabel, qualityDisplay, padx=3)
        tcltk::tkgrid(lineFrame, sticky = "w")
        #### END: line options frame

        #### START: bar options frame
        barTypeCanvas <- tcltk::ttklabelframe(automatedWindow, text = "Bar detection options", padding = 6)
        barFrame <- tcltk::ttkframe(barTypeCanvas)
        barSizeLabel <- tcltk::ttklabel(barFrame, text = "= size", width = 7)
        barSmallButton <- tcltk::tkbutton(barFrame, text = "smallest", relief = "groove", image = barPoint1, command = function(...) {tcltk::tkdelete(barSizeDisplay, "0.0", "end"); tcltk::tkinsert(barSizeDisplay, "1.0", as.character(3)); tcltk::tkconfigure(barSmallButton, relief = "sunken"); tcltk::tkconfigure(barMediumButton, relief = "groove"); tcltk::tkconfigure(barBigButton, relief = "groove");})
        barMediumButton <- tcltk::tkbutton(barFrame, text = "medium", relief = "groove", image = barPoint5, command = function(...) {tcltk::tkdelete(barSizeDisplay, "0.0", "end"); tcltk::tkinsert(barSizeDisplay, "1.0", as.character(9)); tcltk::tkconfigure(barSmallButton, relief = "groove"); tcltk::tkconfigure(barMediumButton, relief = "sunken"); tcltk::tkconfigure(barBigButton, relief = "groove");})
        tcltk::tkconfigure(barMediumButton, relief = "sunken")
        barBigButton <- tcltk::tkbutton(barFrame, text = "big", relief = "groove", image = barPoint15, command = function(...) {tcltk::tkdelete(barSizeDisplay, "0.0", "end"); tcltk::tkinsert(barSizeDisplay, "1.0", as.character(19)); tcltk::tkconfigure(barSmallButton, relief = "groove"); tcltk::tkconfigure(barMediumButton, relief = "groove"); tcltk::tkconfigure(barBigButton, relief = "sunken");})
        barSizeDisplay <- tcltk::tktext(barFrame, foreground = "tomato", height = 1, width = 4)
        tcltk::tkinsert(barSizeDisplay, "1.0", as.character(9))
        tcltk::tkgrid(barSmallButton, barMediumButton, barBigButton, barSizeLabel, barSizeDisplay, padx=3)
        tcltk::tkgrid(barFrame, sticky = "w")
        #### END: bar options frame

        tcltk::tkgrid(juiceItCanvas, padx = 24, pady = 3)
        tcltk::tkgrid(progressCanvas)
        tcltk::tkgrid(lineTypeCanvas)
        tcltk::tkgrid(figureTypeCanvas)
        tcltk::tkgrid(barTypeCanvas)

        tcltk::tkgrid(automatedWindow)

        ##### END: automated frame in notebook
        ########################################


        ########################################
        ##### START: manual frame in notebook

        manualWindow <- tcltk::ttkframe(aJuicrWindow)

        #### START: zoom frame
        zoomFrame <- tcltk::ttkframe(manualWindow)
        zoomCanvas <- tcltk::tkcanvas(zoomFrame, width = 225, height = 225)
        zoomFigure <- tcltk::tcl("image", "create", "photo")
        tcltk::tcl(zoomFigure, "copy", theFigure, "-from", 0, 0, 77, 77, "-zoom", 3)
        zoomWidth <- as.integer(tcltk::tcl("image", "width", zoomFigure))
        zoomHeight <- as.integer(tcltk::tcl("image", "height", zoomFigure))
        zoomImage <- tcltk::tcl(zoomCanvas, "create", "image", 0, 0, image = zoomFigure, anchor = "nw")
        tcltk::tkcreate(zoomCanvas, "rec", (zoomWidth - 1)/2 - 1, (zoomHeight - 1)/2 - 1, (zoomWidth - 1)/2 + 1, (zoomHeight - 1)/2 + 1, outline = "DarkOrange1", fill = "DarkOrange1")
        tcltk::tkcreate(zoomCanvas, "line", (zoomWidth - 1)/2 - 30, (zoomHeight - 1)/2, (zoomWidth - 1)/2 - 16, (zoomHeight - 1)/2, width = 3, fill = "turquoise3")
        tcltk::tkcreate(zoomCanvas, "line", (zoomWidth - 1)/2 + 30, (zoomHeight - 1)/2, (zoomWidth - 1)/2 + 16, (zoomHeight - 1)/2, width = 3, fill = "turquoise3")
        tcltk::tkcreate(zoomCanvas, "line", (zoomWidth - 1)/2, (zoomHeight - 1)/2 - 30, (zoomWidth - 1)/2, (zoomHeight - 1)/2 - 16, width = 3, fill = "turquoise3")
        tcltk::tkcreate(zoomCanvas, "line", (zoomWidth - 1)/2, (zoomHeight - 1)/2 + 30, (zoomWidth - 1)/2, (zoomHeight - 1)/2 + 16, width = 3, fill = "turquoise3")

        coordTypes <- c("pixels", "data"); theValue <- tcltk::tclVar("NA");
        pixelComboBox <- tcltk::ttkcombobox(zoomFrame, value = coordTypes, textvariable = theValue, width = 6, font = "Consolas 8")
        tcltk::tkcreate(zoomCanvas, "window", 5, 206, anchor = "nw", window = pixelComboBox)
        tcltk::tkset(pixelComboBox, coordTypes[1])
        theCOORD <- sprintf("(x,y)=(%5s,%5s)", "NA", "NA")
        zoomText <- tcltk::tkcreate(zoomCanvas, "text", 159, 215, justify = "left", text = theCOORD, fill = "grey", font = "Consolas 9")

        tcltk::tkgrid(zoomCanvas, padx = 7, pady = 5)
        #### END: zoom frame

        #### START: figure type frame
        figureTypeCanvas <- tcltk::ttklabelframe(manualWindow, text = "plot-type (scatter, error bar, other)", padding = 8)
        scatterPlotButton <- tcltk::tkbutton(figureTypeCanvas,
                                      command = function(){
                                        set_juicr("x_error", FALSE); set_juicr("y_error", FALSE); set_juicr("x_regression", FALSE); set_juicr("x_connected", FALSE)
                                        tcltk::tkconfigure(scatterPlotButton, relief = "sunken"); tcltk::tkconfigure(barPlotButton, relief = "raised"); tcltk::tkconfigure(linePlotButton, relief = "raised");
                                        tcltk::tkpack.forget(manualWindowItems[4]); tcltk::tkpack.forget(manualWindowItems[5]); tcltk::tkpack(manualWindowItems[3], after = manualWindowItems[2]);
                                        tcltk::tkcoords(mainFigureCanvas, x_errorLine, 1, 1, 1, 1); tcltk::tkcoords(mainFigureCanvas, y_errorLine, 1, 1, 1, 1);
                                        tcltk::tkcoords(mainFigureCanvas, x_regressionLine, 1, 1, 1, 1);
                                      }, text = "scatter", image = imageScatter)
        tcltk::tkconfigure(scatterPlotButton, relief = "sunken")
        barPlotButton <- tcltk::tkbutton(figureTypeCanvas,
                                  command = function(){
                                    set_juicr("x_error", FALSE); set_juicr("y_error", TRUE); set_juicr("x_regression", FALSE); set_juicr("x_connected", FALSE)
                                    tcltk::tkconfigure(scatterPlotButton, relief = "raised"); tcltk::tkconfigure(barPlotButton, relief = "sunken"); tcltk::tkconfigure(linePlotButton, relief = "raised");
                                    tcltk::tkpack.forget(manualWindowItems[3]); tcltk::tkpack.forget(manualWindowItems[5]); tcltk::tkpack(manualWindowItems[4], after = manualWindowItems[2])
                                  }, text = "error", image = imageBarX)
        linePlotButton <- tcltk::tkbutton(figureTypeCanvas,
                                   command = function(){
                                     set_juicr("y_error", FALSE); set_juicr("x_error", FALSE); set_juicr("x_regression", FALSE); set_juicr("x_connected", FALSE)
                                     tcltk::tkconfigure(scatterPlotButton, relief = "raised"); tcltk::tkconfigure(barPlotButton, relief = "raised"); tcltk::tkconfigure(linePlotButton, relief = "sunken");
                                     tcltk::tkpack.forget(manualWindowItems[3]); tcltk::tkpack.forget(manualWindowItems[4]); tcltk::tkpack(manualWindowItems[5], after = manualWindowItems[2])
                                     tcltk::tkcoords(mainFigureCanvas, x_errorLine, 1, 1, 1, 1); tcltk::tkcoords(mainFigureCanvas, y_errorLine, 1, 1, 1, 1);
                                     tcltk::tkcoords(mainFigureCanvas, x_regressionLine, 1, 1, 1, 1);
                                   }, text = "line", image = imageLine)
        tcltk::tkgrid(scatterPlotButton, barPlotButton, linePlotButton, padx = 8)
        #### END: figure type frame


        #### START: figure calibration frame
        figureCalibration <- tcltk::ttklabelframe(manualWindow, text = "plot-to-data calibration\n     (min/max = plotted values on axis)", padding = 8)
        calibrationXButton <- tcltk::tkbutton(figureCalibration, command = function(){set_juicr("x_calibrate", TRUE); tcltk::tkconfigure(calibrationXButton, relief = "sunken");}, text = "add\nx-axis", width = 5, height = 2, foreground = "tomato")
        calibrationYButton <- tcltk::tkbutton(figureCalibration, command = function(){set_juicr("y_calibrate", TRUE); tcltk::tkconfigure(calibrationYButton, relief = "sunken");}, text = "add\ny-axis", width = 5, height = 2, foreground = "tomato3")

        xcaptionCanvas <- tcltk::ttkframe(figureCalibration)
        figureXminLabel <- tcltk::ttklabel(xcaptionCanvas, text = "min", font = "Arial 8")
        figureXminDisplay <- tcltk::tktext(xcaptionCanvas, foreground = "tomato", height = 1, width = 4)
        figureXmaxLabel <-tcltk:: ttklabel(xcaptionCanvas, text = "max", font = "Arial 8")
        figureXmaxDisplay <- tcltk::tktext(xcaptionCanvas, foreground = "tomato", height = 1, width = 4)
        figureXcaptionLabel <- tcltk::ttklabel(xcaptionCanvas, text = "label", font = "Arial 8")
        figureXcaptionDisplay <- tcltk::tktext(xcaptionCanvas, foreground = "tomato", height = 1, width = 9)
        tcltk::tkinsert(figureXcaptionDisplay, "1.0", "x")
        figureXunitsLabel <- tcltk::ttklabel(xcaptionCanvas, text = "units", font = "Arial 8")
        figureXunitsDisplay <- tcltk::tktext(xcaptionCanvas, foreground = "tomato", height = 1, width = 9)
        tcltk::tkgrid(figureXcaptionLabel, figureXcaptionDisplay, figureXminLabel, figureXminDisplay)
        tcltk::tkgrid(figureXunitsLabel, figureXunitsDisplay, figureXmaxLabel, figureXmaxDisplay)

        ycaptionCanvas <- tcltk::ttkframe(figureCalibration)
        figureYminLabel <- tcltk::ttklabel(ycaptionCanvas, text = "min", font = "Arial 8")
        figureYminDisplay <- tcltk::tktext(ycaptionCanvas, foreground = "tomato3", height = 1, width = 4)
        figureYmaxLabel <- tcltk::ttklabel(ycaptionCanvas, text = "max", font = "Arial 8")
        figureYmaxDisplay <- tcltk::tktext(ycaptionCanvas, foreground = "tomato3", height = 1, width = 4)
        figureYcaptionLabel <- tcltk::ttklabel(ycaptionCanvas, text = "label", font = "Arial 8")
        figureYcaptionDisplay <- tcltk::tktext(ycaptionCanvas, foreground = "tomato3", height = 1, width = 9)
        tcltk::tkinsert(figureYcaptionDisplay, "1.0", "y")
        figureYunitsLabel <- tcltk::ttklabel(ycaptionCanvas, text = "units", font = "Arial 8")
        figureYunitsDisplay <- tcltk::tktext(ycaptionCanvas, foreground = "tomato3", height = 1, width = 9)
        tcltk::tkgrid(figureYcaptionLabel, figureYcaptionDisplay, figureYminLabel, figureYminDisplay)
        tcltk::tkgrid(figureYunitsLabel, figureYunitsDisplay, figureYmaxLabel, figureYmaxDisplay)

        tcltk::tkgrid(calibrationXButton, xcaptionCanvas)
        tcltk::tkgrid(calibrationYButton, ycaptionCanvas)

        x_calibrationLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato", arrow = "both")
        tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", x_calibrationLine)
        #x_calibrate <- FALSE; x_startCalibrate <- FALSE; x_endCalibrate <- FALSE;
        set_juicr("x_calibrate", FALSE); set_juicr("x_startCalibrate", FALSE); set_juicr("x_endCalibrate", FALSE);

        y_calibrationLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato3", arrow = "both")
        tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", y_calibrationLine)
        #y_calibrate <- FALSE; y_startCalibrate <- FALSE; y_endCalibrate <- FALSE;
        set_juicr("y_calibrate", FALSE); set_juicr("y_startCalibrate", FALSE); set_juicr("y_endCalibrate", FALSE);

        #### END: figure calibration frame


        #### END: figure error frame
        figureError <- tcltk::ttklabelframe(manualWindow, text = "add points with error bars\n    (e.g., bar, whisker, box plots)", padding = 8)
        errorXbutton <- tcltk::tkbutton(figureError, width = 70,
                                 command = function(){
                                   set_juicr("x_error", TRUE); set_juicr("y_error", FALSE);
                                   tcltk::tkconfigure(errorXbutton, relief = "sunken"); tcltk::tkconfigure(errorYbutton, relief = "raised");
                                 }, text = "add error\n on x", image = imageBarY)
        errorYbutton <- tcltk::tkbutton(figureError, width = 70,
                                 command = function(){
                                   set_juicr("x_error", FALSE); set_juicr("y_error", TRUE);
                                   tcltk::tkconfigure(errorXbutton, relief = "raised"); tcltk::tkconfigure(errorYbutton, relief = "sunken");
                                 }, text = "add error\n on x", image = imageBarX)
        tcltk::tkconfigure(errorYbutton, relief = "sunken")
        tcltk::tkgrid(errorYbutton, errorXbutton, pady = 4, padx = 5)

        theMean <- tcltk::tclVar("NA"); theError <- tcltk::tclVar("NA"); theSample <- tcltk::tclVar("NA"); theAxisType <- tcltk::tclVar("NA");
        meanTypes <- c("mean", "median", "%", "count", "prediction", "sample", "other", "none")
        meanComboBox <- tcltk::ttkcombobox(figureError, value = meanTypes, textvariable = theMean, width = 6)
        tcltk::tkset(meanComboBox, meanTypes[1])
        errorTypes <- c("SD", "SE", "95%CI", "range", "min", "max", "IQR", "LQ", "UQ", "other", "none")
        errorComboBox <- tcltk::ttkcombobox(figureError, value = errorTypes, textvariable = theError, width = 4)
        tcltk::tkset(errorComboBox, errorTypes[1])
        tcltk::tkgrid(meanComboBox, errorComboBox, sticky = "nwse")

        x_errorLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato", arrow = "first")
        set_juicr("x_error", FALSE); set_juicr("x_startError", FALSE); set_juicr("x_endError", FALSE);

        y_errorLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato3", arrow = "first")
        set_juicr("y_error", FALSE); set_juicr("y_startError", FALSE); set_juicr("y_endError", FALSE);


        #### END: figure error frame

        #### START: figure regression frame
        figureLine <- tcltk::ttklabelframe(manualWindow, text = "add lines\n    (e.g., regression, line plot)", padding = 0)

        regressionButton <- tcltk::tkbutton(figureLine, width = 70,
                                     command = function(){
                                       set_juicr("x_regression", TRUE); set_juicr("x_connected", FALSE);
                                       tcltk::tkconfigure(regressionButton, relief = "sunken"); tcltk::tkconfigure(connectedButton, relief = "raised");
                                     }, text = "add\nslope", image = imageRegression)
        x_regressionLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato")
        #x_regression <- FALSE; x_startRegression <- FALSE; x_endRegression <- FALSE;
        set_juicr("x_regression", FALSE); set_juicr("x_startRegression", FALSE); set_juicr("x_endRegression", FALSE);

        connectedButton <- tcltk::tkbutton(figureLine, width = 70,
                                    command = function(){
                                      set_juicr("x_regression", FALSE); set_juicr("x_connected", TRUE);
                                      if(as.character(tcltk::tkcget(connectedButton, "-relief")) == "sunken") {
                                        xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_connectedLine))
                                        createMultiLine(xyPos)
                                        tcltk::tkcoords(mainFigureCanvas, x_connectedLine, 1,1,1,1)
                                        set_juicr("x_startConnected", FALSE); set_juicr("x_endConnected", FALSE); set_juicr("x_connected", FALSE);
                                        updatedSummary <- point_summary()
                                        tcltk::tkitemconfigure(txtCanvas, theDataText, text = updatedSummary)
                                        tcltk::tkconfigure(regressionButton, relief = "raised"); tcltk::tkconfigure(connectedButton, relief = "raised");
                                      } else {
                                        tcltk::tkconfigure(regressionButton, relief = "raised"); tcltk::tkconfigure(connectedButton, relief = "sunken");
                                      }
                                    }, text = "add\n connected line", image = imageLine)

        x_connectedLine <- tcltk::tkcreate(mainFigureCanvas, "line", 1, 1, 1, 1, width = 0, fill = "tomato")
        #x_connected <- FALSE; x_startConnected <- FALSE; x_endConnected <- FALSE;
        #x_connectedPos <- 1; y_connectedPos <- 1
        set_juicr("x_connected", FALSE); set_juicr("x_startConnected", FALSE); set_juicr("x_endConnected", FALSE);
        set_juicr("x_connectedPos", 1); set_juicr("y_connectedPos", 1);

        tcltk::tkgrid(regressionButton, connectedButton, pady = 4, padx = 5)
        #### END: figure regression frame


        #### START: figure grouping frame
        radioGroup <- tcltk::ttklabelframe(manualWindow, text = "extract-by-group (group=color+label)", padding = 8)

        groupRadio1 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[1], background = "white")
        groupRadio2 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[2], background = "white")
        groupRadio3 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[3], background = "white")
        groupRadio4 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[4], background = "white")
        groupRadio5 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[5], background = "white")
        groupRadio6 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[6], background = "white")
        groupRadio7 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[7], background = "white")
        groupRadio8 <- tcltk::tkradiobutton(radioGroup, foreground = groupColors[8], background = "white")
        groupRadio1Label <- tcltk::tktext(radioGroup, foreground = groupColors[1], height = 1, width = 12, font = "Arial 8")
        groupRadio2Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio3Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio4Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio5Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio6Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio7Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        groupRadio8Label <- tcltk::tktext(radioGroup, foreground = "white", height = 1, width = 12, font = "Arial 8")
        tcltk::tkinsert(groupRadio1Label, "1.0", groupNames[1])
        tcltk::tkinsert(groupRadio2Label, "1.0", groupNames[2])
        tcltk::tkinsert(groupRadio3Label, "1.0", groupNames[3])
        tcltk::tkinsert(groupRadio4Label, "1.0", groupNames[4])
        tcltk::tkinsert(groupRadio5Label, "1.0", groupNames[5])
        tcltk::tkinsert(groupRadio6Label, "1.0", groupNames[6])
        tcltk::tkinsert(groupRadio7Label, "1.0", groupNames[7])
        tcltk::tkinsert(groupRadio8Label, "1.0", groupNames[8])

        pointGroup <- tcltk::tclVar("NA")
        tcltk::tkconfigure(groupRadio1, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio1Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[1]); tcltk::tkconfigure(groupRadio1Label, foreground = groupColors[1]);})
        tcltk::tkconfigure(groupRadio2, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio2Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[2]); tcltk::tkconfigure(groupRadio2Label, foreground = groupColors[2]);})
        tcltk::tkconfigure(groupRadio3, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio3Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[3]); tcltk::tkconfigure(groupRadio3Label, foreground = groupColors[3]);})
        tcltk::tkconfigure(groupRadio4, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio4Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[4]); tcltk::tkconfigure(groupRadio4Label, foreground = groupColors[4]);})
        tcltk::tkconfigure(groupRadio5, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio5Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[5]); tcltk::tkconfigure(groupRadio5Label, foreground = groupColors[5]);})
        tcltk::tkconfigure(groupRadio6, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio6Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[6]); tcltk::tkconfigure(groupRadio6Label, foreground = groupColors[6]);})
        tcltk::tkconfigure(groupRadio7, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio7Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[7]); tcltk::tkconfigure(groupRadio7Label, foreground = groupColors[7]);})
        tcltk::tkconfigure(groupRadio8, variable = pointGroup, value = as.character(tcltk::tcl(groupRadio8Label, "get", "1.0", "end")), command = function() {set_juicr("pointColor", groupColors[8]); tcltk::tkconfigure(groupRadio8Label, foreground = groupColors[8]);})
        tcltk::tcl(groupRadio1, "select");

        tcltk::tkgrid(groupRadio1, groupRadio1Label, groupRadio2, groupRadio2Label, pady = 0)
        tcltk::tkgrid(groupRadio3, groupRadio3Label, groupRadio4, groupRadio4Label, pady = 0)
        tcltk::tkgrid(groupRadio5, groupRadio5Label, groupRadio6, groupRadio6Label, pady = 0)
        tcltk::tkgrid(groupRadio7, groupRadio7Label, groupRadio8, groupRadio8Label, pady = 0)

        tcltk::tkpack(zoomFrame, figureTypeCanvas, figureCalibration, figureError, figureLine, radioGroup)

        tcltk::tkgrid(manualWindow)
        manualWindowItems <- as.character(tcltk::tkpack.slaves(manualWindow))
        tcltk::tkpack.forget(manualWindowItems[4])
        tcltk::tkpack.forget(manualWindowItems[5])

        ##### END: manual frame in notebook
        ########################################

        tcltk::tkadd(notebookFrame, automatedWindow, sticky = "nswe", text = " automated ", compound = "left")
        tcltk::tkinsert(notebookFrame, 0, manualWindow, sticky = "nswe", text = " manual ")

        #################################
        #####   END: options notebook
        #################################


        #######################################
        #####   START: data and save frame
        #######################################

        saveJuicr <- function() {

          # convert tcltk txt into regular txt
          fullNotes <- ""
          for(i in 1:(as.integer(tclvalue(tcl(theNotes, "index", "end"))) - 1)) {
            lineNotes <- tcltk::tcl(theNotes, "get", paste0(i, ".0"), paste0(i, ".end"))
            fullNotes <- paste0(fullNotes, paste0(lineNotes, collapse = " "), "\n")
          }

          # collect juicr settings
          settingsJuicr <- data.frame(
            "theNotes" = fullNotes,
            "circleSmallButton" = as.character(tcltk::tkcget(circleSmallButton, "-relief")),
            "circleMediumButton" = as.character(tcltk::tkcget(circleMediumButton, "-relief")),
            "circleBigButton" = as.character(tcltk::tkcget(circleBigButton, "-relief")),
            "circleSizeDisplay" = as.character(text_get(circleSizeDisplay)),
            "circleCircleButton" = as.character(tcltk::tkcget(circleCircleButton, "-relief")),
            "circleDiamondButton" = as.character(tcltk::tkcget(circleDiamondButton, "-relief")),
            "circleSquareButton" = as.character(tcltk::tkcget(circleSquareButton, "-relief")),
            "circleClosedButton" = as.character(tcltk::tkcget(circleClosedButton, "-relief")),
            "circleOpenButton" = as.character(tcltk::tkcget(circleOpenButton, "-relief")),
            "highQualityButton" = as.character(tcltk::tkcget(highQualityButton, "-relief")),
            "lowQualityButton" = as.character(tcltk::tkcget(lowQualityButton, "-relief")),
            "qualityDisplay" = as.character(text_get(qualityDisplay)),
            "barSmallButton" = as.character(tcltk::tkcget(barSmallButton, "-relief")),
            "barMediumButton" = as.character(tcltk::tkcget(barMediumButton, "-relief")),
            "barBigButton" = as.character(tcltk::tkcget(barBigButton, "-relief")),
            "barSizeDisplay" = as.character(text_get(barSizeDisplay)),
            "figureXminDisplay" = as.character(text_get(figureXminDisplay)),
            "figureXmaxDisplay" = as.character(text_get(figureXmaxDisplay)),
            "figureXcaptionDisplay" = as.character(text_get(figureXcaptionDisplay)),
            "figureXunitsDisplay" = as.character(text_get(figureXunitsDisplay)),
            "figureYminDisplay" = as.character(text_get(figureYminDisplay)),
            "figureYmaxDisplay" = as.character(text_get(figureYmaxDisplay)),
            "figureYcaptionDisplay" = as.character(text_get(figureYcaptionDisplay)),
            "figureYunitsDisplay" = as.character(text_get(figureYunitsDisplay)),
            "meanComboBox" = as.character(tcltk::tkget(meanComboBox)),
            "errorComboBox" = as.character(tcltk::tkget(errorComboBox)),
            "groupRadio1Label" = as.character(text_get(groupRadio1Label)),
            "groupRadio2Label" = as.character(text_get(groupRadio2Label)),
            "groupRadio3Label" = as.character(text_get(groupRadio3Label)),
            "groupRadio4Label" = as.character(text_get(groupRadio4Label)),
            "groupRadio5Label" = as.character(text_get(groupRadio5Label)),
            "groupRadio6Label" = as.character(text_get(groupRadio6Label)),
            "groupRadio7Label" = as.character(text_get(groupRadio7Label)),
            "groupRadio8Label" = as.character(text_get(groupRadio8Label)),
            "groupRadio1LabelStatus" = tclvalue(tcltk::tkcget(groupRadio1Label, "-foreground")),
            "groupRadio2LabelStatus" = tclvalue(tcltk::tkcget(groupRadio2Label, "-foreground")),
            "groupRadio3LabelStatus" = tclvalue(tcltk::tkcget(groupRadio3Label, "-foreground")),
            "groupRadio4LabelStatus" = tclvalue(tcltk::tkcget(groupRadio4Label, "-foreground")),
            "groupRadio5LabelStatus" = tclvalue(tcltk::tkcget(groupRadio5Label, "-foreground")),
            "groupRadio6LabelStatus" = tclvalue(tcltk::tkcget(groupRadio6Label, "-foreground")),
            "groupRadio7LabelStatus" = tclvalue(tcltk::tkcget(groupRadio7Label, "-foreground")),
            "groupRadio8LabelStatus" = tclvalue(tcltk::tkcget(groupRadio8Label, "-foreground"))
          )

          # collect extractions
          resultsJuicr <- list("axes" = getAxisExtractions(sendToFile = TRUE),
                               "points" = getPointExtractions(sendToFile = TRUE),
                               "points_coordinates" = getPointExtractions(sendToFile = TRUE, coordinates = TRUE),
                               "autoBars" = getBarExtractions(sendToFile = TRUE),
                               "errorBars" = getErrorExtractions(sendToFile = TRUE),
                               "regressions" = getRegressionExtractions(sendToFile = TRUE),
                               "lines" = getLineExtractions(sendToFile = TRUE))


          # collect image settings
          theOriginal <- EBImage::readImage(theFigureFile)
          #theStandardized <- theFigure #EBImage::readImage(theStandardizedImageFile)

          theFigureExtractions <- theFigureJuiced #EBImage::readImage(theStandardizedImageFile)

            theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                        resultsJuicr$axes$X.axis[1],
                                                        resultsJuicr$axes$X.axis[2],
                                                        radius = 7, col = grDevices::rgb(t(grDevices::col2rgb("mediumseagreen")), maxColorValue = 255),
                                                        fill = TRUE)
            theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                        resultsJuicr$axes$X.axis[3],
                                                        resultsJuicr$axes$X.axis[4],
                                                        radius = 7, col = grDevices::rgb(t(grDevices::col2rgb("mediumseagreen")), maxColorValue = 255),
                                                        fill = TRUE)


            theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                        resultsJuicr$axes$Y.axis[1],
                                                        resultsJuicr$axes$Y.axis[2],
                                                        radius = 7, col = grDevices::rgb(t(grDevices::col2rgb("mediumseagreen")), maxColorValue = 255),
                                                        fill = TRUE)

            theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                        resultsJuicr$axes$Y.axis[3],
                                                        resultsJuicr$axes$Y.axis[4],
                                                        radius = 7, col = grDevices::rgb(t(grDevices::col2rgb("mediumseagreen")), maxColorValue = 255),
                                                        fill = TRUE)

        if(nrow(resultsJuicr$points_coordinates) != 0) {
          for(i in 1:nrow(resultsJuicr$points_coordinates)) {
            theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                 resultsJuicr$points_coordinates$x.coord[i],
                                                 resultsJuicr$points_coordinates$y.coord[i],
                                                 radius = 3, col = grDevices::rgb(t(grDevices::col2rgb("orange")), maxColorValue = 255),
                                                 fill = TRUE)
          }
        }

        if(nrow(resultsJuicr$errorBars) != 0) {
            for(i in 1:nrow(resultsJuicr$errorBars)) {
              theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                          resultsJuicr$errorBars$mean.x[i],
                                                          resultsJuicr$errorBars$mean.y[i],
                                                          radius = 3, col = grDevices::rgb(t(grDevices::col2rgb("dodgerblue")), maxColorValue = 255),
                                                          fill = TRUE)
              theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                          resultsJuicr$errorBars$error.x[i],
                                                          resultsJuicr$errorBars$error.y[i],
                                                          radius = 3, col = grDevices::rgb(t(grDevices::col2rgb("dodgerblue")), maxColorValue = 255),
                                                          fill = TRUE)
            }
        }

          if(nrow(resultsJuicr$regressions) != 0) {
            for(i in 1:nrow(resultsJuicr$regressions)) {
              theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                          resultsJuicr$regressions$x1.coord[i],
                                                          resultsJuicr$regressions$y1.coord[i],
                                                          radius = 5, col = grDevices::rgb(t(grDevices::col2rgb("violet")), maxColorValue = 255),
                                                          fill = TRUE)
              theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                          resultsJuicr$regressions$x2.coord[i],
                                                          resultsJuicr$regressions$y2.coord[i],
                                                          radius = 5, col = grDevices::rgb(t(grDevices::col2rgb("violet")), maxColorValue = 255),
                                                          fill = TRUE)
            }
          }


          if(nrow(resultsJuicr$lines) != 0) {
            for(i in 1:nrow(resultsJuicr$lines)) {
              theFigureExtractions <- EBImage::drawCircle(theFigureExtractions,
                                                          resultsJuicr$lines$x.coord[i],
                                                          resultsJuicr$lines$y.coord[i],
                                                          radius = 3, col = grDevices::rgb(t(grDevices::col2rgb("slateblue")), maxColorValue = 255),
                                                          fill = TRUE)
            }
          }

          theExtractions <- paste0(tools::file_path_sans_ext(basename(theFigureFile)), "_juicr_extracted.png")
          EBImage::writeImage(theFigureExtractions, file = theExtractions, type = "png")
          EBImage::writeImage(theFigureJuiced, file = theStandardizedImageFile, type = "png")

          filesJurcr <- data.frame("file_name" = c(basename(theFigureFile), basename(theStandardizedImageFile), theExtractions),
                                   "formated" = c("original", "standardized", "standardized with extractions"),
                                   "size_bites" = c(file.info(theFigureFile)$size, file.info(theStandardizedImageFile)$size, file.info(theExtractions)$size),
                                   "date_created" = c(paste(file.info(theFigureFile)$ctime), paste(file.info(theStandardizedImageFile)$ctime), paste(file.info(theExtractions)$ctime)),
                                   "width_pixels" = c(dim(theOriginal)[1], dim(theFigureJuiced)[1], dim(theFigureExtractions)[1]),
                                   "height_pixels" = c(dim(theOriginal)[2], dim(theFigureJuiced)[2], dim(theFigureExtractions)[2]))


          toHTML_table <- function(aDataFrame, theID, aConnection) {
            #message(attributes(aDataFrame))
            if(length(aDataFrame) == 0) {
              cat(paste0("<table style=\"border-spacing: 20px 0px;\" id=\"", theID ,"\">\n"), file = aConnection)
              cat("<tr>\n", paste0("<th>", "no extractions", "</th>\n"), "</tr>\n", file = aConnection)
              cat("<tr>\n", paste0("<th>", "NA"            , "</th>\n"), "</tr>\n", file = aConnection)
              cat("</table>\n", file = aConnection)
              return("");
            }

            cat(paste0("<table style=\"border-spacing: 20px 0px;\" id=\"", theID ,"\">\n"), file = aConnection)
            cat("<tr>\n", paste0("<th>", labels(aDataFrame)[[2]], "</th>\n"), "</tr>\n", file = aConnection)
            for(i in 1:nrow(aDataFrame)) cat("<tr>\n", paste0("<td>", aDataFrame[i, ], "</td>\n"), "</tr>\n", file = aConnection)
            cat("</table>\n", file = aConnection)
          }

          toHTML_image <- function(theImage, aConnection, type = "jpg", theID = "logo") {
            cat(paste0("<img id=\"", theID, "\" src=\"data:image/", type, ";base64,"), file = aConnection)
            rawFile <- readBin(theImage, "raw", file.info(theImage)$size)
            cat(base64Encode(rawFile, "character")[1], file = aConnection)
            cat("\">\n", file = aConnection)
          }

          toHTML_image2 <- function(theImage, aConnection, type = "jpg", theID = "logo") {
            imgTXT <- paste0("<img id=\"", theID, "\" src=\"data:image/", type, ";base64,")
            rawFile <- readBin(theImage, "raw", file.info(theImage)$size)
            imgTXT <- paste0(imgTXT, RCurl::base64Encode(rawFile, "character")[1], "\">")
            return(imgTXT)
          }

          toHTML <- function(theImageFile, allResults) {

            aConnection <- file(paste0(tools::file_path_sans_ext(basename(allResults$files[1,1])), "_juicr.html"), "w")

            cat("<!DOCTYPE html>\n",
                "<!--\n\tLajeunesse, M.J. (2021) Squeezing data from images with the juicr package for R. v 0.1\n-->\n",
                "<html>\n",
                paste0("<head>\n<title>Juicr extraction: ", basename(theFigureFile), "</title>\n"),
                paste0("<meta name=\"descripton\" content=\"image extractions using juicr R package\">\n"),
                paste0("<meta name=\"author\" content=\"juicr v. 0.1\">\n</head>\n"),
                "<body>\n", file = aConnection)

            toHTML_image(getIMG("test_orange3.png"), aConnection, type = "png")
            cat(paste0("<h1>JUICR record of extractions from image:<br>", allResults$files[1,1] , "</h1>\n"), file = aConnection)

            cat(paste0("<br><hr><br><h2>File information</h2><br>\n"), file = aConnection)
            toHTML_table(allResults$files, "files", aConnection)

            cat(paste0("<br>\n"), file = aConnection)

            collectImages <- data.frame(
              file_name = c(allResults$files$file_name),
              image = c(
                toHTML_image2(theFigureFile, theID = "original"),
                toHTML_image2(theStandardizedImageFile, theID = "standardized"),
                toHTML_image2(allResults$files[3,1], theID = "extracted"))
            )
            toHTML_table(collectImages, "images", aConnection)

            cat(paste0("<br><hr><br><h2>Data extractions from: ", allResults$files[2,1], "</h2><br>\n"), file = aConnection)
            cat(paste0("<h3 style=\"color:orange\">extracted data: points</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$points, "points", aConnection)
            cat(paste0("<h3 style=\"color:mediumseagreen\">extracted data: coordinates for X and Y axes</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$axes, "axes", aConnection)
            cat(paste0("<h3 style=\"color:orange\">extracted data: auto-bars</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$autoBars, "autobars", aConnection)
            cat(paste0("<h3 style=\"color:dodgerblue\">extracted data: error Bars</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$errorBars, "errorbars", aConnection)
            cat(paste0("<h3 style=\"color:violet\">extracted data: regressions</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$regressions, "regressions", aConnection)
            cat(paste0("<h3 style=\"color:slateblue\">extracted data: lines</h3>\n"), file = aConnection)
            toHTML_table(allResults$extractions$lines, "lines", aConnection)

            cat(paste0("<br><hr><br><h2>juicr parameters</h2><br>\n"), file = aConnection)
            toHTML_table(allResults$settings, "parameters", aConnection)

            cat("</body>\n", "</html>\n", file = aConnection)
            close(aConnection)

            file.remove(theExtractions)
            file.remove(theStandardizedImageFile)

          }

          allResults <- list("extractions" = resultsJuicr, "settings" = settingsJuicr, "files" = filesJurcr)


          toHTML("", allResults)
          #print(paste0(getwd(), "/", paste0(tools::file_path_sans_ext(basename(allResults$files[1,1])), "_juicr.html")))
          return(paste0(tools::file_path_sans_ext(basename(allResults$files[1,1])), "_juicr.html"))
        }


        ### START OF DATA FRAME

        dataWindow <- tcltk::ttkframe(aJuicrWindow)

        #### start: text summary frame
        txtCanvas <- tcltk::tkcanvas(dataWindow, background = "white", width = 200, height = 440, "-scrollregion", paste(0, 0, 200, 500 * 13))
        theDataText <- tcltk::tkcreate(txtCanvas, "text", 100, 3, justify = "left", text = point_summary(), font = "Consolas 8", anchor = "n")
        theExtractedScroll <- tcltk::ttkscrollbar(dataWindow, command = function(...) tcltk::tcl(txtCanvas, "yview", ...), orient = "vertical")
        tcltk::tkconfigure(txtCanvas, yscrollcommand = function(...) tcltk::tkset(theExtractedScroll, ...))
        #### end: text summary frame

        #### start: notes frame
        notesCanvas <- tcltk::ttklabelframe(dataWindow, text = "Notes (e.g., user name, fig. #, ref.)", padding = 5)
        theNotes <- tcltk::tktext(notesCanvas, height = 4, width = 26, font = "arial 10")
        tcltk::tkinsert(theNotes, "1.0", "")
        #### end: notes frame

        #### start: save frame
        saveWindow <- tcltk::ttkframe(dataWindow)
        getDataWindow <- tcltk::ttkframe(saveWindow)
        viewAllDataButton <- tcltk::ttkbutton(getDataWindow, text = "  save .csv\nextractions",  command = function() {getPointExtractions(sendToWindow = TRUE); getBarExtractions(sendToWindow = TRUE); getErrorExtractions(sendToWindow = TRUE); getRegressionExtractions(sendToWindow = TRUE); getLineExtractions(sendToWindow = TRUE); getAxisExtractions(sendToWindow = TRUE);})
        #exportRButton <- ttkbutton(getDataWindow,text = "export to R",  command = function() get_ExtractionList())
        aboutButton <- tcltk::ttkbutton(getDataWindow, text = "help/cite", command = function() aboutJuicrWindow())


        saveButton <- tcltk::ttkbutton(saveWindow, compound = "left", text = "save\nextractions\nas .html", image = orangeJuiceSave,
                                       command = function() {
                                         #tcltk::tk_choose.dir()
                                          tcltk::tkconfigure(saveButton, text = paste0("saving..."))
                                          tcltk::tcl("update"); Sys.sleep(2);
                                                  set_juicr("theSavedFile", saveJuicr());
                                                  updatedSummary <- point_summary();
                                          tcltk::tkitemconfigure(txtCanvas, theDataText, text = updatedSummary);
                                          tcltk::tkconfigure(saveButton, text = paste0("save\nextractions\nas .html"));})

        #### end: save frame

        tcltk::tkgrid(txtCanvas, theExtractedScroll, sticky = "news")
        tcltk::tkgrid(theNotes, pady = 3)
        tcltk::tkgrid(notesCanvas, sticky = "news")
        tcltk::tkgrid(viewAllDataButton, pady = 1, sticky = "news")
        #tkgrid(exportRButton, pady = 1, sticky = "news")
        tcltk::tkgrid(aboutButton, pady = 1, sticky = "news")

        tcltk::tkgrid(getDataWindow, saveButton, padx = 5, pady = 6, sticky = "news")
        tcltk::tkgrid(saveWindow)



        #######################################
        #####   END: data and save frame
        #######################################

        tcltk::tkpack(figureWindow, side = "left", pady = 15, padx = 15)
        tcltk::tkpack(dataWindow, side = "right", pady = 15, padx = 15)
        tcltk::tkpack(notebookFrame, side = "top", pady = 15, padx = 15)



        # # # # # # # # # # # # # # # # # #
        ##### END OF JUICR GUI WINDOW #####
        # # # # # # # # # # # # # # # # # #

        #############################################################################
        #############################################################################




        ##---------------------------
        ## START: interactivity
        ##---------------------------


        mainFigureMouseOver <- function(x, y){

          xpos <- as.numeric(tcltk::tcl(mainFigureCanvas$ID, "canvasx", as.integer(x)))
          ypos <- as.numeric(tcltk::tcl(mainFigureCanvas$ID, "canvasy", as.integer(y)))

          # update the zoom coordinates
          if(as.character(tcltk::tkget(pixelComboBox)) == "pixels") {
            tcltk::tkitemconfigure(zoomCanvas, zoomText, text = sprintf("(x,y)=(%5s,%5s)", xpos, ypos))
          } else {
            tcltk::tkitemconfigure(zoomCanvas, zoomText, text = sprintf("(x,y)=(%5s,%5s)",
                                                                 signif(coordinate_calibrate(xpos, "x"), 4),
                                                                 signif(coordinate_calibrate(ypos, "y"), 4)))
          }

          xfigMax <- as.integer(tcltk::tcl("image", "width", theFigure))
          yfigMax <- as.integer(tcltk::tcl("image", "height", theFigure))
          zoomFigure <- tcltk::tcl("image", "create", "photo", paste(zoomFigure))
          xmin <- ifelse(xpos <= 38, 0, xpos - 38)
          ymin <- ifelse(ypos <= 38, 0, ypos - 38)
          xmax <- ifelse(xpos >= xfigMax - 38, xfigMax, xpos + 38)
          ymax <- ifelse(ypos >= yfigMax - 38, yfigMax, ypos + 38)
          tcltk::tcl(zoomFigure, "copy", theFigure, "-from", xmin, ymin, xmax, ymax, "-zoom", 3)
          tcltk::tkitemconfigure(zoomCanvas, zoomImage, image = zoomFigure)
          tcltk::tkcoords(zoomCanvas, zoomImage, ifelse(xpos <= 38, (77*3)/2 - xpos*3, 0), ifelse(ypos <= 38, (77*3)/2 - ypos*3, 0))

          ### START: X-axis calibration
          if(get_juicr("x_calibrate") == TRUE && get_juicr("x_startCalibrate") == FALSE) {
            tcltk::tkitemconfigure(mainFigureCanvas, x_calibrationLine, width = 5)
            update_X_axis(xpos, ypos, xpos + 30, ypos)
          }

          if(get_juicr("y_calibrate") == TRUE && get_juicr("y_startCalibrate") == FALSE) {
            tcltk::tkitemconfigure(mainFigureCanvas, y_calibrationLine, width = 5)
            update_Y_axis(xpos, ypos, xpos, ypos + 30)
          }

          if(get_juicr("x_startCalibrate") == TRUE) {
            xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_calibrationLine))
            update_X_axis(xyPos[1], xyPos[2], xpos, xyPos[2])
          }
          if(get_juicr("y_startCalibrate") == TRUE) {
            xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, y_calibrationLine))
            update_Y_axis(xyPos[1], xyPos[2], xyPos[1], ypos)
          }
          ### END: X-axis calibration

          if(as.character(tcltk::tkcget(errorXbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_error") == TRUE && get_juicr("x_startError") == FALSE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_errorLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, x_errorLine, xpos, ypos, xpos, ypos,
                       xpos, ypos - 7, xpos, ypos + 8)
            }
            if(get_juicr("x_startError") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_errorLine))
              tcltk::tkcoords(mainFigureCanvas, x_errorLine, xyPos[1], xyPos[2], xpos, xyPos[2],
                       xpos, xyPos[2] - 7, xpos, xyPos[2] + 8)
            }
          }

          if(as.character(tcltk::tkcget(errorYbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {
            if(get_juicr("y_error") == TRUE && get_juicr("y_startError") == FALSE) {
              tcltk::tkitemconfigure(mainFigureCanvas, y_errorLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, y_errorLine, xpos, ypos, xpos, ypos,
                       xpos - 7, ypos, xpos + 8, ypos)
            }
            if(get_juicr("y_startError") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, y_errorLine))
              tcltk::tkcoords(mainFigureCanvas, y_errorLine, xyPos[1], xyPos[2], xyPos[1], ypos,
                       xyPos[1] - 7, ypos, xyPos[1] + 8, ypos)
            }
          }


          ##############
          if(as.character(tcltk::tkcget(regressionButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_regression") == TRUE && get_juicr("x_startRegression") == FALSE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_regressionLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, x_regressionLine, xpos, ypos, xpos + 2, ypos + 2)
            }
            if(get_juicr("x_startRegression") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_regressionLine))
              tcltk::tkcoords(mainFigureCanvas, x_regressionLine, xyPos[1], xyPos[2], xpos + 2, ypos + 2)
            }
          }

          ##############
          if(as.character(tcltk::tkcget(connectedButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_connected") == TRUE && get_juicr("x_startConnected") == FALSE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_connectedLine, width = 3, arrow = "last")
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_connectedLine))
              if(length(xyPos) == 4) {tcltk::tkcoords(mainFigureCanvas, x_connectedLine, xpos, ypos, xpos + 2, ypos + 2)}
              else {
                tcltk::tkcoords(mainFigureCanvas, x_connectedLine, as.character(c(head(xyPos,-2L), xpos, ypos)))}
            }
            if(get_juicr("x_startConnected") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_connectedLine))
              if(length(xyPos) == 4) {tcltk::tkcoords(mainFigureCanvas, x_connectedLine, xyPos[1], xyPos[2], xpos + 2, ypos + 2)}
              else {tcltk::tkcoords(mainFigureCanvas, x_connectedLine, as.character(c(head(xyPos,-2L), xpos, ypos)))}
            }
          }

        }

        deletePoint <- function() {
          point_delete(point_indexToPoint(point_getTags("current")[1]))
          tcltk::tcl(mainFigureCanvas, "delete", "current")
          tcltk::tkitemconfigure(txtCanvas, theDataText, text = point_summary())
          tcltk::tkitemconfigure(mainFigureCanvas, hoverText,  text = "")
          tcltk::tkcoords(mainFigureCanvas, hoverText, 0, 0)
          tcltk::tkitemconfigure(mainFigureCanvas, hoverShadow, image = "")
          tcltk::tkcoords(mainFigureCanvas, hoverShadow, 0, 0)
        }

        createPoint <- function(xPos, yPos) {
          # create new point
          newPoint <- tcltk::tkcreate(mainFigureCanvas, "oval",
                               xPos - pointSize,
                               yPos - pointSize,
                               xPos + pointSize,
                               yPos + pointSize,
                               width = 1,
                               outline = "white",
                               fill = get_juicr("pointColor"))
          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", newPoint)

          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, as.character(tcltk::tclvalue(pointGroup)), "withtag", newPoint)

          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "point", "withtag", newPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", newPoint)


        }


        autoPoint <- function(xPos, yPos) {
          # create new point
          newPoint <- tcltk::tcl(mainFigureCanvas,
                          "create", "image",
                          xPos - 8, yPos - 8,
                          image = autoPointImage, anchor = "nw")

          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", newPoint)

          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, "auto", "withtag", newPoint)

          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "point", "withtag", newPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", newPoint)
        }


        autoCluster <- function(xPos, yPos) {
          # create new point
          newPoint <- tcltk::tcl(mainFigureCanvas,
                          "create", "image",
                          xPos - 8, yPos - 8,
                          image = clusterPointImage, anchor = "nw")

          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", newPoint)

          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, "cluster", "withtag", newPoint)

          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "point", "withtag", newPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", newPoint)
        }

        autoBar <- function(xPos, yPos, xAdjust = 8, yAdjust = 4) {

          # create new point
          newPoint <- tcltk::tcl(mainFigureCanvas,
                          "create", "image",
                          xPos - xAdjust, yPos - yAdjust,
                          image = theBarImage, anchor = "nw")

          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", newPoint)

          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, "autobar", "withtag", newPoint)

          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "point", "withtag", newPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", newPoint)
        }

        createErrorBarX <- function(x1, y1, x2, y2) {

          errorPoint <- tcltk::tkcreate(mainFigureCanvas, "line",
                                 x1, y1,     x2, y2,
                                 x2, y1 - 7, x2, y2 + 8, # cap
                                 width = 3,
                                 arrow = "first",
                                 fill = get_juicr("pointColor"))
          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", errorPoint)
          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, as.character(tcltk::tclvalue(pointGroup)), "withtag", errorPoint)
          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "error", "withtag", errorPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", errorPoint)

        }

        createErrorBarY <- function(x1, y1, x2, y2) {

          errorPoint <- tcltk::tkcreate(mainFigureCanvas, "line",
                                 x1, y1,     x1,     y2,
                                 x1 - 7, y2, x1 + 8, y2, # cap
                                 width = 3,
                                 arrow = "first",
                                 fill = get_juicr("pointColor"))
          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", errorPoint)
          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, as.character(tcltk::tclvalue(pointGroup)), "withtag", errorPoint)
          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "error", "withtag", errorPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", errorPoint)

        }

        createRegressionLine <- function (x1, y1, x2, y2) {

          regressionPoint <- tcltk::tkcreate(mainFigureCanvas, "line",
                                      x1, y1, x2, y2,
                                      width = 3,
                                      fill = get_juicr("pointColor"))
          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", regressionPoint)
          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, as.character(tcltk::tclvalue(pointGroup)), "withtag", regressionPoint)
          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "regression", "withtag", regressionPoint)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", regressionPoint)


        }

        createMultiLine <- function (theXYs) {

          connectedPoints <- tcltk::tkcreate(mainFigureCanvas, "line",
                                      as.character(c(head(theXYs,-2L))),
                                      width = 3,
                                      fill = get_juicr("pointColor"),
                                      arrow = "last")
          # add unique ID
          tcltk::tkaddtag(mainFigureCanvas, point_pointToIndex(point_add()), "withtag", connectedPoints)
          # add grouping ID
          tcltk::tkaddtag(mainFigureCanvas, as.character(tcltk::tclvalue(pointGroup)), "withtag", connectedPoints)
          # add common ID
          tcltk::tkaddtag(mainFigureCanvas, "line", "withtag", connectedPoints)

          # add all common tag ID
          tcltk::tkaddtag(mainFigureCanvas, "extraction", "withtag", connectedPoints)

        }

        mainFigureClick <- function(x, y) {

          xPos <- as.numeric(tcltk::tcl(mainFigureCanvas$ID, "canvasx", as.integer(x)))
          yPos <- as.numeric(tcltk::tcl(mainFigureCanvas$ID, "canvasy", as.integer(y)))

          if(!any(get_juicr("y_calibrate"), get_juicr("x_calibrate"), get_juicr("y_startCalibrate"), get_juicr("x_startCalibrate"), get_juicr("y_endCalibrate"), get_juicr("x_endCalibrate"), as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken", as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken")) {
            createPoint(xPos, yPos)
          }

          ### START: axis calibration
          if(get_juicr("x_startCalibrate") == TRUE) {
            xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_calibrationLine))
            update_X_axis(xyPos[1], xyPos[2], xPos, xyPos[2])
            set_juicr("x_startCalibrate", FALSE); set_juicr("x_endCalibrate", FALSE); set_juicr("x_calibrate", FALSE);
            tcltk::tkconfigure(calibrationXButton, relief = "raised");
          }

          if(get_juicr("x_calibrate") == TRUE) {
            update_X_axis(xPos, yPos, xPos + 30, yPos)
            set_juicr("x_startCalibrate", TRUE)
          }

          if(get_juicr("y_startCalibrate") == TRUE) {
            xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, y_calibrationLine))
            update_Y_axis(xyPos[1], xyPos[2], xyPos[1], yPos)
            set_juicr("y_startCalibrate", FALSE); set_juicr("y_endCalibrate", FALSE); set_juicr("y_calibrate", FALSE);
            tcltk::tkconfigure(calibrationYButton, relief = "raised");
          }

          if(get_juicr("y_calibrate") == TRUE) {
            update_Y_axis(xPos, yPos, xPos, yPos + 30)
            set_juicr("y_startCalibrate", TRUE)
          }
          ### END: X-axis calibration

          if(as.character(tcltk::tkcget(errorXbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_startError") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_errorLine))
              createErrorBarX(xyPos[1], xyPos[2], xPos, xyPos[2])
              tcltk::tkcoords(mainFigureCanvas, x_errorLine, 1, 1, 1, 1)
              set_juicr("x_startError", FALSE); set_juicr("x_endError", FALSE); set_juicr("x_error", FALSE);
            }

            if(get_juicr("x_error") == TRUE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_errorLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, x_errorLine, xPos, yPos, xPos, yPos,
                       xPos, yPos - 7, xPos, yPos + 8)
              set_juicr("x_startError", TRUE)
            }
          }

          if(as.character(tcltk::tkcget(errorYbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {
            if(get_juicr("y_startError") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, y_errorLine))
              createErrorBarY(xyPos[1], xyPos[2], xyPos[1], yPos)
              tcltk::tkcoords(mainFigureCanvas, y_errorLine, 1, 1, 1, 1)
              set_juicr("y_startError", FALSE); set_juicr("y_endError", FALSE); set_juicr("y_error", FALSE);
            }

            if(get_juicr("y_error") == TRUE) {
              tcltk::tkitemconfigure(mainFigureCanvas, y_errorLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, y_errorLine, xPos, yPos, xPos, yPos,
                       xPos - 7, yPos, xPos + 8, yPos)
              set_juicr("y_startError", TRUE)
            }
          }

          if(as.character(tcltk::tkcget(errorXbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {set_juicr("x_error", TRUE); set_juicr("y_error", FALSE);}
          if(as.character(tcltk::tkcget(errorYbutton, "-relief")) == "sunken" && as.character(tcltk::tkcget(barPlotButton, "-relief")) == "sunken") {set_juicr("y_error", TRUE); set_juicr("x_error", FALSE);}


          if(as.character(tcltk::tkcget(regressionButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_startRegression") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_regressionLine))
              createRegressionLine(xyPos[1], xyPos[2], xPos, yPos)
              tcltk::tkcoords(mainFigureCanvas, x_regressionLine, 1, 1, 1, 1)
              set_juicr("x_startRegression", FALSE); set_juicr("x_endRegression", FALSE); set_juicr("x_regression", FALSE);
            }
            if(get_juicr("x_regression") == TRUE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_regressionLine, width = 3)
              tcltk::tkcoords(mainFigureCanvas, x_regressionLine, xPos, yPos, xPos, yPos)
              set_juicr("x_startRegression", TRUE)
            }
          }

          if(as.character(tcltk::tkcget(regressionButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {set_juicr("x_regression", TRUE);}

          if(as.character(tcltk::tkcget(connectedButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {
            if(get_juicr("x_startConnected") == TRUE) {
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_connectedLine))
              set_juicr("x_connectedPos", xPos); set_juicr("y_connectedPos", yPos)
              tcltk::tkcoords(mainFigureCanvas, x_connectedLine, as.character(c(xyPos, xPos, yPos)))

              set_juicr("x_startConnected", FALSE); set_juicr("x_endConnected", FALSE); #set_juicr("x_connected", FALSE);
            }
            if(get_juicr("x_connected") == TRUE) {
              tcltk::tkitemconfigure(mainFigureCanvas, x_connectedLine, width = 3)
              xyPos <- as.numeric(tcltk::tkcoords(mainFigureCanvas, x_connectedLine))
              if(length(xyPos) == 4) {tcltk::tkcoords(mainFigureCanvas, x_connectedLine, xPos, yPos, xPos, yPos)}
              else {tcltk::tkcoords(mainFigureCanvas, x_connectedLine, as.character(c(head(xyPos,-2L), xPos, yPos)))}
              set_juicr("x_startConnected", TRUE)
            }
          }

          if(as.character(tcltk::tkcget(connectedButton, "-relief")) == "sunken" && as.character(tcltk::tkcget(linePlotButton, "-relief")) == "sunken") {set_juicr("x_connected", TRUE);}

          # update summary canvas
          updatedSummary <- point_summary()
          tcltk::tkitemconfigure(txtCanvas, theDataText, text = updatedSummary)

        }

        ##---------------------------
        ## END: interactivity
        ##---------------------------



        if(openJuicrFile != "") {

          openJuicr <- function(openJuicrFile){

            # collect tables from juicr .html file
            inputTables <- XML::readHTMLTable(openJuicrFile)

            # update parameters
            tcltk::tkdelete(theNotes, "0.0", "end"); tcltk::tkinsert(theNotes, "1.0", inputTables$parameters$theNotes)

            tcltk::tkconfigure(circleSmallButton, relief = inputTables$parameters$circleSmallButton)
            tcltk::tkconfigure(circleMediumButton, relief = inputTables$parameters$circleMediumButton)
            tcltk::tkconfigure(circleBigButton, relief = inputTables$parameters$circleBigButton)
            tcltk::tkdelete(circleSizeDisplay, "0.0", "end"); tcltk::tkinsert(circleSizeDisplay, "1.0", inputTables$parameters$circleSizeDisplay)

            tcltk::tkconfigure(highQualityButton, relief = inputTables$parameters$highQualityButton)
            tcltk::tkconfigure(lowQualityButton, relief = inputTables$parameters$lowQualityButton)
            tcltk::tkdelete(qualityDisplay, "0.0", "end"); tcltk::tkinsert(qualityDisplay, "1.0", inputTables$parameters$qualityDisplay)

            tcltk::tkconfigure(circleCircleButton, relief = inputTables$parameters$circleCircleButton)
            tcltk::tkconfigure(circleDiamondButton, relief = inputTables$parameters$circleDiamondButton)
            tcltk::tkconfigure(circleSquareButton, relief = inputTables$parameters$circleSquareButton)
            tcltk::tkconfigure(circleClosedButton, relief = inputTables$parameters$circleClosedButton)
            tcltk::tkconfigure(circleOpenButton, relief = inputTables$parameters$circleOpenButton)

            tcltk::tkconfigure(barSmallButton, relief = inputTables$parameters$barSmallButton)
            tcltk::tkconfigure(barMediumButton, relief = inputTables$parameters$barMediumButton)
            tcltk::tkconfigure(barBigButton, relief = inputTables$parameters$barBigButton)
            tcltk::tkdelete(barSizeDisplay, "0.0", "end"); tcltk::tkinsert(barSizeDisplay, "1.0", inputTables$parameters$barSizeDisplay)

            tcltk::tkdelete(figureXminDisplay, "0.0", "end"); tcltk::tkinsert(figureXminDisplay, "1.0", inputTables$parameters$figureXminDisplay)
            tcltk::tkdelete(figureXmaxDisplay, "0.0", "end"); tcltk::tkinsert(figureXmaxDisplay, "1.0", inputTables$parameters$figureXmaxDisplay)
            tcltk::tkdelete(figureXcaptionDisplay, "0.0", "end"); tcltk::tkinsert(figureXcaptionDisplay, "1.0", inputTables$parameters$figureXcaptionDisplay)
            tcltk::tkdelete(figureXunitsDisplay, "0.0", "end"); tcltk::tkinsert(figureXunitsDisplay, "1.0", inputTables$parameters$figureXunitsDisplay)
            tcltk::tkdelete(figureYminDisplay, "0.0", "end"); tcltk::tkinsert(figureYminDisplay, "1.0", inputTables$parameters$figureYminDisplay)
            tcltk::tkdelete(figureYmaxDisplay, "0.0", "end"); tcltk::tkinsert(figureYmaxDisplay, "1.0", inputTables$parameters$figureYmaxDisplay)
            tcltk::tkdelete(figureYcaptionDisplay, "0.0", "end"); tcltk::tkinsert(figureYcaptionDisplay, "1.0", inputTables$parameters$figureYcaptionDisplay)
            tcltk::tkdelete(figureYunitsDisplay, "0.0", "end"); tcltk::tkinsert(figureYunitsDisplay, "1.0", inputTables$parameters$figureYunitsDisplay)

            tcltk::tkset(meanComboBox, inputTables$parameters$meanComboBox)
            tcltk::tkset(errorComboBox, inputTables$parameters$errorComboBox)

            for(i in 1:8) {
              eval(parse(text = paste0(
                "    tcltk::tkconfigure(groupRadio", i ,"Label, foreground = inputTables$parameters$groupRadio", i ,"LabelStatus)
                           tcltk::tkdelete(groupRadio", i ,"Label, \"0.0\", \"end\")
                           tcltk::tkinsert(groupRadio", i ,"Label, \"1.0\", inputTables$parameters$groupRadio", i ,"Label)
                      "))
              )
            }

            # collect color groups
            theColorGroups <- c(inputTables$parameters$groupRadio1Label,
                                inputTables$parameters$groupRadio2Label,
                                inputTables$parameters$groupRadio3Label,
                                inputTables$parameters$groupRadio4Label,
                                inputTables$parameters$groupRadio5Label,
                                inputTables$parameters$groupRadio6Label,
                                inputTables$parameters$groupRadio7Label,
                                inputTables$parameters$groupRadio8Label)

            theColorGroupsColor <- c(inputTables$parameters$groupRadio1LabelStatus,
                                     inputTables$parameters$groupRadio2LabelStatus,
                                     inputTables$parameters$groupRadio3LabelStatus,
                                     inputTables$parameters$groupRadio4LabelStatus,
                                     inputTables$parameters$groupRadio5LabelStatus,
                                     inputTables$parameters$groupRadio6LabelStatus,
                                     inputTables$parameters$groupRadio7LabelStatus,
                                     inputTables$parameters$groupRadio8LabelStatus)


            # update calibration lines
            tcltk::tkitemconfigure(mainFigureCanvas, x_calibrationLine, width = 5)
            loadedX <- as.numeric(inputTables$axes$X); update_X_axis(loadedX[1],
                                                                     loadedX[2],
                                                                     loadedX[3],
                                                                     loadedX[4])
            tcltk::tkitemconfigure(mainFigureCanvas, y_calibrationLine, width = 5)
            loadedY <- as.numeric(inputTables$axes$Y); update_Y_axis(loadedY[1],
                                                                     loadedY[2],
                                                                     loadedY[3],
                                                                     loadedY[4])



            # add autobars
            loadedAutoBars <- inputTables$points[inputTables$points$group == "autobar", ]
            if(nrow(loadedAutoBars) != 0) {
              for(i in 1:nrow(loadedAutoBars)) autoBar(as.numeric(loadedAutoBars$x.coord[i]),
                                                       as.numeric(loadedAutoBars$y.coord[i]),
                                                       yAdjust = 3)
            }


            # add autopoints
            loadedAutoPoints <- inputTables$points[inputTables$points$group == "auto", ]
            if(nrow(loadedAutoPoints) != 0) {
              for(i in 1:nrow(loadedAutoPoints)) autoPoint(as.numeric(loadedAutoPoints$x.coord[i]),
                                                           as.numeric(loadedAutoPoints$y.coord[i]))
            }

            # add autoclusters
            loadedAutoClusters <- inputTables$points[inputTables$points$group == "cluster", ]
            if(nrow(loadedAutoClusters) != 0) {
              for(i in 1:nrow(loadedAutoClusters)) autoCluster(as.numeric(loadedAutoClusters$x.coord[i]),
                                                               as.numeric(loadedAutoClusters$y.coord[i]))
            }

            # add manual points
            for(i in 1:8) {
              eval(parse(text = paste0(
                "if(inputTables$parameters$groupRadio", i ,"LabelStatus != \"white\") {
                    loadedManualPoints <- inputTables$points[inputTables$points$group == inputTables$parameters$groupRadio", i ,"Label, ]
                    if(nrow(loadedManualPoints) != 0) {
                      set_juicr(\"pointColor\", inputTables$parameters$groupRadio", i ,"LabelStatus)
                      tcltk::tcl(groupRadio", i ,", \"select\")
                      for(i in 1:nrow(loadedManualPoints)) createPoint(as.numeric(loadedManualPoints$x.coord[i]),
                                                                       as.numeric(loadedManualPoints$y.coord[i]))
                    }
                  }"))
              )
            }

            # add error bars TODO: colors
            loadedErrorBars <- inputTables$errorbars
            if(colnames(loadedErrorBars)[1] != "no extractions") {
              for(i in 1:nrow(loadedErrorBars)) {

                if(loadedErrorBars$axis[i] == "y") {

                  eval(parse(text = paste0("tcltk::tcl(groupRadio", which(theColorGroups == loadedErrorBars$group[i]), " , \"select\")")))
                  set_juicr("pointColor", theColorGroupsColor[which(theColorGroups == loadedErrorBars$group[i])])
                  createErrorBarY(as.numeric(loadedErrorBars$mean.x[i]),
                                  as.numeric(loadedErrorBars$mean.y[i]),
                                  as.numeric(loadedErrorBars$error.x[i]),
                                  as.numeric(loadedErrorBars$error.y[i]))
                }

                if(loadedErrorBars$axis[i] == "x") {
                  eval(parse(text = paste0("tcltk::tcl(groupRadio", which(theColorGroups == loadedErrorBars$group[i]), " , \"select\")")))
                  set_juicr("pointColor", theColorGroupsColor[which(theColorGroups == loadedErrorBars$group[i])])
                  createErrorBarX(as.numeric(loadedErrorBars$mean.x[i]),
                                  as.numeric(loadedErrorBars$mean.y[i]),
                                  as.numeric(loadedErrorBars$error.x[i]),
                                  as.numeric(loadedErrorBars$error.y[i]))
                }
              }
            }


            loadedRegressions <- inputTables$regressions
            if(colnames(loadedRegressions)[1] != "no extractions") {
              for(i in 1:nrow(loadedRegressions)) {
                eval(parse(text = paste0("tcltk::tcl(groupRadio", which(theColorGroups == loadedRegressions$group[i]), " , \"select\")")))
                set_juicr("pointColor", theColorGroupsColor[which(theColorGroups == loadedRegressions$group[i])])
                createRegressionLine(as.numeric(loadedRegressions$x1.coord[i]),
                                     as.numeric(loadedRegressions$y1.coord[i]),
                                     as.numeric(loadedRegressions$x2.coord[i]),
                                     as.numeric(loadedRegressions$y2.coord[i]))
              }
            }

            loadedMultiLines <- inputTables$lines
            if(colnames(loadedMultiLines)[1] != "no extractions") {

              for(j in unique(inputTables$lines$set)) {
                theSet <- inputTables$lines[ which(j == inputTables$lines$set), ]
                eval(parse(text = paste0("tcltk::tcl(groupRadio", which(theColorGroups == theSet$group[1]), " , \"select\")")))

                setCoords <- unlist(strsplit(paste(theSet[, 3],  theSet[, 4], sep = " ", collapse = " "), " "))
                set_juicr("pointColor", theColorGroupsColor[which(theColorGroups == theSet$group[1])])

                createMultiLine(setCoords)
              }
            }

            tcltk::tkitemconfigure(txtCanvas, theDataText, text = point_summary())
          }

          openJuicr(openJuicrFile)
        }





        ####################################
        # START: mouse and keyboard bindings

        tcltk::tkitembind(mainFigureCanvas, mainFigure, "<Motion>", mainFigureMouseOver)
        tcltk::tkitembind(mainFigureCanvas, mainFigure, "<Button-1>", mainFigureClick)

        tcltk::tkitembind(mainFigureCanvas, x_calibrationLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, x_calibrationLine, "<Motion>", mainFigureMouseOver)
        tcltk::tkitembind(mainFigureCanvas, y_calibrationLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, y_calibrationLine, "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, x_errorLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, x_errorLine, "<Motion>", mainFigureMouseOver)
        tcltk::tkitembind(mainFigureCanvas, y_errorLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, y_errorLine, "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, x_regressionLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, x_regressionLine, "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, x_connectedLine, "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, x_connectedLine, "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, "point", "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, "point", "<Any-Enter>", function() {
          if((point_getTags("current")[2] != "autobar") && (point_getTags("current")[2] != "auto") && (point_getTags("current")[2] != "cluster")) {set_juicr("tempPointColor", tcltk::tkitemcget(mainFigureCanvas, "current", "-fill")); tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 4, outline = "tomato3", fill = "tomato3");}
          theCoords <- point_getCoordinates("current")
          theCOORD <- sprintf(" %5s,%5s ", round(theCoords[1], 1), round(theCoords[2], 1))
          tcltk::tkcoords(mainFigureCanvas, hoverText, round(theCoords[1], 2) + 50, round(theCoords[2], 2) - 2)
          tcltk::tkitemconfigure(mainFigureCanvas, hoverText, text = theCOORD)
          tcltk::tkitemconfigure(mainFigureCanvas, hoverShadow, image = hoverImage)
          tcltk::tkcoords(mainFigureCanvas, hoverShadow, round(theCoords[1], 2) + 13, round(theCoords[2], 2) - 9)
          tcltk::tkitemraise(mainFigureCanvas, hoverShadow)
          tcltk::tkitemraise(mainFigureCanvas, hoverText)
        })
        tcltk::tkitembind(mainFigureCanvas, "point", "<Any-Leave>", function() {
          if((point_getTags("current")[2] != "autobar") && (point_getTags("current")[2] != "auto") && (point_getTags("current")[2] != "cluster")) {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 1, outline = "white", fill = get_juicr("tempPointColor"));}
          tcltk::tkitemconfigure(mainFigureCanvas, hoverText,  text = "")
          tcltk::tkcoords(mainFigureCanvas, hoverText, 0, 0)
          tcltk::tkitemconfigure(mainFigureCanvas, hoverShadow, image = "")
          tcltk::tkcoords(mainFigureCanvas, hoverShadow, 0, 0)
        })
        tcltk::tkitembind(mainFigureCanvas, "point", "<Button-3>", deletePoint)
        tcltk::tkitembind(mainFigureCanvas, "point", "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, "error", "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, "error", "<Any-Enter>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 4, fill = "tomato3")})
        tcltk::tkitembind(mainFigureCanvas, "error", "<Any-Leave>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 3, fill = get_juicr("pointColor"))})
        tcltk::tkitembind(mainFigureCanvas, "error", "<Button-3>", deletePoint)
        tcltk::tkitembind(mainFigureCanvas, "error", "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, "regression", "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, "regression", "<Any-Enter>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 4, fill = "tomato3")})
        tcltk::tkitembind(mainFigureCanvas, "regression", "<Any-Leave>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 3, fill = get_juicr("pointColor"))})
        tcltk::tkitembind(mainFigureCanvas, "regression", "<Button-3>", deletePoint)
        tcltk::tkitembind(mainFigureCanvas, "regression", "<Motion>", mainFigureMouseOver)

        tcltk::tkitembind(mainFigureCanvas, "line", "<Button-1>", mainFigureClick)
        tcltk::tkitembind(mainFigureCanvas, "line", "<Any-Enter>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 4, fill = "tomato3")})
        tcltk::tkitembind(mainFigureCanvas, "line", "<Any-Leave>", function() {tcltk::tkitemconfigure(mainFigureCanvas, "current", width = 3, fill = get_juicr("pointColor"))})
        tcltk::tkitembind(mainFigureCanvas, "line", "<Button-3>", deletePoint)
        tcltk::tkitembind(mainFigureCanvas, "line", "<Motion>", mainFigureMouseOver)

        theInputText <- c(paste0("groupRadio", 1:8, "Label"), "figureXcaptionDisplay", "figureYcaptionDisplay", "figureXunitsDisplay", "figureYunitsDisplay", "figureXminDisplay", "figureXmaxDisplay", "figureYminDisplay", "figureYmaxDisplay")
        for(i in theInputText) eval(parse(text = paste0("tcltk::tkbind(", i, ", \"<Key>\", function() {tcltk::tkitemconfigure(txtCanvas, theDataText, text = point_summary())})")))

        ####################################
        # END: mouse and keyboard bindings
      }



    #############################################################################
    #############################################################################


      #############################################################################
      # START: LOAD & PROCESS FIGURE IMAGE

      # FIGURE IMAGE PROCESSING
      get_standardizedFileNames <- function(aFileName) {
        return(paste0(strsplit(aFileName, "[.]")[[1]][1],"_juicr.png"))
      }

      standardizeImage <- function(aFileName) {
        newImage <- EBImage::readImage(aFileName)

        if(standardizeTheImage == TRUE) {
          if(dim(newImage)[1] > standardSize) newImage <- EBImage::resize(newImage, w = standardSize)
        }

        EBImage::writeImage(x = newImage,
                            file = paste0(strsplit(aFileName, "[.]")[[1]][1],"_juicr.png"),
                            type = "png")
        return(get_standardizedFileNames(aFileName))
      }
      # END: LOAD & PROCESS FIGURE IMAGE
      ##################################


    # # # # # # # # # # # # # # # # # # #
    ##### START OF JUICR GUI WINDOW #####
    # # # # # # # # # # # # # # # # # # #



    mainExtractorWindow <- tcltk::tktoplevel(bg = "white", width = 2000, height = 1000)
                           tcltk::tktitle(mainExtractorWindow) <- "juicr: image data extractor"
                           tcltk::tcl("wm", "iconphoto", mainExtractorWindow, juicrLogo)

    # create mainExtractorWindow environment to store globals
    main.env <- new.env()
    set_main <- function(aMainVar, aValue) assign(aMainVar, aValue, envir = main.env)
    get_main <- function(aMainVar) get(aMainVar, envir = main.env)

      # image summary functions
      theFigureSmall <- tcltk::tcl("image", "create", "photo")
      getFigureSmall <- function() return(theFigureSmall)
      update_FigureSmall <- function() {
        tcltk::tcl(theFigureSmall, "copy", get_allJuicrImages()[getCurrentJuicrFrame()], "-subsample", 2, 2)
        tcltk::tkconfigure(button_previewImage, image = getFigureSmall())
      }

      # START of multi-juicr frames
      set_main("numberJuicrFrames", 0)
      addAJuicrFrame <- function() set_main("numberJuicrFrames", get_main("numberJuicrFrames") + 1)

      createNewJuicrFrame <- function(aFileName, sourceHTML) {

        if(sourceHTML == TRUE) {

          # collect tables from juicr .html file
          inputTables <- XML::readHTMLTable(aFileName)

          # collect standardized figure from juicr .html file
          juicrHTML = XML::htmlParse(aFileName)
          inputImages <- XML::xpathSApply(juicrHTML, "//table/tr/td/img", XML::xmlAttrs)["src", ]
          sourceHTML <- aFileName


          # re-create original image but avoid erasing original if in folder

            tempOrginalFileName <- paste0("temp_", inputTables$files$file_name[1])
            file.create(tempOrginalFileName)
            tempImageFile <- file(tempOrginalFileName, "wb")
            writeBin(RCurl::base64Decode(sub(".*,", "", inputImages[1]), mode = "raw"),
                     tempImageFile, useBytes = TRUE)
            close(tempImageFile)
            aFileName <- tempOrginalFileName

          # re-create standardized image
          file.create(inputTables$files$file_name[2])
          tempImageFile <- file(inputTables$files$file_name[2], "wb")
          writeBin(RCurl::base64Decode(sub(".*,", "", inputImages[2]), mode = "raw"),
                   tempImageFile, useBytes = TRUE)
          close(tempImageFile)
          theStandardizedImageFile <- inputTables$files$file_name[2]

        } else {
          theStandardizedImageFile <- standardizeImage(aFileName)
          sourceHTML <- ""
        }

          #theOriginalFigure <- EBImage::readImage(theStandardizedImageFile)

          # the figure displayed in frame widget
          theFigure <- tcltk::tcl("image", "create", "photo", file = theStandardizedImageFile)

          # the figure not displayed but gets juiced for extractions
          theFigureJuiced <- EBImage::readImage(theStandardizedImageFile)

        addAJuicrFrame()
        add_allJuicrImages(as.character(theFigure))

        eval(parse(text = paste0("juicrFrame", get_main("numberJuicrFrames"),
                                 " <- tcltk::tkframe(mainExtractorWindow, background = \"white\"); createJuicrFrame(juicrFrame", get_main("numberJuicrFrames"),
                                 ", aFileName, theStandardizedImageFile, theFigure, theFigureJuiced, ", animateDelay,
                                 ", sourceHTML);")))

        if(get_main("numberJuicrFrames") == 1) {
          eval(parse(text = "tcltk::tkpack(juicrFrame1)"))
          tcltk::tcl(theFigureSmall, "copy", theFigure, "-subsample", 2, 2)
        }
        #file.remove(theStandardizedImageFile)
        #update_ArrowButtons(); if(animateDelay != FALSE) {tcltk::tcl("update"); Sys.sleep(1);}
        eval(parse(text = paste0("return(as.character(juicrFrame", get_main("numberJuicrFrames"), "))")))
      }

      createManyJuicrFrames <- function(aFileList, sourceHTML = FALSE) {
        theJuicrFrames <- c()
        if(length(aFileList) != 1) {
          tempPB <- tcltk::tkProgressBar(title = "juicr: Processing files", label = "",
                                  min = 1, max = length(aFileList), initial = 1, width = 500)
          for(i in 1:length(aFileList)) {
            tcltk::setTkProgressBar(tempPB, i, title = paste("juicr: Processing files = ", basename(aFileList[i])), "")
            theJuicrFrames <- c(theJuicrFrames, createNewJuicrFrame(aFileList[i], sourceHTML)[1])
            #update_ArrowButtons(); if(animateDelay != FALSE) {tcltk::tcl("update"); Sys.sleep(1); };
            tcltk::tcl("update");
          }
          close(tempPB)
        } else {
          theJuicrFrames <- c(theJuicrFrames, createNewJuicrFrame(aFileList, sourceHTML)[1])
          #update_ArrowButtons(); if(animateDelay != FALSE) {tcltk::tcl("update"); Sys.sleep(1); };
        }
        set_main("currentJuicrFrame", 1)
        return(theJuicrFrames)
      }


      # juicr frame management

      set_main("currentJuicrFrame", 0)
      getCurrentJuicrFrame <- function() return(get_main("currentJuicrFrame"))

      previousJuicrFrame <- function() {
        if(getCurrentJuicrFrame() <= 1) return()
        tcltk::tkpack.forget(get_main("allJuicrFrames")[getCurrentJuicrFrame()])
        set_main("currentJuicrFrame", getCurrentJuicrFrame() - 1)
        tcltk::tkpack(get_main("allJuicrFrames")[getCurrentJuicrFrame()])
        return()
      }

      nextJuicrFrame <- function() {
        if(getCurrentJuicrFrame() == length(get_main("allJuicrFrames"))) return()
        tcltk::tkpack.forget(get_main("allJuicrFrames")[getCurrentJuicrFrame()])
        set_main("currentJuicrFrame", getCurrentJuicrFrame() + 1)
        tcltk::tkpack(get_main("allJuicrFrames")[getCurrentJuicrFrame()])
        return()
      }

      set_main("allJuicrFrames", c())
      get_allJuicrFrames <- function() return(get_main("allJuicrFrames"))
      set_allJuicrFrames <- function(aJuicrFrameList) set_main("allJuicrFrames", aJuicrFrameList)
      add_allJuicrFrames <- function(someJuicrFiles, sourceHTML = FALSE) {
        set_allJuicrFrames(c(get_allJuicrFrames(), createManyJuicrFrames(someJuicrFiles, sourceHTML)))
      }

      next_numberJuicrFrames <- function() return(length(get_main("allJuicrFrames")) - getCurrentJuicrFrame())
      previous_numberJuicrFrames <- function() return(length(get_main("allJuicrFrames")) - next_numberJuicrFrames() - 1)

      get_JuicrFilenames <- function() {
        aFile <- tcltk::tkgetOpenFile(filetypes = "{{juicr files} {_juicr.html}} {{All files} *}",
                               multiple = TRUE,
                               title = "juicr: open 1 or many juicr files")

        return(as.character(aFile))
      }

      get_ImageFilenames <- function() {
        aFile <- tcltk::tkgetOpenFile(filetypes = "{{image files} {.jpg .png .tiff}} {{All files} *}",
                               multiple = TRUE,
                               title = "juicr: open 1 or many image files with a plot to extract")
        return(as.character(aFile))
      }

      get_SourceFilenames <- function() {
        aFile <- tcltk::tkgetOpenFile(filetypes = paste0("{{source file} {", theFigureFile[getCurrentJuicrFrame()], "}}"),
                               multiple = TRUE,
                               title = "juicr: source of the current image")
        return(aFile)
      }

      # update_theFigureFile [was global] function(newFiles) theFigureFile <- c(theFigureFile, newFiles)
      update_theFigureFile <- function(newFiles) theFigureFile <- c(theFigureFile, newFiles)

      set_main("allJuicrImages", c())
      get_allJuicrImages <- function() return(get_main("allJuicrImages"))
      set_allJuicrImages <- function(aJuicrImagesList) set_main("allJuicrImages", aJuicrImagesList)
      add_allJuicrImages <- function(someJuicrImage) {
        set_allJuicrImages(c(get_allJuicrImages(), someJuicrImage))
      }


      # START: image manipulation bar
      fhead <- tcltk::tkframe(mainExtractorWindow, relief = "flat", bd = "1", background = "lightgrey", width = 1000)

        # get images or juicr html images
        button_OpenNewImage <- tcltk::tkbutton(fhead, compound = "top",
                                         text = "add new\n image(s)",
                                         width = 80, height = 80, image = theOrange,
                                         relief = "flat",
                                         command = function(){
                                            newFrames <- get_ImageFilenames();
                                            if(!identical(newFrames, character(0))) {
                                              tcltk::tkconfigure(button_OpenNewImage, text = paste0("adding ", length(newFrames), "\nimages..."));
                                              tcltk::tcl("update");
                                              add_allJuicrFrames(newFrames);
                                              update_theFigureFile(newFrames);
                                              update_ArrowButtons();
                                              tcltk::tkconfigure(button_OpenNewImage, text = "add new\n image(s)");
                                              tcltk::tkconfigure(button_previewImage, state = "active")
                                              tcltk::tkconfigure(button_SaveAllImages, state = "active")
                                              tcltk::tkconfigure(button_JuiceAllImages, state = "active")
                                              tcltk::tkconfigure(button_leftArrow, state = "active");
                                              tcltk::tkconfigure(button_rightArrow, state = "active");
                                            }
                                          })

        button_OpenJuicedImage <- tcltk::tkbutton(fhead, compound = "top",
                                     text = "add juiced\n image(s)",
                                     width = 80, height = 80, image = orangeJuice,
                                     relief = "flat",
                                     command = function(){
                                       newFrames <- get_JuicrFilenames();
                                       if(!identical(newFrames, character(0))) {
                                         tcltk::tkconfigure(button_OpenJuicedImage, text = paste0("adding ", length(newFrames), "\nimages..."));
                                         tcltk::tcl("update");
                                         add_allJuicrFrames(newFrames, TRUE);
                                         update_theFigureFile(newFrames);
                                         update_ArrowButtons();
                                         tcltk::tkconfigure(button_OpenJuicedImage, text = "add juiced\n image(s)");
                                         tcltk::tkconfigure(button_previewImage, state = "active")
                                         tcltk::tkconfigure(button_SaveAllImages, state = "active")
                                         tcltk::tkconfigure(button_JuiceAllImages, state = "active")
                                         tcltk::tkconfigure(button_leftArrow, state = "active");
                                         tcltk::tkconfigure(button_rightArrow, state = "active");
                                       }
                                      })

        # start of multi-image toggle buttons
        getText_leftArrow <- function() {
          if(length(get_main("allJuicrFrames")) == 0) return("no other\nimages")
          return(paste0("previous\n", previous_numberJuicrFrames(), " images"))
        }

        getText_rightArrow <- function() {
          if(length(get_main("allJuicrFrames")) == 0) return("no other\nimages")
          return(paste0("next\n", next_numberJuicrFrames(), " images"))
        }

        update_ArrowButtons <- function() {
          tcltk::tkconfigure(button_leftArrow, text = getText_leftArrow())
          tcltk::tkconfigure(button_rightArrow, text = getText_rightArrow())
          tcltk::tkconfigure(button_leftArrow, state = "active")
          tcltk::tkconfigure(button_rightArrow, state = "active")
        }

        button_leftArrow <- tcltk::tkbutton(fhead, compound = "top", state = "disabled",
                                     text = getText_leftArrow(),
                                     width = 80, height = 80, image = leftArrowImage,
                                     relief = "flat",
                                     command = function(){ previousJuicrFrame(); update_ArrowButtons(); update_FigureSmall();})
          button_previewImage <- tcltk::tkbutton(fhead, compound = "center", font = "Helvetica 8 bold", state = "disabled",
                                          foreground = "tomato3",
                                          text = "\n\n\n\n\n              source",
                                          width = 80, height = 80, image = getFigureSmall(),
                                          relief = "flat",
                                          command = function(){get_SourceFilenames();})
          imageInformation <- tcltk::tktext(fhead, foreground = "lightgrey",
                                     height = 6, width = 80, background = "lightgrey",
                                     relief = "flat", font = "Helvetica 7")
        button_rightArrow <- tcltk::tkbutton(fhead, compound = "top", state = "disabled",
                                      text = getText_rightArrow(),
                                      width = 80, height = 80, image = rightArrowImage,
                                      relief = "flat",
                                      command = function(){ nextJuicrFrame(); update_ArrowButtons(); update_FigureSmall();})

        # save multi-image button
        button_SaveAllImages <- tcltk::tkbutton(fhead, compound = "top", state = "disabled",
                                     text = "save all\n juiced image(s)",
                                     width = 80, height = 80, image = juiceContainerSmall,
                                     relief = "flat",
                                     command = function() {
                                      #theSaveDirectory <- tcltk::tkchooseDirectory()
                                      someJuicrFrames <- get_allJuicrFrames()
                                      for(i in 1:length(someJuicrFrames)) {
                                        tcltk::tkconfigure(button_SaveAllImages, text = paste0("saving ", i, " of ", length(someJuicrFrames), "\n.html files"))
                                        tcltk::tcl("update")
                                        tcltk::tkinvoke(paste0(someJuicrFrames[i], ".4.4.2"));
                                        nextJuicrFrame(); update_ArrowButtons(); update_FigureSmall();
                                      }
                                      tcltk::tkconfigure(button_SaveAllImages, text = "save all\n juiced image(s)")
                                     })

        # save multi-image button
        button_JuiceAllImages <- tcltk::tkbutton(fhead, compound = "center", state = "disabled",
                                            text = "juice all\nimages",
                                            width = 80, height = 80, image = juicrLogoSmall,
                                            relief = "flat",
                                            command = function() {
                                              someJuicrFrames <- get_allJuicrFrames()
                                              for(i in 1:length(someJuicrFrames)) {
                                                tcltk::tkconfigure(button_JuiceAllImages, text = paste0("juicing ", i, " of ", length(someJuicrFrames), "\nimages"))
                                                tcltk::tcl("update")
                                                tcltk::tkinvoke(paste0(someJuicrFrames[i], ".2.1.1.1"));
                                                nextJuicrFrame(); update_ArrowButtons(); update_FigureSmall();
                                              }
                                              tcltk::tkconfigure(button_JuiceAllImages, text = "juice all\nimages")
                                            })


          tcltk::tkgrid(button_OpenNewImage  , row = 0, column = 0, sticky = "w", padx = 10, pady = 10)
          tcltk::tkgrid(button_OpenJuicedImage, row = 0, column = 1, sticky = "w", padx = 10, pady = 10)

          tcltk::tkgrid(button_leftArrow, row = 0, column = 2,  sticky = "e", padx = 10, pady = 10)
          tcltk::tkgrid(button_previewImage, row = 0, column = 3,  padx = 10, pady = 10)
          tcltk::tkgrid(button_rightArrow, row = 0, column = 4, sticky = "w", padx = 10, pady = 10)
          tcltk::tkgrid(imageInformation, row = 0, column = 5, sticky = "w", padx = 10, pady = 10)

          tcltk::tkgrid(button_SaveAllImages, row = 0, column = 7, sticky = "e", padx = 10, pady = 10)
          tcltk::tkgrid(button_JuiceAllImages, row = 0, column = 6, sticky = "e", padx = 10, pady = 10)

          tcltk::tkgrid.columnconfigure(fhead, 2, weight = 3)
          tcltk::tkgrid.columnconfigure(fhead, 6, weight = 2)

        tcltk::tkpack(fhead, side = "bottom", fill = "x")

        tcltk::tkbind(button_OpenNewImage, "<Any-Enter>", function() {tcltk::tkconfigure(button_OpenNewImage, background = "floral white");})
        tcltk::tkbind(button_OpenNewImage, "<Any-Leave>", function() {tcltk::tkconfigure(button_OpenNewImage, background = "grey95");})
        tcltk::tkbind(button_OpenJuicedImage, "<Any-Enter>", function() {tcltk::tkconfigure(button_OpenJuicedImage, background = "floral white");})
        tcltk::tkbind(button_OpenJuicedImage, "<Any-Leave>", function() {tcltk::tkconfigure(button_OpenJuicedImage, background = "grey95");})
        tcltk::tkbind(button_leftArrow, "<Any-Enter>", function() {tcltk::tkconfigure(button_leftArrow, background = "floral white");})
        tcltk::tkbind(button_leftArrow, "<Any-Leave>", function() {tcltk::tkconfigure(button_leftArrow, background = "grey95");})

        tcltk::tkbind(button_previewImage, "<Any-Enter>", function() {
          if(theFigureFile != "") {

              theSavedFilename <- paste0(tools::file_path_sans_ext(basename(theFigureFile[getCurrentJuicrFrame()])), "_juicr.html")
              theLastSavedTime <- "never"

              if(file.exists(theSavedFilename) == TRUE) {
                theLastSavedTime <- paste(file.info(theSavedFilename)$ctime)
              } else {
                theSavedFilename <- "NA"
              }

              if(file.exists(theFigureFile[getCurrentJuicrFrame()]) == TRUE) {
                theImageSummary <- paste("current image: ", theFigureFile[getCurrentJuicrFrame()],
                                         "\nsize: ", file.size(theFigureFile[getCurrentJuicrFrame()]),
                                         "\ndimentions: ", paste(paste(dim(EBImage::readImage(theFigureFile[getCurrentJuicrFrame()]))[1:2], collapse = " by "), "pixels"),
                                         "\n\nlast saved: ", theLastSavedTime,
                                         "\nsaved filename: ", theSavedFilename,
                                         "\n\n");
              } else {
                theImageSummary <- paste("current image: ", theFigureFile[getCurrentJuicrFrame()],
                                         "\n\nlast saved: ", theLastSavedTime,
                                         "\nsaved filename: ", theSavedFilename,
                                         "\n\n");
              }

              tcltk::tkinsert(imageInformation, "1.0", theImageSummary);
              tcltk::tkconfigure(imageInformation, foreground = "black");
              tcltk::tkconfigure(button_previewImage, text = "\n\n\n\n\n       get source");
            }
          })

        tcltk::tkbind(button_previewImage, "<Any-Leave>", function() {tcltk::tkconfigure(imageInformation, foreground = "lightgrey");
                                                                      tcltk::tkconfigure(button_previewImage, text = "\n\n\n\n\n              source");
        })


      tcltk::tkbind(button_rightArrow, "<Any-Enter>", function() {tcltk::tkconfigure(button_rightArrow, background = "floral white");})
      tcltk::tkbind(button_rightArrow, "<Any-Leave>", function() {tcltk::tkconfigure(button_rightArrow, background = "grey95");})
      tcltk::tkbind(button_SaveAllImages, "<Any-Enter>", function() {tcltk::tkconfigure(button_SaveAllImages, background = "floral white");})
      tcltk::tkbind(button_SaveAllImages, "<Any-Leave>", function() {tcltk::tkconfigure(button_SaveAllImages, background = "grey95");})
      tcltk::tkbind(button_JuiceAllImages, "<Any-Enter>", function() {tcltk::tkconfigure(button_JuiceAllImages, foreground = "orange");})
      tcltk::tkbind(button_JuiceAllImages, "<Any-Leave>", function() {tcltk::tkconfigure(button_JuiceAllImages, foreground = "black");})


    # CATCHING FILES INPUTED VIA GUI_juicr FUNCTION CALL

    if(theFigureFile != "")  {
      if(length(theFigureFile) == 1) {
          add_allJuicrFrames(theFigureFile);
        }
        else {
          add_allJuicrFrames(theFigureFile);
          update_ArrowButtons();
          tcltk::tkconfigure(button_leftArrow, state = "active")
          tcltk::tkconfigure(button_rightArrow, state = "active")
        }
      tcltk::tkconfigure(button_previewImage, state = "active")
      tcltk::tkconfigure(button_SaveAllImages, state = "active")
      tcltk::tkconfigure(button_JuiceAllImages, state = "active")
    }

    if(theJuicrFile != "")  {
      if(length(theJuicrFile) == 1) {
        add_allJuicrFrames(theJuicrFile, TRUE);
      }
      else {
        add_allJuicrFrames(theJuicrFile, TRUE);
        update_ArrowButtons();
        tcltk::tkconfigure(button_leftArrow, state = "active")
        tcltk::tkconfigure(button_rightArrow, state = "active")
      }
      tcltk::tkconfigure(button_previewImage, state = "active")
      tcltk::tkconfigure(button_SaveAllImages, state = "active")
      tcltk::tkconfigure(button_JuiceAllImages, state = "active")
    }


    tcltk::tkfocus(mainExtractorWindow)

      # # # # # # # # # # # # # # # # #
      ##### END OF JUICR
      # # # # # # # # # # # # # # # # #


    # TCLTK GARBAGE COLLECTION
    # deletes all images (need better solution for avoiding memory leaks)
    imageCleanUp <- function() {
      oldImages <- as.character(tkimage.names())
      oldImages <- oldImages[grep("image", oldImages)]
      for(someImage in oldImages) tcltk::tkimage.delete(someImage)
    }
    tcltk::tkbind(mainExtractorWindow, "<Destroy>", imageCleanUp)

    # only have one juicr window open at a time
    tcltk::tkwait.window(mainExtractorWindow)
    tcltk::tkdestroy(mainExtractorWindow)

  } else {

    .juicrPROBLEM("error",
                  paste("\n tcltk package is missing and is needed to generate the GUI.",
                        "  --> If using Windows/Linux, try 'install.packages('tcltk')'",
                        "  --> If using a Mac, install latest XQuartz application (X11) from:",
                        "        https://www.xquartz.org/",
                        sep = "\n"))
  }

  message(paste0("juicr exit note: if files were saved, they are found here:\n  ", getwd(), "/n"))
  return("")
}
