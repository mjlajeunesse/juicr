## juicr package for R  <img src="inst/images/test_orange3.png" align="right" height = 150/>

### purpose of the juicr package
**juicr** is a GUI interface to automated, semi-automated, and manual tools for extracting data from scientific images -- like scatter or bar plots that contain data, or other images with information that can be converted numerically or coordinates classified. More information about **juicr** can be found at [http://lajeunesse.myweb.usf.edu](http://lajeunesse.myweb.usf.edu).

## layout & vignette (click on image)
<a href="http://lajeunesse.myweb.usf.edu/juicr/juicr_basic_vignette_v0.1.html"><img src="http://lajeunesse.myweb.usf.edu/juicr/main_juicr_window.jpg" height="400"/></a>

### video tutorial
A brief use and installation tutorial for **juicr** can be watched on *Youtube* here:
<a href="https://youtu.be/tiL-gZgN9Qk"><img src="http://lajeunesse.myweb.usf.edu/juicr/youtube_Marc_Lajeunesse_juicr_R_package.png" alt="Youtube LajeunesseLab"></a>

### installation instructions and dependencies
**juicr** has an external dependency that needs to be installed and loaded prior to use in R. This is the **EBImage** R package (Pau et al. 2010) available only from the Bioconductor repository: https://www.bioconductor.org. 
To properly install **juicr**, start with the following R script that loads the Bioconductor resources needed to install the **EBImage** (also accept all dependencies):

``` r
install.packages("BiocManager"); 
BiocManager::install("EBImage")
library(metagear)
``` 

This dependency sometimes results in CRAN failing to generate a binary of **juicr** for your OS -- which sadly happens often. In this case install from the source, for example:

``` r
install.packages("juicr_0.1.tar.gz", repos = "http://cran.us.r-project.org", type = "source", dependencies = TRUE)
library(juicr)
``` 

### How to cite?
Lajeunesse, M.J. (2021) Automated, semi-automated, and manual extraction of numerical data from scientific images, plot, charts, and figures. R package version 0.1. https://CRAN.R-project.org/package=juicr

### Found a bug?
Please email me at lajeunesse@usf.edu with the subject header: "juicr bug" and in the body please include juicr's version, your OS, and a short description of the issue.  I will try to include fixes in following releases.