#'Logistic Model Development
#'
#'Give you ui to create logistic model.
#' @author JIshnu
#' @param See the example
#' @param Note- please download woe package from GitHub using "install_github('riv','tomasgreif')". After installing the correct package if the UI closes automatically please restart the rstudio.
#' @return  you can download the model and all the performance
#' @example example.R
#' @export


LogisticModelDevelopment<-function (var)
{
  data("empty_dat")
  library(shiny)
  library(shinythemes)
  library(DT)
  library(devtools)
  library(woe)
  library(Hmisc)
  library(data.table)
  library(car)
  library(sqldf)
  library(ROCR)
  library(ineq)
  library(Hmisc)
  library(pryr)
  library(scales)
  library(shinythemes)
  library(plotly)
  library(berryFunctions)
  library(rhandsontable)
  m = var
  appDir <- system.file("CODE", package = "LogisticModelDevelopment")
  print(appDir)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `LogisticModelDevelopment`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
  # runApp(appDir = './code')
}



#'decile
#'
#'will decile the data.
#' @export
decile <- function(x) {
  deciles <- vector(length = 10)
  for (i in seq(0.1, 1, .1)) {
    deciles[i * 10] <- quantile(x, i, na.rm = T)
  }
  return (ifelse(x < deciles[1], 1,
                 ifelse(
                   x < deciles[2], 2,
                   ifelse(x < deciles[3], 3,
                          ifelse(
                            x < deciles[4], 4,
                            ifelse(x < deciles[5], 5,
                                   ifelse(
                                     x < deciles[6], 6,
                                     ifelse(x < deciles[7], 7,
                                            ifelse(x < deciles[8], 8,
                                                   ifelse(x <
                                                            deciles[9], 9, 10)))
                                   )))))))}
