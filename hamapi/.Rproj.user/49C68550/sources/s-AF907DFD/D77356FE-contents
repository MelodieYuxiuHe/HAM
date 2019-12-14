#' Which gallery has the most artwork object on a specified floor?
#'
#' @param floor The level of Harvard Art Museum. Valid floor from 1 to 3. Floor 4 is for office use.
#'    Default to be 2.
#' @param size Size specifies how many gallries the user would like to know about. Please note that there are only
#'     20 galleries in floor 1, 24 galleries in floor 2 and 12 galleries in floor 3. Input an input size that is
#'     larger than the current floor will lead to an error warning. Default is set to be 3.
#' @return a gallery name which has the most objects under a specific floor.
#' @examples
#' most_object(floor = 3, size = 2)
#' most_object(floor = 2, size = 20)
#' @export
#' @importFrom svDialogs dlgInput
#' @import httr

# setwd("/Users/Melodie/hamapi")
# source("/Users/Melodie/hamapi/R/get_artwork.R")
# source("/Users/Melodie/hamapi/R/isclassification.R")
# source("/Users/Melodie/hamapi/R/get_token.R")
most_object <- function(floor=2, size=5){
  #library(httr)
  #library(svDialogs)
  #token <- get_token()
  galleryr <- get_gallery_info(floor=floor, size=size)
  obcount <- as.vector(unlist(galleryr$Objectcount))
  #maxob <- max(obcount)
  for (i in 1:size){
    #if (as.numeric(galleryr$Objectcount[i] == maxob)){
    maxob <- max(as.numeric(galleryr$Objectcount[i]))
    result <- galleryr$Name[i]
    #}
  }
  return(result)
}

