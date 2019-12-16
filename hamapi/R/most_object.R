#' Which gallery has the most artwork object on a specified floor?
#'
#' @param floor The level of Harvard Art Museum. Valid floor from 1 to 3. Floor 4 is for office use. Default to be 2.
#' @return a gallery name which has the most objects under a specific floor.
#' @examples
#' most_object(floor = 3)
#' most_object(floor = 2)
#' @importFrom svDialogs dlg_message
#' @importFrom svDialogs dlgInput
#' @import httr
#' @export
most_object <- function(floor=2){
  #library(httr)
  #library(svDialogs)
  #token <- get_token()
  if (floor == 1){
    size1 = 20
  }
  if (floor == 2){
    size1 = 24
  }
  if (floor == 3){
    size1 = 12
  }
  galleryr <- get_gallery_info(floor=floor, size1)
  obcount <- as.vector(unlist(galleryr$Objectcount))
  if (is.null(obcount)==TRUE){
    message("get your token and check floor input")
  }
  else {
  maxob <- max(obcount)
  for (i in 1:size1){
    if ((as.numeric(galleryr$Objectcount[i]) == maxob)){
      result <- galleryr$Name[i]
    }
  }
  print(result)
 }
}
