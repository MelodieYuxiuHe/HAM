#' Get the artworks pictures within a classification
#'
#' @param classification An art classification according to Harvard Art Museum website. The full list of available
#'    classification is: 'Prints', 'Albums', 'Amulets', 'Armor', 'Boxes', 'Calligraphy', 'Cameos', 'Fragments',
#'    'Furnishings', 'Gems', 'Inscriptions', 'Jewelry', 'Mirrors', 'Mosaics', 'Paintings', 'Photographs', 'Plaques',
#'    'Rubbings', 'Sculpture', 'Seals', 'Tablets', 'Tokens', 'Vessels'. Default is set to be 'Prints'.
#' @param size Size specifies how many pieces of artwork the user would like to get. Default is set to be 10.
#' @param numberp Number of pictures that user want to download from the website.Default is set to be 7.
#' @return pictures that are under the specified classification (classification1 input) with the number equals to
#'     specificed numberp downloaded on the user's local folder.
#' @examples
#' get_picture(size=10, numberp = 2, classification = 'Vessels')
#' get_picture(size=5, numberp = 2, classification = 'Prints')
#' @importFrom svDialogs dlgInput
#' @import httr
#' @import RCurl
#' @import rvest
#' @import utils
#' @export
get_picture <- function(classification = 'Prints', size=10, numberp=6){
  #library(httr)
  #library(RCurl)
  #library(svDialogs)
  if (numberp > size){
    stop(
      sprintf(
        "You cannot download pictures more than your input size"
      ),
      call. = FALSE
    )
  }
   if (numberp > 5){
    continue <- dlg_message("Your mac/pc will be flooded by pictures. Do you want to continue?", "yesno")$res
    if (continue == 'no'){
      stop(
        sprintf(
          "Try a smaller number : )"
        ),
        call. = FALSE
      )
    }
    if (continue == 'yes'){
      if (exists("token")==FALSE){
        message("please get your token thorough `token <- get_token()` and input token=token in this function : )")
      }
      else{
        resultform <- get_artwork_info(classification=classification, size=size, token=token)
        objectid1 <- resultform[,8]
        imageurls <- as.vector(unlist(resultform[, 10]))
        nameimage <- list()
        print(url)
        for (i in 1:numberp){
          nameimage[i] <- paste(objectid1[[i]],'.jpeg',sep="")
          download.file(url = as.character(imageurls[i]), destfile = as.character(nameimage[i]),
                        quiet = FALSE, mode="w",cacheOK=TRUE)
        }
        print("Hooray! finished")
      }
    }
  }
  if (numberp <= 5){
    if (exists("token")==FALSE){
      message("please get your token thorough `token <- get_token()` and input token=token in this function : )")
    }
    else{
    resultform <- get_artwork_info(classification=classification, size=size, token=token)
    objectid1 <- resultform[,8]
    imageurls <- as.vector(unlist(resultform[, 10]))
    nameimage <- list()
    for (i in 1:numberp){
      nameimage[i] <- paste(objectid1[[i]],'.jpeg',sep="")
      if (length(as.vector(unlist(nameimage[i]))) != 1){
        message("this item may not have an image")
      }
      else{
        download.file(url = as.character(imageurls[i]), destfile = as.character(nameimage[i]),
                      quiet = FALSE, mode="w",cacheOK=TRUE)
      }
    }
    print("Hooray! finished")
  }
 }
}
