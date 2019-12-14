#' Get a token and save it as a global variable for future use.
#'
#' @return It will return the token that user input
#' @export
#' @importFrom svDialogs dlgInput
#' @import utils
get_token <- function(){
  #library(svDialogs)
  token1 <- dlgInput("What's your Harvard Art Museum token? (without ' ' at the beginning and the end)",
                     Sys.info())$res
  return(token1)
  #utils::globalVariables(c("token"))
  #assign("token", token, envir = globalenv())
}
token <- get_token()
assign("token", token, envir = globalenv())

#' Check if the classification is a valid input.
#'
#' @param class A classification of Harvard Art Museum website.
#' @return If this is a valid classification input for this package, it will return TRUE, otherwise, return FALSE
#' @examples
#' is_classification(class = 'Prints')
#' is_classification(class = 'Music')
#' @export
is_classification <- function(class){
  classlist <- c('Prints', 'Albums', 'Amulets', 'Armor',
                 'Boxes', 'Calligraphy', 'Cameos', 'Fragments', 'Furnishings', 'Gems',
                 'Inscriptions', 'Jewelry', 'Mirrors', 'Mosaics', 'Paintings',
                 'Photographs', 'Plaques', 'Rubbings', 'Sculpture', 'Seals',
                 'Tablets', 'Tokens', 'Vessels')
  if (class %in% classlist){
    TRUE
  }
  else {
    FALSE
  }
}

#' Get the artworks information within a classification
#'
#' @param classification An art classification according to Harvard Art Museum website. The full list of available
#'    classification is: 'Prints', 'Albums', 'Amulets', 'Armor', 'Boxes', 'Calligraphy', 'Cameos', 'Fragments',
#'    'Furnishings', 'Gems', 'Inscriptions', 'Jewelry', 'Mirrors', 'Mosaics', 'Paintings', 'Photographs', 'Plaques',
#'    'Rubbings', 'Sculpture', 'Seals', 'Tablets', 'Tokens', 'Vessels'. Default is set to be 'Prints'.
#' @param size Size specifies how many pieces of artwork the user would like to get. Default is set to be 10.
#' @return a list of artwork information under a specific classification. The information includes title, century, period,
#' culture, division, dclassification, department, objectid, corresponding url and imageurl.
#' @examples
#' get_artwork_info(classification='Prints', size=10)
#' @export
#' @importFrom svDialogs dlgInput
#' @import rvest
#' @import httr
get_artwork_info <- function(classification='Prints', size=10){
  #library(httr)
  #library(svDialogs)
  #token <- get_token()
  url <- list()
  title <- list()
  dclassification <- list()
  division <- list()
  century <- list()
  objectid <- list()
  culture <- list()
  period <- list()
  department <- list()
  imageurl <- list()

  url1 <- 'http://api.harvardartmuseums.org/object?'
  url2 <- 'classification='
  url3 <- '&apikey='
  url4 <- '&size='
  endpoint <- paste(url1, url2, classification, url3, token, url4, size, sep ="")
  if (is_classification(classification)==FALSE){
    stop(
      sprintf(
        "Invalid classification input"
      ),
      call. = FALSE
    )
  }
  else if (size > 100){
    stop(
      sprintf(
        "API limit exceeded"
      ),
      call. = FALSE
    )
  }
  else if (length(endpoint) == 1){
    webinfo <- GET(endpoint)
    web <- content(GET(endpoint))
    status <- as.numeric(status_code(webinfo))
    if (status != 200){
      stop(
        sprintf(
          "Harvard Art Museum request failed",
          status
        ),
        call. = FALSE
      )
    }
    else {
      for (i in 1:size){
        if (is.null(web$records[[i]]$url) == TRUE){
          web$records[[i]]$url = "no record"
        }
        if (is.null(web$records[[i]]$title) == TRUE){
          web$records[[i]]$title = "no record"
        }
        if (is.null(web$records[[i]]$century) == TRUE){
          web$records[[i]]$century = "no record"
        }
        if (is.null(web$records[[i]]$objectid) == TRUE){
          web$records[[i]]$objectid = "no record"
        }
        if (is.null(web$records[[i]]$division) == TRUE){
          web$records[[i]]$division = "no record"
        }
        if (is.null(web$records[[i]]$culture) == TRUE){
          web$records[[i]]$culture = "no record"
        }
        if (is.null(web$records[[i]]$period) == TRUE){
          web$records[[i]]$period = "no record"
        }
        if (is.null(web$records[[i]]$department) == TRUE){
          web$records[[i]]$department = "no record"
        }
        if (is.null(web$records[[i]]$images[[1]]$baseimageurl) == TRUE){
          web$records[[i]]$imageurl = "no record"
        }
        if (is.null(web$records[[i]]$classification) == TRUE){
          web$records[[i]]$classification = "no record"
        }
        objectid[i] <- web$records[[i]]$objectid
        url[i] <- web$records[[i]]$url
        title[i] <- web$records[[i]]$title
        century[i] <- web$records[[i]]$century
        dclassification[i] <- web$records[[i]]$classification
        division[i] <- web$records[[i]]$division
        culture[i] <- web$records[[i]]$culture
        period[i] <- web$records[[i]]$period
        department[i] <- web$records[[i]]$department
        imageurl[i] <- web$records[[i]]$images[[1]]$baseimageurl
      }
      result1 <- list(title, century, period, culture, division, dclassification, department,
                      objectid, url, imageurl)
      result2 <- lapply(result1, function(x) ifelse(x == "NULL", "no record", x))
      dat <- do.call(cbind, lapply(result2, cbind))
      result <- as.data.frame(dat)

      colnames(result) <- c("Title", "Century", "Period", "Culture","Division","Classification",
                            "Department", "Objectid","Url", "Imageurl")
      rownames(result) <- unlist(objectid)
      return(result)
    }
  }
}
