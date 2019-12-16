#' Get the gallery information on a specific floor
#'
#' @param floor The level of Harvard Art Museum. Valid floor from 1 to 3. Floor 4 is for office use.
#'     Default is set to be 2.
#' @param size Size specifies how many gallries the user would like to know about. Please note that there are only
#'     20 galleries in floor 1, 24 galleries in floor 2 and 12 galleries in floor 3. Input an input size that is
#'     larger than the current floor will lead to an error warning. Default is set to be 10.
#' @param token A variable from token <- get_token() function. Please run token <- get_token() before using this function
#' @return a list of gallery information under a specific floor. The information includes gallery name, theme,
#'     gallery id, floor level, number of objects (objectcount), text that introduce the gallery (labeltext) and
#'     the corresponding url on Harvard Art Museum website.
#' @examples
#' get_gallery_info(floor=2, size=14)
#' get_gallery_info(floor=1, size=12)
#' @import svDialogs
#' @import httr
#' @import RCurl
#' @import rvest
#' @export
get_gallery_info <- function(floor=2, size=10, token=token){
  #library(httr)
  #library(RCurl)
  #library(svDialogs)
  if (missing(token)==TRUE){
    message("please get your token thorough `token <- get_token()` and input token=token in this function : )")
  }
  else {
    url1 <- 'http://api.harvardartmuseums.org/gallery?'
    url2 <- 'floor='
    url3 <- '&apikey='
    url4 <- '&size='
    endpoint <- paste(url1, url2, floor, url3, token, url4, size, sep ="")
    name <- list()
    theme <- list()
    galleryid <- list()
    gfloor <- list()
    objectcount <- list()
    labeltext <- list()
    url <- list()

    if (floor > 3){
      stop(
        sprintf(
          "Only Floor 1 - Floor 3 have galleries"
        ),
        call. = FALSE
      )
    }
    if (floor == 1 & size > 20){
      stop(
        sprintf(
          "First floor only have 20 galleries"
        ),
        call. = FALSE
      )
    }
    if (floor == 2 & size > 24){
      stop(
        sprintf(
          "Second floor only have 24 galleries"
        ),
        call. = FALSE
      )
    }
    if (floor == 3 & size > 12){
      stop(
        sprintf(
          "Third flood only have 12 galleries"
        ),
        call. = FALSE
      )
    }
    if (length(endpoint) ==1){
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
        for (i in 1: size){
          if (is.null(web$records[[i]]$url) == TRUE){
            web$records[[i]]$url = "no record"
          }
          if (is.null(web$records[[i]]$name) == TRUE){
            web$records[[i]]$name = "no record"
          }
          if (is.null(web$records[[i]]$galleryid) == TRUE){
            web$records[[i]]$galleryid = "no record"
          }
          if (is.null(web$records[[i]]$objectcount) == TRUE){
            web$records[[i]]$objectcount = "no record"
          }
          if (is.null(web$records[[i]]$theme) == TRUE){
            web$records[[i]]$theme = "no record"
          }
          if (is.null(web$records[[i]]$floor) == TRUE){
            web$records[[i]]$floor = "no record"
          }
          if (is.null(web$records[[i]]$labeltext) == TRUE){
            web$records[[i]]$labeltext = "no record"
          }
          galleryid[i] <- web$records[[i]]$galleryid
          url[i] <- web$records[[i]]$url
          theme[i] <- web$records[[i]]$theme
          name[i] <- web$records[[i]]$name
          objectcount[i] <- web$records[[i]]$objectcount
          labeltext[i] <- web$records[[i]]$labeltext
          gfloor[i] <- web$records[[i]]$floor
        }
        result1 <- list(name, theme, galleryid, gfloor, objectcount, labeltext, url)
        result2 <- lapply(result1, function(x) ifelse(x == "NULL", "no record", x))
        dat <- do.call(cbind, lapply(result2, cbind))
        result <- as.data.frame(dat)

        colnames(result) <- c("Name", "Theme", "Galleryid", "Floor", "Objectcount","Labeltext","Url")
        rownames(result) <- unlist(galleryid)
        return(result)
        #assign("gallery_result_form", gresult, envir = globalenv())
      }

    }
  }
}
