#' Get a token and save it as a global variable for future use.
#'
#' @return It will return the token that user input
#' @importFrom svDialogs dlgInput
#' @import utils
#' @export
get_token <- function(){
  #library(svDialogs)
  token1 <- dlgInput("What's your Harvard Art Museum token? (without ' ' at the beginning and the end)",
                     Sys.info())$res
  return(token1)
  #utils::globalVariables(c("token"))
  #assign("token", token, envir = globalenv())
}
#token <- get_token()
#assign("token", token, envir = globalenv())
