#' Check if the classification is a valid input.
#'
#' @param class A classification of Harvard Art Museum website.
#' @return If this is a valid classification input for this package, it will return TRUE, otherwise, return FALSE
#' @examples
#' is_classification(class = 'Prints')
#' is_classification(class = 'Music')
#' is_classification()
#' @export


is_classification <- function(class="Prints"){
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
