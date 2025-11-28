#' Shiny MLE
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{shinymle()}
shinymle = function(){
  shiny::runApp(system.file("Shiny", package = "MATH2025"), launch.browser = TRUE)



}
