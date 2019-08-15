#' Explore data in the shiny application
#' 
#' Open an interactive app in a browser. 
#' 
#' @param browse Logical. Use browser for running Shiny app.
#' @export
#' @importFrom tools file_ext
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @examples 
#' \dontrun{
#' explore_swuds()
#' }
explore_swuds <- function(browse=TRUE){
  shiny::runApp(system.file("shiny", package = "wateRuseSWUDS"),
                launch.browser = browse)
}