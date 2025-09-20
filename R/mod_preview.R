#' Launch the Wordcloud Shiny App
#'
#' @description This function launches the Shiny app for the wordcloud module.
#' It provides a user interface for generating word clouds from text data.
#'
#' @export
#'
#' @import shiny
mod_preview <- function() {
  # Define the UI
  ui <- mod_ui("wordcloud_module")

  # Define the server logic
  server <- function(input, output, session) {
    mod_server("wordcloud_module", api = NULL)
  }

  # Run the application
  shinyApp(ui, server)
}
