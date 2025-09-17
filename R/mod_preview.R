#' Launch the Wordcloud Shiny App
#'
#' @description This function launches the Shiny app for the wordcloud module.
#'
#' @export
#'
#' @import shiny
mod_preview <- function() {
  # Define the UI
  ui <- fluidPage(
    mod_ui("wordcloud_module")
  )

  # Define the server logic
  server <- function(input, output, session) {
    mod_server("wordcloud_module", api = NULL)
  }

  # Run the application
  shinyApp(ui, server)
}
