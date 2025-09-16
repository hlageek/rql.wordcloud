#' wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h1("Requal module"),
        actionButton(ns("button"), "Test"),
        textOutput(ns("button_output"))
    )
}

#' wordcloud Server Functions
#'
#' @export
mod_server <- function(id, glob) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Define the output for button_output
        output$button_output <- renderText({
            paste("Button clicked", input$button, "times")
        })
    })
}
