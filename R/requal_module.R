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
        h1("Requal module - w"),
        actionButton(ns("button"), "Test"),
        textOutput(ns("button_output")) #,
        #wordcloud2::wordcloud2Output(ns("wordcloud"))
    )
}

#' wordcloud Server Functions
#'
#' @param id Internal parameters for {shiny}.
#' @param con Connection object for Requal database.
#' @param user_id ID of the Requal user.
#' @export
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        loc <- reactiveValues()

        api <- requal::RequalAPI$new(
            NULL,
            user_id = 1,
            project_id = 1,
            mode = "mock"
        )
        api$get_segments()
        print(api$get_segments(
            user_id = api$user_id,
            project_id = api$project_id
        ))
        # if (!is.null(con)) {
        #     loc$wordcloud_text_input <- dplyr::tbl(con, "segments") |>
        #         dplyr::filter(
        #             user_id == !!user_id,
        #             project_id == !!project_id
        #         ) |>
        #         dplyr::select(segment_text) |>
        #         dplyr::collect()
        # } else {
        #     loc$wordcloud_text_input <- data.frame(
        #         segment_text = c(
        #             "Strategic Plan​\n",
        #             "period follows up on the efforts of Charles U…",
        #             "best",
        #             "h ho zrušili, aby firma mohla stát tak potřeb…",
        #             "\n",
        #             "maso",
        #             "peers.​\n\n​\n\nCharles University Strategic …",
        #             ".\nBratr: Já bych na jídle nešetřil no.\nB",
        #             "against",
        #             "Report",
        #             "hlouposti",
        #             "řeba důchodci. Že něco viděj a maj peníze, ta…",
        #             "qua",
        #             "Výzkumnice: (čte otázku) Lidé, kteří šetří pe…",
        #             "pro-1-kolo",
        #             "PřítelV: Tak jako jestli si koupím míň kvalit…",
        #             "PřítelV: No jasný, tak si koupím třeba nový b…",
        #             " jídle, či jiných potřebách, aby",
        #             " jídle, či jiných potřebách, aby",
        #             "Image Administrátor\n\n    Project\n",
        #             "vydržel s tím",
        #             "Codebook"
        #         ),
        #         stringsAsFactors = FALSE
        #     )
        # }

        # observeEvent(input$button, {
        #     loc$wordcloud_df <- tidytext::unnest_tokens(
        #         loc$wordcloud_text_input,
        #         word,
        #         "segment_text"
        #     ) |>
        #         dplyr::count(word, name = "freq")
        # })
        # Define the output for button_output
        output$button_output <- renderText({
            paste("Button clicked", input$button, "times")
        })
        # output$wordcloud <- wordcloud2::renderWordcloud2({
        #     req(loc$wordcloud_df)
        #     wordcloud2::wordcloud2(loc$wordcloud_df)
        # })
    })
}
