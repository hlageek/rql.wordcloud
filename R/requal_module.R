#' Wordcloud UI Function
#'
#' @description A shiny Module for generating a word cloud.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @export
#'
#' @import shiny
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("load_data_button"), "Load Requal Data"),
        selectInput(
            ns("code_select"),
            "Select Code",
            choices = NULL,
            multiple = FALSE
        ),
        h5("Text Processing Options"),
        fluidRow(
            column(
                6,
                checkboxInput(
                    ns("remove_stopwords"),
                    "Remove stop words",
                    value = TRUE
                ),
                conditionalPanel(
                    condition = "input.remove_stopwords",
                    ns = ns,
                    selectInput(
                        ns("language_select"),
                        "Select Language for Stopwords",
                        choices = c("en", "es", "fr", "de"), # Add more languages as needed
                        selected = "en"
                    ),
                    textAreaInput(
                        ns("custom_stopwords"),
                        "Custom Stopwords (one per line or separated by commas)",
                        ""
                    )
                )
            ),
            column(
                6,
                checkboxInput(
                    ns("remove_punctuation"),
                    "Remove punctuation",
                    value = TRUE
                )
            )
        ),
        sliderInput(
            ns("min_frequency"),
            "Minimum Word Frequency",
            min = 1,
            max = 100,
            value = 1
        ),
        div(
            id = ns("word_count_info"),
            style = "margin-bottom: 15px; color: #666; font-size: 0.9em;"
        ),
        conditionalPanel(
            condition = "output.processing",
            ns = ns,
            div(
                style = "text-align: center; padding: 20px;",
                icon(
                    "spinner",
                    class = "fa-spin",
                    style = "font-size: 24px; margin-right: 10px;"
                ),
                "Processing text data..."
            )
        ),
        wordcloud2::wordcloud2Output(ns("wordcloud"))
    )
}

#' Wordcloud Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param api An instance of the RequalAPI class.
#'
#' @export
#'
#' @import shiny
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        loc <- reactiveValues(
            wordcloud_df = NULL,
            codes = NULL,
            processing = FALSE
        )

        if (is.null(api)) {
            api <- requal::RequalAPI$new(
                NULL,
                user_id = 1,
                project_id = 1,
                mode = "mock"
            )
        }

        # Initialize code selector with helpful placeholder
        updateSelectInput(
            session,
            "code_select",
            choices = list("Load data first..." = ""),
            selected = ""
        )

        # Load codes data when the button is clicked
        observeEvent(input$load_data_button, {
            loc$codes <- api$get_segments(
                user_id = api$user_id,
                project_id = api$project_id
            )

            updateSelectInput(
                session,
                "code_select",
                choices = c(
                    "Select code..." = "",
                    stats::setNames(
                        loc$codes$code_id,
                        loc$codes$code_name
                    )
                ),
                selected = ""
            )
        })

        # Show/hide UI elements based on state
        output$processing <- reactive({
            loc$processing
        })
        outputOptions(output, "processing", suspendWhenHidden = FALSE)

        # Process text data reactively when code selection or options change
        observeEvent(
            {
                list(
                    input$code_select,
                    input$remove_stopwords,
                    input$remove_punctuation,
                    input$language_select,
                    input$custom_stopwords
                )
            },
            {
                # Skip if no valid code selected
                if (
                    is.null(input$code_select) ||
                        length(input$code_select) == 0 ||
                        input$code_select == "" ||
                        is.null(loc$codes)
                ) {
                    return()
                }

                loc$processing <- TRUE

                # Handle multiple code selection
                selected_codes <- if (length(input$code_select) > 1) {
                    input$code_select
                } else {
                    as.integer(input$code_select)
                }

                # Fetch segments from the API based on the selected code(s)
                wordcloud_text_input <- loc$codes |>
                    dplyr::filter(code_id %in% selected_codes) |>
                    dplyr::select(segment_text)

                if (nrow(wordcloud_text_input) == 0) {
                    loc$wordcloud_df <- NULL
                    loc$processing <- FALSE
                    return()
                }

                # Process the text data into a word frequency data frame
                processed_df <- wordcloud_text_input |>
                    tidytext::unnest_tokens(
                        word,
                        segment_text,
                        strip_punct = input$remove_punctuation
                    )

                # Remove stop words if requested
                if (input$remove_stopwords) {
                    # Load stopwords for the selected language
                    language_stopwords <- tidytext::stop_words |>
                        dplyr::filter(lexicon == input$language_select)

                    # Parse custom stopwords
                    custom_stopwords <- strsplit(
                        input$custom_stopwords,
                        "\\s*,\\s*|\\s*\\n\\s*"
                    )[[1]]
                    custom_stopwords <- custom_stopwords[custom_stopwords != ""]

                    # Combine default and custom stopwords
                    all_stopwords <- unique(c(
                        language_stopwords$word,
                        custom_stopwords
                    ))

                    processed_df <- processed_df |>
                        dplyr::filter(!word %in% all_stopwords)
                }

                # Count word frequencies
                loc$wordcloud_df <- processed_df |>
                    dplyr::count(word, name = "freq") |>
                    dplyr::arrange(desc(freq))

                # Update slider range based on actual frequency data
                if (nrow(loc$wordcloud_df) > 0) {
                    max_freq <- max(loc$wordcloud_df$freq, na.rm = TRUE)
                    updateSliderInput(
                        session,
                        "min_frequency",
                        min = 1,
                        max = max_freq,
                        value = min(input$min_frequency, max_freq)
                    )
                }

                loc$processing <- FALSE
            }
        )

        # Reactive filtered data for wordcloud
        filtered_wordcloud_data <- reactive({
            req(loc$wordcloud_df)

            filtered_df <- loc$wordcloud_df |>
                dplyr::filter(freq >= input$min_frequency)

            # Update word count info
            output$word_count_info <- renderText({
                total_words <- nrow(loc$wordcloud_df)
                shown_words <- nrow(filtered_df)
                paste("Showing", shown_words, "of", total_words, "unique words")
            })

            filtered_df
        })

        # Render the word cloud
        output$wordcloud <- wordcloud2::renderWordcloud2({
            data <- filtered_wordcloud_data()

            if (is.null(data) || nrow(data) == 0) {
                return(NULL)
            }

            wordcloud2::wordcloud2(data)
        })
    })
}
