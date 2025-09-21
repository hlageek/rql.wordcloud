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

    fluidRow(
        # First column - All inputs
        column(
            6,
            actionButton(ns("load_data_button"), "Load Requal Data"),
            br(),
            br(),
            selectInput(
                ns("code_select"),
                "Select Code",
                choices = list("Load data first..." = ""),
                multiple = FALSE
            ),
            h5("Text Processing Options"),
            checkboxInput(
                ns("remove_stopwords"),
                "Remove stop words",
                value = TRUE
            ),
            conditionalPanel(
                condition = "input.remove_stopwords",
                ns = ns,
                checkboxInput(
                    ns("use_language_stopwords"),
                    "Use language-specific stopwords",
                    value = TRUE
                ),
                conditionalPanel(
                    condition = "input.use_language_stopwords",
                    ns = ns,
                    selectInput(
                        ns("language_select"),
                        "Select Language for Stopwords",
                        choices = NULL
                    )
                ),
                textAreaInput(
                    ns("custom_stopwords"),
                    "Custom Stopwords (one per line or separated by commas)",
                    "",
                    placeholder = "Enter custom stopwords here...",
                    rows = 3
                )
            ),
            checkboxInput(
                ns("remove_punctuation"),
                "Remove punctuation",
                value = TRUE
            ),
            checkboxInput(
                ns("convert_lowercase"),
                "Convert to lowercase",
                value = TRUE
            ),
            uiOutput(ns("frequency_slider")),
            div(
                id = ns("word_count_info"),
                style = "margin-bottom: 15px; color: #666; font-size: 0.9em;"
            )
        ),
        # Second column - Wordcloud
        column(
            6,
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
            wordcloud2::wordcloud2Output(ns("wordcloud"), height = "500px")
        )
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

        # Reactive values
        loc <- reactiveValues(
            wordcloud_df = NULL,
            segments = NULL,
            processing = FALSE,
            max_frequency = 100
        )

        # Initialize API if not provided
        if (is.null(api)) {
            api <- requal::RequalAPI$new()
        }

        # Initialize UI elements
        observe({
            # Populate language choices using stopwords package
            language_choices <- stopwords::stopwords_getlanguages(
                "stopwords-iso"
            )
            language_names <- setNames(
                language_choices,
                paste0(language_choices, " (", toupper(language_choices), ")")
            )

            updateSelectInput(
                session,
                "language_select",
                choices = language_names,
                selected = "en"
            )
        })

        # Load segments data
        observeEvent(input$load_data_button, {
            tryCatch(
                {
                    loc$segments <- api$get_segments()

                    updateSelectInput(
                        session,
                        "code_select",
                        choices = c(
                            "All codes (no filter)" = "all",
                            "Select specific code..." = "",
                            setNames(
                                loc$segments$code_id,
                                loc$segments$code_name
                            )
                        ),
                        selected = "all"
                    )
                },
                error = function(e) {
                    showNotification(
                        paste("Error loading data:", e$message),
                        type = "error"
                    )
                }
            )
        })

        # Processing indicator
        output$processing <- reactive({
            loc$processing
        })
        outputOptions(output, "processing", suspendWhenHidden = FALSE)

        # Stopwords removal function
        remove_stopwords <- function(
            df,
            use_language,
            language,
            custom_words,
            convert_lowercase
        ) {
            # Get language stopwords only if enabled
            language_stopwords <- if (use_language && !is.null(language)) {
                stopwords::stopwords(language, source = "stopwords-iso")
            } else {
                character(0) # Empty if language stopwords disabled
            }

            # Parse custom stopwords
            custom_stopwords <- if (
                !is.null(custom_words) && custom_words != ""
            ) {
                parsed_words <- strsplit(custom_words, "[,\n]+")[[1]] %>%
                    trimws() %>%
                    .[. != ""]

                if (convert_lowercase) {
                    parsed_words <- tolower(parsed_words)
                }
                parsed_words
            } else {
                character(0) # Empty if no custom stopwords
            }

            # Combine stopwords based on user settings
            all_stopwords <- if (
                use_language && (is.null(custom_words) || custom_words == "")
            ) {
                # Only language stopwords
                language_stopwords
            } else if (
                !use_language && (!is.null(custom_words) && custom_words != "")
            ) {
                # Only custom stopwords
                custom_stopwords
            } else if (
                use_language && (!is.null(custom_words) && custom_words != "")
            ) {
                # Both language and custom stopwords
                unique(c(language_stopwords, custom_stopwords))
            } else {
                # Neither enabled or no stopwords provided
                character(0)
            }

            # Only filter if we have stopwords to remove
            if (length(all_stopwords) > 0) {
                df %>% dplyr::filter(!word %in% all_stopwords)
            } else {
                df # Return unchanged if no stopwords
            }
        }

        # Text processing function
        process_text_data <- function() {
            if (is.null(input$code_select) || is.null(loc$segments)) {
                return(NULL)
            }

            if (input$code_select == "") {
                return(NULL)
            }

            loc$processing <- TRUE
            on.exit(loc$processing <- FALSE)

            # Get text data
            wordcloud_text_input <- if (input$code_select == "all") {
                loc$segments %>% dplyr::select(segment_text)
            } else {
                selected_codes <- if (length(input$code_select) > 1) {
                    input$code_select
                } else {
                    as.integer(input$code_select)
                }

                loc$segments %>%
                    dplyr::filter(code_id %in% selected_codes) %>%
                    dplyr::select(segment_text)
            }

            if (
                is.null(wordcloud_text_input) || nrow(wordcloud_text_input) == 0
            ) {
                return(NULL)
            }

            # Tokenize text
            processed_df <- wordcloud_text_input %>%
                tidytext::unnest_tokens(
                    word,
                    segment_text,
                    to_lower = input$convert_lowercase,
                    strip_punct = input$remove_punctuation
                )

            # Apply stopwords filtering
            if (input$remove_stopwords) {
                processed_df <- remove_stopwords(
                    processed_df,
                    input$use_language_stopwords,
                    input$language_select,
                    input$custom_stopwords,
                    input$convert_lowercase
                )
            }

            # Count frequencies
            word_freq_df <- processed_df %>%
                dplyr::count(word, name = "freq") %>%
                dplyr::arrange(desc(freq)) %>%
                dplyr::filter(freq >= 1)

            return(word_freq_df)
        }

        # Main reactive for processing text
        observeEvent(
            {
                list(
                    input$code_select,
                    input$remove_stopwords,
                    input$use_language_stopwords,
                    input$remove_punctuation,
                    input$convert_lowercase,
                    input$language_select,
                    input$custom_stopwords
                )
            },
            {
                result <- process_text_data()
                loc$wordcloud_df <- result

                # Update max frequency reactive value
                if (!is.null(result) && nrow(result) > 0) {
                    loc$max_frequency <- max(result$freq, na.rm = TRUE)
                } else {
                    loc$max_frequency <- 100
                }
            },
            ignoreInit = TRUE
        )

        # Render dynamic slider based on data
        output$frequency_slider <- renderUI({
            max_val <- loc$max_frequency

            current_val <- if (!is.null(input$min_frequency)) {
                min(input$min_frequency, max_val)
            } else {
                1
            }

            sliderInput(
                ns("min_frequency"),
                "Minimum Word Frequency",
                min = 1,
                max = max_val,
                value = current_val,
                step = 1
            )
        })

        # Filtered data for wordcloud
        filtered_wordcloud_data <- reactive({
            req(loc$wordcloud_df)

            filtered_df <- loc$wordcloud_df %>%
                dplyr::filter(freq >= input$min_frequency)

            return(filtered_df)
        })

        # Update word count display
        output$word_count_info <- renderText({
            if (is.null(loc$wordcloud_df) || nrow(loc$wordcloud_df) == 0) {
                return("No data to display")
            }

            filtered_df <- loc$wordcloud_df %>%
                dplyr::filter(freq >= input$min_frequency)

            total_words <- nrow(loc$wordcloud_df)
            shown_words <- nrow(filtered_df)
            paste("Showing", shown_words, "of", total_words, "unique words")
        })

        # Render wordcloud
        output$wordcloud <- wordcloud2::renderWordcloud2({
            data <- filtered_wordcloud_data()

            if (is.null(data) || nrow(data) == 0) {
                return(NULL)
            }

            wordcloud2::wordcloud2(
                data,
                size = 0.8,
                minRotation = -pi / 6,
                maxRotation = pi / 6,
                rotateRatio = 0.4
            )
        })
    })
}
