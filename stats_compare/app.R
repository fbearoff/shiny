library(shiny)
library(bslib)

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  window_title = "Let's Do Some Stats!",
  title = "Let's Do Some Stats!",
  sidebar = sidebar(
    width = 300,
    downloadButton(outputId = "downloadExamples", label = "Example Data", class = "btn-info"),
    fileInput(
      inputId = "file1",
      label = h5("Choose CSV File"),
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      ),
    ),
    uiOutput("controls"),
    uiOutput("download"),
  ),
  uiOutput("results")
)

server <- function(input, output, session) {
  raw <- reactive({
    req(input$file1)
    upload <- vroom::vroom(
      input$file1$datapath,
      na = c("NA", "N/A", "#DIV/0!", ""),
      col_types = c(sample = "c", condition = "f", .default = "d"),
      show_col_types = FALSE,
      progress = FALSE
    )

    # validate proper columns
    if (any(c("sample", "condition") %in% names(upload))) {
    } else {
      shinyalert::shinyalert("Oops!", "`sample` and `condition` columns not present in input csv", type = "error")
      validate(message = FALSE)
    }

    # validate at least 3 samples for every variable
    if (
      upload |>
        tidyr::pivot_longer(
          !(sample:condition), # nolint: object_usage_linter.
          names_to = "var",
          values_to = "value"
        ) |>
        dplyr::filter(!is.na(value)) |> # nolint: object_usage_linter.
        dplyr::group_by(var, condition) |>
        dplyr::summarise(
          n = dplyr::n(), # nolint: object_usage_linter.
          .groups = "keep"
        ) |>
        dplyr::pull(n) |>
        min() >= 3
    ) {
      return(upload)
    } else {
      shinyalert::shinyalert("Oops!", "Need at least 3 replicates per condition", type = "error")
      validate(message = FALSE)
    }
  })


  n_cond <- reactive({
    req(raw())
    nlevels(raw()$condition)
  })

  processed <- reactive({
    req(n_cond())

    if (n_cond() == 2) {
      source("ttest.R", local = TRUE)
      out <- multi_ttest( # nolint: object_usage_linter.
        data = raw(),
        parametric = input$parametric,
        var.equal = input$var.equal,
        paired = input$paired,
        conf.level = input$conf.level,
        adjust = input$adjust,
        title = input$title
      )
    } else if (n_cond() > 2) {
      source("ANOVA.R", local = TRUE)
      out <- multi_anova( # nolint: object_usage_linter.
        data = raw(),
        adjust = input$adjust,
        title = input$title
      )
    }
    return(out)
  })

  # Observers
  observe({
    updateTextInput(
      inputId = "title",
      placeholder = paste(processed()$pretty_title, "of each variable")
    )
  })

  # Outputs
  output$table <- renderTable(striped = TRUE, align = "c", {
    req(processed())
    table <- processed()[["table"]]
    return(table)
  })

  output$raw <- renderTable(striped = TRUE, {
    req(raw())
    raw() |>
      dplyr::mutate(dplyr::across(!c(sample, condition), ~ format(.x, digits = 3))) # nolint: object_usage_linter.
  })

  output$plot <- renderPlot({
    req(processed(), input$select)
    processed()[["boxplots"]][[input$select]]
  })

  # render UI elements after input completes
  output$controls <- renderUI({
    req(raw())
    # test specific controls
    if (n_cond() == 2) {
      card(
        h5("Controls"),
        uiOutput("title"),
        uiOutput("select"),
        uiOutput("paired"),
        uiOutput("parametric"),
        uiOutput("var.equal"),
        uiOutput("adjust"),
        uiOutput("conf.level")
      )
    } else {
      card(
        h5("Controls"),
        uiOutput("title"),
        uiOutput("select"),
        uiOutput("adjust"),
      )
    }
  })

  output$select <- renderUI({
    req(processed())
    selectizeInput(
      inputId = "select",
      label = strong("Select a Plot"),
      choices = names(processed()[["boxplots"]])
    )
  })

  output$title <- renderUI({
    req(raw())
    textInput(
      inputId = "title",
      placeholder = "Enter a Title",
      label = strong("Title"),
      value = NULL
    )
  })

  output$parametric <- renderUI({
    req(n_cond())
    checkboxInput(
      inputId = "parametric",
      label = strong("Parametric Data?"),
      value = TRUE
    )
  })

  output$var.equal <- renderUI({
    req(n_cond())
    checkboxInput(inputId = "var.equal", label = strong("Eqal Variance?"))
  })

  output$paired <- renderUI({
    req(n_cond())
    checkboxInput(inputId = "paired", label = strong("Paired Samples?"))
  })

  output$conf.level <- renderUI({
    req(n_cond())
    sliderInput(
      inputId = "conf.level",
      label = strong("Confidence Level"),
      value = 0.95,
      min = 0.90,
      max = 0.99,
      step = 0.01
    )
  })

  output$adjust <- renderUI({
    req(n_cond())
    radioButtons(
      inputId = "adjust",
      label = strong("P Adjust Method"),
      list(
        "BY",
        "Bonferroni" = "bonferroni",
        "FDR" = "fdr",
        "Hochberg" = "hochberg",
        "Holm" = "holm",
        "Hommel" = "hommel"
      ),
      selected = "fdr"
    )
  })

  output$results <- renderUI({
    req(processed())
    layout_columns(
      card(
        h3("Input Data"),
        tableOutput("raw"),
      ),
      card(
        h3(processed()$pretty_title),
        plotOutput("plot"),
        tableOutput("table")
      ),
    )
  })

  output$download <- renderUI({
    req(processed())
    downloadButton(
      outputId = "downloadData",
      label = "Download",
      class = "btn-success"
    )
  })

  # zip together report and interpolated results table
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file1$name), "_result.zip")
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      vroom::vroom_write(
        processed()[["res"]],
        file.path(
          temp_directory,
          paste(
            tools::file_path_sans_ext(input$file1$name),
            processed()[["test"]],
            "result.csv",
            sep = "_"
          )
        ),
        delim = ",",
        progress = FALSE
      )
      ggplot2::ggsave(
        filename = file.path(
          temp_directory,
          paste(tools::file_path_sans_ext(input$file1$name), processed()[["test"]], "boxplots.pdf", sep = "_")
        ),
        plot = processed()[["plot_grid"]],
        width = 22,
        height = 17,
        units = "in",
        dpi = "print",
        device = cairo_pdf
      )

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )

  # zip together example datasets
  output$downloadExamples <- downloadHandler(
    filename = function() {
      "example_data.zip"
    },
    content = function(file) {
      source("example.R", local = TRUE)
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      vroom::vroom_write(
        two_conds, # nolint: object_usage_linter.
        file.path(temp_directory, "two_conditions.csv"),
        delim = ",",
        progress = FALSE
      )
      vroom::vroom_write(
        ANOVA,
        file.path(temp_directory, "ANOVA.csv"),
        delim = ",",
        progress = FALSE
      )

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
