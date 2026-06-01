options(sass.cache = FALSE)

library(shiny)
library(bslib)
library(patchwork)

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  title = "Let's Relabel Samples!",
  sidebar = sidebar(
    card(
      id = "relabel_card",
      min_height = 600,
      h4("Print Replacement Labels?"),
      textInput(
        inputId = "id",
        label = strong("Label ID"),
        placeholder = "Label ID"
      ),
      dateInput(
        inputId = "date",
        label = strong("Collection Date"),
        value = Sys.Date(),
        format = "MM dd, yyyy"
      ),
      radioButtons(
        inputId = "specimen",
        label = strong("Specimen Type"),
        choices = c(
          "Plasma" = "Plasma",
          "Serum" = "Serum",
          "RNA/DNA" = "RNA/DNA",
          "PBMC" = "PBMC",
          "Urine" = "Urine",
          "CSF" = "CSF"
        ),
      ),
      numericInput(
        inputId = "label_number",
        label = strong("Quantity"),
        value = 1,
        min = 1,
        max = 10,
      ),
      actionButton(
        inputId = "print",
        label = "Print Labels",
        icon = icon("print"),
        disabled = TRUE
      ),
    ),
  ),
  card(
    id = "plot_card",
    card_body(
      class = "align-items-center",
      htmlOutput("title"),
      plotOutput("plot"),
      htmlOutput("id"),
      htmlOutput("date"),
      htmlOutput("specimen")
    )
  ),
)

server <- function(input, output, session) {
  observe({
    req(input$id)
    # enabled printing only when ID is entered
    updateActionButton(
      session = session,
      inputId = "print",
      disabled = FALSE
    )
  })

  observe({
    req(input$id)

    date <- input$date |> lubridate::ymd() |> format("%m/%d/%Y")

    string <<- paste(toupper(input$id), date, input$specimen, sep = "_") # nolint: object_usage_linter.

    qr <- ggplotify::as.ggplot(
      ~ plot(qrcode::qr_code(string)),
      scale = 1.1
    ) +
      ggplot2::coord_fixed()

    label_text <- grid::textGrob(
      paste(
        input$id,
        input$date |> lubridate::ymd() |> format("%b %d %Y"),
        input$specimen,
        sep = "\n"
      ),
      gp = grid::gpar(col = "black", fontsize = 5)
    )

    label_plot <- qr + label_text + plot_layout(nrow = 2)

    temp <- tempfile(pattern = "label_", fileext = ".pdf")

    ggplot2::ggsave(
      label_plot,
      file = temp,
      height = 1,
      width = 1,
      units = "in"
    )

    # Send N labels to printer
    for (quant in 1:input$label_number) {
      system(paste0("lp -d Zebra-411 ", temp))
    }

    # Notify
    shinyalert::shinyalert(
      html = TRUE,
      type = "success",
      title = "Barcodes sent to printer!",
    )
  }) |>
    bindEvent(input$print)

  output$title <- renderUI({
    req(input$id)
    span(h1(paste0("Printing ", input$label_number, " New Labels")))
  })
  output$plot <- renderPlot({
    req(input$id)
    string2 <- paste(toupper(input$id), input$date, input$specimen, sep = "_")
    plot(qrcode::qr_code(string2))
  })
  output$id <- renderText({
    req(input$id)
    paste0(span(strong("ID: ", .noWS = "after"), toupper(input$id)))
  })
  output$date <- renderText({
    req(input$id)
    paste0(span(strong("Collection Date: ", .noWS = "after"), input$date))
  })
  output$specimen <- renderText({
    req(input$id)
    paste0(span(strong("Specimen: ", .noWS = "after"), input$specimen))
  })
}

shinyApp(ui, server)
