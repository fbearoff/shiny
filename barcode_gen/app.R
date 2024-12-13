library(shiny)
library(bslib)
library(patchwork)

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  title = "Let's Print Some Barcodes!",
  sidebar = sidebar(
    width = 300,
    h4("Enter an ID String"),
    textInput("id", label = NULL, placeholder = "ID_Date_Specimen"),
    actionButton("print", "Print Label", icon = icon("print"))
  ),
  card(
    card_body(
      max_height = 250,
      plotOutput("plot")
    ),
    card_body(
      class = "align-items-center",
      htmlOutput("id"),
      htmlOutput("date"),
      htmlOutput("specimen")
    )
  ),
)

server <- function(input, output, session) {
  # Main loop

  id <- reactive({
    req(input$id)

    id <- input$id |> stringr::str_split_1(pattern = "_")

    id[2] <- id[2] |>
      lubridate::mdy() |>
      format("%b %d %Y")

    return(id)
  })

  label_id <- reactive({
    req(input$id)

    text <- paste(id()[1], id()[2], id()[3], sep = "\n")
    return(text)
  })

  barcode <- reactive({
    req(input$id)
    qrcode::qr_code(input$id)
  })

  barcode_plot <- reactive({
    req(input$id)

    string <<- input$id # nolint: object_usage_linter.
    qr <- ggplotify::as.ggplot(~ plot(qrcode::qr_code(string)), scale = 1.1) + ggplot2::coord_fixed()
    label_text <- grid::textGrob(label_id(), gp = grid::gpar(col = "black", fontsize = 5))
    qr + label_text + plot_layout(nrow = 2)
  })

  label_plot <- reactive({
    req(input$id)

    label <- barcode_plot() | barcode_plot() | barcode_plot() | barcode_plot() | plot_layout(ncol = 4)
    label & ggplot2::theme(plot.margin = ggplot2::margin(t = 0.1, r = 0.1, l = 0.1, unit = "in"))
  })


  # Button click
  temp <- tempfile(pattern = "label_", fileext = ".pdf")

  observe({
    # Notify
    shinyalert::shinyalert(
      html = TRUE,
      type = "success",
      title = "Barcode sent to printer!",
      text = tagList(
        downloadButton("downloadData", "Save PDF?")
      )
    )

    # Save pdf
    ggplot2::ggsave(
      label_plot(),
      file = temp,
      height = 1,
      width = 4,
      units = "in"
    )
    # Send pdf to printer
    system(paste0("lp ", temp))
  }) |>
    bindEvent(input$print)

  # Outputs
  output$plot <- renderPlot({
    plot(barcode())
  })
  output$id <- renderText({
    paste0(span(strong("ID: ", .noWS = "after"), id()[1]))
  })
  output$date <- renderText({
    paste0(span(strong("Date: ", .noWS = "after"), id()[2]))
  })
  output$specimen <- renderText({
    paste0(span(strong("Specimen: ", .noWS = "after"), id()[3]))
  })
  output$downloadData <- downloadHandler(
    filename = "label.pdf",
    content = function(file) {
      file.copy(temp, file)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
