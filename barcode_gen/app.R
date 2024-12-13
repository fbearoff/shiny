library(shiny)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
  textInput("id", label = NULL, placeholder = "ID_Date_Specimen"),
  plotOutput("plot"),
  textOutput("id"),
  textOutput("date"),
  textOutput("specimen"),
  actionButton("print", "Print Label"),
)

server <- function(input, output, session) {
  # Main loop

  id <- reactive({
    req(input$id)

    id <- input$id |> stringr::str_split_1(pattern = "_")

    id[2] <- id[2] |>
      lubridate::mdy() |>
      format("%b %d %Y")

    id
  })

  label_id <- reactive({
    req(input$id)

    text <- paste(id()[1], id()[2], id()[3], sep = "\n")
  })

  barcode <- reactive({
    req(input$id)
    qrcode::qr_code(input$id)
  })

  barcode_plot <- reactive({
    req(input$id)

    string <<- input$id
   qr <- ggplotify::as.ggplot(~ plot(qrcode::qr_code(string)), scale = 1.1) + coord_fixed()
    label_text <- grid::textGrob(label_id(), gp = grid::gpar(col = "black", fontsize = 5))
    qr + label_text + plot_layout(nrow = 2)
  })

  label_plot <- reactive({
    req(input$id)

    label <- barcode_plot() | barcode_plot() | barcode_plot() | barcode_plot() | plot_layout(ncol = 4)
    label & theme(plot.margin = margin(t = 0.1, r = 0.1, l = 0.1, unit = "in"))
  })


  # Button click

  observe({
    # Notify
    showModal(modalDialog(title = "Print","Barcode sent to printer"))

    # Save pdf
    ggsave(
      label_plot(),
      file = "label.pdf",
      height = 1,
      width = 4,
      units = "in"
    )

    # Send pdf to printer
    system("lp /srv/shiny-server/barcode_gen/label.pdf")
  }) |>
    bindEvent(input$print)

  # Outputs
  output$plot <- renderPlot({
    plot(barcode())
  })
  output$id <- renderText({
    paste0("ID: ", id()[1])
  })
  output$date <- renderText({
    paste0("Date: ", id()[2])
  })
  output$specimen <- renderText({
    paste0("Specimen: ", id()[3])
  })
}

shinyApp(ui, server)
