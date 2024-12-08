library(shiny)

ui <- fluidPage(
  titlePanel("Process ELLA Export File"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose ELLA CSV Export File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      tableOutput("table")
    ),
  )
)

server <- function(input, output) {
  raw <- reactive({
    req(input$file1)
    vroom::vroom(input$file1$datapath, show_col_types = FALSE)
  })

  processed <- reactive({
    req(input$file1)
    raw() |>
      dplyr::select(c("Sample Name", "Sample Type", "Analyte Name", "Mean Conc.")) |>
      tidyr::pivot_wider(names_from = "Analyte Name", values_from = c("Mean Conc."))
  })

  output$table <- renderTable({
    req(input$file1)
    processed()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(basename(input$file1$name), "_processed.csv")
    },
    content = function(file) {
      vroom::vroom_write(processed(), file, delim = ",")
    }
  )
}

shinyApp(ui, server)
