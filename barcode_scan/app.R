library(shiny)
library(bslib)
library(bsicons)

# pull newest inventory on connect
system(command = "sh dl_inventory.sh", intern = FALSE)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  shinyjs::useShinyjs(),
  # sets focus to barcode input box on page load
  tags$head(
    tags$script(
      HTML("
        $(document).ready(function() {
        $('#barcode').focus();
        })
        ")
    )
  ),
  value_box(
    title = strong("Scan Barcode"),
    value = textInput(
      inputId = "barcode",
      label = NULL,
      placeholder = "Scan Barcode"
    ),
    showcase = bs_icon("qr-code-scan"),
    max_height = 200,
  ),
  layout_column_wrap(
    width = 0.5,
    value_box(
      title = "Label ID",
      value = textOutput("label"),
      showcase = bs_icon("card-text")
    ),
    value_box(
      title = "Process Date",
      value = textOutput("date"),
      showcase = bs_icon("calendar-day")
    ),
    value_box(
      title = "Specimen Type",
      value = textOutput("specimen"),
      showcase = bs_icon("bucket")
    ),
    value_box(
      title = "Subject/Control",
      value = textOutput("subject_type"),
      showcase = bs_icon("controller")
    ),
    value_box(
      title = "GUID",
      value = textOutput("guid"),
      showcase = bs_icon("file-medical")
    ),
    value_box(
      title = "Collection Site",
      value = textOutput("site"),
      showcase = bs_icon("hospital")
    ),
    value_box(
      title = "Storage Location",
      value = htmlOutput("location"),
      showcase = bs_icon("bookshelf")
    ),
  ),
)

server <- function(input, output, session) {
  db <- reactive({
    vroom::vroom("pbmc.csv")
  })


  select <- reactive({
    db() |>
      dplyr::filter(sample_id == input$barcode)
  })

  observeEvent(select(), {
    shinyjs::runjs('document.getElementById("barcode").focus();')
    shinyjs::runjs('document.getElementById("barcode").select();')
  })

  output$label <- renderText({
    select() |>
      dplyr::pull(label)
  })
  output$date <- renderText({
    select() |>
      dplyr::pull(process_date) |>
      # convert excel date format
      as.Date(origin = "1899-12-30") |>
      format("%b %d %Y")
  })
  output$subject_type <- renderText({
    select() |>
      dplyr::pull(subject_type)
  })
  output$guid <- renderText({
    select() |>
      dplyr::pull(guid)
  })
  output$specimen <- renderText({
    id <- input$barcode |> stringr::str_split_1(pattern = "_")
    return(id[3])
  })
  output$site <- renderText({
    select() |>
      dplyr::pull(site)
  })

  output$location <- renderText({
    freezer <- select() |>
      dplyr::pull(freezer)
    rack <- select() |>
      dplyr::pull(rack_location)
    location <- paste0(p(freezer), br(), p(rack))
    return(location)
  })
}


shinyApp(ui, server)
