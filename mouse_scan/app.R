library(shiny)
library(bslib)
library(bsicons)

# pull newest inventory on connect
system(command = "sh dl_mouselog.sh", intern = FALSE)

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  shinyjs::useShinyjs(),
  # sets focus to chip input box on page load
  tags$head(
    tags$script(
      HTML("
        $(document).ready(function() {
        $('#scan').focus();
        })
        ")
    )
  ),
  value_box(
    title = strong("Scan Chip"),
    value = textInput(
      inputId = "scan",
      label = NULL,
      placeholder = "Scan Chip"
    ),
    showcase = bs_icon("qr-code-scan"),
    max_height = 200,
  ),
  layout_column_wrap(
    width = 0.5,
    value_box(
      title = "ID",
      value = textOutput("id"),
      showcase = bs_icon("card-text")
    ),
    value_box(
      title = "Age",
      value = textOutput("age"),
      showcase = bs_icon("clock")
    ),
    value_box(
      title = "Sex",
      value = textOutput("sex"),
      showcase = bs_icon("gender-ambiguous")
    ),
    value_box(
      title = "Genotype",
      value = textOutput("genotype"),
      showcase = bs_icon("yin-yang")
    ),
    value_box(
      title = "Sire",
      value = textOutput("sire"),
      showcase = bs_icon("gender-male")
    ),
    value_box(
      title = "Dam",
      value = textOutput("dam"),
      showcase = bs_icon("gender-female")
    ),
    value_box(
      title = "Date of Birth",
      value = textOutput("dob"),
      showcase = bs_icon("calendar-day")
    ),
    value_box(
      title = "Notes",
      value = htmlOutput("notes"),
      showcase = bs_icon("clipboard-data")
    ),
    value_box(
      title = "Status",
      value = textOutput("status"),
      showcase = bs_icon("list-check")
    ),
  ),
)

server <- function(input, output, session) {
  db <- reactive({
    vroom::vroom(
      "master_summary.csv",
      progress = FALSE,
      show_col_types = FALSE
    )
  })

  select <- reactive({
    db() |>
      dplyr::filter(AVID_id == input$scan)
  })

  observeEvent(select(), {
    shinyjs::runjs('document.getElementById("scan").focus();')
    shinyjs::runjs('document.getElementById("scan").select();')
  })

  output$id <- renderText({
    select() |>
      dplyr::pull(ID)
  })
  output$dob <- renderText({
    select() |>
      dplyr::pull(DOB) |>
      # convert excel date format
      as.Date(origin = "1899-12-30") |>
      format("%b %d %Y")
  })
  output$sex <- renderText({
    select() |>
      dplyr::pull(Sex)
  })
  output$age <- renderText({
    age <- select() |>
      dplyr::pull(Age)
    age <- paste(age, "days")
    return(age)
  })
  output$genotype <- renderText({
   select() |>
      dplyr::pull(Genotype) |>
      dplyr::case_match(
        "M" ~ "Mutant",
        "C" ~ "Control"
      )
  })
  output$sire <- renderText({
    select() |>
      dplyr::pull(Sire)
  })
  output$dam <- renderText({
    select() |>
      dplyr::pull(Dam)
  })
  output$notes <- renderText({
    select() |>
      dplyr::pull(Notes)
  })
  output$status <- renderText({
    select() |>
      dplyr::pull(Status)
  })
}

shinyApp(ui, server)
