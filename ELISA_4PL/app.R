library(shiny)
library(bslib)
library(tidyverse)
library(dr4pl)
library(patchwork)
library(gt)

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  title = "ELISA 4 Parameter Logistic Analysis",
  sidebar = sidebar(
    width = 300,
    fileInput("file1", h5("Choose ELISA CSV File"),
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    card(
      h5("Controls"),
      textInput(
        "analyte",
        strong("Analyte"),
        placeholder = "Enter analyte"
      ),
      numericInput(
        "od",
        strong("Absorbance (nm)"),
        value = 450,
        min = 300,
        step = 10,
      ),
      radioButtons(
        "units",
        label = strong("Units"),
        choices = list(
          "pg/ml" = "pg/ml",
          "ng/ml" = "ng/ml"
        )
      ),
      dateInput(
        inputId = "date",
        label = strong("Date"),
      ),
      numericInput(
        "df",
        strong("Dilution Factor"),
        value = 1,
        min = 1,
      ),
    ),
    uiOutput("download")
  ),
  uiOutput("results")
)

server <- function(input, output) {
  raw <- reactive({
    req(input$file1, input$analyte)
    vroom::vroom(input$file1$datapath, show_col_types = FALSE, progress = FALSE)
  })

  processed <- reactive({
    req(input$file1, input$analyte)

    unknowns <- raw() |>
      filter(is.na(conc) & !is.na(od))

    standards <- raw() |>
      filter(!conc == 0 & !od == 0) |>
      mutate(across(conc, as.double))

    # 4PL fit model
    model <- dr4pl(od ~ conc, data = standards, method.init = "Mead")

    # return concentrations from ODs using 4PL fit model
    sol <- function(object, x) {
      UL <- object$parameters[1]
      EC50 <- object$parameters[2]
      Slope <- object$parameters[3]
      LL <- object$parameters[4]

      value <- EC50 * ((x - UL) / (LL - x))^(-1 / Slope)
      return(value)
    }

    # interpolate unknowns from model
    unknowns$conc <- sol(model, unknowns$od)

    # adjust for dilution factor
    unknowns <- unknowns |> mutate(conc = conc * input$df)

    # calculate SDs
    sdevs <- unknowns |>
      group_by(name) |>
      summarise(conc_sd = sd(conc), od_sd = sd(od))

    # collapse technical replicates
    unknowns <- unknowns |>
      group_by(name) |>
      mutate(across(c(conc, od), mean), .keep = "used") |>
      distinct() |>
      left_join(sdevs, by = "name") |>
      rename("conc ({ input$units })" := conc)

    # plot title
    title <- paste(input$analyte, "ELISA", input$date)

    # calculate model error
    rmse <- residuals(model)^2 |>
      mean() |>
      sqrt()

    coeff <- as_tibble(summary(model)$coefficients, rownames = "Values")

    # generate goodness of fit dataframe
    ## silence gof output
    sink(nullfile())
    gof <- bind_rows(
      gof(model)[1, ],
      gof(model)[2, ]
    ) |>
      as_tibble() |>
      add_column(
        "Error Source" = rownames(gof(model)),
        .before = -1
      ) |>
      add_column("RMSE" = noquote(c(round(rmse, 4), "")))
    sink()

    # model plot
    p1 <- plot(model,
      text.title =  title,
      text.y = paste0("OD ", input$od, " mm"),
      text.x = input$units,
    ) + geom_smooth(
      method = "loess",
      formula = "y ~ x",
      linetype = 0,
      span = 1
    )

    if (input$df != 1) {
      p1 <- p1 + labs(subtitle = paste0("DF=", input$df))
    }

    # Best fit values table
    p2 <- coeff |>
      gt() |>
      fmt_number() |>
      tab_header(title = gt::md("**Best-fit Values**"))

    # GOF table
    p3 <- gof |>
      gt() |>
      tab_header(title = md("**Goodness of Fit**"))

    # arrange report
    report <- (p1 + p2 + p3 + plot_layout(nrow = 3, ncol = 1, heights = c(1.5, 1, 1)))

    # make list of exports
    out <- list(
      "unknowns" = unknowns,
      "report" = report,
      "gof" = gof,
      "coeff" = coeff,
      "plot" = p1
    )

    return(out)
  })

  # Outputs
  output$table <- renderTable({
    req(input$file1, input$analyte)
    processed()[["unknowns"]]
  })

  output$gof <- renderTable({
    req(input$file1, input$analyte)
    processed()[["gof"]]
  })

  output$coeff <- renderTable({
    req(input$file1, input$analyte)
    processed()[["coeff"]]
  })

  output$report <- renderPlot({
    req(input$file1, input$analyte)
    processed()["plot"]
  })

  # render results UI elements after input completes
  output$results <- renderUI({
    req(input$file1, input$analyte)
    layout_columns(
      card(
        h3("Report"),
        plotOutput("report"),
        tableOutput("gof"),
        tableOutput("coeff"),
      ),
      card(
        h3("Interpolated"),
        tableOutput("table")
      )
    )
  })
  output$download <- renderUI({
    req(input$file1, input$analyte)
    downloadButton("downloadData", "Download")
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
        processed()[["unknowns"]],
        file.path(temp_directory, paste0(tools::file_path_sans_ext(input$file1$name), "_result.csv")),
        delim = ","
      )
      ggsave(
        filename = file.path(temp_directory, paste0(tools::file_path_sans_ext(input$file1$name), "_report.pdf")),
        plot = processed()[["report"]],
        width = 6.5,
        height = 7,
        units = "in"
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
