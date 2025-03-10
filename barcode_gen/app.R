library(shiny)
library(bslib)
library(patchwork)

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  title = "Let's Print Some Barcodes!",
  sidebar = sidebar(
    width = 300,
    h4(textOutput("title")),
    input_switch("switch", "Manual mode?"),
    shinyjs::hidden(
      card(
        id = "manual",
        textAreaInput(
          "label1",
          rows = 3,
          label = strong("Label 1"),
          placeholder = "Label 1"
        ),
        textAreaInput(
          "label2",
          rows = 3,
          label = strong("Label 2"),
          placeholder = "Label 2"
        ),
        textAreaInput(
          "label3",
          rows = 3,
          label = strong("Label 3"),
          placeholder = "Label 3"
        ),
        textAreaInput(
          "label4",
          rows = 3,
          label = strong("Label 4"),
          placeholder = "Label 4"
        )
      )
    ),
    card(
      id = "id_card",
      textInput(
        "id",
        label = strong("ID String"),
        placeholder = "ID_Date_Specimen"
      ),
    ),
    actionButton("print", "Print Label", icon = icon("print"))
  ),
  card(
    id = "id_card_plot",
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
  card(
    id = "manual_plot_card",
    card_body(
      layout_column_wrap(
        card(
          card_body(
            class = "align-items-center",
            h2("Label 1"),
            plotOutput("manual_plot1"),
            h3(htmlOutput("man_lab1"))
          )
        ),
        card(
          card_body(
            class = "align-items-center",
            h2("Label 2"),
            plotOutput("manual_plot2"),
            h3(htmlOutput("man_lab2"))
          )
        ),
        card(
          card_body(
            class = "align-items-center",
            h2("Label 3"),
            plotOutput("manual_plot3"),
            h3(htmlOutput("man_lab3"))
          )
        ),
        card(
          card_body(
            class = "align-items-center",
            h2("Label 4"),
            plotOutput("manual_plot4"),
            h3(htmlOutput("man_lab4"))
          )
        ),
      ),
    )
  ),
)

server <- function(input, output, session) {
  # ID_Date_Specimen input
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
    plot <- plot(qrcode::qr_code(input$id))
    return(plot)
  })

  barcode_plot <- reactive({
    req(input$id)
    string <<- input$id # nolint: object_usage_linter.
    qr <- ggplotify::as.ggplot(~ plot(qrcode::qr_code(string)), scale = 1.1) +
      ggplot2::coord_fixed()
    label_text <- grid::textGrob(
      label_id(),
      gp = grid::gpar(col = "black", fontsize = 5)
    )
    plot <- qr + label_text + plot_layout(nrow = 2)
    return(plot)
  })

  # manual input
  manual <- reactive({
    inputs <- list(
      lab1 = input$label1,
      lab2 = input$label2,
      lab3 = input$label3,
      lab4 = input$label4
    )

    for (i in inputs) {
      split <- i |> stringr::str_split_1("\n")
      # 4 line maximum
      if (length(split) <= 4) {
      } else {
        shinyalert::shinyalert("Oops!", "Only 4 lines allowed.", type = "error")
        validate(message = FALSE)
      }
      for (l in split) {
        # 14 character maximium/line
        if (stringr::str_length(l) <= 14) {
        } else {
          shinyalert::shinyalert(
            "Oops!",
            "Only 14 characters allowed per line.",
            type = "error"
          )
          validate(message = FALSE)
        }
      }
    }

    qrs <- list()
    plots <- list()
    for (i in names(inputs)) {
      i <<- i # nolint: object_usage_linter.
      val <<- inputs[[i]] # nolint: object_usage_linter.
      if (shiny::isTruthy(val)) {
        # nolint: object_usage_linter.
        qrs[[i]] <- qrcode::qr_code(val) # nolint: object_usage_linter.
        qr <- ggplotify::as.ggplot(~ plot(qrcode::qr_code(val)), scale = 1.1) +
          ggplot2::coord_fixed()
        label_text <- grid::textGrob(
          val, # nolint: object_usage_linter.
          gp = grid::gpar(col = "black", fontsize = 5)
        )
        plots[[i]] <- qr + label_text + plot_layout(nrow = 2)
      } else {
        plots[[i]] <- ggplot2::ggplot() +
          ggplot2::theme_void()
      }
    }

    return(
      list(
        "qrs" = qrs,
        "plots" = plots,
        "inputs" = inputs
      )
    )
  })

  label_plot <- reactive({
    if (input$switch == FALSE) {
      label <- barcode_plot() |
        barcode_plot() |
        barcode_plot() |
        barcode_plot() |
        plot_layout(ncol = 4)
    } else if (input$switch == TRUE) {
      label <- manual()$plots$lab1 |
        manual()$plots$lab2 |
        manual()$plots$lab3 |
        manual()$plots$lab4 |
        plot_layout(ncol = 4)
    }
    label <- label &
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 0.1, r = 0.1, l = 0.1, unit = "in")
      )
    return(label)
  })

  # manual mode

  observe(
    shinyjs::toggle("manual", condition = input$switch)
  )
  observe(
    shinyjs::toggle("manual_plot_card", condition = input$switch)
  )
  observe(
    shinyjs::toggle("id_card", condition = !input$switch)
  )
  observe(
    shinyjs::toggle("id_card_plot", condition = !input$switch)
  )

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
  output$title <- renderText({
    ifelse(
      input$switch,
      yes = "Enter up to 4 Lines",
      no = "Enter an ID String"
    )
  })
  output$plot <- renderPlot({
    barcode()
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
  output$manual_plot1 <- renderPlot({
    req(input$label1)
    plot(manual()$qrs$lab1)
  })
  output$manual_plot2 <- renderPlot({
    req(input$label2)
    plot(manual()$qrs$lab2)
  })
  output$manual_plot3 <- renderPlot({
    req(input$label3)
    plot(manual()$qrs$lab3)
  })
  output$manual_plot4 <- renderPlot({
    req(input$label4)
    plot(manual()$qrs$lab4)
  })
  output$man_lab1 <- renderText({
    req(input$label1)
    manual()$inputs$lab1 |> stringr::str_replace_all("\n", "<br>")
  })
  output$man_lab2 <- renderText({
    req(input$label2)
    manual()$inputs$lab2 |> stringr::str_replace_all("\n", "<br>")
  })
  output$man_lab3 <- renderText({
    req(input$label3)
    manual()$inputs$lab3 |> stringr::str_replace_all("\n", "<br>")
  })
  output$man_lab4 <- renderText({
    req(input$label4)
    manual()$inputs$lab4 |> stringr::str_replace_all("\n", "<br>")
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
