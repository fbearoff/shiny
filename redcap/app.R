# NOTE: Add demographics
# NOTE: Clinical Trials added to redcap
library(shiny)
library(bslib)

# silence readr output
options(readr.num_columns = 0)

ui <- page_fillable(
  title = "Enrollment in Active Studies",
  h1("Enrollment in Active Studies"),
  card(plotOutput(outputId = "summary_plot")),
  layout_column_wrap(
    heights_equal = "row",
    width = 0.5,
    card(
      fill = TRUE,
      tableOutput(outputId = "summary_table")
    ),
    card(
      h2("Approachable"),
      fill = TRUE,
      tableOutput(outputId = "needs_approach")
    ),
  )
)
server <- function(input, output) {
  # load credentials
  creds <- REDCapR::retrieve_credential_local(
    "credentials.csv",
    project_id = "4308"
  )

  # retrieve data and metadata dictionary from redcap
  meta <- REDCapR::redcap_metadata_read(
    redcap_uri = creds$redcap_uri,
    token = creds$token,
    verbose = FALSE
  )
  rc <- REDCapR::redcap_read(
    redcap_uri = creds$redcap_uri,
    token = creds$token,
    verbose = FALSE
  )

  # find fields with coded choices
  coded_choices <- meta$data |>
    dplyr::filter(stringr::str_detect(
      select_choices_or_calculations, # nolint: object_usage_linter.
      "1, "
    )) |>
    dplyr::select(
      field_name, # nolint: object_usage_linter.
      select_choices_or_calculations
    ) |>
    tidyr::separate_rows(select_choices_or_calculations, sep = "\\|") |>
    tidyr::separate_wider_delim(
      select_choices_or_calculations,
      delim = ",",
      names = c("Code", "Choice")
    ) |>
    dplyr::mutate(
      Code = as.numeric(Code), # nolint: object_usage_linter.
      Choice = stringr::str_trim(Choice) # nolint: object_usage_linter.
    )

  # get active studies
  # NOTE: should have non-zero inex sum
  active <- rc$data |>
    dplyr::select(dplyr::starts_with("inex_")) |>
    {
      # nolint: brace_linter.
      \(.) {
        # NOTE: assume NAs are inelligible?
        replace(., is.na(.), 0)
      }
    }() |> # nolint: brace_linter.
    dplyr::select(dplyr::where(function(x) sum(x) > 0)) |>
    names() |>
    stringr::str_remove("^inex_")

  # get studies and corresponding var abbreviations
  vars <- meta$data |>
    dplyr::filter(stringr::str_starts(
      field_name, # nolint: object_usage_linter.
      "inex_"
    )) |>
    dplyr::mutate(
      field_name = stringr::str_remove(field_name, "inex_"),
      field_label = stringr::str_remove(
        field_label, # nolint: object_usage_linter.
        "^Does the subject meet the inclusion/exclusion criteria for "
      ),
      field_label = stringr::str_remove(field_label, "\\?$")
    ) |>
    dplyr::select(field_name, field_label) |>
    dplyr::filter(field_name %in% active)

  # recode data to actual value definitions
  recode_col <- function(x) {
    recode_vec <- coded_choices |>
      dplyr::filter(field_name == dplyr::cur_column()) |> # nolint: object_usage_linter.
      dplyr::pull(Choice, name = Code) # nolint: object_usage_linter.

    dplyr::recode(x, !!!recode_vec)
  }

  recoded <- rc$data |>
    dplyr::mutate(
      dplyr::across(
        .cols = unique(coded_choices$field_name),
        .fns = recode_col
      )
    )

  # summarize counts/study
  counts <- function(study) {
    res <- recoded |> # nolint: object_usage_linter.
      dplyr::filter(patorcon == "Patient") |> # nolint: object_usage_linter.
      dplyr::summarise(
        Elligible = sum(!!dplyr::sym(paste0("inex_", study)), na.rm = TRUE),
        Approached = sum(!!dplyr::sym(paste0("ap", study)), na.rm = TRUE),
        Consented = sum(!!dplyr::sym(paste0(study, "consent")), na.rm = TRUE),
        Defering = sum(!!dplyr::sym(paste0("def", study)), na.rm = TRUE),
        Declined = sum(!!dplyr::sym(paste0("dec", study)), na.rm = TRUE),
      )
  }

  # summary table/study
  totals <- dplyr::bind_rows(lapply(vars$field_name, counts)) |>
    tibble::add_column(Study = vars$field_label, .before = 1)

  # plot totals/study
  summary_plot <- totals |>
    tidyr::pivot_longer(cols = !Study, values_to = "Count") |> # nolint: object_usage_linter.
    dplyr::filter(Count > 0) |> # nolint: object_usage_linter.
    ggplot2::ggplot(ggplot2::aes(
      fill = forcats::fct_inorder(name), # nolint: object_usage_linter.
      y = Count, # nolint: object_usage_linter.
      x = Study
    )) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::scale_x_discrete(drop = TRUE) +
    ggplot2::scale_y_continuous(
      label = scales::label_number(accuracy = 1),
      expand = c(0, 0) # remove whitespace
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(
        viridis::viridis(5, direction = -1, option = "viridis"),
        totals |> dplyr::select(!Study) |> names()
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 14
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = 14
      ),
      axis.text.x = ggplot2::element_text(
        size = 16,
        angle = 45,
        hjust = 1,
      ),
      axis.text.y = ggplot2::element_text(
        size = 14
      )
    )

  # find ellgible patients that have not been approached
  needs_approach <- function(study) {
    res <- recoded |> # nolint: object_usage_linter.
      dplyr::filter(
        patorcon == "Patient", # nolint: object_usage_linter.
        !!dplyr::sym(paste0("inex_", study)) == 1,
        !!dplyr::sym(paste0("ap", study)) != 1 |
          is.na(!!dplyr::sym(paste0("ap", study))),
      ) |>
      dplyr::mutate(study = study, .before = first_name) |> # nolint: object_usage_linter.
      dplyr::mutate(name = paste(first_name, last_name)) |> # nolint: object_usage_linter.
      dplyr::select(study, name) # nolint: object_usage_linter.
  }
  approach <- dplyr::bind_rows(lapply(vars$field_name, needs_approach)) |>
    dplyr::left_join(vars, dplyr::join_by(study == field_name)) |> # nolint: object_usage_linter.
    dplyr::mutate(study = field_label) |> # nolint: object_usage_linter.
    dplyr::select(!field_label) |>
    dplyr::rename_with(stringr::str_to_title) # nolint: object_usage_linter.

  # outputs
  output$summary_plot <- renderPlot({
    summary_plot
  })

  output$summary_table <- function() {
    totals |>
      knitr::kable("html") |>
      kableExtra::kable_styling(
        full_width = TRUE,
        bootstrap_options = c("striped", "condensed")
      )
  }

  output$needs_approach <- function() {
    approach |>
      knitr::kable("html") |>
      kableExtra::collapse_rows(
        columns = 1,
        valign = "middle"
      ) |>
      kableExtra::kable_styling(
        bootstrap_options = "striped",
        full_width = FALSE,
        position = "left"
      )
  }
}

shinyApp(ui, server)
