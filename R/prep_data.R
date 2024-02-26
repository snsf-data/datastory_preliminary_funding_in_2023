# Color settings: Colors for the LVL 1 instruments are defined as in the
# Data Portal (YAAY scheme)

get_color_palette_lvl_1 <- function(lang) {
  tibble(
    FundingInstrumentGaLevel1 = c(
      translate("Projects", "Projekte", "Projets", 
                lang),
      translate("Careers", "Karrieren", "Carrières", lang),
      translate("Programmes", "Programme", "Programmes", lang),
      translate("Infrastructure", "Infrastrukturen", "Infrastructures", lang),
      translate("Science communication", "Wissenschaftskommunikation", 
                "Communication scientifique", lang), 
      translate("Horizon Europe Transitional Measures", 
                "Übergangsmassnahmen Horizon Europe", 
                "Mesures transitoires Horizon Europe", lang)
    ),
    # Use slightly adapted SNSF colors for this kind of plot
    color_lvl1 = c("#3D7D9F", "#83D0F5", "#FBBE5E", "#71B294", "#9D90B9",
                   "#3D7D9F")
  )
}

# Function to preprocess the funding data and create summarized data objects
# for the bar plot main bar and the bar that summarizes also the two programmes
# components (general programmes + Horizon transition measures)
preprocess_plot_data <- function(funding_data, lang, reverse_order = FALSE) {
  # Mapping table for the translation of the Level 1 Funding Instruments
  mapping_fi <-
    tibble(
      en = c("Projects", "Careers", "Programmes", "Horizon Europe Transitional Measures",
             "Infrastructure", "Science communication"),
      de = c("Projekte", "Karrieren", "Programme", "Übergangsmassnahmen Horizon Europe",
             "Infrastrukturen", "Wissenschaftskommunikation"),
      fr = c("Projets", "Carrières", "Programmes", "Mesures transitoires Horizon Europe", 
             "Infrastructures", "Communication scientifique")
    ) |> 
    # Now choose the English string (to join and the destination lang string)
    mutate(FundingInstrumentGaLevel1 = en) |> 
    select(FundingInstrumentGaLevel1, destination = any_of(lang))
  
  # Compile the text string to be displayed in the tooltip
  tooltip_text <-
    funding_data |>
    mutate(
      FundingInstrumentGaReporting =
        case_when(
          lang == "de" ~ FundingInstrumentGaReporting_De,
          lang == "fr" ~ FundingInstrumentGaReporting_Fr,
          lang == "en" ~ FundingInstrumentGaReporting
        )
    ) |> 
    mutate(
      grouping_scheme =
        if_else(
          str_detect(FundingInstrumentGaReporting, "Horizon Europe"), 
          FundingInstrumentGa,
          FundingInstrumentGaReporting
        )
    ) |> 
    summarise(
      scheme_granted = sum(AmountGranted, na.rm = TRUE),
      n = n(),
      .by = c(FundingInstrumentGaLevel1, grouping_scheme)
    ) |> 
    arrange(desc(scheme_granted)) |> 
    summarise(
      tooltip =
        paste0(
          "<strong>", grouping_scheme, "</strong> (",
          translate("CHF ", "", "", lang), 
          # For smaller amounts, don't round down to zero
          if_else(
            scheme_granted < 50000, 
            print_num(round(scheme_granted / 1000000, digits = 2)), 
            print_num(round(scheme_granted / 1000000, digits = 1))
          ),
          translate(
            " million approved in ", 
            " Mio. CHF zugesprochen in ", 
            " mio CHF approuvés pour ", 
            lang
          ), 
          print_num(n), 
          translate(" grants)", " Beiträgen)", " contributions)", lang),
          collapse = "<br>"
        ),
      .by = FundingInstrumentGaLevel1
    )
  
  # Helper function to reverse the order depending on an argument
  order_data <- function(data, reverse_order = FALSE) {
    if (reverse_order) {
      data |>
        mutate(order = nrow(.) - row_number())
    } else {
      data |>
        mutate(order = row_number())
    }
  }
  
  calc_seg_start <- function(dat, x) {
    dat |>
      filter(order < x) |>
      pull(lvl_1_granted) |>
      sum(na.rm = TRUE)
  }
  
  # Prepare plot data, calculate frequencies of granted amounts per LVL 1 FI
  # (Careers, Programmes...)
  plot_data <-
    funding_data |> 
    summarise(
      lvl_1_granted = sum(AmountGranted, na.rm = TRUE),
      n = n(),
      .by = FundingInstrumentGaLevel1
    ) |>
    # Get translated version of FundingInstrumentGaLevel1
    left_join(mapping_fi, by = "FundingInstrumentGaLevel1") |> 
    # Add scheme-level tooltip text data
    left_join(tooltip_text, by = "FundingInstrumentGaLevel1") |> 
    # Overwrite with the destination language string
    mutate(FundingInstrumentGaLevel1 = destination) |> 
    mutate(lvl_1_freq = lvl_1_granted / sum(lvl_1_granted)) |>
    # Order the data for the plot: by LVL 1 share size
    arrange(desc(lvl_1_freq)) |>
    order_data(reverse_order = reverse_order) |>
    # Now calculate where the segment has to start (and end) per LVL 1 FI
    (\(dat)
     dat |>
     mutate(
       seg_start =
         map_dbl(
           order, \(x) calc_seg_start(dat, x)
         ),
       seg_end = seg_start + lvl_1_granted
     )
    )()
  
  
  # Return the two data objects as list
  return(plot_data)
  
}

prepare_plot_data <- function(dat, lang, reverse_order = FALSE) {
  
  # Without Horizon
  preproc_data <-
    preprocess_plot_data(
      funding_data =
        dat |> 
        filter(!str_detect(FundingInstrumentGaReporting, "Horizon")),
      lang, 
      reverse_order = reverse_order
    ) |>  
    mutate(
      type = 
        translate(
          paste0("Regular SNSF funding instruments (CHF ", round(sum(lvl_1_granted) / 1000000), " million)"),
          paste0("Reguläre Förderinstrumente (", round(sum(lvl_1_granted) / 1000000), " Mio. CHF)"),
          paste0("Instruments d’encouragement réguliers du FNS (", round(sum(lvl_1_granted) / 1000000), " mio CHF)"), 
          lang
        )
    ) |> 
    # Add only Horizon
    bind_rows(
      preprocess_plot_data(
        funding_data =
          dat |> 
          filter(str_detect(FundingInstrumentGaReporting, "Horizon")),
        lang, 
        reverse_order = reverse_order
      ) |>  
        mutate(
          type = 
            translate(
              "Horizon Europe transitional measures*",
              "Übergangsmassnahmen Horizon Europe*",
              "Mesures transitoires Horizon Europe*", 
              lang
            )
        )
    ) |>
    mutate(type = fct(type))
  
  return(preproc_data)
}
