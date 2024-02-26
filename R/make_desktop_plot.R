# Function to draw the bar chart for desktops
draw_funding_portfolio_5 <- function(lang = "en") {
  
  ## Prepare the data for the plot
  preproc_data <-
    prepare_plot_data(funding_in_2023, lang, reverse_order = FALSE)
  
  # Get manual color palette based on lang-specific values
  color_palette_lvl1 <- get_color_palette_lvl_1(lang)
  
  # Create the plot
  p_funding_portfolio_5 <-
    preproc_data |>
    filter(!str_detect(type, "Horizon")) |>
    ggplot() +
    aes(
      x = 1,
      y = lvl_1_granted,
      fill = reorder(FundingInstrumentGaLevel1, -order), 
      tooltip = tooltip
    ) +
    geom_col_interactive(color = "white", width = 0.7) +
    # Level 1 FI labels
    # Bold text (FI and percentage)
    geom_text(
      aes(
        x = if_else(lvl_1_granted < 35 * 10^6, 1.6, 2), 
        y =
          if_else(
            lvl_1_granted < 35 * 10^6,
            seg_start + (seg_end - seg_start) / 2,
            seg_start
          ),
        label = FundingInstrumentGaLevel1, 
        angle = if_else(lvl_1_granted < 35 * 10^6, 90, 0),
        hjust = 0,
        vjust = if_else(lvl_1_granted < 35 * 10^6, 0.5, 1)
      ),
      color = "#4d4d4d", family = "Theinhardt",
      size = 3,
      fontface = "bold"
    ) +
    # Plain text (amount)
    geom_text(
      aes(
        x = if_else(lvl_1_granted < 35 * 10^6, 1.375, 1.7),
        y = if_else(lvl_1_granted < 35 * 10^6, seg_start + 0.018, seg_start),
        label = 
          if_else(
            # Don't show amount for <30 mio, not enough space
            lvl_1_granted < 35 * 10^6, 
            "", 
            paste0(
              translate("CHF ", "", "", lang),
              round((lvl_1_granted / 1000000)),
              translate(" million", " Mio. CHF", " mio CHF", lang)
            )
          ),
        angle = if_else(lvl_1_granted < 35 * 10^6, 90, 0)),
      vjust = 1,
      hjust = 0,
      color = "#4d4d4d",
      family = "Theinhardt",
      size = 2.5,
      fontface = "plain"
      ) +
    coord_flip() +
    get_datastory_theme(remove_plot_margin = TRUE, family = "Theinhardt") + 
    scale_fill_manual(
      values =
        setNames(
          color_palette_lvl1$color_lvl1, 
          color_palette_lvl1$FundingInstrumentGaLevel1
        ), 
      guide = "none"
    ) +
    scale_color_manual(
      guide = "none",
      values = get_datastory_scheme()
    ) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    expand_limits(x = 3.5) +
    facet_wrap(~type, ncol = 1) + 
    theme(strip.text = element_text(hjust = 0, size = 9))
  
  p_funding_portfolio_5
  
}

# Function to draw the bar chart for desktops
draw_funding_portfolio_5_horizon <- function(lang = "en") {
  
  ## Prepare the data for the plot
  preproc_data <-
    prepare_plot_data(funding_in_2023, lang, reverse_order = FALSE)

  # Get manual color palette based on lang-specific values
  color_palette_lvl1 <- get_color_palette_lvl_1(lang)
  
  # Create the plot
  p_funding_portfolio_5_horizon <-
    preproc_data |>
    filter(str_detect(type, "Horizon")) |>
    ggplot() +
    aes(
      x = 0,
      y = lvl_1_granted,
      fill = reorder(FundingInstrumentGaLevel1, -order), 
      tooltip = tooltip
    ) +
    geom_col_interactive(color = "white", width = 0.87) +
    # Level 1 FI labels
    # Bold text (FI and percentage)
    geom_text(
      aes(
        x = 0, 
        y = lvl_1_granted,
        label =
          paste0(
            translate("CHF ", "", "", lang),
            round(lvl_1_granted / 1000000),
            translate(" million", " Mio. CHF", " mio CHF", lang)
          )
      ),
      vjust = 0.5,
      hjust = -0.125,
      color = "#4d4d4d", family = "Theinhardt",
      size = 2.75,
      fontface = "plain"
    ) +
    coord_flip() +
    get_datastory_theme(remove_plot_margin = TRUE) + 
    scale_fill_manual(
      values =
        setNames(
          color_palette_lvl1$color_lvl1, 
          color_palette_lvl1$FundingInstrumentGaLevel1
        ), 
      guide = "none") +
    scale_color_manual(
      guide = "none",
      values = get_datastory_scheme()
    ) +
    labs(
      x = NULL,
      y = NULL,
      caption =
        translate(
          "*Financed with additional funds from the federal government and funds from the career funding budget (Eccellenza scheme).",
          "*Finanziert durch zusätzliche Mittel des Bundes und durch Mittel aus dem Budget der Karriereförderung (Instrument Eccellenza).", 
          "*Financé par des fonds supplémentaires de la Confédération et par des fonds du budget d’encouragement de carrière (instrument Eccellenza).", 
          lang
        )
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.x = element_blank(),
    ) +
    expand_limits(y = sum(preproc_data$lvl_1_granted[1:5]), x = 1.3) +
    facet_wrap(~type, ncol = 1) +
    theme(
      strip.text = element_text(hjust = 0, size = 9),
      plot.caption = element_text(hjust = 0.04, face = "plain", size = 7)
    )
  
  p_funding_portfolio_5_horizon
  
}