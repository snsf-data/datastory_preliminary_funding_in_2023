# Function to draw the bar chart for mobile devices
draw_funding_portfolio_5_mob <- function(lang = "en") {
  
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
    geom_col_interactive(color = "white", width = 0.3) +
    # Level 1 FI labels
    # Bold text (FI and percentage)
    geom_text(
      aes(
        x = 1.5 * 0.8,
        y =
          if_else(
            lvl_1_granted < 35 * 10^6,
            seg_start + (seg_end - seg_start) / 2,
            seg_start
          ),
        label = if_else(
          str_detect(FundingInstrumentGaLevel1, "Horizon"), 
          str_wrap(FundingInstrumentGaLevel1, 25), 
          FundingInstrumentGaLevel1
        ), 
        hjust = 0,
        vjust = if_else(lvl_1_granted < 35 * 10^6, 0.5, 1)
      ),
      lineheight = 0.75, 
      color = "#4d4d4d", family = "Theinhardt",
      size = 4,
      fontface = "bold"
    ) +
    # Plain text (amount)
    geom_text(
      aes(x = 1.5 * 0.8,
          y =
            if_else(
              str_detect(FundingInstrumentGaLevel1, "Horizon"), 
              seg_start + 70 * 10^6, 
              seg_start + 35 * 10^6
            ),
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
            )
      ), 
      vjust = 1,
      hjust = 0,
      color = "#4d4d4d", family = "Theinhardt",
      size = 4,
      fontface = "plain"
    ) +
    get_datastory_theme() + 
    scale_fill_manual(
      values =
        setNames(
          color_palette_lvl1$color_lvl1,
          color_palette_lvl1$FundingInstrumentGaLevel1
        ),
      guide = "none") +
    scale_y_reverse(
      labels = function(x) { paste0(x / 10^6) },
      breaks = seq(0, 10^9, 2 * 10^8)
    ) +
    scale_color_manual(
      guide = "none",
      values =
        setNames(
          color_palette_lvl1$color_lvl1,
          color_palette_lvl1$FundingInstrumentGaLevel1
        )
    ) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    expand_limits(x = 2) + 
    facet_wrap(~type, ncol = 1) + 
    theme(
      strip.text = element_text(hjust = 0, size = 12),
      axis.text.y = element_blank()
    )
  
  p_funding_portfolio_5_horizon <-
    preproc_data |>
    filter(str_detect(type, "Horizon")) |>
    ggplot() +
    aes(
      x = 1,
      y = lvl_1_granted,
      fill = reorder(FundingInstrumentGaLevel1, -order), 
      tooltip = tooltip
    ) +
    geom_col_interactive(color = "white", width = 0.3) +
    # Level 1 FI labels
    # Bold text (FI and percentage)
    geom_text(
      aes(x = 1.5 *  0.8,
          y = seg_end,
          label =
            if_else(
              str_detect(type, "Horizon"), 
              str_wrap(type, 25), 
              type
            ), 
          hjust = 0
      ),
      vjust = 1,
      lineheight = 0.75, 
      color = "#4d4d4d",
      family = "Theinhardt",
      size = 4,
      fontface = "bold"
    ) +
    # Plain text (amount)
    geom_text(
      aes(x = 1.5 * 0.8,
          y = if_else(
            str_detect(type, "Horizon"), 
            seg_end - 70 * 10^6, 
            seg_end - 35 * 10^6
          ),
          label = 
            if_else(
              # Don't show amount for <30 mio, not enough space
              lvl_1_granted < 35 * 10^6, 
              "", 
              paste0(translate("CHF ", "", "", lang),
                     round((lvl_1_granted / 1000000)),
                     translate(" million", " Mio. CHF", " mio CHF", lang)
              )
            )
      ), 
      vjust = 1,
      hjust = 0,
      color = "#4d4d4d",
      family = "Theinhardt",
      size = 4,
      fontface = "plain"
    ) +
    get_datastory_theme() + 
    scale_fill_manual(
      values =
        setNames(
          color_palette_lvl1$color_lvl1,
          color_palette_lvl1$FundingInstrumentGaLevel1
        ),
      guide = "none") +
    scale_y_continuous(
      labels = function(x) { paste0(x / 10^6) },
      breaks = seq(0, 10^9, 2 * 10^8)
    ) +
    scale_color_manual(
      guide = "none",
      values =
        setNames(
          color_palette_lvl1$color_lvl1,
          color_palette_lvl1$FundingInstrumentGaLevel1
        )
    ) +
    labs(
      x = NULL,
      y = NULL,
      caption =
        str_wrap(
          translate(
            "*Financed with additional funds from the federal government and funds from the career funding budget (Eccellenza scheme).",
            "*Finanziert durch zusätzliche Mittel des Bundes und durch Mittel aus dem Budget der Karriereförderung (Instrument Eccellenza).", 
            "*Financé par des fonds supplémentaires de la Confédération et par des fonds du budget d’encouragement de carrière (instrument Eccellenza).",
            lang
          ),
          width = 75
        )
    ) +
    theme(
      axis.text.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    expand_limits(x = 2) + 
    facet_wrap(~type, ncol = 1) + 
    theme(
      strip.text = element_text(hjust = 0, size = 12), 
      axis.text.y = element_blank(),
      plot.caption = element_text(hjust = 0, face = "plain", size = 11)
    )
  
  p_funding_portfolio_5  + p_funding_portfolio_5_horizon +
    plot_layout(ncol = 1, heights = c(0.75, 0.185))
}