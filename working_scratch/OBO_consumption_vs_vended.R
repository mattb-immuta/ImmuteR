# Scratch: needs plot_df, x_ticks, y_ticks, ratio_max, pin_band, pin_mid,
# ratio_minimal, ratio_moderate and hline() defined first. Not sourced by load_all().
library(plotly)

# ── Plot ─────────────────────────────────────────────────────────────────────
p <- plot_ly(
  plot_df,
  x = ~totalVendedRoles, y = ~y_plot,
  type = "scatter", mode = "markers+text",
  color = ~colour,
  colors = c(green = "#22C55E", amber = "#F59E0B", red = "#EF4444", black = "#111111"),
  size = ~arr, sizes = c(8, 60),
  marker = list(opacity = 0.75, line = list(width = 1, color = "#FFFFFF")),
  text         = ~`Account Name`,          # on-chart label
  textposition = "top center",
  textfont     = list(size = 10, color = "#374151"),
  hovertext    = ~tip,                     # tooltip
  hoverinfo    = "text"
) |>
  layout(
    title = "Consumption vs Agentic Vended Roles",
    xaxis = list(
      title     = "Total vended roles (all instances, log scale)",
      type      = "log",
      tickvals  = x_ticks,
      ticktext  = scales::label_number(scale_cut = scales::cut_short_scale())(x_ticks),
      showgrid  = TRUE,
      gridcolor = "#EEEEEE"
    ),
    yaxis = list(
      title    = "Consumption ratio (registered \u00F7 licensed users)",
      range    = c(0, ratio_max + pin_band),
      tickvals = c(y_ticks, pin_mid),
      ticktext = c(sprintf("%.2f", y_ticks), paste0(">", ratio_max)),
      zeroline = FALSE
    ),
    shapes = list(
      hline(ratio_minimal),
      hline(ratio_moderate),
      hline(ratio_max, dash = "solid"),
      list(type = "rect", xref = "paper", x0 = 0, x1 = 1,
           y0 = ratio_max, y1 = ratio_max + pin_band,
           fillcolor = "#F3F4F6", line = list(width = 0), layer = "below")
    ),
    legend = list(title = list(text = "Adoption rating"))
  )

p


plotly::save_image(p, "output/consumption_vs_vended_roles.png", width = 1400, height = 900, scale = 2)
