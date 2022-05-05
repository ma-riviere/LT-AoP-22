#=============#
#### Theme ####
#=============#

log.main("[CONFIG] Setting project's graphics theme ...")

#--------------#
#### Colors ####
#--------------#

color_bg_light <- "white"
color_main_light <- "#0d6efd"
color_bg_dark <- "#222"
color_main_dark <- "#20c997"

color_text_bi <- "#a8aeb4"

colors_cond_sarah <- c("#777777", "#ff6000")

my_palettes_d <- list(colors_cond_sarah, scales::viridis_pal()(3), scales::viridis_pal()(5), scales::viridis_pal()(10), scales::viridis_pal()(15))

options(
  ggplot2.discrete.colour = my_palettes_d,
  ggplot2.discrete.fill = my_palettes_d,
  ggplot2.continuous.colour = \() scale_color_viridis_c(),
  ggplot2.continuous.fill = \() scale_fill_viridis_c(),
  ggplot2.binned.colour = \() scale_color_viridis_b(),
  ggplot2.binned.fill = \() scale_fill_viridis_b()
)

#-------------#
#### Plots ####
#-------------#

invis_custom <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    ## Legend
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

theme_light_mar <- invis_custom + 
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
    ## Titles
    plot.title = ggtext::element_markdown(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 15, face = "italic"),
    ## Legend
    legend.title = ggplot2::element_text(face = "bold"),
    ## Facets
    strip.background = ggplot2::element_rect(fill = "#adb5bd"),
    strip.text = ggplot2::element_text(size = 16, face = "bold"),
    ## Axes
    axis.title.x = ggtext::element_markdown(size = 16, face = "bold", hjust = 0.5),
    axis.title.y = ggtext::element_markdown(size = 16, face = "bold", hjust = 0.5),
    axis.text = ggplot2::element_text(color = "black"),
    axis.text.x = ggplot2::element_text(size = 13),
    axis.text.y = ggplot2::element_text(size = 13),
    text = ggplot2::element_text(color = "black", family = "")
  )

theme_dark_mar <- ggplot2::theme(
    panel.border = ggplot2::element_rect(colour = "white"),
    ## Titles
    plot.title = ggtext::element_markdown(colour = "white"),
    plot.subtitle = ggtext::element_markdown(colour = "white"),
    ## Legend
    legend.title = ggplot2::element_text(colour = "white"),
    ## Facets
    strip.text = ggplot2::element_text(colour = "white"),
    ## Axes
    axis.title.x = ggtext::element_markdown(colour = "white"),
    axis.title.y = ggtext::element_markdown(colour = "white"),
    axis.text = ggplot2::element_text(color = "white"),
    text = ggplot2::element_text(color = "white")
  )

ggplot2::theme_set(theme_light_mar)

#--------------#
#### Tables ####
#--------------#

gt_style_light <- function(gt_tbl) {
  return(
    gt_tbl 
    |> format_gt()
    |> gt::tab_style(
      style = list(
        cell_fill(color = "white", alpha = 1),
        cell_text(color = color_main_light, weight = "bold"),
        cell_borders(sides = c("top", "bottom"), color = color_main_light, style = "solid", weight = px(2))
      ),
      locations = list(cells_title(), cells_column_labels())
    ) 
    |> gt::tab_style(
      style = list(
        cell_fill(color = "white", alpha = 1),
        cell_text(color = "black")
      ),
      locations = list(cells_stub(), cells_body(), cells_row_groups(), cells_footnotes(), cells_source_notes())
    )
    |> gt::tab_options(container.width = pct(100), table.width = pct(100))
  )
}

gt_style_dark <- function(gt_tbl) {
  return(
    gt_tbl 
    |> format_gt()
    |> gt::tab_style(
      style = list(
        cell_fill(color = color_bg_dark, alpha = 1),
        cell_text(color = color_main_dark, weight = "bold"),
        cell_borders(sides = c("top", "bottom"), color = color_main_dark, style = "solid", weight = px(2))
      ),
      locations = list(cells_title(), cells_column_labels())
    ) 
    |> gt::tab_style(
      style = list(
        cell_fill(color = color_bg_dark, alpha = 1),
        cell_text(color = "white")
      ),
      locations = list(cells_stub(), cells_body(), cells_row_groups(), cells_footnotes(), cells_source_notes())
    )
    |> gt::tab_options(container.width = pct(100), table.width = pct(100))
  )
}

#--------------------------#
#### Knitr custom plots ####
#--------------------------#

## Inspired by: https://debruine.github.io/quarto_demo/dark_mode.html
knit_print.ggplot <- function(x, options, ...) {
  if(any(stringr::str_detect(class(x), "patchwork"))) {
    plot_dark <- x & theme_dark_mar
  } else {
    plot_dark <- x + theme_dark_mar
  }

  cat('\n<div class="light-mode">\n')
  print(x)
  cat('</div>\n')
  cat('<div class="dark-mode">\n')
  print(plot_dark)
  cat('</div>\n\n')
}
registerS3method("knit_print", "ggplot", knit_print.ggplot)

knit_print.gt_tbl <- function(x, options, ...) {
  cat('\n<div class="light-mode">\n')
  print(gt_style_light(x))
  cat('</div>\n')
  cat('<div class="dark-mode">\n')
  print(gt_style_dark(x))
  cat('</div>\n\n')
}
registerS3method("knit_print", "gt_tbl", knit_print.gt_tbl)