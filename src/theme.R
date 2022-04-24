#======================#
#### Theme ####
#======================#

log.main("[CONFIG] Setting project's graphics theme ...")

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

custom_light <- invis_custom + 
  ggplot2::theme(
    panel.border = ggplot2:: element_rect(fill = NA, colour = "black"),
    ## Titles
    plot.title = ggtext::element_markdown(size = 12, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 11, face = "italic"),
    ## Legend
    legend.title = ggplot2::element_text(face = "bold"),
    ## Facets
    strip.background = ggplot2::element_rect(fill = "#C6C6C6"),
    strip.text = ggplot2::element_text(size = 10, face = "bold"),
    ## Axes
    axis.title.x = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5),
    axis.title.y = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5),
    axis.text = ggplot2::element_text(color = "black"),
    axis.text.x = ggplot2::element_text(size = 18, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 15, face = "bold"),
    text = ggplot2::element_text(color = "black", family = "")
  )

ggplot2::theme_set(custom_light)


#--------------#
#### Colors ####
#--------------#

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