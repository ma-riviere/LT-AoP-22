#======================#
#### Visualizations ####
#======================#

kable_html <- function(data, .type = "html") {
  return(
    knitr::kable(
      data, 
      format = .type,
      col.names = gsub("[.]", " ", names(data)), 
      booktabs = TRUE, 
      align = "c", 
      digits = 3, 
      format.args = list(big.mark = ",", scientific = FALSE), 
      escape = FALSE, 
      linesep = c("")
    ) 
    |> kableExtra::kable_styling(
      font_size = 15, 
      latex_options = c("striped", "scale_down", "hold_position"), 
      bootstrap_options = c("striped", "condensed", "responsive"), 
      position = "center",
      full_width = TRUE
    ) 
    |> kableExtra::row_spec(0, font_size = 18, background = "#d4dbde", bold = T, color = "#2b4894")
  )
}

#-----------#
#### EDA ####
#-----------#

corr_matrix_plot <- function(df, vars) {
  mutate(
    df,
    across(where(is.character), \(.x) factor(.x)),
    across(where(is.factor), \(.x) label_encoding(.x))
  ) |> 
    correlation::correlation(select = vars, include_factors = TRUE, redundant = TRUE, method = "auto") |> 
    rename(R = matches("^r$|^rho$")) |> 
    mutate(across(matches("Parameter[1-2]"), \(c) factor(c, levels = vars))) |> 
    ggplot(aes(x = Parameter1, y = Parameter2)) +
    geom_tile(aes(fill = R), colour = "white", size = 1.2, stat = "identity") + 
    geom_text(aes(label = round(R, 2), colour = abs(R) > 0.5), size = rel(4.5)) +
    scale_color_manual(values = c("black", "white")) +
    scale_fill_gradient2(na.value = "white", breaks = seq(-1, 1, 0.2), limits = c(-1, 1)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    guides(fill = guide_colourbar(title = "R", barheight = rel(17), title.hjust = 0.15), colour = "none") +
    labs(title = "Correlation Matrix") +
    theme(
      plot.title = element_markdown(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(face = "bold", angle = 30, hjust = 0, size = 8),
      axis.text.y = element_text(face = "bold", angle = 45, hjust = 1, size = 8)
    )
}


hist_plot <- function(.df, var, facet = "Condition", facet2 = NULL, resp_name = NULL) {
  
  if(is.null(resp_name)) resp_name <- get_response_name(var)
  
  .title <- glue::glue("Distribution of [{var}]")
  .subtitle <- glue::glue("by [{facet}]")
  if (!is.null(facet2)) .subtitle <- glue::glue("{.subtitle} and [{facet2}]")
  
  .res <- ggplot(.df, aes(x = .data[[var]], fill = .data[[facet]], color = .data[[facet]])) + 
    geom_histogram(aes(y = ..density..), alpha = 0.8, boundary = 1) +
    # geom_density(aes(y = ..density..), color = "grey50", fill = "grey30", alpha = 0.4) +
    theme(
      legend.position = "none", 
      axis.title.y = element_blank(), 
      axis.title.x = element_markdown(face = "bold", hjust = 0.5),
      axis.text.y = element_blank()
    ) +
    labs(
      x = resp_name, 
      y = "Frequency",
      title = .title,
      subtitle = .subtitle
    )
  
  if(is.null(facet2)) {
    .res <- .res + facet_wrap( ~ .data[[facet]], ncol = 3)
  }
  else {
    .res <- .res + facet_grid(rows = vars(.data[[facet2]]), cols = vars(.data[[facet]]))
  }
  
  return(.res)
}


temporal_plot <- function(df, resp, pred, time, cluster = "Mouse", link = "I", y_name = NULL) {
  
  if(is.null(y_name)) y_name <- get_response_name(resp)
  
  p1 <- ggplot(df, aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3, position = position_dodge(0.2)) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 3, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line", size = 1.2) +
    # geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
    labs(y = y_name) +
    theme(legend.position = "none")
  
  p2 <- ggplot(df, aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 3, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line", size = 1.2) +
    geom_line(aes(group = .data[[cluster]]), linetype = "dashed", alpha = 0.3) +
    # geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
    facet_wrap(~ .data[[pred]]) +
    labs(y = y_name) +
    theme(legend.position = "bottom")
  
  p1 / p2
}


zi_temporal_plot <- function(df, resp, pred, time, link = "I", y_name = NULL, zi_y_name = "%") {

  if(is.null(y_name)) y_name <- get_response_name(resp)

  p1 <- df |>
    group_by(across(c(pred, time))) |>
    summarize(Succeeded = sum(.data[[resp]] > 0) * 100 / n()) |>
    ggplot(aes(x = .data[[pred]], color = .data[[pred]], fill = .data[[pred]])) +
    geom_col(aes(y = Succeeded), alpha = 0.8) +
    facet_wrap(~ .data[[time]], ncol = length(unique(df[[time]]))) +
    labs(y = zi_y_name) +
    theme(legend.position = "none")

  p2 <- df |> filter(.data[[resp]] > 0) |>
    ggplot(aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3, position = position_dodge(0.2)) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 2, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line") +
    geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
    labs(y = y_name) +
    theme(legend.position = "none", axis.title.x = element_blank())

  p3 <- df |> filter(.data[[resp]] > 0) |>
    ggplot(aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 2, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line") +
    geom_line(aes(group = Mouse), linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
    facet_wrap(~ .data[[pred]]) +
    labs(y = y_name) +
    theme(legend.position = "bottom")

  p1 / p2 / p3 + plot_layout(heights = c(2, 3, 5))
}


compositional_plot <- function(dat, responses, prefix) {
  
  .pattern <- paste0("^(", paste0(prefix, collapse = "|"), ")_(", ".*)$")
  .pattern_prop <- paste0("^Prop_(", paste0(prefix, collapse = "|"), ")_(", ".*)$")
  
  responses <- str_subset(responses, paste0("^(", paste0(prefix, collapse = "|"), ")", "_[^Tot]"))
  
  dat <- dat |> select(Sample, Mouse, Stage, Condition, any_of(responses))

  .dat_long <- left_join(
    dat |>
      pivot_longer(cols = any_of(responses), names_pattern = .pattern, names_to = c("Type", "Layer"), values_to = "Value") |>
      filter(Layer %ni% c("Tot", "Total")) |> 
      mutate(Type = factor(Type, levels = prefix)),
    dat |>
      group_by(Stage, Sample) |>
      mutate(
        purrr::map_dfc(
          prefix,
          \(pre) across(matches(glue("^{pre}_[^Tot]")), \(.x) .x / sum(c_across(matches(glue("^{pre}_[^Tot]"))), na.rm = TRUE), .names = "Prop_{.col}")
        )
      ) |>
      ungroup() |>
      pivot_longer(cols = matches("Prop_"), names_pattern = .pattern_prop, names_to = c("Type", "Layer"), values_to = "Percentage") |> 
      select(Stage, Sample, Mouse, Condition, Type, Layer, Percentage)
  ) |> 
  group_by(Stage, Layer, Type, Condition) |> 
  summarize(
    Value = mean(Value, na.rm = TRUE),
    Percentage = mean(Percentage, na.rm = TRUE)
  ) |> 
  ungroup()
  
  .plot <- ggplot(.dat_long, aes(x = Condition, y = Value, fill = Layer)) +
    geom_col() +
    geom_text(aes(label = scales::percent(Percentage, accuracy	= 0.1)), position = position_stack(vjust = 0.5), color = "white") +
    labs(x = " ", y = " ")
  
  if (length(unique(.dat_long$Stage)) == 1) .plot <- .plot + facet_wrap(vars(Type), scales = "free_y", ncol = length(unique(.dat_long$Type)))
  if (length(unique(.dat_long$Stage)) > 1) .plot <- .plot + facet_grid(vars(Type), vars(Stage), scales = "free_y")
  
  return(.plot)
}


#-------------------------#
#### Model Diagnostics ####
#-------------------------#

make_acf_plot <- function(mod) {
  forecast::ggAcf(residuals(mod, type = "response", retype = "normalized"), color = "#1b6ca8") + 
    geom_point() + 
    labs(
      title = "Autocorrelation of residuals",
      subtitle = "Data (lines) should be inside the dashed area"
    ) + 
    see::theme_lucid() +
    theme(title = element_text(size = 16))
}


ppc_plots <- function(mod, simulations, term = "Condition", type = "fixed", is_count = NULL, max_cols_per_plot = 3) {
  Y <- insight::get_response(mod)
  n_unique <- n_distinct(insight::get_data(mod)[[term]])
  
  .title <- "Simulation-based Posterior Predictive Checks"
  
  if(is.null(is_count)) is_count <- ifelse(insight::get_family(mod)$family |> str_detect("binom|poiss"), TRUE, FALSE)
  
  # ppc_fun <- ifelse(is_count, bayesplot::ppc_bars, bayesplot::ppc_dens_overlay)
  ppc_fun_grouped <- ifelse(is_count, bayesplot::ppc_bars_grouped, bayesplot::ppc_dens_overlay_grouped)
  ppc_fun_pred_grouped <- bayesplot::ppc_intervals_grouped
  
  if(type %in% c("fixed", "fe")) {
    .term <- insight::get_predictors(mod)[[term]]
    
    # ppc_global <- ppc_fun(y = Y, yrep = simulations) 
    if(is_count) ppc_root <- bayesplot::ppc_rootogram(Y, simulations, style = "suspended")
    
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
  }
  else if(type %in% c("random", "re")) {
    .term <- insight::get_random(mod)[[term]]
    
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
  }
  
  return(
    if(type %in% c("fixed", "fe")) {
      if(is_count) { 
        (ppc_root / ppc_grouped / ppc_pred_grouped) + plot_layout(guides = 'collect', ncol = 1, nrow = 3) +
          plot_annotation(title = .title, subtitle = glue::glue("For [{term}]")) & theme(legend.position = 'right', axis.title.x = element_blank())
      } else {
        (ppc_grouped / (ppc_pred_grouped + theme(axis.title.x = element_blank()))) + plot_layout(ncol = 1, nrow = 2) + 
          plot_annotation(title = .title, subtitle = glue::glue("For [{term}]")) & theme(legend.position = 'right')
      }
    }
    else list(ppc_grouped, ppc_pred_grouped)
  )
}


ppc_stat_plots <- function(mod, simulations, term = "Condition", type = "fixed", stats = c("min", "max", "mean", "sd"), n_cols = 2, max_cols_per_plot = 5) {
  
  n_unique <- n_distinct(insight::get_data(mod)[[term]])
  
  if(type %in% c("fixed", "fe")) .term <- insight::get_predictors(mod)[[term]]
  else if(type %in% c("random", "re")) .term <- insight::get_random(mod)[[term]]
  
  return(
    patchwork::wrap_plots(
      purrr::map(
        stats, 
        \(.x) bayesplot::ppc_stat_grouped(
          insight::get_response(mod), 
          simulations, group = .term, stat = .x,
          facet_args = list(ncol = min(max_cols_per_plot, n_unique))
        ) + scale_x_continuous(labels = \(l) signif(l, digits = 2))
      ), 
      ncol = n_cols, guides = 'auto'
    ) + 
    plot_annotation(title = "Simulation-based Predictive Checks (on statistics)", subtitle = glue::glue("For [{term}]")) & 
      theme(legend.position = 'right', axis.text.x = element_text(size = rel(1.5), angle = 30, hjust = 1))
  )
}

#------------------------#
#### Effects analysis ####
#------------------------#

make_signif_boxplot <- function(
    mod, xaxis = "Condition", facet = NULL, cluster = "Mouse", add_averages_by = "Mouse", 
    scale = "link", adjust = "none", method = "pairwise", display_labels = FALSE, resp_name = NULL, print_eqs = FALSE
  ) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  resp <- insight::find_response(mod)
  if(is.null(resp_name)) resp_name <- get_response_name(resp)
  
  dat <- insight::get_data(mod) |> group_by(across(any_of(c(xaxis, facet)))) |> mutate(N = glue("N = {get_n_units(cur_data())}")) |> ungroup()
  
  max <- max(dat[[resp]])
  min <- min(dat[[resp]])
  amp <- abs(max - min)
  
  if(adjust == "none") correction <- "(uncorrected)"
  else correction <- glue::glue("({adjust} corrected)")
  
  # -----------[ Contrasts ]----------- #
  
  specs <- paste0(" ~ ", xaxis)
  if(!is.null(facet)) specs <- paste0(specs, " | ", facet)
  specs <- as.formula(specs)
  
  emmeans <- emmeans::emmeans(mod, specs = specs, type = "response")
  if (tolower(scale) %in% c("response", "resp")) emmeans <- regrid(emmeans, transform = "response")
  
  contrasts <- emmeans::contrast(emmeans, method = method, adjust = adjust, infer = TRUE) |> 
    as.data.frame() |> 
    rename(Contrast = contrast) |> 
    tidyr::extract(col = Contrast, into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = F)
  
  p_data_contrasts <- contrasts |>
    group_by(across(any_of(c(facet)))) |>
    mutate(
      x1 = match(X1, levels(dat[[xaxis]])),
      x2 = match(X2, levels(dat[[xaxis]])),
      p.signif = glue("{scales::pvalue(p.value)} {gtools::stars.pval(p.value)}")
    ) |>
    arrange(x.diff := abs(x2 - x1)) |>
    mutate(
      step = 1:n(),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    ) |>
    ungroup()
  
  # -----------[ Plot ]----------- #
  
  plot <- (ggplot(dat, aes_string(x = xaxis, y = resp, color = xaxis))
    + geom_boxplot(outlier.alpha = 0, size = 1.1)
    + stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1.1, linetype = "dotted")
    + { if (!is.null(cluster)) geom_jitter(size = 2, width = 0.1, alpha = 0.3)
        else geom_jitter(aes_string(fill = xaxis), shape = 23, color = "black", size = 3, width = 0.1, alpha = 0.8)
    }
    + {if (!is.null(add_averages_by)) stat_summary(
      aes_string(group = add_averages_by, fill = xaxis), geom = "point", fun = mean, 
      size = ifelse(is.null(facet), 4, 3), shape = 23, color = "black", alpha = 0.8, position = position_dodge(0.2)
    )}
    + geom_errorbarh(
      data = p_data_contrasts, aes(xmin = x1, xmax = x2, y = pos.y), inherit.aes = FALSE, 
      color = "black", height = 0.03 * amp, size = 0.5
    )
    + geom_text(
      data = p_data_contrasts, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp)
    )
    + geom_label(aes(y = min - 0.05 * amp, fontface = "bold", label = N, color = .data[[xaxis]]), size = 5, alpha = 0.7)
    + theme(
      legend.position = "none", 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = ggtext::element_markdown()
    )
    + labs(y = resp_name)
    # + {if (!is.null(add_averages_by)) 
    #     labs(caption = glue::glue("Small round points are individual measurements\n Diamonds represent {add_averages_by}-averages"))
    #   }
    + scale_x_discrete(labels = \(l) str_replace(l, "^H", "IH"))
    + {if (!is.null(facet)) facet_wrap( ~ .data[[facet]])}
    + {if (display_labels) plot <- plot + labs(
        title = glue::glue("BoxPlot of [{resp}] by [{xaxis}]"), 
        subtitle = glue::glue("With p-values {correction}")
      )}
  )

  # -----------[ Formatted results ]----------- #
  
  if (print_eqs) {
    contrasts_eqs <- contrasts |> rowwise() |> mutate(
      contrast_name = cur_data() |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = cur_data() |> colnames() |> str_subset("^(z|t|F)"),
      Equation = glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(lower.CL, 3)>>, <<round(upper.CL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(Contrast, any_of(facet), Equation)
    
    print(contrasts_eqs)
  }
  
  return(plot)
}


make_signif_boxplot_inter <- function(
    mod, xaxis = "Condition", facet, cluster = "Mouse", add_averages_by = "Mouse", 
    scale = "link", adjust = "none", display_labels = FALSE, resp_name = NULL, print_eqs = FALSE
) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  resp <- insight::find_response(mod)
  if(is.null(resp_name)) resp_name <- get_response_name(resp)
  
  dat <- insight::get_data(mod) |> group_by(across(any_of(c(xaxis, facet)))) |> mutate(N = glue("N = {get_n_units(cur_data())}")) |> ungroup()
  
  max <- max(dat[[resp]])
  min <- min(dat[[resp]])
  amp <- abs(max - min)
  
  if(adjust == "none") correction <- "(uncorrected)"
  else correction <- glue::glue("({adjust} corrected)")
  
  # -----------[ Contrasts ]----------- #
  # WARN: Make sure 'facet' and 'xaxis' are factors
  
  specs <- paste0(" ~ ", xaxis)
  if(!is.null(facet)) specs <- paste0(specs, " | ", facet)
  specs <- as.formula(specs)
  
  emmeans <- emmeans::emmeans(mod, specs = specs, type = "response")
  if (tolower(scale) %in% c("response", "resp")) emmeans <- regrid(emmeans, transform = "response")
  
  contrasts <- emmeans::contrast(emmeans, method = "pairwise", adjust = adjust, infer = TRUE) |> 
    as.data.frame() |> 
    rename(Contrast = contrast) |> 
    tidyr::extract(col = Contrast, into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = F)
  
  p_data_contrasts <- contrasts |>
    group_by(across(any_of(c(facet)))) |>
    mutate(
      x1 = (match(.data[[facet]], levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X1, levels(dat[[xaxis]])),
      x2 = (match(.data[[facet]], levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X2, levels(dat[[xaxis]])),
      p.signif = glue("{scales::pvalue(p.value)} {gtools::stars.pval(p.value)}")
    ) |>
    arrange(x.diff := abs(x2 - x1)) |>
    mutate(
      step = 1:n(),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    ) |>
    ungroup()
  
  contrasts_interactions <- emmeans::contrast(emmeans, interaction = c("pairwise"), by = NULL, adjust = "none", infer = TRUE) |> 
    as.data.frame() |> 
    tidyr::extract(col = paste0(xaxis, "_pairwise"), into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = F) |> 
    tidyr::extract(col = paste0(facet, "_pairwise"), into = c("F1", "F2"), regex = "(.*) [- | /] (.*)", remove = F)
  
  p_data_interactions <- contrasts_interactions |>
    mutate(
      x1 = 0.5 * ((match(F1, levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X1, levels(dat[[xaxis]])) +
                    (match(F1, levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X2, levels(dat[[xaxis]]))),
      x2 = 0.5 * ((match(F2, levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X1, levels(dat[[xaxis]])) +
                    (match(F2, levels(dat[[facet]])) - 1) * length(unique(dat[[xaxis]])) + match(X2, levels(dat[[xaxis]]))),
      p.signif = glue("{scales::pvalue(p.value)} {gtools::stars.pval(p.value)}")
    ) |>
    arrange(x.diff := abs(x2 - x1)) |>
    mutate(
      step = 1:n() + choose(length(unique(dat[[xaxis]])), 2),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    )
  
  # -----------[ Plot ]----------- #
  
  plot <- (ggplot(dat, aes(x = interaction(.data[[xaxis]], .data[[facet]], sep = "_"), y = .data[[resp]], color = .data[[xaxis]]))
    + geom_boxplot(outlier.alpha = 0, size = 1.1)
    + stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1.1, linetype = "dotted")
    + { if (!is.null(cluster)) geom_jitter(size = 2, width = 0.1, alpha = 0.3)
      else geom_jitter(aes_string(fill = xaxis), shape = 23, color = "black", size = 3, width = 0.1, alpha = 0.8)
    }
    + {if (!is.null(add_averages_by)) stat_summary(
      aes_string(group = add_averages_by, fill = xaxis), geom = "point", fun = mean, 
      size = 3, shape = 23, color = "black", alpha = 0.8, position = position_dodge(0.2)
    )}
    + geom_errorbarh(
      data = p_data_contrasts, aes(xmin = paste(X1, .data[[facet]], sep = "_"), xmax = paste(X2, .data[[facet]], sep = "_"), y = pos.y), inherit.aes = FALSE,
      color = "black", height = 0.02 * amp, size = 0.5
    )
    + geom_text(
      data = p_data_contrasts, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp)
    )
    + geom_label(aes(y = min - 0.05 * amp, fontface = "bold", label = N, color = .data[[xaxis]]), size = 5, alpha = 0.7)
    ## Interactions
    + geom_errorbarh(
      data = p_data_interactions, aes(xmin = x1, xmax = x2, y = pos.y), inherit.aes = FALSE,
      color = "black", height = 0.02 * amp, size = 0.5
    )
    + geom_text(
      data = p_data_interactions, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp)
    )
    + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.title.y = ggtext::element_markdown()
    )
    + labs(y = resp_name)
    + scale_x_discrete(labels = \(l) str_replace(l, "_", "\n") |> str_replace("^H", "IH"))
    + {if (display_labels) plot <- plot + labs(
        title = glue::glue("BoxPlot of [{resp}] by [{xaxis}]"),
        subtitle = glue::glue("With p-values {correction}")
      )}
  )
  
  # -----------[ Formatted results ]----------- #
  
  if (print_eqs) {
    contrasts_eqs <- contrasts |> rowwise() |> mutate(
      contrast_name = cur_data() |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = cur_data() |> colnames() |> str_subset("^(z|t)"),
      Equation = glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(lower.CL, 3)>>, <<round(upper.CL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(Contrast, any_of(facet), Equation)
    
    print(contrasts_eqs)
    
    contrasts_interactions_eqs <- contrasts_interactions |> rowwise() |> mutate(
      contrast_name = cur_data() |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = cur_data() |> colnames() |> str_subset("^(z|t)"),
      Equation = glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(lower.CL, 3)>>, <<round(upper.CL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(matches("_pairwise"), Equation)
    
    print(contrasts_interactions_eqs)
  }
  
  return(plot)
}


modeled_temporal_plot <- function(
    mod, treatment = "Condition", time = "Stage",  
    xlims = NULL, show_observed = FALSE, resp_name = NULL, zero_hline = FALSE
) {
  
  resp <- insight::find_response(mod)
  data <- insight::get_data(mod)
  
  emmeans_formula <- paste0("~ ", paste(c(time, treatment), collapse = " | ")) |> as.formula()
  
  emmeans <- emmeans::emmeans(mod, specs = emmeans_formula, type = "response") |> as.data.frame() |> 
    select({{ time }}, {{ treatment }}, Modeled = matches("response|emmean"), matches("CL$"))
  
  if (show_observed) {
    emmeans <- inner_join(
      emmeans,
      distribution_summary(data, dvs = resp, between = c(treatment, time)) |> select({{ time }}, {{ treatment }}, Observed = Mean)
    ) |> select({{ time }}, {{ treatment }}, Modeled, everything())
  }
  
  contrasts_formula <- paste0("~ ", paste(c(treatment, time), collapse = " | ")) |> as.formula()
  
  contrasts <- emmeans::emmeans(object = mod, specs = contrasts_formula, type = "response") |> 
    emmeans::contrast(method = "pairwise", adjust = "none", infer = TRUE) |> 
    as.data.frame() |> 
    select(contrast, {{ time }}, matches("estimate|risk|odds|^ratio|^difference"), matches("CL$"), p.value)
  
  LCL_name <- purrr::keep(colnames(emmeans), grepl(pattern = "LCL|lower", x = colnames(emmeans)))
  UCL_name <- purrr::keep(colnames(emmeans), grepl(pattern = "UCL|upper", x = colnames(emmeans)))
  
  p_data <- (left_join(
    emmeans |> select({{ time }}, {{ treatment }}, any_of(c("Observed")), Modeled, matches("CL$")),
    contrasts |> select({{ time }}, contrast, p.value),
    by = time
  ) 
  |> group_by(.data[[time]]) 
  |> summarize(
    pos.y = 1.1 * max(.data[[UCL_name]]),
    p.value = mean(p.value)
  ) 
  |> ungroup() 
  |> mutate(p.signif = ifelse(p.value > alpha, NA_character_, glue("{scales::pvalue(p.value)}  \n {gtools::stars.pval(p.value)}")))
  )
  
  plot <- (ggplot(emmeans, aes_string(x = time, y = "Modeled", color = treatment))
    + geom_jitter(data = data, aes_string(y = resp), alpha = 0.4, size = 2, width = 0.1, height = 0) # Data points
    + geom_point(size = 3, position = position_dodge(0.15)) # Modeled emmeans
    + geom_errorbar(aes_string(ymin = LCL_name, ymax = UCL_name), width = 0.25, size = 1.1, position = position_dodge(0.15))
    + geom_line(aes_string(group = treatment), position = position_dodge(0.15))
    + geom_text(
     data = p_data,
     aes_string(label = "p.signif", x = time, y = "pos.y"),
     vjust = 0.5, hjust = 0.5,
     size = 5, color = "black"
    )
    + labs(y = resp_name %||% get_response_name(resp))
    + theme(
     legend.position = "right",
     legend.key.size = unit(1.5, 'cm'), 
     legend.title = element_text(size = 14),
    )
  )
  
  if (!is.null(xlims)) plot <- plot + scale_x_discrete(limits = xlims)
  if (show_observed) plot <- plot + geom_point(aes_string(y = "Observed", fill = treatment), stroke = 1.05, size = 2, shape = 23, color = "black", position = position_dodge(0.20))
  if (zero_hline) plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4")
  
  return(plot)
}