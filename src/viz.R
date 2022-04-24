#======================#
#### Visualizations ####
#======================#

#-----------#
#### EDA ####
#-----------#

get_response_name <- function(var) {
  if(exists(paste0(var, "_name"))) return(eval(parse(text = get_var_name(!!paste0(var, "_name")))))
  else return(var)
}

get_model_tag <- function(mod) {
  resp <- insight::find_response(mod)
  family <- insight::get_family(mod)$family
  link <- insight::get_family(mod)$link
  
  return(glue::glue("{resp} - {family}({link})"))
}


corr_matrix_plot <- function(df, vars) {
  mutate(
    df,
    across(where(is.character), \(.x) factor(.x)),
    across(where(is.factor), \(.x) label_encoding(.x))
  ) |> 
    correlation::correlation(select = vars, include_factors = TRUE, redundant = TRUE, method = "auto") |> 
    rename(R = matches("^r$|^rho$")) |> 
    ggplot(aes(x = Parameter1, y = Parameter2)) +
    geom_tile(aes(fill = R), colour = "white", size = 1.2, stat = "identity") + 
    geom_text(aes(label = R |> round(2)), color = "black", size = rel(4)) +
    scale_fill_gradient2(na.value = "white", breaks = seq(-1, 1, 0.2), limits = c(-1, 1)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    guides(fill = guide_colourbar(title = "R", barheight = rel(17), title.hjust = 0.15)) +
    labs(title = "Correlation Matrix") +
    theme(
      plot.title = element_markdown(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(face = "bold", angle = 30, hjust = 0, size = 8),
      axis.text.y = element_text(face = "bold", angle = 45, hjust = 1, size = 8)
    )
}


hist_plot <- function(.df, var, facet = "Condition", resp_name = NULL) {
  
  if(is.null(resp_name)) resp_name <- get_response_name(var)
  
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
      title = glue::glue("Distribution of [{var}] by [{facet}]"),
      subtitle = "With overlayed density"
    ) +
    facet_wrap( ~ .data[[facet]], ncol = 3)
  
  return(.res)
}


temporal_dist_plot <- function(df, yaxis, group = "Condition", time = "Stage") {
  ggplot(df, aes(x = .data[[yaxis]], fill = .data[[group]], color = .data[[group]])) + 
    geom_histogram(aes(y = ..density..), alpha = 0.8, boundary = 1) +
    # geom_density(aes(y = ..density..), color = "grey50", fill = "grey30", alpha = 0.4) +
    theme(
      legend.position = "none", 
      axis.title.y = element_blank(), 
      axis.title.x = element_markdown(face = "bold", hjust = 0.5),
      axis.text.y = element_blank()
    ) +
    labs(
      x = get_response_name(yaxis), 
      y = "Frequency",
      title = glue("Distribution of [{yaxis}] by [Condition]")
      # subtitle = "With overlayed density"
    ) +
    facet_grid(rows = vars(.data[[group]]), cols = vars(.data[[time]]))
}


temporal_plot <- function(df, resp, pred, time, cluster = "Mouse", link = "I", y_name = NULL) {
  
  if(is.null(y_name)) y_name <- get_response_name(resp)
  
  p1 <- ggplot(df, aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3, position = position_dodge(0.2)) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 2, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line") +
    geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
    labs(y = y_name) +
    theme(legend.position = "none")
  
  p2 <- ggplot(df, aes(x = .data[[time]], y = .data[[resp]], group = .data[[pred]], color = .data[[pred]])) +
    geom_point(alpha = 0.3) +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "point", size = 2, shape = "triangle") +
    stat_summary(fun = \(y) match.fun(link)(mean(y)), geom = "line") +
    geom_line(aes(group = .data[[cluster]]), linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 0, color = "firebrick4", linetype = "dashed") +
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

make_acf_plot <- function(mod, title = "Autocorrelation of residuals") {
  forecast::ggAcf(residuals(mod, type = "response", retype = "normalized"), color = "blue") + 
    geom_point() + 
    labs(title = title) + 
    theme(plot.title = element_markdown(size = 20))
}


ppc_plots <- function(mod, simulations, term = "Condition", type = "fixed", is_count = NULL) {
  Y <- insight::get_response(mod)
  
  if(is.null(is_count)) is_count <- ifelse(insight::get_family(mod)$family |> str_detect("binom|poiss"), TRUE, FALSE)
  
  ppc_fun <- ifelse(is_count, bayesplot::ppc_bars, bayesplot::ppc_dens_overlay)
  ppc_fun_grouped <- ifelse(is_count, bayesplot::ppc_bars_grouped, bayesplot::ppc_dens_overlay_grouped)
  ppc_fun_pred_grouped <- bayesplot::ppc_intervals_grouped
  
  if(type %in% c("fixed", "fe")) {
    .term <- insight::get_predictors(mod) |> pull(.data[[term]])
    
    ppc_global <- ppc_fun(y = Y, yrep = simulations)
    if(is_count) ppc_root <- bayesplot::ppc_rootogram(Y, simulations, style = "suspended")
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term)
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95)
  }
  else if(type %in% c("random", "re")) {
    .term <- insight::get_random(mod) |> pull(.data[[term]])
    
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term)
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95)
  }
  
  return(
    if(type %in% c("fixed", "fe")) {
      if(is_count) ((ppc_global + ppc_root) / (ppc_grouped + ppc_pred_grouped)) + 
        plot_layout(guides = 'collect') & theme(legend.position = 'bottom', axis.title.x = element_blank())
      else (ppc_global + ppc_grouped + plot_layout(guides = 'collect')) / ppc_pred_grouped + 
        plot_layout(guides = 'keep') & theme(legend.position = 'bottom', axis.title.x = element_blank())
    }
    else list(ppc_grouped, ppc_pred_grouped)
  )
}


ppc_stat_plots <- function(mod, simulations, term = "Condition", type = "fixed", stats = c("min", "max", "mean", "sd")) {

  if(type %in% c("fixed", "fe")) .term <- insight::get_predictors(mod) |> pull(.data[[term]])
  else if(type %in% c("random", "re")) .term <- insight::get_random(mod) |> pull(.data[[term]])
  
  return(
    patchwork::wrap_plots(
      purrr::map(stats, \(.x) bayesplot::ppc_stat_grouped(insight::get_response(mod), simulations, group = .term, stat = .x)), 
      nrow = ceiling(length(stats) / 2), ncol = 2, 
      guides = 'auto'
    ) & theme(legend.position = 'bottom')
  )
}


#------------------------#
#### Effects analysis ####
#------------------------#

make_signif_boxplot <- function(
    mod, xaxis = "Condition", facet = NULL, cluster = "Mouse", agg_by = NULL, 
    scale = "link", adjust = "none", display_labels = FALSE, resp_name = NULL, print_eqs = FALSE
  ) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  resp <- insight::find_response(mod)
  if(is.null(resp_name)) resp_name <- get_response_name(resp)
  
  dat <- insight::get_data(mod)
  if(!is.null(agg_by)) dat <- dat |> group_by(across(c(xaxis, agg_by))) |> summarize({{ resp }} := mean(.data[[resp]])) |> ungroup()
  dat <- dat |> group_by(across(c(xaxis, facet))) |> mutate(N = glue("N = {get_n_units(cur_data())}")) |> ungroup()
  
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
  
  contrasts <- emmeans::contrast(emmeans, method = "pairwise", adjust = adjust, infer = TRUE) |> 
    as.data.frame() |> 
    rename(Contrast = contrast) |> 
    tidyr::extract(col = Contrast, into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = F)
  
  p_data_contrasts <- contrasts |>
    group_by(across(c(facet))) |>
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
  
  plot <- (ggplot(dat, aes(x = .data[[xaxis]], y = .data[[resp]], color = .data[[xaxis]]))
    + geom_boxplot(outlier.alpha = 0, size = 1.1)
    + stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1.1, linetype = "dotted")
    + geom_jitter(size = 3, width = 0.1, alpha = 0.7)
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
    + scale_x_discrete(labels = \(l) str_replace(l, "^H", "IH"))
  )
  
  if(!is.null(facet)) plot <- plot + facet_wrap( ~ .data[[facet]])

  if(display_labels) plot <- plot + labs(
    title = glue::glue("BoxPlot of [{resp}] by [{xaxis}]"), 
    subtitle = glue::glue("With p-values {correction}")
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
    mod, xaxis = "Condition", facet, cluster = "Mouse", agg_by = NULL, 
    scale = "link", adjust = "none", display_labels = FALSE, resp_name = NULL, print_eqs = FALSE
) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  resp <- insight::find_response(mod)
  if(is.null(resp_name)) resp_name <- get_response_name(resp)
  
  dat <- insight::get_data(mod)
  if(!is.null(agg_by)) dat <- dat |> group_by(across(c(xaxis, agg_by))) |> summarize({{ resp }} := mean(.data[[resp]])) |> ungroup()
  dat <- dat |> group_by(across(c(xaxis, facet))) |> mutate(N = glue("N = {get_n_units(cur_data())}")) |> ungroup()
  
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
    group_by(across(c(facet))) |>
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
    + geom_jitter(size = 3, width = 0.1, alpha = 0.7)
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
  )
  
  if (display_labels) plot <- plot + labs(
    title = glue::glue("BoxPlot of [{resp}] by [{xaxis}]"),
    subtitle = glue::glue("With p-values {correction}")
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
    mod, emmeans, contrasts, data = NULL, # In case insight can't get the data from the model
    treatment = "Condition", time = "Stage", 
    data_transform = "I", xlims = NULL, show_observed = FALSE, y_title = NULL, zero_hline = TRUE
  ) {
  
  resp <- insight::find_response(mod)
  data <- match.fun(data_transform)(insight::get_data(mod) %||% data)
  LCL_name <- purrr::keep(colnames(emmeans), grepl(pattern = "LCL|lower", x = colnames(emmeans)))
  UCL_name <- purrr::keep(colnames(emmeans), grepl(pattern = "UCL|upper", x = colnames(emmeans)))
  
  p_data <- (left_join(
    emmeans |> select({{ time }}, {{ treatment }}, Observed, Modeled, matches("CL$")),
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
   + geom_jitter(data = data, aes_string(y = resp), alpha = 0.7, size = 3, width = 0.2, height = 0) # Data points
   + geom_point(size = 3, position = position_dodge(0.15)) # Modeled emmeans
   + geom_errorbar(aes_string(ymin = LCL_name, ymax = UCL_name), width = 0.25, size = 1.1, position = position_dodge(0.15))
   + geom_line(aes_string(group = treatment), position = position_dodge(0.15))
   + geom_text(
     data = p_data,
     aes_string(label = "p.signif", x = time, y = "pos.y"),
     vjust = 0.5, hjust = 0.5,
     size = 4, color = "black"
   )
   + labs(
     y = y_title %||% get_response_name(resp)
   )
   + theme(
     legend.position = "right",
     legend.key.size = unit(1.5, 'cm'), 
     legend.title = element_text(size = 14),
   )
  )
  
  if (!is.null(xlims)) plot <- plot + scale_x_discrete(limits = xlims)
  if (show_observed) plot <- plot + geom_point(aes_string(y = "Observed", fill = treatment), size = 2, shape = 23, color = "black", position = position_dodge(0.20))
  if (zero_hline) plot <- plot + geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4")
  
  return(plot)
}