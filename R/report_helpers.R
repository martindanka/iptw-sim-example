# Balance tables --------------------------------------------------------------------------------------------------
summarise_balance_table <- function(
    corr_all,
    corr_across,
    ess_summary,
    methods_keep = c("multinom", "CBPS", "GBM", "npCBPS")) {
  method_lookup <- c(
    multinom = "multinomial",
    CBPS = "CBPS",
    GBM = "GBM",
    npCBPS = "npCBPS"
  )

  set_lookup <- c(
    raw = "Raw weights",
    win = "Winsorised weights (99th pct)"
  )

  pct_gt10 <- corr_all |>
    dplyr::filter(method %in% methods_keep) |>
    dplyr::summarise(
      pct_gt10_imp = mean(val > 0.10, na.rm = TRUE) * 100,
      .by = c(set, method, imp)
    ) |>
    dplyr::summarise(
      pct_gt10 = mean(pct_gt10_imp, na.rm = TRUE),
      .by = c(set, method)
    )

  corr_summary <- corr_across |>
    dplyr::filter(method %in% methods_keep) |>
    dplyr::select(
      set,
      method,
      mean_abs_corr = mean,
      max_abs_corr = max
    )

  ess_tab <- ess_summary |>
    dplyr::filter(method %in% methods_keep) |>
    dplyr::select(
      set,
      method,
      mean_ess,
      p5_ess
    )

  balance_tab <- corr_summary |>
    dplyr::left_join(pct_gt10, by = c("set", "method")) |>
    dplyr::left_join(ess_tab, by = c("set", "method")) |>
    dplyr::mutate(
      method_label = dplyr::recode(method, !!!method_lookup),
      set_label = dplyr::recode(set, !!!set_lookup)
    ) |>
    dplyr::arrange(
      factor(set, levels = c("raw", "win")),
      factor(method, levels = methods_keep)
    ) |>
    dplyr::select(
      set_label,
      method_label,
      mean_abs_corr,
      pct_gt10,
      max_abs_corr,
      mean_ess,
      p5_ess
    )

  # return two blocks
  list(
    raw = balance_tab |>
      dplyr::filter(set_label == "Raw weights") |>
      dplyr::select(
        Method = method_label,
        `Mean |corr|` = mean_abs_corr,
        `% |corr| > 0.10` = pct_gt10,
        `Max |corr|` = max_abs_corr,
        `Mean ESS` = mean_ess,
        `5th pct ESS` = p5_ess
      ),
    win = balance_tab |>
      dplyr::filter(set_label == "Winsorised weights (99th pct)") |>
      dplyr::select(
        Method = method_label,
        `Mean |corr|` = mean_abs_corr,
        `% |corr| > 0.10` = pct_gt10,
        `Max |corr|` = max_abs_corr,
        `Mean ESS` = mean_ess,
        `5th pct ESS` = p5_ess
      )
  )
}

make_balance_flextable <- function(balance_tables) {
  tab_raw <- balance_tables$raw
  tab_win <- balance_tables$win

  format_balance_for_display <- function(tab) {
    tab |>
      dplyr::mutate(
        `Mean |corr|` = sprintf("%.3f", round(`Mean |corr|`, 3)),
        `% |corr| > 0.10` = sprintf("%.1f", round(`% |corr| > 0.10`, 1)),
        `Max |corr|` = sprintf("%.3f", round(`Max |corr|`, 3)),
        `Mean ESS` = scales::comma(round(`Mean ESS`, 0)),
        `5th pct ESS` = scales::comma(round(`5th pct ESS`, 0))
      )
  }

  tab_raw_fmt <- tab_raw |>
    format_balance_for_display() |>
    dplyr::mutate(.section_header = FALSE)

  tab_win_fmt <- tab_win |>
    format_balance_for_display() |>
    dplyr::mutate(.section_header = FALSE)

  raw_header_row <- tibble::tibble(
    Method = "Raw weights",
    `Mean |corr|` = "",
    `% |corr| > 0.10` = "",
    `Max |corr|` = "",
    `Mean ESS` = "",
    `5th pct ESS` = "",
    .section_header = TRUE
  )

  win_header_row <- tibble::tibble(
    Method = "Winsorised weights (99th pct)",
    `Mean |corr|` = "",
    `% |corr| > 0.10` = "",
    `Max |corr|` = "",
    `Mean ESS` = "",
    `5th pct ESS` = "",
    .section_header = TRUE
  )

  table_combined <- dplyr::bind_rows(
    raw_header_row,
    tab_raw_fmt,
    win_header_row,
    tab_win_fmt
  )

  ft <- flextable::flextable(
    table_combined |>
      dplyr::select(-.section_header)
  ) |>
    autofit() |>
    bold(part = "header", bold = TRUE)

  section_rows <- which(table_combined$.section_header)
  data_rows <- setdiff(seq_len(nrow(table_combined)), section_rows)

  all_cols <- setdiff(names(table_combined), ".section_header")
  num_cols <- c("Mean |corr|", "% |corr| > 0.10", "Max |corr|", "Mean ESS", "5th pct ESS")

  ft <- ft |>
    bg(i = section_rows, bg = "#F2F2F2", part = "body") |>
    bold(i = section_rows, part = "body", bold = TRUE) |>
    align(
      i = section_rows,
      j = all_cols,
      align = "left",
      part = "body"
    ) |>
    align(
      i = data_rows,
      j = "Method",
      align = "left",
      part = "body"
    ) |>
    align(
      i = data_rows,
      j = num_cols,
      align = "center",
      part = "body"
    )

  ft
}


# Export flextable to Word ----------------------------------------------------------------------------------------
export_table_docx <- function(
    ft,
    path,
    title = NULL,
    footnote = NULL,
    title_style = "Normal",
    footnote_style = "Normal") {
  # start a new empty Word doc
  doc <- officer::read_docx()

  # optional title paragraph BEFORE table
  if (!is.null(title) && nzchar(title)) {
    doc <- officer::body_add_par(
      x = doc,
      value = title,
      style = title_style
    )
  }

  # add the flextable itself
  doc <- flextable::body_add_flextable(
    x = doc,
    value = ft
  )

  # optional footnote AFTER table
  if (!is.null(footnote) && nzchar(footnote)) {
    doc <- officer::body_add_par(
      x = doc,
      value = footnote,
      style = footnote_style
    )
  }

  # write the .docx file
  print(
    x = doc,
    target = path
  )
}


# Latex tables ----------------------------------------------------------------------------------------------------

### Move \caption{} inside threeparttable environment ###
# This is needed because kableExtra puts \caption{} before threeparttable.
# INPUT: path_in = path to input .tex file
# OUTPUT: path_out = path to output .tex file (overwrites input by default)
move_caption_inside_threepart <- function(path_in, path_out = path_in) {
  tex <- readr::read_lines(path_in)
  i_cap <- str_which(tex, "^\\s*\\\\caption\\{")[1]
  i_tpt <- str_which(tex, "^\\s*\\\\begin\\{threeparttable\\}")[1]
  
  # Continue only if both caption and threeparttable are found, and caption is before threeparttable
  if (!is.na(i_cap) && !is.na(i_tpt) && i_cap < i_tpt) {
    # Extract caption line
    cap <- tex[i_cap]
    tex <- tex[-i_cap]
    
    # Find threeparttable again after removal
    i_tpt <- str_which(tex, "^\\s*\\\\begin\\{threeparttable\\}")[1]
    
    # Insert caption right after \begin{threeparttable}
    tex <- append(tex, values = cap, after = i_tpt)
  }
  
  readr::write_lines(tex, path_out)
  invisible(TRUE)
}

### Make footnote start on the same line as 'Note' ###
# This is needed because kableExtra puts \item on a new line after 'Note:'.
# INPUT: path_in = path to input .tex file
# OUTPUT: path_out = path to output .tex file (overwrites input by default)
place_footnote_after_note <- function(path_in, path_out = path_in) {
  tex <- readr::read_lines(path_in)
  i_note <- stringr::str_which(tex, "\\\\textit\\{Note: \\}")[1]
  i_footnote <- i_note + 1
  
  # Continue only if i_note is found
  if (!is.na(i_note)) {
    tex[i_footnote] <- str_remove(tex[i_footnote], "\\\\item ")
    tex[i_note] <- str_replace(tex[i_note], "Note: ", "Note:")
  }
  
  readr::write_lines(tex, path_out)
  invisible(TRUE)
}

### Fix symbol footnote indentation ###
fix_footnote_indentation <- function(path_in, path_out = path_in) {
  tex <- readr::read_lines(path_in)
  i_note <- stringr::str_which(tex, stringr::fixed("\\item[*]"))
  
  # Continue only if i_note is found
  if (!is.na(i_note)) {
    tex[i_note] <- stringr::str_replace(tex[i_note], stringr::fixed("\\item[*] "), "\\item \\^{*}")
    readr::write_lines(tex, path_out)
    invisible(TRUE)
  } else {
    warning("No footnote found to fix.")
  }
}

# Balance plot ----------------------------------------------------------------------------------------------------

pal <- c(
  "Unadjusted" = "#000000", # black
  "multinom" = "#E69F00", # orange
  "CBPS" = "#0072B2", # blue
  "GBM" = "#499E4E", # blue-green
  "npCBPS" = "#CC79A7"
)

# Distinct filled shapes 
shape_vec <- c(
  "Unadjusted" = 16, # filled circle
  "multinom" = 17, # filled triangle
  "CBPS" = 15, # filled square
  "GBM" = 18, # filled diamond
  "npCBPS" = 19
)

# Helper to build one panel
make_balance_plot <- function(
    bal_obj,
    pretty_names,
    panel_title,
    panel_subtitle,
    x_limits = c(-0.25, 0.40)) {
  
  # Check if all methods in balance object also in palette
  method_names <- setdiff(rownames(bal_obj$Observations), "All")
  if (!all(method_names %in% names(pal))) {
    stop("Not all weight methods in balance object have defined colours/shapes. Edit `pal` and `shape_vec` 
         in the helper script.")
  }
  
  # Get the order of weight methods in the balance object
  w_order <- setdiff(rownames(bal_obj$Observations), "All")
  w_order <- c("Unadjusted", w_order)
  
  # Display names
  display_names <- w_order |>
    dplyr::recode(multinom = "Multinomial")
  
  # Map shapes and colours to the methods. Preserve order in the balance object.
  shape_vec <- shape_vec[w_order]
  pal <- pal[w_order]
  
  # Base cobalt plot
  p <- cobalt::love.plot(
    bal_obj,
    threshold = 0.1, # draws dashed vlines at ±0.10
    var.order = "unadjusted",
    var.names = pretty_names,
    limits = x_limits,
    line = FALSE, # <- no connecting lines
    size = 3.0, # slightly bigger points
    shapes = unname(shape_vec),
    colors = unname(pal),
    sample.names = display_names,
    position = "bottom"
  )
  
  # Add shaded acceptable band |corr| ≤ 0.10
  # and a clear 0 reference line in black.
  p <- p +
    annotate(
      "rect",
      xmin = -0.10,
      xmax = 0.10,
      ymin = -Inf,
      ymax = Inf,
      fill = "#D9D9D9",
      alpha = 0.08 # lighter
    ) +
    geom_vline(
      xintercept = 0,
      colour = "#000000",
      linewidth = 0.4
    ) +
    scale_x_continuous(
      breaks = seq(-0.2, 0.4, by = 0.1),
      limits = x_limits
    ) +
    coord_cartesian(clip = "off")
  
  # Final theming for readability + accessibility
  p <- p +
    labs(
      title = panel_title,
      subtitle = panel_subtitle,
      x = "Correlation with exposure",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # backgrounds
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      
      # grid: keep vertical gridlines faint; drop horizontal
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        linewidth = 0.3,
        colour = "#D0D0D0"
      ),
      
      # axes
      axis.text.y = element_text(
        size = 10,
        colour = "#000000",
        hjust = 1
      ),
      axis.text.x = element_text(
        size = 10,
        colour = "#000000"
      ),
      axis.title.x = element_text(
        size = 11,
        colour = "#000000"
      ),
      axis.line.x = element_line(
        linewidth = 0.4,
        colour = "#000000"
      ),
      axis.ticks.x = element_line(
        linewidth = 0.4,
        colour = "#000000"
      ),
      axis.ticks.y = element_blank(),
      
      # titles
      plot.title = element_text(
        face = "bold",
        size = 13,
        colour = "#000000",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = "#000000",
        hjust = 0.5
      ),
      
      # legend
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = "#000000"),
      legend.key.width = unit(1.5, "lines"),
      legend.key.height = unit(0.8, "lines"),
      
      # give right margin so CIs to the right don't get clipped
      plot.margin = margin(t = 10, r = 30, b = 10, l = 10)
    )
  
  p
}



# Forest plot -----------------------------------------------------------------------------------------------------

create_forest_methods_overlay <- function(
    data,
    method_col, # e.g. method_label
    set_col, # e.g. set_label ("Raw weights", "Winsorised weights (99th pct)")
    estimate_col,
    ci_lower_col,
    ci_upper_col,
    label_col = NULL, # e.g. rr_label (optional)
    which_sets = c("Raw weights", "Winsorised weights (99th pct)"),
    duplicate_unweighted = FALSE, # Toggle to duplicate unweighted methods in winsorised set
    y_label = "Risk ratio (95% CI) per 4−point greater Malaise Inventory score",
    x_label = NULL,
    hline_yintercept = 1,
    raw_colour = "#000000",
    win_colour = "#2C7FB8",
    raw_shape = 16,
    win_shape = 17,
    raw_legend = "Raw weights / unweighted",
    win_legend = "Winsorised (99th pct, IPTW only)",
    base_size = 14,
    title = NULL,
    caption = NULL,
    sort_methods = c("custom", "effect"),
    method_order = NULL  # only used if sort_methods = "custom"
) {
  dodge_width <- 0.5
  sort_methods <- match.arg(sort_methods)
  
  # Capture columns -----------------------------------------------------------
  method_quo <- rlang::enquo(method_col)
  set_quo <- rlang::enquo(set_col)
  est_quo <- rlang::enquo(estimate_col)
  lcl_quo <- rlang::enquo(ci_lower_col)
  ucl_quo <- rlang::enquo(ci_upper_col)
  lbl_quo <- rlang::enquo(label_col)
  
  has_label <- !rlang::quo_is_null(lbl_quo)
  
  # Master ordering of sets (winsorised first so raw plots above after flip)
  set_order_full <- c(
    "Winsorised weights (99th pct)",
    "Raw weights"
  )
  
  # Which sets the caller explicitly asked for
  set_order_use <- intersect(set_order_full, which_sets)
  
  # Colours / shapes / legend labels -----------------------------------------
  set_colours <- c(
    "Raw weights" = raw_colour,
    "Winsorised weights (99th pct)" = win_colour
  )
  
  set_shapes <- c(
    "Raw weights" = raw_shape,
    "Winsorised weights (99th pct)" = win_shape
  )
  
  legend_labels <- c(
    "Raw weights" = raw_legend,
    "Winsorised weights (99th pct)" = win_legend
  )
  
  # ---------------------------------------------------------------------------
  # data_axis: full data (all sets), for:
  # - ordering of methods
  # - global axis limits
  # - global label column position
  # ---------------------------------------------------------------------------
  
  data_axis <- data |>
    dplyr::mutate(
      .method = !!method_quo,
      .set = forcats::fct_relevel(!!set_quo, set_order_full),
      .est = !!est_quo,
      .lcl = !!lcl_quo,
      .ucl = !!ucl_quo
    )
  
  if (has_label) {
    data_axis <- data_axis |>
      dplyr::mutate(.lbl = !!lbl_quo)
  } else {
    data_axis <- data_axis |>
      dplyr::mutate(.lbl = NA_character_)
  }
  
  # Which methods are IPTW-weighted?
  data_axis <- data_axis |>
    dplyr::mutate(
      is_weighted_method = !as.character(.method) %in% c("Unadjusted", "Adjusted")
    )
  
  # ---------------------------------------------------------------------------
  # Method order
  # ---------------------------------------------------------------------------
  
  present_methods <- data_axis |>
    dplyr::pull(.method) |>
    as.character() |>
    unique()
  
  if (sort_methods == "effect") {
    # Order by Raw weights estimate (ascending), like original
    method_order_tmp <- data_axis |>
      dplyr::filter(.set == "Raw weights") |>
      dplyr::arrange(.est) |>
      dplyr::pull(.method) |>
      as.character() |>
      unique()
    
    method_order_tmp <- method_order_tmp[method_order_tmp %in% present_methods]
    method_levels_for_factor <- c(
      method_order_tmp,
      setdiff(present_methods, method_order_tmp)
    )
  } else {
    # Custom order (TOP -> BOTTOM)
    if (is.null(method_order)) {
      method_order <- c(
        "Unadjusted",
        "Adjusted",
        "Multinomial",
        "CBPS",
        "npCBPS",
        "GBM"
      )
    }
    
    method_order <- as.character(method_order)
    
    method_order_top_to_bottom <- method_order[method_order %in% present_methods]
    method_order_top_to_bottom <- c(
      method_order_top_to_bottom,
      setdiff(present_methods, method_order_top_to_bottom)
    )
    
    # coord_flip() means factor levels go bottom->top,
    # so reverse to make TOP->BOTTOM match method_order_top_to_bottom
    method_levels_for_factor <- rev(method_order_top_to_bottom)
  }
  
  # ---------------------------------------------------------------------------
  # Global label position + axis limits
  # ---------------------------------------------------------------------------
  
  global_label_x <- max(data_axis$.ucl, na.rm = TRUE) * 1.05
  y_min <- min(data_axis$.lcl, na.rm = TRUE) * 0.9
  y_max <- max(data_axis$.ucl, na.rm = TRUE) * 1.1
  
  # ---------------------------------------------------------------------------
  # Build data_plot = actual rows to draw
  # ---------------------------------------------------------------------------
  
  requested_sets <- set_order_use
  
  only_raw_requested <- identical(requested_sets, "Raw weights")
  only_win_requested <- identical(requested_sets, "Winsorised weights (99th pct)")
  both_requested <- length(requested_sets) == 2
  
  if (only_raw_requested) {
    data_plot <- data_axis |>
      dplyr::filter(.set == "Raw weights")
  } else if (only_win_requested) {
    data_plot <- data_axis |>
      dplyr::filter(
        (.set == "Winsorised weights (99th pct)" & is_weighted_method) |
          (.set == "Raw weights" & !is_weighted_method)
      )
  } else if (both_requested) {
    data_plot <- data_axis |>
      dplyr::filter(
        .set == "Raw weights" |
          (
            .set == "Winsorised weights (99th pct)" &
              (is_weighted_method | duplicate_unweighted)
          )
      )
  } else {
    data_plot <- data_axis |>
      dplyr::filter(
        .set %in% requested_sets
      )
  }
  
  # Apply final factor levels, ordering, and common label column
  data_plot <- data_plot |>
    dplyr::mutate(
      .set = forcats::fct_relevel(.set, set_order_full),
      .method = factor(.method, levels = method_levels_for_factor),
      .label_x = global_label_x
    )
  
  # Legend visibility and dodging
  sets_plotted <- unique(as.character(data_plot$.set))
  show_legend <- length(sets_plotted) > 1
  
  pos_fun <- if (show_legend) {
    ggplot2::position_dodge(width = dodge_width)
  } else {
    ggplot2::position_identity()
  }
  
  legend_breaks <- c("Raw weights", "Winsorised weights (99th pct)")
  legend_breaks <- legend_breaks[legend_breaks %in% sets_plotted]
  
  # ---------------------------------------------------------------------------
  # Build the plot
  # ---------------------------------------------------------------------------
  
  p <- ggplot(
    data_plot,
    aes(
      x = .method,
      y = .est,
      colour = .set,
      shape = .set,
      group = .set
    )
  ) +
    geom_errorbar(
      aes(ymin = .lcl, ymax = .ucl),
      width = 0.2,
      position = pos_fun,
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    geom_point(
      size = 2.8,
      position = pos_fun,
      show.legend = show_legend
    ) +
    geom_hline(
      yintercept = hline_yintercept,
      linetype = "dashed",
      colour = "black"
    )
  
  if (has_label) {
    p <- p +
      geom_text(
        data = data_plot,
        aes(
          x = .method,
          y = .label_x,
          label = .lbl,
          colour = .set,
          group = .set
        ),
        position = pos_fun,
        hjust = 0,
        vjust = 0.5,
        size = 4,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }
  
  # ---------------------------------------------------------------------------
  # Scales and theme
  # ---------------------------------------------------------------------------
  
  p <- p +
    coord_flip() +
    scale_y_continuous(
      limits = c(y_min, y_max),
      expand = ggplot2::expansion(mult = c(0, 0.25))
    ) +
    scale_colour_manual(
      values = set_colours,
      breaks = legend_breaks,
      labels = legend_labels[legend_breaks],
      name = NULL
    ) +
    scale_shape_manual(
      values = set_shapes,
      breaks = legend_breaks,
      labels = legend_labels[legend_breaks],
      name = NULL
    ) +
    labs(
      x = x_label,
      y = y_label,
      title = title,
      caption = caption
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.text.y = element_text(size = 12, hjust = 1),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(
        colour = "grey80",
        linewidth = 0.3
      ),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank()
    )
  
  p
}


# ============================================================================ #
# Generic: write_tex_macros
# Creates \newcommand definitions from a tibble.
#
# - data: tibble / data frame
# - macro_path: where to write the .tex file
# - id_cols: character vector of columns that define the 'key' (e.g. c("set","Method"))
# - value_cols: columns you want macros for (e.g. c("mean_abs_corr","mean_ess"))
# - prefix: macro name prefix, e.g. "bcsBal"
# - formatters: named list of functions (per value_col) to format values as strings
#               e.g. list(mean_abs_corr = ~ sprintf("%.3f", .x))
# ============================================================================ #
write_tex_macros <- function(
    data,
    macro_path,
    id_cols,
    value_cols = NULL,
    prefix = "",
    formatters = NULL) {
  
  # basic checks
  stopifnot(all(id_cols %in% names(data)))
  stopifnot(all(value_cols %in% names(data)))
  
  # helper to clean things for use inside macro names (latex-safe-ish)
  clean_id <- function(x) {
    x |>
      as.character() |>
      stringr::str_replace_all("[^A-Za-z0-9]", "")  # keep only letters/digits
  }
  
  # If value columns not supplied, use all columns except id_cols
  if (is.null(value_cols)) {
    value_cols <- setdiff(names(data), id_cols)
  }
  
  # 1) make sure all id columns are character
  data_clean <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(id_cols),
        as.character
      )
    )
  
  # 2) pivot value columns long so each row = (ids, col, value)
  long_df <- data_clean |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "value_col",
      values_to = "value"
    )
  
  # 3) build macro names and formatted value strings
  macros <- long_df |>
    dplyr::mutate(
      # combine all id columns into one identifier
      id_part = purrr::pmap_chr(
        dplyr::across(dplyr::all_of(id_cols)),
        \(...) {
          vals <- list(...)
          vals |>
            vapply(clean_id, FUN.VALUE = character(1)) |>
            paste0(collapse = "")
        }
      ),
      col_part = clean_id(value_col),
      macro_name = paste0(prefix, id_part, col_part),
      value_str = dplyr::case_when(
        !is.null(formatters) & value_col %in% names(formatters) ~
          # apply the formatter function for this column
          purrr::map2_chr(
            value,
            value_col,
            \(val, col) {
              if (!col %in% names(formatters)) {
                as.character(val)
              } else {
                fmt_fun <- formatters[[col]]
                fmt_fun(val)
              }
            }
          ),
        TRUE ~ as.character(value)
      ),
      macro_line = glue::glue("\\newcommand{{\\{macro_name}}}{{{value_str}}}")
    ) |>
    dplyr::pull(macro_line)
  
  # 4) write lines to file
  readr::write_lines(macros, macro_path)
  
  invisible(macro_path)
}


# Rename columns to camelCase -------------------------------------------------------------------------------------

camelise_all <- function(df, case = "lower_camel") {
  fn <- if (case == "upper_camel") {
    snakecase::to_upper_camel_case
  } else {
    snakecase::to_lower_camel_case
  }
  dplyr::rename_with(df, fn)
}

