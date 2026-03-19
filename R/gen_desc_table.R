library(tidyverse)
library(flextable)

# Helpers -----------------------------------------------------------------------------------------------------

safe_label <- function(var, variable_labels) {
  # Use human-readable label if provided, else fall back to var name
  if (!is.null(variable_labels) && !is.null(variable_labels[[var]])) {
    variable_labels[[var]]
  } else {
    var
  }
}

# Format integer-like counts with chosen thousands separator
fmt_int <- function(x,
                    thousands_sep = c("comma", "thin_space", "none")) {
  sep <- switch(thousands_sep,
    comma = ",",
    thin_space = "\u2009", # thin space
    none = "",
    "," # fallback
  )

  formatC(
    x,
    format = "d",
    big.mark = sep
  )
}

# Format numeric summaries (means / SDs) with chosen thousands separator
fmt_num <- function(x,
                    digits,
                    thousands_sep = c("comma", "thin_space", "none")) {
  sep <- switch(thousands_sep,
    comma = ",",
    thin_space = "\u2009",
    none = "",
    "," # fallback
  )

  formatC(
    x,
    digits = digits,
    format = "f",
    big.mark = sep
  )
}

# Format percentages with journal-friendly rules:
# 0% not 0.0%, 100% not 100.0%, else keep decimals
pct_to_string <- function(pct,
                          pct_digits = 1) {
  out <- sprintf(paste0("%.", pct_digits, "f"), pct)
  out[abs(pct) < 1e-9] <- "0"
  out[abs(pct - 100) < 1e-9] <- "100"
  out
}

# Summarise a single variable -------------------------------------------------------------------------------
summarise_single_var <- function(data,
                                 var,
                                 variable_labels = NULL,
                                 mean_digits = 1,
                                 sd_digits = 1,
                                 pct_digits = 1,
                                 thousands_sep = c("comma", "thin_space", "none")) {
  # Numeric / continuous -----------------------------------------------------------------
  if (is.numeric(data[[var]])) {
    mean_val <- mean(data[[var]], na.rm = TRUE)
    sd_val <- sd(data[[var]], na.rm = TRUE)

    mean_sd <- paste0(
      fmt_num(mean_val, mean_digits, thousands_sep),
      " (",
      fmt_num(sd_val, sd_digits, thousands_sep),
      ")"
    )

    missing_n <- sum(is.na(data[[var]]))
    total_n <- nrow(data)
    missing_pct <- (missing_n / total_n) * 100
    pct_text <- pct_to_string(missing_pct, pct_digits)

    missing_stat <- paste0(
      fmt_int(missing_n, thousands_sep),
      " (",
      pct_text,
      "%)"
    )

    tibble(
      variable = safe_label(var, variable_labels),
      stat = mean_sd,
      missing = missing_stat
    )

    # Factor / categorical -------------------------------------------------------
  } else if (is.factor(data[[var]])) {
    counts <- data |>
      filter(!is.na(.data[[var]])) |>
      count(level = .data[[var]]) |>
      mutate(
        pct = n / sum(n) * 100,
        pct_text = pct_to_string(pct, pct_digits),
        stat = paste0(
          fmt_int(n, thousands_sep),
          " (",
          pct_text,
          "%)"
        )
      )

    missing_n <- sum(is.na(data[[var]]))
    total_n <- nrow(data)
    missing_pct <- (missing_n / total_n) * 100
    pct_text_miss <- pct_to_string(missing_pct, pct_digits)

    missing_stat <- paste0(
      fmt_int(missing_n, thousands_sep),
      " (",
      pct_text_miss,
      "%)"
    )

    tibble(
      variable = c(
        safe_label(var, variable_labels),
        paste0("   ", counts$level)
      ),
      stat = c(
        "",
        counts$stat
      ),
      missing = c(
        missing_stat,
        rep("", nrow(counts))
      )
    )

    # Unsupported types ----------------------------------------------------------
  } else {
    tibble(
      variable = character(),
      stat = character(),
      missing = character()
    )
  }
}

# Main function -----------------------------------------------------------------------------------------------

gen_desc_table <- function(data,
                           var_groups,
                           variable_labels = NULL,
                           caption = "Descriptive statistics",
                           output = c("summary_table", "flextable"),
                           mean_digits = 1,
                           sd_digits = 1,
                           pct_digits = 1,
                           thousands_sep = c("comma", "thin_space", "none")) {
  output <- match.arg(output)
  thousands_sep <- match.arg(thousands_sep)

  # Build each group's block
  group_tables <- purrr::imap(var_groups, \(vars, group_name) {
    group_body <- purrr::map_dfr(
      vars,
      \(v) summarise_single_var(
        data = data,
        var = v,
        variable_labels = variable_labels,
        mean_digits = mean_digits,
        sd_digits = sd_digits,
        pct_digits = pct_digits,
        thousands_sep = thousands_sep
      )
    )

    tibble(
      variable = group_name,
      stat = "",
      missing = ""
    ) |>
      bind_rows(group_body)
  })

  summary_table <- bind_rows(group_tables)

  if (output == "summary_table") {
    return(summary_table)
  }

  # Otherwise build flextable -------------------------------------------------
  group_headers <- names(var_groups)

  ft <- flextable(summary_table) |>
    bold(
      i = summary_table$variable %in% group_headers,
      bold = TRUE
    ) |>
    # indent only "Variable" column for factor levels (rows starting with 3 spaces)
    padding(
      i = grepl("^   ", summary_table$variable),
      j = "variable",
      padding.left = 10
    ) |>
    # left-align variable names, right-align numeric-looking cols
    align(
      j = "variable",
      align = "left",
      part = "all"
    ) |>
    align(
      j = c("stat", "missing"),
      align = "center",
      part = "all"
    ) |>
    autofit() |>
    set_header_labels(
      variable = "Variable",
      stat = "N (%)/Mean (SD)",
      missing = "Missing"
    ) |>
    fontsize(
      size = 10,
      part = "all"
    ) |>
    # add_header_lines(values = caption) |>
    bold(
      part = "header"
    ) |>
    bold(
      i = 1,
      bold = TRUE,
      part = "header"
    )

  ft
}


# Single row for binary variables ---------------------------------------------------------------------------------

collapse_binary_blocks <- function(tbl,
                                   which_level = c("second", "first"),
                                   label_style = c("level_only", "label_plus_level")) {
  which_level <- match.arg(which_level)
  label_style <- match.arg(label_style)
  keep_child <- if (which_level == "second") 2L else 1L

  out <- list()
  i <- 1
  n <- nrow(tbl)

  while (i <= n) {
    current <- tbl[i, ]

    # Candidate parent row of a factor:
    # - stat empty
    # - missing non-empty
    # - next row is indented (starts with 3 spaces)
    if (current$stat == "" &&
      nzchar(current$missing) &&
      i < n &&
      startsWith(tbl$variable[i + 1], "   ")) {
      # Count how many indented rows follow
      j <- i + 1
      while (j <= n && startsWith(tbl$variable[j], "   ")) {
        j <- j + 1
      }
      n_child <- j - i - 1

      # Only collapse if exactly 2 levels (binary)
      if (n_child == 2L) {
        child_start <- i + 1L
        child_idx <- child_start + keep_child - 1L
        child_row <- tbl[child_idx, ]

        new_row <- current
        level_label <- stringr::str_trim(child_row$variable)

        new_row$variable <- dplyr::case_when(
          label_style == "level_only" ~ level_label,
          label_style == "label_plus_level" ~ paste0(current$variable, " – ", level_label)
        )
        new_row$stat <- child_row$stat
        # keep current$missing as the missing info for the whole variable

        out[[length(out) + 1L]] <- new_row

        # Skip all the children
        i <- j
        next
      }
    }

    # Default: just keep the row
    out[[length(out) + 1L]] <- current
    i <- i + 1L
  }

  dplyr::bind_rows(out)
}


# Collapse a single named binary variable with explicit control over which level to keep and what label to use.
# parent_label: the variable label as it appears in the summary table (e.g. "Sex")
# keep_child:   1L = first factor level, 2L = second factor level
# new_label:    the label for the collapsed row (e.g. "Female")
collapse_one_binary <- function(tbl, parent_label, keep_child, new_label) {
  parent_row <- which(tbl$variable == parent_label)
  if (length(parent_row) == 0L) return(tbl)

  # Find indented children immediately following the parent
  child_rows <- integer(0)
  j <- parent_row + 1L
  while (j <= nrow(tbl) && startsWith(tbl$variable[j], "   ")) {
    child_rows <- c(child_rows, j)
    j <- j + 1L
  }

  if (length(child_rows) != 2L) return(tbl)

  keep_idx <- child_rows[keep_child]
  tbl$variable[parent_row] <- new_label
  tbl$stat[parent_row] <- tbl$stat[keep_idx]
  tbl[-child_rows, ]
}
