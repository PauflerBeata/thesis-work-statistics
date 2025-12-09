
library(kableExtra)

descriptive <- function(column, data, group_by_cols,
                        include_n = TRUE, include_mean = TRUE, include_sd = TRUE, 
                        include_sem = TRUE, include_median = TRUE, include_mode = TRUE, 
                        include_iqr = TRUE, include_min = TRUE, include_max = TRUE) {
  
  if (!is.null(group_by_cols)) {
    grouped_data <- data %>%
      group_by(across(all_of(group_by_cols)))
  } else {
    grouped_data <- data
  }
  
  stats <- grouped_data %>%
    summarize(
      n = if (include_n) n() - sum(is.na(!!sym(column))) else NA,
      mean = if (include_mean) mean(!!sym(column), na.rm = TRUE) else NA,
      sd = if (include_sd) sd(!!sym(column), na.rm = TRUE) else NA,
      sem = if (include_sem) sd(!!sym(column), na.rm = TRUE) / sqrt(n()) else NA,
      median = if (include_median) median(!!sym(column), na.rm = TRUE) else NA,
      mode = if (include_mode) {
        unique_x <- unique(na.omit(!!sym(column)))
        unique_x[which.max(tabulate(match(!!sym(column), unique_x)))]
      } else NA,
      iqr = if (include_iqr) IQR(!!sym(column), na.rm = TRUE) else NA,
      min = if (include_min) min(!!sym(column), na.rm = TRUE) else NA,
      max = if (include_max) max(!!sym(column), na.rm = TRUE) else NA,
      .groups = "drop"
    ) %>%
    select_if(~ !all(is.na(.)))
  
  # Table
  
  table <- stats %>% kbl(caption = column) %>% kable_classic(full_width = FALSE, position = "left")
  
  return(list("data" = stats, "table" = table))
}

