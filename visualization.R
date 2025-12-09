library(ggplot2)
library(rstatix)
library(dplyr)
library(ggpubr)


color_scales <- list(
  Brewer = function(palette = "Dark2", palette_indices = NULL) {
    all_cols <- RColorBrewer::brewer.pal(8, palette)
    
    if (!is.null(palette_indices)) {
      scale_fill_manual(values = all_cols[palette_indices])
    } else {
      scale_fill_brewer(palette = palette)
    }
  }
)



# Boxplot
boxplot <- function(data, column, group_by_col, palette = "Dark2", palette_indices = NULL, test_output = NULL, title = NULL, ylab = NULL, xlab = NULL) {
  
  if (is.null(title)) {
    title <- paste(column, "by", group_by_col)
  }
  if (is.null(ylab)) {
    ylab <- column
  }
  if (is.null(xlab)) {
    xlab <- group_by_col
  }
  
  data <- data %>% 
    dplyr::filter(!is.na(.data[[column]]))
  
  plot <- ggplot(data, aes(x = .data[[group_by_col]], y = .data[[column]], fill = .data[[group_by_col]])) +
    geom_boxplot(width = 0.5, alpha = 0.9, linewidth = 0.5, outlier.shape = NA) +
    geom_jitter(shape = 21, size = 2, stroke = 0.25, color = "black", fill = "red", width = 0.1, height = 0, alpha = 0.7) +
    color_scales$Brewer(palette = palette, palette_indices = palette_indices) +
    stat_pvalue_manual(test_output$test_result, label = "p.signif", size = 6) +
    labs(title = title,
         x = xlab,
         y = ylab,
         caption = ifelse(
            test_output$test_result$p < 0.05,
            paste0(test_output$test_result$p.signif, " (p < 0.05) — ", test_output$test_type),
            paste0("(p ≥  0.05) — ", test_output$test_type)
          )
    ) +
    theme_classic() +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12), 
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 13, hjust = 0.5, face = "italic")
    )
  
  return(plot)
}


# Column plot
colplot <- function(data, group_by_col, fill_col, colors, test_output = NULL,
                    ylab = "Percentage", title = NULL, xlab = NULL) {
  
    if (is.null(title)) {
      title <- paste(fill_col, "distribution by", group_by_col)
    }
    if (is.null(xlab)) {
      xlab <- group_by_col
    }

    plot <- ggplot(data, aes(x = .data[[group_by_col]], y = percent, fill = .data[[fill_col]])) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = paste0(percent, "%")),
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = colors) +
    stat_pvalue_manual(test_output$test_result, label = "p.signif", size = 6) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = fill_col,
      caption = ifelse(
        test_output$test_result$p[1] < 0.05,
        paste0(test_output$test_result$p.signif[1], " (p < 0.05) — Fisher's exact test"),
        paste0("(p ≥  0.05) — Fisher's exact test")
        )
    ) +
    theme_classic() +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.05))
      ) +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12), 
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 13, hjust = 0.5, face = "italic")
    ) +
      coord_cartesian(ylim = c(0, max(data$percent) * 1.25))
  return(plot)
}


