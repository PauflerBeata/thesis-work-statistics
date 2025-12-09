library(ggplot2)
library(dplyr)
library(rstatix)

# Parametric and non parametric tests

tests <- function(column, data, group_by_col, p.adjust.method, small_sample=TRUE) {

  if (!(column %in% colnames(data))) {
    stop("This column: ('", column, "') is not exist.")
  }
  
  if (!(group_by_col %in% colnames(data))) {
    stop("The grouping column ('", group_by_col, "') does not exist.")
  }
  
  posthoc_t_test <- NULL
  tukey_test <- NULL
  anova_table <- NULL
  dunn_test_result <- NULL
  
# Normality test
## Histogram
  gg_hist <- ggplot(data, aes(x = .data[[column]])) +
    geom_histogram(binwidth = 100000, fill = "steelblue", color = "black") +
    ggtitle(paste(column, "Distribution")) +
    theme_minimal()

  ## Q-Q plot
  gg_qq <- ggplot(data, aes(sample = .data[[column]])) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste(column, "Q-Q Plot")) +
    theme_minimal()
  
 
  ## Shapiro-Wilk or Kolmogorov-Smirnov test
    if (small_sample) {
      shapiro_result <- shapiro.test(data[[column]])
      shapiro_result$data.name <- column
      ks_result <- NULL
      normal <- shapiro_result$p.value > 0.05
      normality_method <- "Normality test: Shapiro-Wilk"
      normality_results <- shapiro_result
    } else {
      shapiro_result <- NULL
      ks_result <- ks.test(data[[column]], 'pnorm')
      ks_result$data.name <- column
      normal <- ks_result$p.value > 0.05
      normality_method <- "Normality test: Kolmogorov-Smirnov"
      normality_results <- ks_result
    }

  n_groups <- n_distinct(na.omit(data[[group_by_col]]))

  # t-test or Wilcoxon-test
if (n_groups == 2) {  
  if (normal) {
    test_result <- data %>% 
      t_test(
        formula = as.formula(paste0("`", column, "` ~ ", group_by_col)),
        var.equal = TRUE
       )%>% 
         add_significance() %>%  
         adjust_pvalue(method = p.adjust.method)
    test_type <- "t-test"
    } else {
    test_result <- data %>%
      wilcox_test(
        formula = as.formula(paste0("`", column, "` ~ ", group_by_col))
      ) %>% 
      add_significance() %>%
      adjust_pvalue(method = p.adjust.method)
    test_type <- "Wilcoxon-test"
    }
  test_result <- test_result %>%
    mutate(
      y.position = max(data[[column]], na.rm = TRUE) * 1.05
    )
  
    } else {
    if (normal) {
      test_result <- data %>% 
        anova_test(
          formula = as.formula(paste0("`", column, "` ~ ", group_by_col)),
        ) %>% 
           add_significance()
      anova_table <- get_anova_table(test_result)
      test_type <- paste0("One-way ANOVA with ", p.adjust.method, " posthoc")
      
      tukey_test <- data %>%
        tukey_hsd(formula = as.formula(paste0("`", column, "` ~ ", group_by_col))
        ) %>%
        add_significance()
      
      tukey_test <- tukey_test %>%
        add_xy_position(x = group_by_col)
        
    } else {
      test_result <- data %>%
        kruskal_test(
          formula = as.formula(paste0("`", column, "` ~ ", group_by_col))
        ) %>% 
        add_significance() %>% 
        adjust_pvalue(method = p.adjust.method)
      test_type <- "Kruskal-Wallis test"
      
      dunn_test_result <- data %>% 
        dunn_test(formula = as.formula(paste0("`", column, "` ~ ", group_by_col)),
                  p.adjust.method = p.adjust.method) %>% 
        add_significance()
      
      dunn_test_result <- dunn_test_result %>%
        add_xy_position(x = group_by_col)
    }
  }

  return(list(
    "column_name" = column,
    "hist" = gg_hist,
    "qq" = gg_qq,
    "normality_method" = normality_method,
    "test_result" = test_result,
    "posthoc_t" = posthoc_t_test,
    "tukey_test" = tukey_test,
    "test_type" = test_type,
    "normality_results" = normality_results,
    "anova_table" = anova_table,
    "dunn_test" = dunn_test_result
  ))
}


#Fisher's exact test
fisher_test <- function(data, var1, var2, y_max){
  
  data[[var1]] <- factor(data[[var1]])
  data[[var2]] <- factor(data[[var2]])
  
  contingency_table <- table(data[[var1]], data[[var2]])
  
  fisher_res <- fisher.test(contingency_table)
  p_val <- fisher_res$p.value
  p_signif <- ifelse(
    p_val < 0.001, "***",
    ifelse(p_val < 0.01, "**",
           ifelse(p_val < 0.05, "*", "ns")
    )
  )
  
  groups <- rownames(contingency_table)
  y_pos <- y_max * 1.1
  stat_df <- data.frame(
    group1    = groups[1],
    group2    = groups[2],
    p         = p_val,
    p.signif  = p_signif,
    y.position = y_pos
  )
  
  result_text <- ifelse(fisher_res$p.value < 0.05,
                        "(p < 0.05) - Fisher test",
                        "(p ≥ 0.05) - No significant differences between groups."
  )
  
  return(list(
    table = contingency_table,
    test_type = "Fisher's exact test",
    test_result = stat_df,
    result = result_text
  ))
}
