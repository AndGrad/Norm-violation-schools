library(cowplot)
library(gghalves)


ordinal_barplot <- function(data, variable, title) {
 
var_name <- deparse(substitute(variable))  # Get the variable name as a string
  
  # Calculate proportions
  prop_data <- data %>%
    group_by({{variable}}) %>%
    summarise(count = n()) %>%
    mutate(proportion = count / sum(count))
  
  # Calculate the median
  median_val <- median(data[[var_name]], na.rm = TRUE)
  
  # Create the histogram with proportions
  p <- ggplot(prop_data, aes(x = as.factor({{variable}}), y = proportion)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    geom_vline(xintercept = median_val + 1 , color = "red", linetype = "solid", linewidth = 1) + # Added median line
    labs(x = var_name, y = "Proportion")  # Changed y-axis label
  return(p)
}

faceted_barplot <- function(data, x_var, facet_var, title_text) {
  
  data <- data %>% dplyr::filter(!is.na({{ x_var }}))
  
  facet_var_name <- deparse(substitute(facet_var))
  ggplot(data, aes(x = factor({{ x_var }}), y = after_stat(prop), group = {{ facet_var }}, fill = factor({{ facet_var }},levels=c("0","1")))) +
    geom_bar(position = "identity") +
    labs(
      title = title_text,
      x = "",
      y = "Proportion",
    ) +
    scale_fill_brewer(palette = "Set2") +
  data %>%     theme(legend.position = "none")
}


