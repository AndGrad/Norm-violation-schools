

summarize_network <- function(graph) {
  if (!igraph::is_igraph(graph)) stop("Input must be an igraph object.")
  
  # Basic structure
  num_nodes <- vcount(graph)
  num_edges <- ecount(graph)
  density <- edge_density(graph,loops = FALSE)
  degrees <- degree(graph, mode = "in")
  avg_degree <- mean(degrees)
  degree_sd <- sd(degrees)
  max_degree <- max(degrees)
  min_degree <- min(degrees)
  reciprocity <- reciprocity(graph)
  
  # Component analysis
  comps <- components(graph)
  num_components <- comps$no
  largest_component_size <- max(comps$csize)
  
  # Path-based metrics: use largest component
  if (is_connected(graph)) {
    diameter_val <- diameter(graph)
    avg_path_len <- mean_distance(graph)
  } else {
    largest_comp <- induced_subgraph(graph, which(comps$membership == which.max(comps$csize)))
    diameter_val <- diameter(largest_comp)
    avg_path_len <- mean_distance(largest_comp)
  }
  
  # Return summary table
  tibble::tibble(
    Metric = c(
      "Number of nodes",
      "Number of edges",
      "Density",
      "Reciprocity",
      "Average degree",
      "Degree (min - max)",
      "Degree SD",
      "Number of components",
      "Largest component size",
      "Diameter (largest component)",
      "Average path length (largest component)"
    ),
    Value = c(
      num_nodes,
      num_edges,
      round(density, 3),
      round(reciprocity,3),
      round(avg_degree, 2),
      paste0(min_degree, " - ", max_degree),
      round(degree_sd, 2),
      num_components,
      largest_component_size,
      diameter_val,
      round(avg_path_len, 2)
    )
  )
}
