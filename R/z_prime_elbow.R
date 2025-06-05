#' Create z-prime plot per replicate including increasing numbers of features in the calculation
#' @param features_scaled dataframe of scaled features per sample
#' @param meta dataframe with metadata (Replicate, cell_line etc)
#' @return Plot of LDA separation of WT and MUT per increased feature, plot of z-prime values per increased feature
#'
#' @export

z_prime_elbow <- function(features_scaled, meta){
  # Plot distribution of features
  melted <- as.data.frame(features_scaled) |>
    data.frame(cell_line=meta$cell_line) |>
    melt()
  a <- ggplot(melted, aes(x = value)) +
    geom_histogram(aes(y=..density..), bins = 50) +
    geom_density(aes(color=cell_line))+
    facet_wrap(~variable, scales = "free") +
    theme_minimal()+
    ggtitle("Feature distribution across all samples")
  print(a)

  # Subset Ctrl samples
  control_only <- meta$Compound == "ctrl"
  meta_control <- meta |> filter(Compound == "ctrl")
  features_control <- features_scaled[control_only,]

  # Plot distribution for features across control samples
  melted <- as.data.frame(features_control) |>
    data.frame(cell_line=meta_control$cell_line) |>
    melt()
  b <- ggplot(melted, aes(x = value)) +
    geom_histogram(aes(y=..density..), bins = 50) +
    geom_density(aes(color=cell_line))+
    facet_wrap(~variable, scales = "free") +
    theme_minimal()+
    ggtitle("Feature distribution across ctrl samples")
  print(b)

  # Primary LDA with all features
  lda_coeffs_sorted <- lda_coeffs_sorted_all_features(meta_control=meta_control, features_control=features_control)


  # View top contributing features
  d <- lda_coeffs_sorted |>
    ggplot(aes(x = LD1, y = reorder(name, LD1))) +
    geom_col() +
    theme_minimal() +
    labs(title = "Top LDA Features", x = "LD1", y = "feature")
  print(d)

  # calculation of z-prime
  number_of_total_features <- ncol(features_scaled)
  LD1_values_df <- data.frame(cell_line=meta$cell_line, Compound=meta$Compound, Replicate = meta$Replicate)
  ld1_plot_list <- list()
  zprime_per_replicate_df <- data.frame(Replicate=unique(LD1_values_df$Replicate) |> as.character())
  zprime_total_df <- data.frame(V1 = 0)


  for(i in 2:number_of_total_features){
    features_scaled_subset <- features_scaled |>
      as.data.frame() |>
      dplyr::select(head(lda_coeffs_sorted$name, n=i))
    features_control <- features_scaled_subset[control_only,]

    lda_input_ctrl <- data.frame(cell_line = meta_control$cell_line, features_control)
    lda_input_ctrl$cell_line <- as.factor(lda_input_ctrl$cell_line)
    lda_model_ctrl <- lda(cell_line ~ ., data = lda_input_ctrl)
    lda_pred_ctrl <- predict(lda_model_ctrl)
    lda_df_ctrl <- data.frame(LD1 = lda_pred_ctrl$x[,1], cell_line = lda_input_ctrl$cell_line)

    ld1_plot_list[[i]] <- ggplot(lda_df_ctrl, aes(x = LD1, fill = cell_line)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(title = paste0("Top ", i, " features"))
    # project all samples in LD1 space
    features_scaled_subset <- as.data.frame(features_scaled_subset)
    lda_projection <- predict(lda_model_ctrl, newdata = features_scaled_subset)
    LD1_values_df$LD1 <- lda_projection$x[,1]
    zprime_per_replicate_df[,i] <- z_prime_per_replicate(LD1_values_df, zprime_per_replicate_df)
    zprime_total_df[,i-1] <- z_prime_total(LD1_values_df)
  }

  names(zprime_per_replicate_df) <- c("Replicate",paste0("top", c(2:number_of_total_features)))
  names(zprime_total_df) <- c(paste0("top", c(2:number_of_total_features)))
  ld1_plot_list <- ld1_plot_list[-1]

  # plot of LD1 values
  e <- wrap_plots(ld1_plot_list)+
    plot_annotation(
      title = "LDA: WT vs Mut only ctrl wells"
    )
  print(e)

  m1 <- glue::glue("# Z-Prime per Replicate (ctrl samples) \n")
  cat(m1, "\n")
  df_long <- pivot_longer(zprime_per_replicate_df, !1, names_to = "top", values_to = "z_prime")
  df_long$top <- factor(df_long$top, level=paste0("top", c(2:number_of_total_features)))
  f <- ggplot(df_long, aes(x=top, y=z_prime))+
    geom_boxplot()+
    geom_point(aes(color=as.factor(Replicate)))+
    labs(color="Replicate")
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(f)

  m2 <- glue::glue("# Z-Prime for total experiment (ctrl samples) \n")
  cat(m2, "\n")
  df_total_long <- pivot_longer(zprime_total_df, everything(), names_to = "top", values_to = "z_prime")
  df_total_long$top <- factor(df_total_long$top, level=paste0("top", c(2:number_of_total_features)))
  g <- ggplot(df_total_long, aes(x=top, y=z_prime))+
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)

  top_row <- slice_max(df_total_long, z_prime, n = 1)
  n_feat <- top_row$top[[1]]
  message <- glue::glue("The best total z-prime is achieved by using the {n_feat} features.\n")
  cat(message, "\n")
  print(knitr::kable(top_row))
}
