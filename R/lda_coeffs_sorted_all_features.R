#' Create z-prime plot per replicate including increasing numbers of features in the calculation
#' @param features_control dataframe of scaled features per sample, only control samples
#' @param meta_control dataframe with metadata (Replicate, cell_line etc), only control samples
#' @return data frame with features sorted by the absolute LD1 value
#'
#' @export


lda_coeffs_sorted_all_features <- function(meta_control, features_control){
  lda_input_ctrl <- data.frame(cell_line = meta_control$cell_line, features_control)
  lda_input_ctrl$cell_line <- as.factor(lda_input_ctrl$cell_line)

  lda_model_ctrl <- lda(cell_line ~ ., data = lda_input_ctrl)
  lda_pred_ctrl <- predict(lda_model_ctrl)

  lda_coeffs <- as.data.frame(lda_model_ctrl$scaling)
  lda_coeffs$LD1_abs <- abs(lda_coeffs$LD1)
  lda_coeffs_sorted <- arrange(lda_coeffs, -LD1_abs)
  lda_coeffs_sorted$name <- rownames(lda_coeffs_sorted)
  return(lda_coeffs_sorted)
}

