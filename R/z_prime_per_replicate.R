#' Create z-prime value for each replicate
#' @param LD1_df data.frame with LD1 value for each sample in a column called LD1, additionally cell_line, and Replicate columns have to be present
#' @return vector of zprime values per replicate
#'
#' @export
z_prime_per_replicate <- function(LD1_values_df, zprime_per_replicate_df){
  for(i in seq_along(unique(LD1_values_df$Replicate))){
    wt_ld1 <- LD1_values_df |> filter(Replicate==unique(LD1_values_df$Replicate)[i], cell_line=="WT", Compound == "ctrl")
    mut_ld1 <- LD1_values_df |> filter(Replicate==unique(LD1_values_df$Replicate)[i], cell_line=="MUT", Compound == "ctrl")
    zprime_per_replicate_df$zprime[i] <- 1 - (3 * (sd(wt_ld1$LD1) + sd(mut_ld1$LD1)) / abs(mean(wt_ld1$LD1) - mean(mut_ld1$LD1)))
  }
  return(zprime_per_replicate_df$zprime)
}
