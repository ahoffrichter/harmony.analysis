#' Filter data per field of view by the number of nuclei present per field of view
#' @param data_list list of data containing measurements per field of view
#' @param feature_for_filter name of an element in data_list for which data should be filtered, default is "data_neuronal_nuclei"
#' @param data_filtered_list name of the filtered data_list
#' @param Compound Compound list including columns "Row", "Column", "Compound"
#' @return Statement, which Compounds were already missing in the input data and which compounds were removed in the filtering step
#'
#' @export



filtering_report_Compounds <- function(data_list, feature_for_filter, data_filtered_list, Compound){
  df_unfiltered <- unique(data_list[[feature_for_filter]][, c("Row", "Column", "Timepoint")]) |> left_join(Compound, by=c("Row", "Column"))
  df_filtered <- unique(data_filtered_list[[paste0(feature_for_filter, "_filtered")]][, c("Row", "Column", "Timepoint")]) |> left_join(Compound, by=c("Row", "Column"))
  Compounds_not_in_unfiltered <- setdiff(Compound$Compound,df_unfiltered$Compound)
  Compounds_removed <- setdiff(df_unfiltered$Compound, df_filtered$Compound)
  print(paste0("Compounds that were not in unfiltered data: ", paste(Compounds_not_in_unfiltered, collapse = ", ")))
  print(paste0("Compounds that were removed due to filtering for ", feature_for_filter, ": ", paste(Compounds_removed, collapse = ", ")))
}
