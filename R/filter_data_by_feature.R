#' Filter data per field of view by the number of nuclei present per field of view
#' @param data_list list of data containing measurements per field of view
#' @param feature_for_filter name of an element in data_list for which data should be filtered, default is "data_neuronal_nuclei"
#' @param min minimum number of nuclei to keep field of view. default = 0
#' @param max maximum number of nuclei to keep field of view. default = 200
#' @return List of filtered data for each of the five measurements
#'
#' @export


filter_data_by_feature <- function(data_list, feature_for_filter = "data_neuronal_nuclei", min=0, max=200){
  object_numbers_feature_for_filter <- count(data_list[[feature_for_filter]], Row, Column, Field)
  filter_numbers <- object_numbers_feature_for_filter[object_numbers_feature_for_filter$n>min&object_numbers_feature_for_filter$n<max,]
  data_filtered_list <- list()
  for(i in seq_along(data_list)){
    data_filtered_list[[i]] <- semi_join(data_list[[i]], filter_numbers, by=c("Row", "Column", "Field"))
  }
  names(data_filtered_list) <- paste0(names(data_list), "_filtered")
  # data_filtered_list$data_neuronal_nuclei_filtered <- semi_join(data_list$data_neuronal_nuclei, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_SV2_neurites_filtered <- semi_join(data_list$data_SV2_neurites, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_coloc_punctae_filtered <- semi_join(data_list$data_coloc_punctae, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_homer_spots_filtered <- semi_join(data_list$data_homer_spots, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_neurite_segments_filtered <- semi_join(data_list$data_neurite_segments, filter_nnn, by=c("Row", "Column", "Field"))
  return(data_filtered_list)
}
