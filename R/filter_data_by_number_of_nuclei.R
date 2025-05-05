#' Filter data per field of view by the number of nuclei present per field of view
#' @param data_list list of data containing five list elements (data_neuronal_nuclei, data_SV2_neurites, data_coloc_punctae, data_homer_spots, data_neurite_segemnts)
#' @param min minimum number of nuclei to keep field of view. default = 0
#' @param max maximum number of nuclei to keep field of view. default = 200
#' @return List of filtered data for each of the five measurements
#'
#' @export


filter_data_by_number_of_nuclei <- function(data_list, min=0, max=200){
  object_numbers_neuronal_nuclei <- count(data_list$data_neuronal_nuclei, Row, Column, Field)
  filter_nnn <- object_numbers_neuronal_nuclei[object_numbers_neuronal_nuclei$n>min&object_numbers_neuronal_nuclei$n<max,]
  data_filtered_list <- list()
  for(i in seq_along(data_list)){
    data_filtered_list[[i]] <- semi_join(data_list[[i]], filter_nnn, by=c("Row", "Column", "Field"))
  }
  names(data_filtered_list) <- paste0(names(data_list), "_filtered")
  # data_filtered_list$data_neuronal_nuclei_filtered <- semi_join(data_list$data_neuronal_nuclei, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_SV2_neurites_filtered <- semi_join(data_list$data_SV2_neurites, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_coloc_punctae_filtered <- semi_join(data_list$data_coloc_punctae, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_homer_spots_filtered <- semi_join(data_list$data_homer_spots, filter_nnn, by=c("Row", "Column", "Field"))
  # data_filtered_list$data_neurite_segments_filtered <- semi_join(data_list$data_neurite_segments, filter_nnn, by=c("Row", "Column", "Field"))
  return(data_filtered_list)
}
