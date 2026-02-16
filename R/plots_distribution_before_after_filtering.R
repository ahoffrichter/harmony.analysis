#' Plot data points per field of view as histogram
#' @param data_list list of data containing measurements per field of view
#' @param feature_for_filter name of an element in data_list for which data should be filtered, usually neuronal nuclei
#' @param min minimum number of filtering feature (nuclei) to keep field of view. default = 0
#' @param max maximum number of filtering feature (nuclei) to keep field of view. default = 200
#' @return Histograms showing the distribution of number of measurements per field of view
#'
#' @export

plots_distribution_before_after_filtering <- function(data_list, feature_for_filter, min=0, max=200){
  object_numbers_list <- list()
  for(i in seq_along(data_list)){
    object_numbers_list[[i]] <- dplyr::count(data_list[[i]], Row, Column, Field)
  }
  names(object_numbers_list) <- paste0("object_numbers_", sub("^data_", "",names(data_list)))

  plot_list <- list()
  for(i in seq_along(object_numbers_list)){
    plot_list[[i]] <- ggplot(object_numbers_list[[i]], aes(x=n))+
      geom_histogram()+
      ggtitle(sub("^data_", "",names(data_list)[[i]]))#+
    #geom_vline(xintercept = c(min, max), color= "red", linetype = "dashed")
  }

  for(i in seq_along(plot_list)){
    if (plot_list[[i]]$labels$title==sub("^data_", "", feature_for_filter))
      plot_list[[i]] <- plot_list[[i]]+
        geom_vline(xintercept = c(min, max), color= "red", linetype = "dashed")
  }


  before_filtering <- wrap_plots(plot_list, nrow = 1)

  # Check if the filter feature exists in data_list
  if (!feature_for_filter %in% names(data_list)) {
    stop("The specified feature_for_filter is not in data_list.")
  }

  # Get the data for that feature
  feature_data <- data_list[[feature_for_filter]]
  object_numbers_feature_for_filter <- dplyr::count(feature_data, Row, Column, Field)

  object_numbers_filtered_list <- list()
  object_numbers_filtered_list$filter <- object_numbers_feature_for_filter[object_numbers_feature_for_filter$n>min&object_numbers_feature_for_filter$n<max,]
  for(i in seq_along(object_numbers_list)[-which(names(object_numbers_list)==paste0("object_numbers_", sub("^data_", "",feature_for_filter)))]){
    object_numbers_filtered_list[[i]] <- object_numbers_list[[i]] |>
      semi_join(object_numbers_filtered_list$filter, by=c("Row", "Column", "Field"))
  }


  plot_list <- list()
  for(i in seq_along(object_numbers_filtered_list)){
    plot_list[[i]] <- ggplot(object_numbers_filtered_list[[i]], aes(x=n))+
      geom_histogram(color="black", fill="white")+
      ggtitle(sub("^data_", "",names(data_list)[[i]]))
  }


  after_filtering <- wrap_plots(plot_list, nrow = 1)
  before_filtering/after_filtering
}
