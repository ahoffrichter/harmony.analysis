#' Find list element names for arguments given to create_plate_results to calculate number of objects
#' @param data_filtered_list list of data containing measurements per field of view, filtered for a feature
#' @param target_names target names that should be column names or partial column names of one of the measurements data frames
#' @return List with list name and column name for each target
#'
#' @export
#'

find_partial_list_elements <- function(data_filtered_list, number_of_objects){
  ## Always work with a character vector
  target_names <- as.character(number_of_objects)

  # Store matched names
  matched_names <- character()

  for (i in seq_along(target_names)) {
    target <- target_names[i]

    # Perform case-insensitive matching
    matches <- str_which(str_to_lower(names(data_filtered_list)), str_to_lower(target))

    if (length(matches) == 0) {
      warning(paste0("No match found for '", target, "'."))
      next

    } else if (length(matches) > 1) {
      warning(paste0("Multiple matches found for '", target, "'. Using the first match: ", names(data_filtered_list)[matches[1]]))
      matched_names[i] <- names(data_filtered_list)[matches[1]]

    } else {
      matched_names[i] <- names(data_filtered_list)[matches]
      names(matched_names)[i] <- target
    }
  }
  return(matched_names)
}
