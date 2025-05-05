#' Find column names and respective list for arguments given to create_plate_results
#' @param data_filtered_list list of data containing measurements per field of view, filtered for a feature
#' @param target_names target names that should be column names or partial column names of one of the measurements data frames
#' @return List with list name and column name for each target
#'
#' @export

find_partial_columns <- function(data_filtered_list, target_names) {
  target_names <- as.character(target_names)

  # Prepare an empty result list
  matches_result <- list()

  for (target in target_names) {
    found_match <- FALSE

    # Loop through each list element
    for (list_name in names(data_filtered_list)) {
      df <- data_filtered_list[[list_name]]
      if (!is.data.frame(df)) next  # Skip non-data.frames if any

      # Check for partial matches in the columns
      column_matches <- str_which(str_to_lower(names(df)), str_to_lower(target))

      if (length(column_matches) > 0) {
        # If there are multiple matches, use the first one (with warning)
        if (length(column_matches) > 1) {
          warning(paste0("Multiple column matches found for '", target, "' in '", list_name, "'. Using the first match: ", names(df)[column_matches[1]]))
        }

        matches_result[[target]] <- list(
          list_element = list_name,
          column_name = names(df)[column_matches[1]]
        )
        found_match <- TRUE
        break  # Stop after finding the first match
      }
    }

    if (!found_match) {
      warning(paste0("No match found for '", target, "' in any data.frame."))
      matches_result[[target]] <- NA
    }
  }

  return(matches_result)
}
