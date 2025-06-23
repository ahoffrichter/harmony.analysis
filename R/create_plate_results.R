#' Creating plate results from FOV filtered lists
#' @param data_filtered_list list of data containing measurements per field of view, filtered for a feature
#' @param feature_for_filter name of an element in data_list for which data should be filtered, should be the same as used in filter_data_by_feature
#' @param objective name of the objective used for the analysis ("objective_40x", "objective_63x")
#' @param number_of_objects character vector of features that should be counted
#' @param Mean_per_Well character vector containing names or partial names of the columns of which the mean should be calculated
#' @param StdDev_per_Well character vector containing names or partial names of the columns of which the standard deviation should be calculated
#' @param Sum_per_Well character vector containing names or partial names of the columns of which the sum should be calculated
#' @param Max_per_Well character vector containing names or partial names of the columns of which the max should be calculated
#' @param Min_per_Well character vector containing names or partial names of the columns of which the min should be calculated
#' @param Median_per_Well character vector containing names or partial names of the columns of which the median should be calculated
#' @param spots_per_length_neurite haracter vector containing names or partial names of feature should for which number per neurite length should be calculated
#' @param Neurite_Length_per_neuron TRUE or FALSE, should the neurite length per neuron be calculated. Default is NULL.
#' @param ... additional arguments that can be passed to the function in order to add them to plate results. E.g. Cell_line, Compound, Replicate
#' @return data frame with plate results per well
#' @export


create_plate_results <- function(data_filtered_list,
                                 feature_for_filter = feature_for_filter,
                                 objective = c("objective_40x", "objective_63x", "objective_20x"),
                                 number_of_objects = NULL,
                                 Mean_per_Well = NULL,
                                 StdDev_per_Well = NULL,
                                 Sum_per_Well = NULL,
                                 Max_per_Well = NULL,
                                 Min_per_Well = NULL,
                                 Median_per_Well = NULL,
                                 spots_per_length_neurite = NULL,
                                 spots_per_100_um_neurite_length = NULL,
                                 Neurite_Length_per_neuron = NULL,
                                 ...){
  objective <- match.arg(objective)
  chosen_objective <- switch(objective,
                             objective_40x = 0.2967,
                             objective_63x = 0.18837,
                             objective_20x = 0.59337)
  args <- list(...)  # capture all additional named arguments
  # replace NA values with median
  data_filtered_list <- map(data_filtered_list, ~ {
    .x |>
      mutate(across(where(is.numeric), ~ replace(., is.na(.), median(., na.rm = TRUE))))
  })

  df <- unique(data_filtered_list[[paste0(feature_for_filter, "_filtered")]][,c("Row","Column","Timepoint")])
  rownames(df) <- NULL
  n_rows <- nrow(df)

  # ... arguments
  for (name in names(args)) {
    value <- args[[name]]

    if (length(value) == 1) {
      # Single value -> repeat it for all rows
      df[[name]] <- rep(value, n_rows)

    } else if (is.data.frame(value)) {
      # Vector with correct length
      df <- df |> left_join(value |>
                              group_by(Row, Column))

    } else {
      # Wrong length
      stop(paste0("Length of argument '", name, "' (", length(value),
                  ") does not match number of rows in data (", n_rows, ")."))
    }
  }

  # Number of objects

  if (!is.null(number_of_objects)){
    matched_names <- find_partial_list_elements(data_filtered_list, number_of_objects)

    for(i in seq_along(matched_names)){
      df <- df |> left_join(data_filtered_list[[matched_names[i]]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(names(matched_names)[i], "_number_of_objects") := n()))
    }
  }


  # Mean per Well
  if (!is.null(Mean_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, Mean_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_Mean_per_Well") := mean(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # Sum per Well
  if (!is.null(Sum_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, Sum_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_Sum_per_Well") := sum(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # Max_per_Well
  if (!is.null(Max_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, Max_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_Max_per_Well") := max(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # Min_per_Well
  if (!is.null(Min_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, Min_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_Min_per_Well") := min(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # StdDev_per_Well
  if (!is.null(StdDev_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, StdDev_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_StdDev_per_Well") := sd(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # Median_per_Well
  if (!is.null(Median_per_Well)){
    targets_list <- find_partial_columns(data_filtered_list, Median_per_Well)
    for(i in seq_along(targets_list)){
      df <- df |> left_join(data_filtered_list[[targets_list[[i]]$list_element]] |>
                              group_by(Row, Column, Timepoint) |>
                              summarise(!!paste0(targets_list[[i]]$column_name, "_Median_per_Well") := median(.data[[targets_list[[i]]$column_name]])))
    }
  }

  # spots_per_length_neurite

  if (!is.null(spots_per_length_neurite)){
    # targets_list <- find_partial_columns(data_filtered_list, spots_per_length_neurite)
    df <- df |> left_join(data_filtered_list$data_neurite_segments_filtered |>
                            group_by(Row, Column, Timepoint) |>
                            summarise(Neurite.Segments...Segment.Length_Sum_per_Well=sum(Neurite.Segments...Segment.Length)))
    target_names <- as.character(spots_per_length_neurite)

    # Store matched names
    matched_names <- character()

    for (i in seq_along(target_names)) {
      target <- target_names[i]

      # Perform case-insensitive matching
      matches <- str_which(str_to_lower(names(df)), str_to_lower(target))
      if (length(matches) == 0) {
        warning(paste0("No match found for '", target, "'."))
        next

      } else if (length(matches) > 1) {
        warning(paste0("Multiple matches found for '", target, "'. Using the first match: ", names(df)[matches[1]]))
        matched_names[i] <- names(df)[matches[1]]

      } else {
        matched_names[i] <- names(df)[matches]
        names(matched_names)[i] <- target
      }
    }

    for(i in seq_along(spots_per_length_neurite)){
      df <- df |>
        mutate(!!paste0(spots_per_length_neurite[i], "_per_length_neurite") := .data[[matched_names[i]]]/(.data[["Neurite.Segments...Segment.Length_Sum_per_Well"]]*chosen_objective)*100)
    }
  }

  # Neurite_Length_per_neuron
  if(isTRUE(Neurite_Length_per_neuron)){
    df <- df |> mutate(Neurite_Length_per_neuron=(Neurite.Segments...Segment.Length_Sum_per_Well*chosen_objective)/neuronal_nuclei_number_of_objects)
  }

  # Analyzed fields (Neuronal nuclei)

  df <- df |> left_join(data_filtered_list[[paste0(feature_for_filter, "_filtered")]] |>
                          group_by(Row, Column, Timepoint) |>
                          summarise(Analyzed_fields = length(unique(Field))))

  return(df)
}
