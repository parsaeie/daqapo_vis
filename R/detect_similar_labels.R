#' Search for similar labels in a column
#'
#' Function that tries to detect spelling mistakes in a given activity log column
#' @inheritParams detect_activity_frequency_violations
#' @param column_labels The name of the column(s) in which to search for spelling mistakes
#' @param max_edit_distance The maximum number of insertions, deletions and substitutions that are allowed to be executed in order for two strings to be considered similar.
#' @param show_NA A boolean indicating if labels that do not show similarities with others should be shown in the output
#' @param ignore_capitals A boolean indicating if capitalization should be included or excluded when calculating the edit distance between two strings
#' @return tbl_df providing an overview of similar labels for the indicated column
#' @examples
#' \donttest{
#' data("hospital_actlog")
# detect_similar_labels(activitylog = hospital_actlog,
#      column_labels = "activity",
#      max_edit_distance = 3)
#' }
#' @export


detect_similar_labels <- function(activitylog, column_labels, max_edit_distance, show_NA,ignore_capitals, filter_condition) {
  UseMethod("detect_similar_labels")
}

#' @export

detect_similar_labels.activitylog <- function(activitylog, column_labels, max_edit_distance = 3, show_NA = FALSE, ignore_capitals = FALSE, filter_condition = NULL) {

  # Predefined variables
  similar_to <- NULL
  data <- NULL
  labels_x <- NULL
  lebels_y <- NULL
  distance_level <- NULL
  violate_max_dis <- NULL
  
  if (!inherits(column_labels, "character")) {
    stop("column_labels must be a character vector")
  }

  if(!all(column_labels %in% names(activitylog))) {
    warning(glue("Some provided column labels don't exist and will be ignored: {str_c(column_labels[!(column_labels %in% names(activitylog))], collapse = ', ')}"))
    column_labels <- column_labels[(column_labels %in% names(activitylog))]
  }


  # Apply filter condition when specified
  filter_specified <- FALSE
  tryCatch({
    is.null(filter_condition)
  }, error = function(e) {
    filter_specified <<- TRUE
  }
  )

  if(!filter_specified) {
    # geen filter gespecifieerd.

  } else {
    filter_condition_q <- enquo(filter_condition)
    activitylog <- APPLY_FILTER(activitylog, filter_condition_q = filter_condition_q)
  }


  activitylog %>%
    select_at(.vars = column_labels) %>%
    map(class) -> classes


  if(!all(classes %in% c("character","factor"))) {
    warning(glue("Not all provided columns are of type character or factor and will be ignored: {str_c(classes[!(classes %in% c('character','factor'))] %>% names(), collapse = ',')}"))
    a <- classes[classes %in% c("character","factor")] %>% names
  }

  if(length(a) < 1) {
    stop("column_labels must be a vector of one or more valid column labels of type factor or character.")
  }


  similarities <- tibble(column_labels) %>%
    mutate(data = map(column_labels, gather_similar_labels,
                         activitylog = activitylog,
                         ignore_capitals = ignore_capitals,
                         max_edit_distance = max_edit_distance)) %>%
    unnest(data)

  if(!show_NA){
    similarities <- similarities %>% filter(!is.na(similar_to))
  }
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start

  #extract activity labels from activity log
  
  for (i in 1:length(column_labels)) {
    if (inherits(activitylog[[column_labels[i]]], "character")) {
      labels <- unique(activitylog[[column_labels[i]]])
      
    } else if (inherits(activitylog[[column_labels[i]]], "factor")) {
      labels <- levels(activitylog[[column_labels[i]]])
    }
    
    if (!show_NA) {
      labels <- labels[!is.na(labels)]
    }
    ggplot_sringdist_heatmap_sim_lab(labels, ignore_capitals, max_edit_distance,column_labels[i]) %>% print()
  }

  #NEW Section >>>>>>>>>>>>>>>>>> END 
  
  return(similarities)

}

gather_similar_labels <- function(variable, activitylog, ignore_capitals = ignore_capitals, max_edit_distance = max_edit_distance) {

  if(inherits(activitylog[[variable]], "character")) {
    labels <- unique(activitylog[[variable]])

  } else if (inherits(activitylog[[variable]], "factor")) {
    labels <- levels(activitylog[[variable]])
  }

  similars <- detect_similar_labels_COLUMN(labels = labels, max_edit_distance = max_edit_distance, ignore_capitals = ignore_capitals)

  tibble(labels) %>%
    mutate(similar_to = similars) %>% 
    return()
}

detect_similar_labels_COLUMN <- function(labels, max_edit_distance, ignore_capitals) {
  x <- labels
  out <- character(length = length(labels))

  for(i in seq_along(labels)){
    # Results holds all similar labels for this specific iteration
    results <- c()

    # NA cannot have similarities, so skip it altogether
    if ( !is.na(x[i]) & !is.null(x[i]) ) {

      # x will be the current label. Iterate over all others to compute similarties
      for(label in labels){
        # Set the variables to compare depending on ignore_capitals
        if(!ignore_capitals) {
          compare_x <- x[i]
          compare_label <- label
        } else {
          compare_x <- str_to_lower(x[i])
          compare_label <- str_to_lower(label)
        }
        # Make the comparison
        if( (compare_x != compare_label) & (ain(compare_x, compare_label, maxDist = max_edit_distance) == TRUE) ) {
          # If the comparison is found to be positive, add it to the results
          results <- results %>% append(label)
        }
      }
    }

    # Results holds labels for this iteration. Add it to the general output
    if(!is.null(results)){
      out[i] <- paste(results, collapse = " - ")
    } else {
      out[i] <- NA
    }

  }
  return(out)
}


ggplot_sringdist_heatmap_sim_lab <-
  function(labels,
           ignore_capitals,
           max_edit_distance,
           column_label) {
    labels_x <- NULL
    lebels_y <- NULL
    distance_level <- NULL
    violate_max_dis <- NULL
    
    labels <- labels %>% replace(is.na(.), " ")
    
    #calculate the similarity matrix
    similarity_datafram <-
      adist(labels, labels, ignore.case = ignore_capitals) %>% data.frame()
    colnames(similarity_datafram) <- labels
    
    #transform row labels as column
    similarity_datafram <- similarity_datafram %>%
      mutate(labels_x = labels)
    
    #pivot matrix n*n to 3*(n^2)
    similarity_datafram_pivot <- pivot_longer(
      similarity_datafram,
      cols = !labels_x,
      names_to = "lebels_y",
      values_to = "distance_level"
    )
    
    similarity_datafram_pivot <-
      similarity_datafram_pivot %>% mutate(violate_max_dis = case_when(distance_level <= max_edit_distance ~ "Similar"))
    
    #ggplot object
    detect_incorrect_activity_names_heatmap <-
      similarity_datafram_pivot %>%
      ggplot(aes(x = labels_x,
                 y = lebels_y,
                 fill = distance_level)) +
      geom_tile(color = "black") +
      geom_point(
        aes(x = labels_x,
            y = lebels_y,
            shape = violate_max_dis),
        show.legend = F,
        na.rm = T
      ) +
      scale_fill_gradient(low = "red4",
                          high = "white") +
      theme(
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.title = element_text(vjust = 6,
                                    hjust = 0.5),
        plot.title = element_text(
          vjust = 2,
          hjust = 0.5,
          size = 10
        )
      ) +
      labs(
        fill = "String Distance",
        x = element_blank(),
        y = element_blank(),
        title = paste0("String Distance of Labels in Column: ", column_label)
      )
    
    return(detect_incorrect_activity_names_heatmap)
  }
#NEW Section >>>>>>>>>>>>>>>>>> END 