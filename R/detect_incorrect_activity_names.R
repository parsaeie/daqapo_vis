#' Detect incorrect activity names
#'
#' Function returning the incorrect activity labels in the log as indicated by the user. If details are requested, the entire activity log's rows containing incorrect activities are returned.
#' @inheritParams detect_activity_frequency_violations
#' @param allowed_activities Vector with correct activity labels. If NULL, user input will be asked.
#' @return activitylog containing the rows of the original activity log having incorrect activity labels
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_incorrect_activity_names(activitylog = hospital_actlog,
#'      allowed_activities = c(
#'          "Registration",
#'          "Triage",
#'          "Clinical exam",
#'          "Treatment",
#'          "Treatment evaluation"))
#' }
#' @import shiny
#' @import miniUI
#' @export
#' @importFrom utils adist
#' @importFrom tidyr pivot_longer
#'
detect_incorrect_activity_names <- function(activitylog, allowed_activities, details, filter_condition) {
  UseMethod("detect_incorrect_activity_names")
  
}
#' @export
detect_incorrect_activity_names.activitylog <- function(activitylog, allowed_activities = NULL, details = TRUE, filter_condition = NULL){
  
  # Predefine variables
  activity <- NULL
  absolute_frequency <- NULL
  
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
  
  # Show all unique activity names to the user
  unique_names <- activitylog %>% activities() %>%
    mutate(label = str_c(!!activity_id_(activitylog), " (", absolute_frequency,")", sep = ""))
  # message("The following activities were detected in the activity log:", "\n")
  # print.data.frame(unique_names)
  
  if(is.null(allowed_activities)) {
    
    
    ui <- miniPage(
      gadgetTitleBar("Detect Incorrect activity names"),
      miniContentPanel(
        selectInput("activities", "Incorrect activities:", choices = unique_names$label, multiple = T)
      )
    )
    server <- function(input, output, session){
      observeEvent(input$done, {
        to_remove <- input$activities
        to_remove <- unique_names[unique_names$label %in% to_remove,] %>% pull(1) %>% as.character()
        n_to_remove <- length(to_remove)
        
        message("*** OUTPUT ***")
        message(n_to_remove, " out of ", nrow(unique_names), " (", round(n_to_remove / nrow(unique_names) * 100, 2), "% ) activity labels are identified to be incorrect.")
        
        if (n_to_remove > 0) {
          if (!details) {
            message("These activity labels are:\n")
            anomalies <- to_remove
          } else {
            message("These activity labels are:", "\n", paste(to_remove, collapse = " - "))
            anomalies <- activitylog %>% filter_activity(to_remove)
            message("Given this information, ", nrow(anomalies), " of ", nrow(activitylog), " (", round(nrow(anomalies) / nrow(activitylog) * 100, 2), "%) rows in the activity log are incorrect. These are the following:")
          }
          
        }
        stopApp(anomalies)
      })
    }
    runGadget(ui, server, viewer = dialogViewer("Detect Incorrect Activity Names", height = 400))
    
    # # Ask the user for input on the wrong names
    # wrong_indices <- readline("Please indicate by index and separated by a comma which names are incorrect (Enter N if everything is correct): ")
    # if(str_to_upper(wrong_indices) == "N") {
    #   to_remove <- NULL
    #   n_to_remove <- 0
    # } else {
    #   wrong_indices <- wrong_indices %>% str_split(",") %>% unlist() %>% str_squish() %>% as.integer()
    #     # Get the activity labels from the entered indices
    #   to_remove <- unique_names[wrong_indices,] %>% pull(1) %>% as.character()
    #
    # }
    
  } else {
    to_remove <- activity_labels(activitylog)[!(activity_labels(activitylog) %in% allowed_activities)] %>% as.character()
    n_to_remove <- length(to_remove)
    
    
    message("*** OUTPUT ***")
    
    #NEW Section >>>>>>>>>>>>>>>>>> Start
    ggplot_sringdist_heatmap_incor_act_nam(activitylog,allowed_activities) %>% print()
    #NEW Section >>>>>>>>>>>>>>>>>> END  
    
    message(n_to_remove, " out of ", nrow(unique_names), " (", round(n_to_remove / nrow(unique_names) * 100, 2), "% ) activity labels are identified to be incorrect.")
    if (n_to_remove > 0) {
      if (!details) {
        message("These activity labels are:\n")
        return(to_remove)
      } else {
        message("These activity labels are:", "\n", paste(to_remove, collapse = " - "))
        anomalies <- activitylog %>% filter_activity(to_remove)
        message("Given this information, ", nrow(anomalies), " of ", nrow(activitylog), " (", round(nrow(anomalies) / nrow(activitylog) * 100, 2), "%) rows in the activity log are incorrect. These are the following:")
        return(anomalies)
      }
    } else {
      return(NULL)
    }
  }
}

#NEW Section >>>>>>>>>>>>>>>>>> Start
ggplot_sringdist_heatmap_incor_act_nam <- function(activitylog, allowed_activities) {
  current_activity_labels <- NULL
  correct_activity_labels <- NULL
  distance_level <- NULL
  #extract activity labels from activity log
  activity_labels_vector <- activity_labels(activitylog)
  
  #calculate the similarity matrix
  distance_datafram <- utils::adist(activity_labels_vector, allowed_activities) %>% data.frame
  
  colnames(distance_datafram) <- allowed_activities
  
  distance_datafram <- distance_datafram %>%
    mutate(current_activity_labels = activity_labels_vector)
  
  #pivot matrix n*n to 3*(n^2)
  distance_datafram_pivot <-
    tidyr::pivot_longer(
      distance_datafram,
      cols = !current_activity_labels,
      names_to = "correct_activity_labels",
      values_to = "distance_level"
    )
  
  #ggplot object
  ggplot_heatmap <-
    distance_datafram_pivot %>%
    ggplot(aes(x = correct_activity_labels, y = current_activity_labels, fill = distance_level)) +
    geom_tile(color = "black") +
    coord_fixed() +
    scale_fill_gradient(low = "red4", high = "white") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(vjust = 6, hjust = 0.5),
      plot.title = element_text(
        vjust = 2,
        hjust = 0.5,
        size = 10
      )
    ) +
    labs(
      y = "Acivity Labels",
      x = "Allowed Activity Labels",
      fill = "String Distance",
      title = "String Distance Between Current and Allowed Activity Labels"
    )
  
  return(ggplot_heatmap)
}
#NEW Section >>>>>>>>>>>>>>>>>> END 