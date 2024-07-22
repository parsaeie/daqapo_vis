#' Check activity frequencies
#'
#' Function that detects activity frequency anomalies per case
#' @param activitylog The activity log
#' @param ... Named vectors with name of the activity, and value of the threshold.
#' @param details Boolean indicating whether details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return tbl_df providing an overview of cases for which activities are executed too many times
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_activity_frequency_violations(activitylog = hospital_actlog,
#'      "Registration" = 1, "Clinical exam" = 1)
#' }
#' @export
#' @import ggplot2 

detect_activity_frequency_violations <- function(activitylog, ..., details, filter_condition) {
  UseMethod("detect_activity_frequency_violations")
}

# @describeIn detect_activity_frequency_violations Detect activity frequency violations in activity log
#' @export

detect_activity_frequency_violations.activitylog <- function(activitylog, ... , details = TRUE, filter_condition = NULL) {
  
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
  
  # Unpack the parameters in the ellipsis
  params <- list(...)
  # Prepare the filter condition for anomaly detection
  anomaly_filter <- paste0(glue::glue("({activity_id(activitylog)} == '{names(params)}' & n > {params})"), collapse = " | ")
  
  n_cases <- n_cases(activitylog)
  
  # Case level: interesting activities are those that occur >= threshold times
  anomalies <- activitylog %>%
    filter_activity(names(params)) %>%
    group_by(!!case_id_(activitylog), !!activity_id_(activitylog)) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    filter(!!rlang::parse_expr(anomaly_filter))
  
  # Prepare output numbers
  n_anomalies <- anomalies %>% pull(!!case_id_(activitylog)) %>% unique() %>% length()
  n_anomalies_relative <- n_anomalies / n_cases * 100
  
  message("*** OUTPUT ***")
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start
  
  # Print plot
  
  ggplot_stacked_bar_act_ferq(activitylog = activitylog) %>% print()
  
  #NEW Section >>>>>>>>>>>>>>>>>> END
  
  
  message(glue::glue("For {n_anomalies} cases in the activity log ({n_anomalies_relative}%) an anomaly is detected."))
  
  if(details == TRUE & nrow(anomalies) > 0){
    message("The anomalies are spread over the following cases:", "\n")
    return(anomalies)
  }
}



#NEW Section >>>>>>>>>>>>>>>>>> Start

ggplot_stacked_bar_act_ferq <- function(activitylog) {
  # Predefined objects
  percentage <- NULL
  frequency <- NULL
  
  #extract the case and activity column as normal data frame
  case_activity <-
    activitylog %>%
    c() %>%
    data.frame() %>%
    select(case_id(activitylog), activity_id(activitylog))
  
  #rename the columns
  case_activity <-
    rename(
      case_activity,
      case_id = case_id(activitylog),
      activity_id = activity_id(activitylog)
    )
  
  
  #count instances of each activity based on the number of frequency in cases,
  case_activity_count <-
    case_activity %>%
    group_by(case_id, activity_id) %>%
    summarise(frequency = paste(n(), "time(s)"), .groups = 'drop') %>%
    group_by(frequency, activity_id) %>%
    summarise(count = n(), .groups = 'drop')
  
  #Add relative frequency
  case_activity_count <-
    case_activity_count %>%
    group_by(activity_id) %>%
    mutate(percentage = round(count /
                                sum(count), 3))
  
  
  #return ggplot object
  ggplotobject <- ggplot(case_activity_count,
                         aes(x = activity_id, y = count, fill = frequency)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(
      aes(
        label = paste0(percentage * 100, "%", sep = ""),
        colour = frequency
      ),
      position = position_fill(vjust = 0.5),
      stat = "identity",
      size = 3,
      show.legend = FALSE
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(
        vjust = 2,
        hjust = 0.5,
        size = 10
      )
    ) +
    labs(
      y = "Precentage of Cases",
      x = "Activities",
      fill = "Frequency",
      title = "Frequncy of Activity Instances"
    ) +
    
    theme(axis.title.y = element_text(vjust = 2)) +
    scale_fill_manual(
      values = c(
        "#253D58",
        "#C9A959",
        "#F8F8FF",
        "#AC8181",
        "#CFCECA",
        "green4",
        "yellow4",
        "purple4"
      ),
      aesthetics = "fill"
    ) +
    scale_fill_manual(
      values = c(
        "white",
        "black",
        "black",
        "black",
        "black",
        "black",
        "black",
        "white"
      ),
      aesthetics = "colour"
    )
  
  
  return(ggplotobject)
  
}
#NEW Section >>>>>>>>>>>>>>>>>> END
