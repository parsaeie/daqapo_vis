#' Detect multi-registration
#'
#' Function detecting multi-registration for the same case or by the same resource at the same point in time
#' @param activitylog The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param level_of_aggregation Level of aggregation at which multi-registration should be detected (either "resource" or "case")
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both")
#' @param threshold_in_seconds Threshold which is applied to determine whether multi-registration occurs (expressed in seconds) (time gaps smaller than threshold are considered as multi-registration)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return activitylog containing the rows of the original activity log for which multi-registration is present
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_multiregistration(activitylog = hospital_actlog, threshold_in_seconds = 10)
#' }
#' @export
#' @importFrom ggpubr ggarrange


detect_multiregistration <- function(activitylog,
                                     level_of_aggregation,
                                     timestamp,
                                     threshold_in_seconds,
                                     details,
                                     filter_condition) {
  UseMethod("detect_multiregistration")
}

#' @export

detect_multiregistration.activitylog <- function(activitylog,
                                                 level_of_aggregation = c("resource", "case"),
                                                 timestamp = c("complete","start","both"),
                                                 threshold_in_seconds,
                                                 details = TRUE,
                                                 filter_condition = NULL){
  
  # Predefine variables
  less_than_th <- NULL
  also_include <- NULL
  prior_start <- NULL
  prior_resource <- NULL
  time_gap <- NULL
  next_less_than_th <- NULL
  next_resource <- NULL
  prior_complete <- NULL
  prior_case <- NULL
  next_case <- NULL
  resource <- NULL
  start <- NULL
  complete <- NULL
  case_id <- NULL
  
  # Initiate warning variables
  warning.filtercondition <- FALSE
  
  
  level_of_aggregation <- match.arg(level_of_aggregation)
  timestamp <- match.arg(timestamp)
  
  
  
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
  
  
  # Determine whether multi-registration is present
  if(level_of_aggregation == "resource"){
    multi_reg <- detect_multiregistration_resource(activitylog, timestamp, threshold_in_seconds)
  } else{
    multi_reg <- detect_multiregistration_case(activitylog, timestamp, threshold_in_seconds)
  }
  
  # Prepare_output
  pct_resource <- round(length(unique(multi_reg[[resource_id(activitylog)]])) / length(unique(activitylog[[resource_id(activitylog)]])) * 100,2)
  pct_case <- round(length(unique(multi_reg[[case_id(activitylog)]])) / length(unique(activitylog[[case_id(activitylog)]])) * 100,2)
  
  # Print output
  #>>>>>>>>>>>>>>>>>>>>>>>New Section Start
  if (nrow(multi_reg) != 0) {
    ID1 <- case_id(activitylog)
    ID2 <- activity_id(activitylog)
    ID3 <- resource_id(activitylog)
    ID4 <- timestamp(activitylog)
    
    no_multi_reg <- dplyr::anti_join(activitylog, multi_reg, by = join_by(!!ID1, !!ID2, !!ID3, !!ID4[1]))
    
    combined_process_or_resource_map_multi_reg(activitylog, no_multi_reg, level_of_aggregation) %>% print()
    
    ggplot_double_case_trace_multi_reg(activitylog, no_multi_reg, level_of_aggregation)  %>% print()
  }
  #>>>>>>>>>>>>>>>>>>>>>>>New Section END
  
  message("Selected level of aggregation: ", level_of_aggregation)
  message("Selected timestamp parameter value: ", timestamp, "\n")
  
  message("*** OUTPUT ***")
  
  if(level_of_aggregation == "resource"){
    message("Multi-registration is detected for ", length(unique(multi_reg[[resource_id(activitylog)]])), " of the ", length(unique(activitylog[[resource_id(activitylog)]])), " resources (", pct_resource, "%). These resources are:")
    message(paste(unique(multi_reg[[resource_id(activitylog)]]), collapse = " - "))
    cat("\n")
  } else{
    message("Multi-registration is detected for ", length(unique(multi_reg[[case_id(activitylog)]])), " of the ", length(unique(activitylog[[case_id(activitylog)]])), " cases (", pct_case, "%) of the cases. These cases are:")
    message(paste(unique(multi_reg[[case_id(activitylog)]]), collapse = " - "))
    message("\n")
  }
  
  multi_reg <- re_map(multi_reg, mapping(activitylog))
  
  if(details == TRUE){
    if(level_of_aggregation == "resource" & pct_resource > 0){
      message("For the following rows in the activity log, multi-registration is detected:")
      return(multi_reg)
    } else if(level_of_aggregation == "case" & pct_case > 0){
      message("For the following rows in the activity log, multi-registration is detected:")
      return(multi_reg)
    }
  }
}

detect_multiregistration_resource <- function(activitylog, timestamp, threshold_in_seconds) {
  less_than_th <- NULL
  also_include <- NULL
  prior_start <- NULL
  prior_resource <- NULL
  time_gap <- NULL
  next_less_than_th <- NULL
  next_resource <- NULL
  prior_complete <- NULL
  prior_case <- NULL
  next_case <- NULL
  resource <- NULL
  start <- NULL
  complete <- NULL
  case_id <- NULL
  
  if(timestamp == "start"){
    activity_log <- activitylog %>%
      group_by(!!resource_id_(activitylog)) %>%
      arrange(start) %>%
      mutate(prior_start = lag(start)) %>%
      mutate(time_gap = as.double(start - prior_start, units = "secs"))
    
  } else if(timestamp == "complete"){
    activity_log <- activitylog %>%
      group_by(!!resource_id_(activitylog)) %>%
      arrange(complete) %>%
      mutate(prior_complete = lag(complete)) %>%
      mutate(time_gap = as.double(complete - prior_complete, units = "secs"))
    
  } else{
    activity_log <- activitylog %>%
      group_by(!!resource_id_(activitylog)) %>%
      arrange(start, complete) %>%
      mutate(prior_start = lag(start),
             prior_complete = lag(complete)) %>%
      mutate(time_gap = as.double(start - prior_complete, units = "secs"))
    
  }
  
  activity_log %>%
    mutate(less_than_th = time_gap <= threshold_in_seconds) %>%
    mutate(also_include = lead(less_than_th, default = F)) -> multi_reg
  
  # # Determine lines that qualify as multi-registration
  # multi_reg <- activity_log
  # multi_reg$less_than_th <- multi_reg$resource == multi_reg$prior_resource & multi_reg$time_gap <= threshold_in_seconds
  # # Following lines ensure that the first line in case of multi-registration is also recorded
  # multi_reg$next_less_than_th <- c(multi_reg$less_than_th[-1], NA)
  # multi_reg$next_resource <- c(multi_reg$resource[-1], NA)
  # multi_reg$also_include <- multi_reg$resource == multi_reg$next_resource & multi_reg$next_less_than_th == TRUE
  #
  if(timestamp == "start"){
    multi_reg <- multi_reg %>%
      filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_start, time_gap, less_than_th, also_include))
  } else if(timestamp == "complete"){
    multi_reg <- multi_reg %>%
      filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_complete, time_gap, less_than_th, also_include))
  } else{
    multi_reg <- multi_reg %>%
      filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_start, prior_complete, time_gap, less_than_th, also_include))
  }
  return(multi_reg)
}

detect_multiregistration_case <- function(activitylog, timestamp, threshold_in_seconds) {
  less_than_th <- NULL
  also_include <- NULL
  prior_start <- NULL
  prior_resource <- NULL
  time_gap <- NULL
  next_less_than_th <- NULL
  next_resource <- NULL
  prior_complete <- NULL
  prior_case <- NULL
  next_case <- NULL
  resource <- NULL
  start <- NULL
  complete <- NULL
  case_id <- NULL
  
  if(timestamp == "start"){
    activity_log <- activitylog %>%
      group_by(!!case_id_(activitylog)) %>%
      arrange(start) %>%
      mutate(
        prior_start = lag(start)
      ) %>%
      mutate(time_gap = as.double(start - prior_start, units = "secs"))
    
  } else if(timestamp == "complete"){
    activity_log <- activitylog %>%
      group_by(!!case_id_(activitylog)) %>%
      arrange(complete) %>%
      mutate(
        prior_complete = lag(complete)
      ) %>%
      mutate(time_gap = as.double(complete - prior_complete, units = "secs"))
    
  } else{
    activity_log <- activitylog %>%
      group_by(!!case_id(activitylog)) %>%
      arrange(start,complete) %>%
      mutate(
        prior_start = lag(start),
        prior_complete = lag(complete)
      ) %>%
      mutate(time_gap = as.double(start - prior_complete, units = "secs"))
  }
  
  activity_log %>%
    mutate(less_than_th = time_gap <= threshold_in_seconds) %>%
    mutate(also_include = lead(less_than_th, default = F)) -> multi_reg
  
  if(timestamp == "start"){
    multi_reg <- multi_reg %>%
      filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_start, time_gap, less_than_th, also_include))
  } else if(timestamp == "complete"){
    multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_complete, time_gap, less_than_th, also_include))
  } else{
    multi_reg <- multi_reg %>%
      filter(less_than_th == TRUE | also_include == TRUE) %>%
      select(-c(prior_start,prior_complete, time_gap, less_than_th, also_include))
  }
  return(multi_reg)
}

#>>>>>>>>>>>>>>>>>>>>>>>New Section Start
ggplot_double_case_trace_multi_reg<- function(activitylog, no_multi_reg, level_of_aggregation) {
  max_trace <- trace_length(activitylog, "log") %>% pull(max)
  
  if(level_of_aggregation == "resource"){
    activitylog <- activitylog %>% 
      set_activity_id(resource_id(activitylog)) %>% 
      set_resource_id(activity_id(activitylog))
    
    no_multi_reg <- no_multi_reg %>% 
      set_activity_id(resource_id(activitylog)) %>% 
      set_resource_id(activity_id(activitylog))
  }
  
  
  
  traces_activitylog <- activitylog %>% trace_explorer(
    n_traces = 10,
    coverage_labels = c("absolute", "relative", "cumulative")
  )
  
  traces_activitylog <- traces_activitylog +
    coord_cartesian(xlim = c(0.5, max_trace[1] + 0.5)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank()
    ) +
    labs(y = "Full Activity Log")
  
  traces_no_multi_reg <- no_multi_reg %>% trace_explorer(
    n_traces = 10,
    coverage_labels = c("absolute", "relative", "cumulative")
  )
  
  max_trace <- trace_length(activitylog, "log") %>% pull(max)
  
  traces_no_multi_reg <- traces_no_multi_reg +
    coord_cartesian(xlim = c(0.5, max_trace[1] + 0.5)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank()
    ) +
    labs(y = "Activity Log without Multi. Reg.")
  
  ggpubr::ggarrange(
    traces_activitylog,
    traces_no_multi_reg,
    ncol = 1,
    nrow = 2,
    common.legend = TRUE ,
    align = "v"
  ) %>% return()
}

combined_process_or_resource_map_multi_reg <- function(activitylog,
                                             no_multi_reg,
                                             level_of_aggregation) {
  if (level_of_aggregation == "case") {
    processs_map_activitylog <- activitylog %>%
      process_map(frequency("relative_case"), sec = frequency("absolute_case"))
    
    processs_map_no_multi_reg <- no_multi_reg %>%
      process_map(frequency("relative_case"), sec = frequency("absolute_case"))
    header <- tags$div(
      "From Top to Bottom: Process map of All Cases and Without Multiregistration",
      style = "justify-content: center;
                     align-items: center;text-align: center;"
    ) %>% tags$br() %>% tags$br()
    
  } else if (level_of_aggregation == "resource") {
    processs_map_activitylog <- activitylog %>%
      resource_map(frequency("relative_case"), sec = frequency("absolute_case"))
    
    processs_map_no_multi_reg <- no_multi_reg %>%
      resource_map(frequency("relative_case"), sec = frequency("absolute_case"))
    header <- tags$div(
      "From Top to Bottom: Resource map of All Cases and Without Multiregistration",
      style = "justify-content: center;
                     align-items: center;text-align: center;"
    ) %>% tags$br() %>% tags$br()
  }
  
  manipulateWidget::combineWidgets(
    processs_map_activitylog,
    processs_map_no_multi_reg,
    ncol = 1,
    nrow = 2,
    header = header,
    byrow = T,
    height  = 500
  ) %>% return()
}
#>>>>>>>>>>>>>>>>>>>>>>>New Section Start
