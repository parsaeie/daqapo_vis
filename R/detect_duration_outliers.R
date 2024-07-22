#' Detect activity duration outliers
#'
#' Function detecting duration outliers for a particular activity
#' @inheritParams detect_activity_frequency_violations
#' @param ... for each activity to be checked, an argument "activity_name" = duration_within(...) to define bounds. See ?duration_within
#' @return activitylog containing the rows of the original activity log for which activity duration outliers are detected
#' Information on the presence of activity duration outliers
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_duration_outliers(activitylog = hospital_actlog,
#'      Treatment = duration_within(bound_sd = 1))
#' }
#' @seealso \code{\link{duration_within}}
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @export

detect_duration_outliers <- function(activitylog,
                                     ...,
                                     details,
                                     filter_condition) {
  UseMethod("detect_duration_outliers")
}
#' @export
detect_duration_outliers.activitylog <- function(activitylog,
                                                 ...,
                                                 details = TRUE,
                                                 filter_condition = NULL){
  
  # Predefine variables
  duration <- NULL
  activity <- NULL
  `<named list>` <- NULL
  value <- NULL
  act <- NULL
  complete <- NULL
  start <- NULL
  bound_sd <- NULL
  lower_bound <- NULL
  upper_bound <- NULL
  
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
  
  
  
  params <- list2(...)
  
  tibble(act = names(params), params) %>%
    mutate(params = map(params, tibble)) %>%
    mutate(params = map(params, ~mutate(.x, names = names(`<named list>`)))) %>%
    unnest(params) %>%
    mutate(value = as.numeric(`<named list>`)) %>%
    select(act, names,  value) %>%
    spread(names, value)  -> params
  
  
  if(all(!(params$act %in% activity_labels(activitylog)))) {
    stop("None of the activities were found in the data. Perhaps you misspelled them?")
  } else if(any(!(params$act %in% activity_labels(activitylog)))) {
    warning("Some activities not found in log: {str_c(params$act[!(params$act %in% activity_labels_activitylog)],collapse = ', ')}.")
  }
  
  activitylog %>%
    filter_activity(activities = params$act) %>%
    mutate(duration = as.double(complete - start, units = "mins")) -> activitylog_durations
  
  # Determine whether warning for negative durations is required
  if(nrow(activitylog_durations %>% filter(duration < 0)) > 0){
    warning("Negative durations detected. Check function time_anomalies for more details.")
  }
  
  activitylog_durations %>%
    group_by(!!activity_id_(activitylog)) %>%
    summarize(mean = mean(duration, na.rm = T), sd = sd(duration, na.rm = T)) %>%
    mutate_at(activity_id(activitylog), as.character) -> activitylog_durations_summary
  
  colnames(params)[colnames(params) == "act"] <- activity_id(activitylog)
  
  activitylog_durations_summary %>%
    inner_join(params, by = activity_id(activitylog)) %>%
    group_by(!!activity_id_(activitylog)) %>%
    mutate(lower_bound = ifelse(is.na(lower_bound), max(0, mean-bound_sd*sd), lower_bound),
           upper_bound = ifelse(is.na(upper_bound), mean+bound_sd*sd, upper_bound)) -> activitylog_durations_summary
  
  activitylog_durations %>%
    mutate_at(activity_id(activitylog), as.character) %>%
    inner_join(activitylog_durations_summary, by = activity_id(activitylog)) -> outliers
  
  
  # Outlier determination
  outliers <- outliers %>% filter(duration < lower_bound | duration > upper_bound)
  
  # Print output
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start
  for (i in 1:nrow(params)) {
    upper_bound <- round(activitylog_durations_summary$lower_bound[i], 2)
    lower_bound <- round(activitylog_durations_summary$upper_bound[i], 2)
    time_series_ggplot_outlier(
      dplyr::filter(
        activitylog_durations,
        !!activity_id_(activitylog) == params$activity[i]
      )
      ,
      upper_bound,
      lower_bound,
      params$activity[i]
    ) %>% plotly::ggplotly() %>% print()
  }
  #NEW Section >>>>>>>>>>>>>>>>>> END
  
  message("*** OUTPUT ***")
  message("Outliers are detected for following activities")
  for(i in seq_len(nrow(activitylog_durations_summary)))
    message(glue("{activitylog_durations_summary[i,1]} \t Lower bound: {round(activitylog_durations_summary$lower_bound[i], 2)} \t Upper bound: {round(activitylog_durations_summary$upper_bound[i],2)}"))
  message("A total of ", nrow(outliers)," is detected (", round(nrow(outliers) / nrow(activitylog) * 100, 2), "% of the activity executions)")
  
  if(details == TRUE){
    if(nrow(outliers) > 0){
      message("For the following activity instances, outliers are detected:")
      return(outliers)
    }
  }
}

#NEW Section >>>>>>>>>>>>>>>>>> Start

time_series_ggplot_outlier <- function(activitylog_durations,
                               upper_bound,
                               lower_bound,
                               activity) {
  start <- NULL
  duration <- NULL
  
  ggplot(activitylog_durations, aes(x = start, y = duration)) +
    geom_point() +
    xlab("") +
    geom_line(color = "#253D58") +
    geom_hline(yintercept = upper_bound,
               linetype = 2,
               col = 'red4') +
    geom_hline(yintercept = lower_bound,
               linetype = 2,
               col = 'red4') +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(
        vjust = 2,
        hjust = 0.5,
        size = 10
      )
    ) +
    labs(
      y = "Duration (mins)",
      x = "Start of Activity",
      title = paste0("Time series of Durations of ", activity)) %>%
    
    return()
}

#NEW Section >>>>>>>>>>>>>>>>>> END
