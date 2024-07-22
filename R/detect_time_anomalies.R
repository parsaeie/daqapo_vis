#' Detect time anomalies
#'
#' Function detecting time anomalies, which can refer to activities with negative or zero duration
#' @inheritParams detect_activity_frequency_violations
#' @param anomaly_type Type of anomalies that need to be detected (either "negative", "zero" or "both")
#' @return activitylog containing the rows of the original activity log for which a negative or zero duration is detected, together with the duration value and whether it constitutes a zero or negative duration
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_time_anomalies(activitylog = hospital_actlog)
#' }
#' @export
#' @importFrom utils adist
#' @importFrom tidyr drop_na

detect_time_anomalies <- function(activitylog, anomaly_type = c("both", "negative","zero") ,
                                  details = TRUE, filter_condition = NULL){
  
  # Predefine variables
  type <- NULL
  duration <- NULL
  activity <- NULL
  complete <- NULL
  start <- NULL
  anomaly <- NULL
  
  
  anomaly_type <- match.arg(anomaly_type)
  # Generate warning if inappropriate anomaly type is selected
  
  
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
  
  # Calculate durations
  activitylog %>%
    mutate(duration = as.double(complete - start, units = "mins")) -> anomalies
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start
  anomalies %>%
    mutate(anomaly = case_when(
      duration < 0 ~ "negative",
      duration == 0 ~ "zero",
      duration > 0 ~ "Normal"
    )) %>%
    data.frame() %>%
    select(activity_id = activity_id(activitylog),
           duration,
           anomaly) ->
    anomalies_log
  
  anomalies_log %>% tidyr::drop_na() -> anomalies_log
  #NEW Section >>>>>>>>>>>>>>>>>> END
  
  # Determine time anomalies
  if(anomaly_type == "negative"){
    anomalies <- anomalies %>% filter(duration < 0)
  } else if(anomaly_type == "zero"){
    anomalies <- anomalies %>% filter(duration == 0)
  } else{
    anomalies <- anomalies %>% filter(duration <= 0) %>%
      mutate(type = ifelse(duration < 0, "negative duration", "zero duration"))
  }
  
  # Print output
  message("Selected anomaly type: ", anomaly_type, "\n")
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start
  ggplot_boxplot_anomaly(anomalies_log, 
                         length(activity_labels(activitylog))) %>% print()
  #NEW Section >>>>>>>>>>>>>>>>>> END
  
  message("*** OUTPUT ***")
  message(
    "For ",
    nrow(anomalies),
    " rows in the activity log (",
    round(nrow(anomalies) / nrow(activitylog) * 100, 2),
    "%), an anomaly is detected."
  )
  
  if (nrow(anomalies) > 0) {
    message("The anomalies are spread over the activities as follows:")
    if (anomaly_type == "both") {
      print(
        anomalies %>% group_by(!!activity_id_(activitylog), type) %>% summarize(n = n()) %>% arrange(desc(n))
      )
    } else{
      print(anomalies %>% group_by(!!activity_id_(activitylog)) %>% summarize(n = n()) %>% arrange(desc(n)))
    }
    
    if (details == TRUE) {
      message("Anomalies are found in the following rows:")
      return(anomalies)
    }
  }
}

#NEW Section >>>>>>>>>>>>>>>>>> Start
ggplot_boxplot_anomaly <- function(anomalies_log, n) {
  duration <- NULL
  anomaly <- NULL
  
  ggplot_object <- anomalies_log %>%
    ggplot(aes(x = activity_id, y = duration)) +
    geom_point(
      aes(color = anomaly),
      position = position_jitter(width = 0.3, height = 0),
      size = 0.4,
      alpha = 0.4
    ) +
    geom_boxplot(outliers = F, alpha = 0.8) +
    coord_flip() +
    scale_color_manual(
      breaks = c("negative", "zero", "Normal"),
      values = c("red4", "black", "green4")
    ) +
    facet_wrap(activity_id ~ .,
               scales = "free",
               nrow = n,
               ncol = 1) +
    labs(colour = "") +
    xlab("") +
    ylab("Duration (Minuts)") +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank())
  
  return(ggplot_object)
}
#NEW Section >>>>>>>>>>>>>>>>>> END