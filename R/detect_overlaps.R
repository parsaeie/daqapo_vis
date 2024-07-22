#' Detect overlapping acitivity instances
#' @inheritParams detect_activity_frequency_violations
#' @param level_of_aggregation Look for overlapping activity instances within a case or within a resource.
#' @return tbl_df providing an overview of activities which are performed in parallel by a resource, together with the occurrence frequency of the overlap and the average time overlap in minutes
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_overlaps(activitylog = hospital_actlog)
#' }
#' @importFrom purrr map2
#' @importFrom purrr map2_lgl
#' @importFrom purrr map_dbl
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' @importFrom rlang set_names
#' @export

detect_overlaps <- function(activitylog,
                            details,
                            level_of_aggregation,
                            filter_condition)  {
  UseMethod("detect_overlaps")
}

#' @export

detect_overlaps.activitylog <- function(activitylog,
                                        details = FALSE,
                                        level_of_aggregation = c("case", "resource"),
                                        filter_condition = NULL) {
  
  ACTIVITY_CLASSIFIER <- NULL
  RESOURCE_CLASSIFIER <- NULL
  CASE_CLASSIFIER <- NULL
  start <- NULL
  complete <- NULL
  start.x <- NULL
  start.y <- NULL
  INTERVAL.x <- NULL
  INTERVAL.y <- NULL
  overlaps <- NULL
  activity_a <- NULL
  activity_b <- NULL
  time_of_overlap_mins <- NULL
  
  level_of_aggregation <- match.arg(level_of_aggregation)
  
  
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
  
  if(level_of_aggregation == "case") {
    detect_overlaps_case(activitylog, details)
  } else {
    detect_overlaps_resource(activitylog, details)
  }
}

detect_overlaps_case <- function(activitylog, details) {
  ACTIVITY_CLASSIFIER <- NULL
  RESOURCE_CLASSIFIER <- NULL
  CASE_CLASSIFIER <- NULL
  start <- NULL
  complete <- NULL
  start.x <- NULL
  start.y <- NULL
  INTERVAL.x <- NULL
  INTERVAL.y <- NULL
  overlaps <- NULL
  activity_a <- NULL
  activity_b <- NULL
  time_of_overlap_mins <- NULL
  
  activitylog %>%
    as.data.frame() %>%
    rename(ACTIVITY_CLASSIFIER := !!activity_id_(activitylog),
           CASE_CLASSIFIER := !!case_id_(activitylog)) %>%
    select(ACTIVITY_CLASSIFIER, CASE_CLASSIFIER, start, complete) %>%
    mutate(INTERVAL = map2(start, complete, interval)) -> intervals
  
  intervals %>%
    inner_join(intervals, by = "CASE_CLASSIFIER") %>%
    filter(start.x < start.y) %>%
    mutate(overlaps = map2_lgl(INTERVAL.x, INTERVAL.y, lubridate::int_overlaps)) %>%
    filter(overlaps) %>%
    mutate(time_of_overlap_mins = map2(INTERVAL.x, INTERVAL.y, lubridate::intersect)) %>%
    mutate(time_of_overlap_mins = map_dbl(time_of_overlap_mins, as.double, units = "mins")) %>%
    filter(time_of_overlap_mins > 0) %>%
    select(-overlaps) -> activitylog_overlap #>>>>>>>>>>>>>>>>>>>>>>>New Variable
  
  activitylog_overlap %>%
    select(-start.x:-INTERVAL.x, -start.y:-INTERVAL.y) %>%
    set_names(c("activity_a",case_id(activitylog),"activity_b","time_of_overlap_mins")) %>%
    select(!!case_id_(activitylog), everything())  -> raw
  
  #>>>>>>>>>>>>>>>>>>>>>>>New Section Start
  if (nrow(raw) != 0){
    ggplot_multiple_errorbar_case_facet_overlap(activitylog_overlap) %>% print()
  }
  #>>>>>>>>>>>>>>>>>>>>>>>New Section END
  
  
  if(details) {
    raw
  } else {
    raw %>%
      group_by(activity_a, activity_b) %>%
      summarize(n = n(), avg_overlap_mins = mean(time_of_overlap_mins)) %>%
      ungroup()
    
  }
}

detect_overlaps_resource <- function(activitylog, details) {
  ACTIVITY_CLASSIFIER <- NULL
  RESOURCE_CLASSIFIER <- NULL
  CASE_CLASSIFIER <- NULL
  start <- NULL
  complete <- NULL
  start.x <- NULL
  start.y <- NULL
  INTERVAL.x <- NULL
  INTERVAL.y <- NULL
  overlaps <- NULL
  activity_a <- NULL
  activity_b <- NULL
  time_of_overlap_mins <- NULL
  originator <- NULL
  
  activitylog %>% data.frame() %>% #bug was here
    dplyr::rename(ACTIVITY_CLASSIFIER := !!activity_id_(activitylog),
                  RESOURCE_CLASSIFIER := !!resource_id_(activitylog),
                  CASE_CLASSIFIER := !!case_id_(activitylog)) %>%
    select(ACTIVITY_CLASSIFIER, RESOURCE_CLASSIFIER, CASE_CLASSIFIER, start, complete) %>%
    mutate(INTERVAL = map2(start, complete, interval)) -> intervals
  
  intervals %>%
    inner_join(intervals, by = "RESOURCE_CLASSIFIER") %>%
    filter(start.x < start.y) %>%
    mutate(overlaps = map2_lgl(INTERVAL.x, INTERVAL.y, lubridate::int_overlaps)) %>%
    filter(overlaps) %>%
    mutate(time_of_overlap_mins = map2(INTERVAL.x, INTERVAL.y, lubridate::intersect)) %>%
    mutate(time_of_overlap_mins = map_dbl(time_of_overlap_mins, as.double, units = "mins")) %>%
    filter(time_of_overlap_mins > 0) %>% 
    select(-overlaps) -> activitylog_overlap #>>>>>>>>>>>>>>>>>>>>>>>New Variable
  
  activitylog_overlap %>%
    select(-start.x:-INTERVAL.x, -start.y:-INTERVAL.y) %>%
    set_names(c("activity_a",resource_id(activitylog),"case_a", "activity_b", "case_b", "time_of_overlap_mins")) %>%
    select(!!resource_id_(activitylog), everything()) -> raw
  
  #>>>>>>>>>>>>>>>>>>>>>>>New Section Start
  if (nrow(raw) != 0){
    ggplot_multiple_errorbar_resource_facet_overlap(activitylog_overlap) %>% print()
  }
  #>>>>>>>>>>>>>>>>>>>>>>>New Section END
  
  if(details) {
    raw
  } else {
    raw %>%
      group_by(originator) %>% #another bug was here
      summarize(n = n(), avg_overlap_mins = mean(time_of_overlap_mins)) %>%
      ungroup()
  }
}

#>>>>>>>>>>>>>>>>>>>>>>>New Section Start

ggplot_multiple_errorbar_case_facet_overlap <- function(activitylog_overlap) {
  ACTIVITY_CLASSIFIER.x <- NULL
  CASE_CLASSIFIER <- NULL
  start.x <- NULL
  complete.x <- NULL
  complete.x <- NULL
  CTIVITY_CLASSIFIER.y <- NULL
  start.y <- NULL
  complete.y <- NULL
  start <- NULL
  complete <- NULL
  avg <- NULL
  ACTIVITY_CLASSIFIER.y <- NULL
  
  df_1 <- activitylog_overlap %>% select(ACTIVITY_CLASSIFIER.x, CASE_CLASSIFIER, start.x, complete.x)
  
  df_2 <- activitylog_overlap %>% select(
    ACTIVITY_CLASSIFIER.x = ACTIVITY_CLASSIFIER.y,
    CASE_CLASSIFIER,
    start.x = start.y,
    complete.x = complete.y
  )
  
  df <- bind_rows(distinct(df_1), distinct(df_2))
  
  df <- df %>%
    group_by(CASE_CLASSIFIER) %>%
    mutate(
      start = difftime(start.x, min(start.x, na.rm = TRUE), units = "mins"),
      complete = difftime(complete.x, min(start.x, na.rm = TRUE), units = "mins"),
      avg = as.difftime(start + complete) / 2,
      units = "min"
    ) %>% ungroup()
  
  ggplot_object <- ggplot(df) +
    geom_errorbar(
      aes(
        x = ACTIVITY_CLASSIFIER.x,
        ymin = start,
        ymax = complete,
        color = ACTIVITY_CLASSIFIER.x
      ),
      width = .5
    ) +
    geom_point(aes(x = ACTIVITY_CLASSIFIER.x, y = avg , color = ACTIVITY_CLASSIFIER.x)) +
    facet_wrap(CASE_CLASSIFIER ~ ., scales = "free") +
    scale_x_discrete() +
    scale_y_continuous() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(title = "Cases with Overlapping Acitivity Instances",
         y = "Duration of Cases and Activities (min)",
         color = element_blank())
  
  return(ggplot_object)
}

ggplot_multiple_errorbar_resource_facet_overlap <- function(activitylog_overlap) {
  ACTIVITY_CLASSIFIER.x <- NULL
  RESOURCE_CLASSIFIER <- NULL
  CASE_CLASSIFIER.x <- NULL
  start.x <- NULL
  complete.x <- NULL
  ACTIVITY_CLASSIFIER.y <- NULL
  start.y <- NULL
  complete.y <- NULL
  start <- NULL
  complete <- NULL
  ACTIVITY_CLASSIFIER <- NULL
  CASE_CLASSIFIER <- NULL
  avg <- NULL
  CASE_CLASSIFIER.y <- NULL
  
  df_1 <- activitylog_overlap %>% as.data.frame() %>%
    select(
      ACTIVITY_CLASSIFIER = ACTIVITY_CLASSIFIER.x,
      CASE_CLASSIFIER = CASE_CLASSIFIER.x,
      RESOURCE_CLASSIFIER,
      start = start.x,
      complete = complete.x
    )
  
  df_2 <- activitylog_overlap %>% as.data.frame() %>%
    select(
      ACTIVITY_CLASSIFIER = ACTIVITY_CLASSIFIER.y,
      CASE_CLASSIFIER = CASE_CLASSIFIER.y,
      RESOURCE_CLASSIFIER,
      start = start.y,
      complete = complete.y
    )
  
  df <- bind_rows(distinct(df_1), distinct(df_2))
  
  df <- df %>%
    mutate(avg = as.POSIXct((
      as.numeric(start) + as.numeric(complete)
    ) / 2))
  
  ggplot_object <- ggplot(df) +
    geom_errorbar(aes(
      x = paste0(ACTIVITY_CLASSIFIER, CASE_CLASSIFIER),
      ymin = start,
      ymax = complete,
      color = ACTIVITY_CLASSIFIER
    ),
    width = .5) +
    geom_point(aes(
      x = paste0(ACTIVITY_CLASSIFIER, CASE_CLASSIFIER),
      y = avg ,
      color = ACTIVITY_CLASSIFIER
    )) +
    geom_text(
      aes(
        x = paste0(ACTIVITY_CLASSIFIER, CASE_CLASSIFIER),
        y = avg,
        label = CASE_CLASSIFIER
      ),
      vjust = -1,
      hjust = -0.5
    ) +
    facet_wrap(RESOURCE_CLASSIFIER ~ ., scales = "free") +
    scale_x_discrete() +
    scale_y_datetime(date_labels = "%b %d %R") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(title = "Resources with Overlapping Acitivity Instances",
         y = "Duration of Activities",
         color = element_blank())
  
  return(ggplot_object)
}
#>>>>>>>>>>>>>>>>>>>>>>>New Section END
