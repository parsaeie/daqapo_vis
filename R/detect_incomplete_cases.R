#' Detect incomplete cases
#'
#' Function detecting incomplete cases in terms of the activities that need to be recorded for a case. The function only checks the presence of activities, not the completeness of the rows describing the activity executions.
#' @inheritParams detect_activity_frequency_violations
#' @param activities A vector of activity names which should be present for a case
#' @return tbl_df providing an overview of the traces (i.e. the activities executed for a particular case) in which the specified activities are not present, together with its occurrence frequency and cases having this trace
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_incomplete_cases(activitylog = hospital_actlog,
#'      activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))
#' }
#' @export
#'
#'
detect_incomplete_cases <- function(activitylog, activities, details, filter_condition) {
  UseMethod("detect_incomplete_cases")
}
#' @export
#'
detect_incomplete_cases.activitylog <- function(activitylog, activities, details = TRUE, filter_condition = NULL){
  
  # Predefine variables
  activity_list <- NULL
  case_id <- NULL
  activity <- NULL
  complete <- NULL
  
  
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
  
  # Filter out activities which are not part of activity_vector
  # activity_log <- activity_log %>% filter(activity %in% activity_vector)
  
  # Determine activities executed for each case
  #    OBSOLETE - this duplicates repeated activities in the activity_list. For a case to be deemed complete, repetition is not an issue
  #    activity_log <- activity_log %>% group_by(case_id) %>% arrange(activity) %>% summarize(activity_list = paste(activity, collapse = " - "))
  
  
  # Determine deviations between required activities and recorded activities
  # req_act_list <- paste(sort(activity_vector), collapse = " - ")
  # incomplete <- activity_log %>% filter(!(activity_list == req_act_list))
  
  activitylog %>%
    filter_activity_presence(activities = activities, method = "all", reverse = T) -> incomplete
  
  #NEW Section >>>>>>>>>>>>>>>>>> Start
  
  
  if (all (activities %in% activity_labels(activitylog))) {
    activitylog %>%
      filter_activity_presence(activities =  activities,
                               method = "all",
                               reverse = F) -> complete
    
    stacked_barplot_ggplot_incom_case(complete, incomplete) %>% print()
    combined_process_map_incom_case(activitylog, complete) %>% print()
  }
  
  #NEW Section >>>>>>>>>>>>>>>>>> End
  
  
  # Prepare output
  stat_false <- round(nrow(incomplete) / nrow(activitylog) * 100, 2)
  stat_true <- 100 - stat_false
  
  # Print output
  
  message("*** OUTPUT ***")
  
  
  message("It was checked whether the activities ", paste(sort(activities), collapse = ", "), " are present for cases.")
  message("These activities are present for ",
          n_cases(activitylog) - n_cases(incomplete), " (", stat_true, "%) of the cases and are not present for ",
          n_cases(incomplete), " (", stat_false, "%) of the cases.")
  message("Note: this function only checks the presence of activities for a particular case, not the completeness of these entries in the activity log or the order of activities.", "\n")
  
  if(details == TRUE){
    if(stat_false > 0){
      message("For cases for which the aforementioned activities are not all present, the following activities are recorded (ordered by decreasing frequeny of occurrence):", "\n")
      incomplete_summary <- incomplete %>% group_by(!!activity_id_(activitylog)) %>% summarize(n = n(), case_ids = paste(!!case_id_(activitylog), collapse = " - ")) %>% arrange(desc(n))
      return(incomplete_summary)
    }
  }
}

#NEW Section >>>>>>>>>>>>>>>>>> Start

stacked_barplot_ggplot_incom_case <- function(complete, incomplete) {
  absolute.x <- NULL
  absolute.y <- NULL
  activity <- NULL
  absolute <- NULL
  com_incom <- NULL
  
  merge_com_incom <- merge(
    x = activity_presence(complete),
    y = activity_presence(incomplete),
    by = activity_id(incomplete),
    all.x = T,
    all.y = T
  ) %>% select(
    activity = activity_id(incomplete),
    complete = absolute.x ,
    incomplete = absolute.y
  )
  
  merge_com_incom_pivot <- merge_com_incom %>%
    tidyr::pivot_longer(!activity, names_to = "com_incom", values_to = "absolute")
  
  ggplotobject <- ggplot(merge_com_incom_pivot,
                         aes(x = activity, y = absolute, fill = com_incom)) +
    geom_bar(position = position_stack(reverse = T),
             stat = "identity",
             na.rm = T)  +
    geom_text(
      aes(label = absolute, colour = com_incom),
      position = position_stack(vjust = 0.5, reverse = T),
      stat = "identity",
      size = 3,
      show.legend = FALSE,
      na.rm = T
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
      y = "Count of Cases",
      x = "Activities",
      fill = "",
      title = "Count of Complete/Incomplete Cases for Each Activity"
    ) +
    
    theme(axis.title.y = element_text(vjust = 2)) +
    scale_fill_manual(values = c("#253D58", "#C9A959"),
                      aesthetics = "fill") +
    scale_fill_manual(values = c("white", "black"), aesthetics = "colour")
  
  return(ggplotobject)
  
  # merge_com_incom %>%
  #   plot_ly(x = ~activity,
  #           y = ~absolute.x,
  #           type = 'bar',
  #           text = ~absolute.x,
  #           textposition = 'middle',
  #           name = 'Complete Cases',
  #           marker = list(color = '#253D58')) %>%
  #   add_trace(y = ~absolute.y,
  #             name = 'Incomplete Cases',
  #             text = ~absolute.y,
  #             marker = list(color = '#C9A959')) %>%
  #   layout(xaxis = list(title ="Count of Cases"),
  #          yaxis = list(title =""),
  #          barmode = 'stack')%>%
  #   print()
  #
  
}


combined_process_map_incom_case <- function(activitylog, complete) {
  processs_map_activitylog <- activitylog %>%
    process_map(frequency("relative_case"), sec = frequency("absolute_case"))
  
  processs_map_complete <- complete %>%
    process_map(frequency("relative_case"), sec = frequency("absolute_case"))
  
  header <- tags$div("From Top to Bottom: Process map of All Cases and Complete Cases",
                     style = "justify-content: center;align-items: center;text-align: center;") %>%
    tags$br()
  
  manipulateWidget::combineWidgets(
    processs_map_activitylog,
    processs_map_complete,
    ncol = 1,
    nrow = 2,
    header = header,
    byrow = T,
    width = 500
  ) %>% return()
  
}
#NEW Section >>>>>>>>>>>>>>>>>> End
