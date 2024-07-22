#' Detect value range violations
#'
#' Function detecting violations of the value range, i.e. values outside the range of tolerable values
#' @inheritParams detect_activity_frequency_violations
#' @param ... Define domain range using domain_numeric, domain_categorical and/or domain_time for each column
#' @return activitylog containing the rows of the original activity log for which the provided value range is violated
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_value_range_violations(activitylog = hospital_actlog,
#'      triagecode = domain_numeric(from = 0, to = 5))
#' }
#' @seealso \code{\link{domain_categorical}},\code{\link{domain_time}},\code{\link{domain_numeric}}
#' @importFrom glue glue
#' @importFrom rlang list2
#' @export
#'
detect_value_range_violations <- function(activitylog, ..., details, filter_condition) {
  UseMethod("detect_value_range_violations")
}
#' @export
detect_value_range_violations.activitylog <- function(activitylog, ... , details = TRUE, filter_condition = NULL){


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
  
  classes <- map(params, ~class(.x)[1]) %>% unlist()
  if(any(classes != "value_range")) {
    stop("Domains should be defined with domain_ functions.")
  }

  columns <- names(params)
  if(any(!(columns %in% names(activitylog)))) {
    warning(glue::glue("The following columns are not found and ignored: {str_c(columns[!(columns %in% names(activitylog))], collapse = ', ')}. Did you spelled them wrong?"))
    columns <- columns[(columns %in% names(activitylog))]
  }

  violated <- vector(mode = "list", length = length(params))
  violated_with_bound <- vector(mode = "list", length = length(params))
  

  message("*** OUTPUT ***")
  for (i in seq_along(params)) {
    type <- params[[i]]$type
    column <- names(params)[i]
    
    #>>>>>>>>>>>>>>>>>>>>>>>New Section Start
    if (type == "categorical") {
      from <- NA
      to <- NA
    } else if (type == "numeric") {
      from <-  params[[i]]$from
      to <-  params[[i]]$to
    } else if (type == "time") {
      from <- as.POSIXct(params[[i]]$from)
      to <- as.POSIXct(params[[i]]$to)
    }
    #>>>>>>>>>>>>>>>>>>>>>>>New Section End
    
    FUN <- switch(type,
                  numeric = check_domain_numeric,
                  categorical = check_domain_character,
                  time = check_domain_time)
    df <- activitylog
    df$ID <- seq.int(nrow(df))
    
    violated[[i]] <-
      FUN(df, column, params[[i]])
    message("")
    
    #>>>>>>>>>>>>>>>>>>>>>>>New Section Start
    if (type == "categorical") {
      ggoplot_dounble_donut_ran_vio(violated[[i]], df, column) %>%
        print()
    } else {
      ggplot_errorbar_vio(violated[[i]], df, from, to, column) %>%
        print()
    }
    #>>>>>>>>>>>>>>>>>>>>>>>New Section End
  }
  
  violated <- bind_rows(violated)
  
  if(details == TRUE){
    if(nrow(violated) > 0){
      message("The following rows fall outside the specified domain range for indicated column:")
      return(violated)
    }
  }
}

check_domain_time <- function(activitylog, column, domain_range) {
  column_checked <- NULL
  activitylog %>%
    filter(is.na(!!sym(column)) | !!sym(column) < domain_range$from | !!sym(column) > domain_range$to) -> violated

  # Prepare output
  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed between {domain_range$from} and {domain_range$to}"))
  message("The values fall within the specified domain range for ",
      nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
      nrow(violated), " (", stat_outside, "%) of these rows.")
  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())

}

check_domain_numeric <- function(activitylog, column, domain_range) {
  column_checked <- NULL

  activitylog %>%
    filter(is.na(!!sym(column)) | !between(!!sym(column), domain_range$from, domain_range$to)) -> violated

  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed between {domain_range$from} and {domain_range$to}"))
  message("The values fall within the specified domain range for ",
          nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
          nrow(violated), " (", stat_outside, "%) of these rows.")

  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())
}

check_domain_character <- function(activitylog, column, domain_range) {
  column_checked <- NULL

  activitylog %>%
    filter(is.na(!!sym(column)) | !(!!sym(column) %in% domain_range$allowed)) -> violated

  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed: {str_c(domain_range$allowed, collapse = ', ')}"))
  message("The values fall within the specified domain range for ",
          nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
          nrow(violated), " (", stat_outside, "%) of these rows.")
  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())
}

#>>>>>>>>>>>>>>>>>>>>>>>New Section Start
ggplot_errorbar_vio <- function(violated,df,from, to, column ){
  violation <- NULL
  ID <- NULL
  
  df2 <-
    as.data.frame(violated) %>%
    mutate(violation = "Outside") %>%
    select(ID, violation)
  
  df <- merge.data.frame(df, df2, by = "ID", all.x = T)
  
  df$violation[is.na(df$violation)] <- "Inside"
  
  ggplot_object <- ggplot(df)  +
    geom_point(aes(x = "",
                   y = .data[[column]] ,
                   color = violation),
               position = position_jitter(width = 0.1, height = 0.3), 
               alpha = 0.5,
               size = 1
               ) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "green4",
        "red4"
      ),
      aesthetics = "color"
    ) +
    labs(
      y = NULL,
      x = NULL,
      color = NULL,
      title = paste0("Value Range Violation: ", column),
    )  +
    annotate("errorbar", x = "",
          ymin = from,
          ymax = to,
      color = "#253D58",
      width = 0.25,
      size = 1,
      linetype  = 2
        )     
  
  return(ggplot_object)
  
}

ggoplot_dounble_donut_ran_vio <- function(violated, df, column) {
  ID <- NULL
  violation <- NULL
  fraction <- NULL
  ymax <- NULL
  ymin <- NULL
  group <- NULL
  label <- NULL
  
  df2 <-
    as.data.frame(violated) %>%
    mutate(violation = "Outside") %>%
    select(ID, violation)
  
  df <- merge.data.frame(df, df2, by = "ID", all.x = T) %>% select(!!column, violation, ID)
  
  df$violation[is.na(df$violation)] <- "Inside"
  
  df <- df %>% group_by(.[[1]], violation) %>% summarise(n = n()) %>% ungroup()
  
  colnames(df) <- c("col", "violation", "n")
  
  df <- df[order(df$violation, decreasing = TRUE, df$n), ]
  
  df$fraction <- df$n / sum(df$n)
  
  df <- df %>%
    mutate(ymax = cumsum(fraction)) %>%
    mutate(ymin = lag(ymax, default = 0)) %>% ungroup()
  
  df1 <- df %>% select(tag = col, ymin, ymax, n, fraction) %>% mutate(group = 2)
  
  df2 <- df %>%
    group_by(violation) %>%
    summarise(
      ymax = max(ymax),
      ymin = min(ymin),
      n = sum(n),
      fraction = sum(fraction)
    ) %>% mutate(group = 1) %>% ungroup()
  df2 <- rename(df2, c('tag' = 'violation'))
  
  df3 <- rbind(df1, df2) %>% mutate(label = case_when(tag == "Outside" |
                                                        tag == "Inside" ~ -0.5, .default = 0.5))
  
  ggplot_object <- ggplot(df3) +
    geom_rect(
      aes(
        xmin = group / 2 - 0.25,
        xmax = group / 2 + 0.25,
        ymin = ymin,
        ymax = ymax,
        fill = tag
      ),
      color = "white",
      show.legend = F
    ) +
    xlim(c(-1, 2.5)) +
    geom_label(aes(
      label = paste0(tag, "\n", "Count: ", n, "\n", "%", round(fraction * 100)),
      x = group / 2 + label,
      y = (ymin + ymax) / 2
    )) +
    theme_void() +
    scale_fill_manual(
      aesthetics = "fill",
      values = c(
        "red4",
        "green4",
        "#253D58",
        "#C9A959",
        "#AC8181",
        "#CFCECA",
        "yellow4",
        "purple4",
        "navy",
        "blue3",
        "blue2",
        "skyblue3",
        "skyblue2",
        "skyblue1",
        "#F8F8FF"
      ),
      
      breaks = append (c("Outside", "Inside"), as.character(df1$tag))
    ) +
    coord_polar("y", start = 0)
}
#>>>>>>>>>>>>>>>>>>>>>>>New Section END
