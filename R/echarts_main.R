
# start: ------------------------------------------------------------------
echarts_main <- function(
    data,                   
    x_col,                  
    y_col,                  
    group_col = NULL,       
    group_levels = NULL,    
    title = NULL,           
    x_axis_name = "",       
    y_axis_name = "",       
    tooltip_formatter = NULL, 
    chart_type = "line"
) {
  # Load required packages
  require(echarts4r)
  require(dplyr)
  
  # Check if required columns exist in the data
  if (!x_col %in% colnames(data)) {
    stop(paste("x_col '", x_col, "' not found in data"))
  }
  if (!y_col %in% colnames(data)) {
    stop(paste("y_col '", y_col, "' not found in data"))
  }
  # Don't stop for group_col - we'll handle it with a warning later
  
  # Prepare the data
  # First check if all required columns exist
  if (!is.null(group_col) && !group_col %in% colnames(data)) {
    warning(paste("group_col '", group_col, "' not found in data. Ignoring grouping."))
    group_col <- NULL
  }
  
  # Select data columns based on what's available
  if (is.null(group_col)) {
    plot_data <- data %>% select(all_of(c(x_col, y_col)))
  } else {
    plot_data <- data %>% select(all_of(c(x_col, y_col, group_col)))
    
    # Apply custom factor levels if provided
    if (!is.null(group_levels)) {
      plot_data <- plot_data %>%
        mutate(!!sym(group_col) := factor(get(group_col), levels = group_levels))
    }
    
    # Group by the specified column
    plot_data <- plot_data %>% dplyr::group_by(!!sym(group_col))
  }
  
  # Start building the chart
  chart <- plot_data %>%
    echarts4r::e_charts_(x_col)
  
  # Add series based on chart type
  if (chart_type == "line") {
    chart <- chart %>% echarts4r::e_line_(y_col)
  } else if (chart_type == "bar") {
    chart <- chart %>% echarts4r::e_bar_(y_col)
  } else if (chart_type == "scatter") {
    chart <- chart %>% echarts4r::e_scatter_(y_col)
  } else {
    # Default to line
    chart <- chart %>% echarts4r::e_line_(y_col)
  }
  
  # Add title if provided
  if (!is.null(title)) {
    chart <- chart %>% echarts4r::e_title(title)
  }
  
  # Add x-axis configuration
  chart <- chart %>%
    echarts4r::e_x_axis(
      name = x_axis_name,
      type = "category"
    )
  
  # Add y-axis configuration
  chart <- chart %>%
    echarts4r::e_y_axis(
      name = y_axis_name,
      scale = TRUE
    )
  
  # Configure legend
  chart <- chart %>% 
    echarts4r::e_legend(
      bottom = "0%",
      orient = "horizontal",
      x = "center",
      padding = c(5, 10, 5, 10)
    )
  
  # Add tooltip
  if (is.null(tooltip_formatter)) {
    # Default tooltip formatter for numeric values with 3 decimal places
    tooltip_formatter <- htmlwidgets::JS("
      function(params) {
        var xValue = params[0].axisValue;
        var result = xValue;
        params.forEach(function(param) {
          var value = Number(param.value[1]).toFixed(3);
          result += '<br/>' + param.marker + param.seriesName + ': ' + value;
        });
        return result;
      }
    ")
  }
  
  chart <- chart %>%
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = tooltip_formatter,
      axisPointer = list(
        type = "cross"
      )
    )
  
  # Add grid configuration
  chart <- chart %>%
    echarts4r::e_grid(
      containLabel = TRUE,
      top = "10%",  
      bottom = "7%",
      left = "1%",
      right = "1%"
    )
  
  # Add toolbox with features
  chart <- chart %>%
    echarts4r::e_toolbox(
      orient = "horizontal",
      right = 15,
      top = 0,
      feature = list(
        # Image download option
        saveAsImage = list(
          title = "Save as image",
          name = "chart_export",
          pixelRatio = 2,
          type = "png"
        ),
        # Restore chart
        restore = list(
          title = "Restore"
        ),
        # Data zoom feature
        dataZoom = list(
          title = list(
            zoom = "Zoom area",
            back = "Reset zoom"
          )
        ),
        # Magic type for changing chart types
        magicType = list(
          type = c("line", "bar"),
          title = list(
            line = "Switch to Line",
            bar = "Switch to Bar"
          )
        )
      )
    )
  
  return(chart)
}

# end: --------------------------------------------------------------------
