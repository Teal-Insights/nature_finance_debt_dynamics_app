
# starts ------------------------------------------------------------------
server_create_country_headers <- function(id, output, input) {
  metrics <- c("pb", "ir", "gdp", "ih", "ip", "gh", "gp", "gt")
  
  lapply(metrics, function(metric) {
    output_name <- paste0("selected_country_header_", metric)
    output[[output_name]] <- renderText({
      input$id_country
    })
  })
}

# ends --------------------------------------------------------------------


