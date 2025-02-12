# starts: -----------------------------------------------------------------
ui_footer_component <- function() {
  tags$footer(
    class = "custom-footer",
    tags$div(
      class = "footer-content",
      tags$a(
        class = "footer-link",
        href = "https://www.naturefinance.net/",
        target = "_blank",
        "Nature Finance"
      ),
      span(
        class = "footer-divider",
        HTML("&nbsp;|&nbsp;")
      ),
      tags$a(
        class = "footer-link",
        href = "#",
        "Teal Insights"
      ),
      span(
        class = "footer-divider",
        HTML("&nbsp;|&nbsp;©&nbsp;"),
        lubridate::year(Sys.Date())
      )
    )
  )
}

# ends: -------------------------------------------------------------------
