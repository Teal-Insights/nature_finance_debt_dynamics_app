# starts: -----------------------------------------------------------------
ui_footer_component <- function() {
  tags$footer(
    class = "custom-footer",
    tags$div(
      class = "footer-content",
      tags$a(
        class = "footer-link",
        href = "https://www.ssdh.net/",
        target = "_blank",
        tags$img(
          src = "ssdh_logo.svg",
          class = "header-logo",
          style = "border-right: 2px solid white; padding-right: 10px; height: 50px"
        )
      ),
      tags$a(
        class = "footer-link",
        # style = "border-right: 2px solid white; padding-right: 10px; height: 70px",
        href = "#",
        "Teal Insights"
      ),
      span(
        class = "footer-divider",
        style = "padding-right: 10px;height : auto",
        # HTML("&nbsp;|&nbsp;©&nbsp;"),
        lubridate::year(Sys.Date())
      )
    )
  )
}

# ends: -------------------------------------------------------------------
