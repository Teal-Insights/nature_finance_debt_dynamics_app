# starts: -----------------------------------------------------------------
ui_footer_component <- function() {
  tags$footer(
    class = "custom-footer",
    tags$div(
      class = "footer-content",
      tags$a(
        class = "footer-link logo-link",
        href = "https://www.ssdh.net/",
        target = "_blank",
        tags$img(
          src = "ssdh_logo.svg",
          class = "header-logo ssdh-logo"
        )
      ),
      tags$a(
        class = "footer-link logo-link",
        href = "https://www.linkedin.com/company/teal-insights/",
        target = "_blank",
        tags$img(
          src = "teal_logo.png",
          class = "header-logo teal-logo"
        )
      ),
      span(
        class = "footer-divider",
        lubridate::year(Sys.Date())
      )
    )
  )
}
# ends: -------------------------------------------------------------------
