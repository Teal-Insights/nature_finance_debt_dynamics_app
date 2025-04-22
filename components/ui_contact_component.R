
# start: ------------------------------------------------------------------
ui_contact_component <- function() {
  tags$div(
    class = "contact-container",
    tags$div(class = "team-header", tags$h2("Contact")),
    tags$div(
      class = "profiles-grid",
      # First Profile
      profile_card(
        name = "Arend Kulenkampff",
        title = "Lead Innovative Finance",
        company = "Sustainability-Linked Sovereign Debt Hub (SSDH)",
        photo_src = "profiles/arend.jpeg",
        linkedin_url = "https://www.linkedin.com/in/arend-kulenkampff-a3796215",
        email = "arend.kulenkampff@naturefinance.net"
      ),
      # Second Profile
      profile_card(
        name = "Teal Emery",
        title = "Founder & Lead Researcher",
        company = "Teal Insights",
        photo_src = "profiles/teal.jpeg",
        linkedin_url = "https://www.linkedin.com/in/ltealemery/",
        email = "lte@tealinsights.com"
      )
    )
  )
}

# profile function --------------------------------------------------------
profile_card <- function(name, title, company, photo_src, linkedin_url, email) {
  tags$div(
    class = "profile-card",
    tags$div(
      class = "profile-photo-container",
      tags$img(
        src = photo_src,
        class = "profile-photo"
      )
    ),
    tags$div(
      class = "profile-info",
      # Grid layout with fixed rows for consistent alignment
      tags$div(class = "profile-grid",
               # Row 1: Name
               tags$div(class = "grid-row name-row",
                        tags$h3(class = "profile-name", name)
               ),
               # Row 2: Title
               tags$div(class = "grid-row title-row",
                        tags$h4(class = "profile-title", title)
               ),
               # Row 3: Company
               tags$div(class = "grid-row company-row",
                        tags$h4(class = "profile-company", HTML(gsub(" ", "&nbsp;", company)))
               ),
               # Row 4: LinkedIn
               tags$div(class = "grid-row linkedin-row",
                        tags$a(
                          class = "profile-linkedin",
                          href = linkedin_url,
                          target = "_blank",
                          tags$i(class = "bi bi-linkedin me-2", `aria-hidden` = "true"),
                          "LinkedIn"
                        )
               ),
               # Row 5: Email
               tags$div(class = "grid-row email-row",
                        tags$a(
                          class = "profile-email",
                          href = paste0("mailto:", email),
                          tags$i(class = "bi bi-envelope-fill me-2", `aria-hidden` = "true"),
                          email
                        )
               )
      )
    )
  )
}


# end: --------------------------------------------------------------------


