#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = styles$skin_color,
      theme = styles$css_files,
      dashboardHeader(title = "Open Trade Statistics"),
      dashboardSidebar(
        useShinyjs(),
        useWaiter(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome"),
          menuItem("Countries", tabName = "co"),
          menuItem("Products", tabName = "pr"),
          menuItem("Cite", tabName = "cite")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome")
          ),
          tabItem(
            tabName = "co",
            mod_countries_ui("co")
          ),
          tabItem(
            tabName = "pr",
            mod_products_ui("pr")
          ),
          tabItem(
            tabName = "cite",
            mod_cite_ui("cite")
          )
        ),

        # Footer ----

        fluidRow(
          col_12(
            hr(),
            htmlOutput("site_footer", container = tags$p)
          )
        )
      ),
      uiOutput(outputId = "dynamicUI")
    ),
    tags$footer(
      tags$link(rel = "shortcut icon", href = "img/favicon.ico")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  # Add JavaScript to add an id to the <section> tag 
  # so we can overlay waiter on top of it
  add_id_to_section <- "
  $( document ).ready(function() {
    var section = document.getElementsByClassName('content');
    section[0].setAttribute('id', 'waiter-content');
  });"

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Open Trade Statistics"
    ),
    tags$script(add_id_to_section),
    tags$style(
      ".waiter-overlay-content{
        position: absolute;
        top: 45vh;
        left: 50%;
      }"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
