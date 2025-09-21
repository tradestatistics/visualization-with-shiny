#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom highcharter highchartOutput
#' @importFrom shinyhelper helper
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaitress
#' @importFrom golem add_resource_path favicon bundle_resources
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
        useWaitress(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome"),
          menuItem("Countries", tabName = "countries"),
          menuItem("Products", tabName = "products"),
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
            tabName = "countries",
            mod_countries_ui("countries")
          ),
          tabItem(
            tabName = "products",
            h1("complete soon")
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
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Open Trade Statistics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
