#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tabler
#' @import d3po
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic using tabler
    tablerPage(
      title = "Open Trade Statistics",
      layout = "fluid-vertical",
      navbar = tablerSidebar(
        title = "Open Trade Statistics",
        useShinyjs(),
        useWaiter(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome"),
          menuItem("Countries", tabName = "co"),
          menuItem("Products", tabName = "pr"),
          menuItem("Cite", tabName = "cite")
        )
      ),
      body = tablerBody(
        tablerTabItems(
          tablerTabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome")
          ),
          tablerTabItem(
            tabName = "co",
            mod_countries_ui("co")
          ),
          tablerTabItem(
            tabName = "pr",
            mod_products_ui("pr")
          ),
          tablerTabItem(
            tabName = "cite",
            mod_cite_ui("cite")
          )
        )
      ),
      footer = tablerFooter(left = "Made by Mauricio 'Pacha' Vargas Sepulveda", right = paste("Open Trade Statistics", get_year()))
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

  tags$head(
    favicon(),
    # Add JavaScript to add an id to the <section> tag so we can overlay
    # waiter on top of it. Run this before bundled resources (including
    # waiter.js) so the element exists when waiter initializes. Add a
    # small retry in case the element is added slightly later.
    tags$script(HTML("(function(){\n  function setWaiterId(){\n    var section = document.getElementsByClassName('content');\n    if (section && section.length > 0) {\n      section[0].setAttribute('id', 'waiter-content');\n      return true;\n    }\n    return false;\n  }\n  if (!setWaiterId()) {\n    var tries = 0;\n    var t = setInterval(function(){\n      tries++;\n      if (setWaiterId() || tries > 10) clearInterval(t);\n    }, 100);\n  }\n})();")),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Open Trade Statistics"
    ),
    # Include custom JS that equalizes card heights where needed
    tags$script(src = "www/custom.js"),
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
