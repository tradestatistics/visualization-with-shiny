#' @title The application User-Interface
#' @param request Internal parameter for `Shiny`. DO NOT REMOVE.
#' @import shiny
#' @import tabler
#' @import d3po
#' @noRd
app_ui <- function(request) {
  tagList(
    # External resources
    golem_add_external_resources(),

    # Your application UI logic using tabler
    tabler_page(
      title = "Open Trade Statistics",

      # layout = "navbar-sticky",
      # navbar = topbar(
      #   title = "Open Trade Statistics",
      #   menu_item("Welcome", tab_name = "welcome", icon = "home"),
      #   menu_item("Countries", tab_name = "co", icon = "globe-filled"),
      #   menu_item("Products", tab_name = "pr", icon = "shopping-cart-filled"),
      #   menu_item("Cite", tab_name = "cite", icon = "book-filled")
      # ),

      layout = "vertical",
      theme = "light",
      color = "teal",
      navbar = sidebar_menu(
        title = "Open Trade Statistics",
        menu_item("Welcome", tab_name = "welcome", icon = "home"),
        menu_item("Countries", tab_name = "co", icon = "globe-filled"),
        menu_item("Products", tab_name = "pr", icon = "shopping-cart-filled"),
        menu_item("Cite", tab_name = "cite", icon = "book-filled")
      ),
      body = tabler_body(
        useShinyjs(),
        useWaiter(),
        tabler_tab_items(
          tabler_tab_item(
            tab_name = "welcome",
            mod_welcome_ui("welcome")
          ),
          tabler_tab_item(
            tab_name = "co",
            mod_countries_ui("co")
          ),
          tabler_tab_item(
            tab_name = "pr",
            mod_products_ui("pr")
          ),
          tabler_tab_item(
            tab_name = "cite",
            mod_cite_ui("cite")
          )
        )
      ),
      footer = tabler_footer(left = "Made by Mauricio 'Pacha' Vargas Sepulveda", right = paste("Open Trade Statistics", get_year()))
    ),
    tags$footer(
      tags$link(rel = "shortcut icon", href = "img/favicon.ico")
    )
  )
}

#' @title Add external Resources to the Application
#' @description This function is internally used to add external
#'  resources inside the Shiny application.
#' @import shiny
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
    ),

    # Include custom JS that equalizes card heights where needed and waiter helper
    tags$script(src = "www/tabler-waiter.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/tabler.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/waiter.css")
  )
}
