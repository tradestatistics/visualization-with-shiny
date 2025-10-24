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
      layout = "fluid-vertical",
      navbar = tabler_sidebar(
        title = "Open Trade Statistics",
        sidebar_menu(
          useShinyjs(),
          useWaiter(),
          menu_item("Welcome", tab_name = "welcome"),
          menu_item("Countries", tab_name = "co"),
          menu_item("Products", tab_name = "pr"),
          menu_item("Cite", tab_name = "cite")
        )
      ),
      body = tabler_body(
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

  # Add JavaScript to add an id to the <section> tag
  # so we can overlay waiter on top of it
  add_id_to_section <- "
  $( document ).ready(function() {
    var section = document.getElementsByClassName('page-body');
    section[0].setAttribute('id', 'waiter-content');
  });"

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Open Trade Statistics"
    ),
    # Include custom JS that equalizes card heights where needed
    tags$script(src = "www/card-height.js"),
    tags$script(add_id_to_section),
    tags$style(
      ".waiter-overlay-content{
        position: absolute;
        top: 45vh;
        left: 50%;
      }"
    )
  )
}
