#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = FALSE),
      header = dbHeader(),
      sidebar = dbSidebar(
        switchInput(
          inputId = "time_mode_yearly",
          onLabel = "Yearly",
          offLabel = "Monthly",
          value = TRUE
        ),
        menuItem(
          text = "Home",
          tabName = "home",
          icon = shiny::icon("home")
        ),
        menuItem(
          text = "Income",
          tabName = "income",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Debt",
          tabName = "debt",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Assets",
          tabName = "assets",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Expenses",
          tabName = "expenses",
          icon = shiny::icon("chart-bar")
        )
      ),
      body = dbBody(
        # home--------
        tabItem("home", fluidRow(
          box(
            title = h1("Home"),
            width = 12L
          ),
        )),
        # home--------
        tabItem("income", fluidRow(
          box(
            title = h1("Income"),
            width = 12L
          ),
        )),
        # home--------
        tabItem("debt", fluidRow(
          box(
            title = h1("Debt"),
            width = 12L
          ),
        )),
        # home--------
        tabItem("assets", fluidRow(
          box(
            title = h1("Assets"),
            width = 12L
          ),
        )),
        # home--------
        tabItem("expenses", fluidRow(
          box(
            title = h1("Expenses"),
            width = 12L
          ),
        ))
      ),
      controlbar = dbControlbar(
        awesomeCheckbox(
          inputId = "allow_multiple_groups",
          label = "Allow Multiple Groups",
          value = FALSE
        )
      ),
      footer = TCD_NF(),
      skin = "black"
    )
  )
}
