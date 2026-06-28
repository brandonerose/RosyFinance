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
          text = "Incomes",
          tabName = "incomes",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Expenses",
          tabName = "expenses",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Assets",
          tabName = "assets",
          icon = shiny::icon("chart-bar")
        ),
        menuItem(
          text = "Debts",
          tabName = "debts",
          icon = shiny::icon("chart-bar")
        )
      ),
      body = dbBody(
        # home--------
        tabItem("home", fluidRow(
          box(
            width = 12L,
            plotly::plotlyOutput("sankey", height = "800px")
          ),
          box(
            width = 12L,
            plotly::plotlyOutput("treemap", height = "600px")
          )
        )),
        # incomes--------
        tabItem("incomes", fluidRow(box(
          title = h1("Incomes"),
          width = 12L,
          DT::DTOutput("dt_incomes")
        ))),
        # debts--------
        tabItem("debts", fluidRow(box(
          title = h1("Debts"),
          width = 12L,
          DT::DTOutput("dt_debts")
        ))),
        # assets--------
        tabItem("assets", fluidRow(box(
          title = h1("Assets"),
          width = 12L,
          DT::DTOutput("dt_assets")
        ))),
        # expenses--------
        tabItem("expenses", fluidRow(box(
          title = h1("Expenses"),
          width = 12L,
          DT::DTOutput("dt_expenses")
        )))
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
