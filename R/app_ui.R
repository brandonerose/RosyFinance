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
        shinyWidgets::switchInput(
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
        ),
        actionButton("reset", "Reset/Example"),
        actionButton("remove_all", "Remove All"),
        fileInput(
          inputId = "finances_file",
          label = "Upload",
          accept = c(
            ".xlsx"
          )
        ),
        shinyFiles::shinySaveButton(
          id = "download",
          label = "Save Finances",
          title = "Save RosyFinance as...",
          filetype = list(
            excel = "xlsx"
          ),
          filename = "RosyFinance",
          viewtype = "list"
        )
      ),
      body = dbBody(
        # home--------
        tabItem("home", fluidRow(
          box(
            width = 12L,
            fluidRow(
              valueBoxOutput("incomes_box", width = 3),
              valueBoxOutput("expenses_box", width = 3),
              valueBoxOutput("assets_box", width = 3),
              valueBoxOutput("debts_box", width = 3)
            ),
            fluidRow(
              valueBoxOutput("taxes_box", width = 3),
              valueBoxOutput("essentials_box", width = 3),
              valueBoxOutput("growth_box", width = 3),
              valueBoxOutput("interest_box", width = 3)
            ),
            fluidRow(
              valueBoxOutput("left_over_box", width = 6),
              valueBoxOutput("net_worth_box", width = 6)
            )
          ),
          box(
            width = 12L,
            plotly::plotlyOutput("sankey", height = "800px")
          ),
          box(
            width = 12L,
            plotly::plotlyOutput("treemap", height = "800px")
          )
        )),
        # incomes--------
        tabItem(
          tabName = "incomes",
          box(
            title = h1("Incomes"),
            width = 12L
            # fluidRow(box(width = 12, uiOutput("expense_boxes")))
          ),
          box(
            title = h1("Modify"),
            width = 12L,
            fluidRow(
              column(
                width = 3,
                textInput(
                  inputId = "income_name",
                  label = "Income Name"
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "income_gross",
                  label = "Gross",
                  value = 86000,
                  min = 0,
                  max = NA,
                  step = 1000
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "income_take_home",
                  label = "Take Home",
                  value = 55000,
                  min = 0,
                  max = NA,
                  step = 1000
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "pre_tax_deductions",
                  label = "Pre-tax Deductions",
                  value = 5000,
                  min = 0,
                  max = NA,
                  step = 100
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton(
                  inputId = "update_selected_income",
                  label = "Add/Update Income",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_selected_income",
                  label = "Remove Selected Income",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_incomes",
                  label = "Remove Incomes",
                  width = "100%"
                )
              )
            )
          ),
          box(
            title = h1("Table"),
            width = 12,
            DT::DTOutput("dt_incomes")
          )
        ),
        # debts--------
        tabItem(
          tabName = "debts",
          box(
            title = h1("Debts"),
            width = 12L
            # fluidRow(box(width = 12, uiOutput("expense_boxes")))
          ),
          box(
            title = h1("Modify"),
            width = 12L,
            fluidRow(
              column(
                width = 3,
                textInput(
                  inputId = "debt_name",
                  label = "Debt Name"
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "debt_value",
                  label = "Value",
                  value = 100000,
                  min = 0,
                  max = NA,
                  step = 1000
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "debt_interest_rate",
                  label = "Interest Rate",
                  value = 0.056,
                  min = 0,
                  # max = 1,
                  step = 0.001
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "debt_payment",
                  label = "Monthly Payment",
                  value = 300,
                  min = 0,
                  max = NA,
                  step = 10
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton(
                  inputId = "update_selected_debt",
                  label = "Add/Update Debt",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_selected_debt",
                  label = "Remove Selected Debt",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_debts",
                  label = "Remove Debts",
                  width = "100%"
                )
              )
            )
          ),
          box(
            title = h1("Table"),
            width = 12,
            DT::DTOutput("dt_debts")
          )
        ),
        # assets--------
        tabItem(
          tabName = "assets",
          box(
            title = h1("Assets"),
            width = 12L
            # fluidRow(box(width = 12, uiOutput("expense_boxes")))
          ),
          box(
            title = h1("Modify"),
            width = 12L,
            fluidRow(
              column(
                width = 4,
                textInput(
                  inputId = "asset_name",
                  label = "Asset Name"
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = "asset_value",
                  label = "Asset Value",
                  value = 1000,
                  min = 0,
                  max = NA,
                  step = 100
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = "asset_growth",
                  label = "Asset Growth",
                  value = 0.1,
                  # min = 0,
                  # max = 1,
                  step = 0.01
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                numericInput(
                  inputId = "asset_contribution",
                  label = "Asset Contribution",
                  value = 0,
                  min = 0,
                  max = NA,
                  step = 100
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "asset_employer_contribution",
                  label = "Asset Employer Contribution",
                  value = 0,
                  min = 0,
                  max = NA,
                  step = 100
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  inputId = "asset_contribution_tax_type",
                  label = "Asset Contribution Tax Type",
                  choices = c("Pre", "Post"),
                  selected = "Post"
                )
              ),
              column(
                width = 3,
                textInput(
                  inputId = "asset_income_link",
                  label = "Income Link"
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton(
                  inputId = "update_selected_asset",
                  label = "Add/Update Asset",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_selected_asset",
                  label = "Remove Selected Asset",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_assets",
                  label = "Remove Assets",
                  width = "100%"
                )
              )
            )
          ),
          box(
            title = h1("Table"),
            width = 12,
            DT::DTOutput("dt_assets")
          )
        ),
        # expenses--------
        tabItem(
          tabName = "expenses",
          box(
            title = h1("Expenses"),
            width = 12L,
            fluidRow(box(width = 12, uiOutput("expense_boxes")))
          ),
          box(
            title = h1("Modify"),
            width = 12L,
            fluidRow(
              column(
                width = 3,
                textInput(
                  inputId = "expense_name",
                  label = "Expense Name"
                )
              ),
              column(
                width = 3,
                textInput(
                  inputId = "expense_category",
                  label = "Category"
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "expense_amount",
                  label = "Amount",
                  value = 150,
                  min = 0,
                  max = NA,
                  step = 5
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  inputId = "expense_type",
                  label = "Type",
                  choices = c("monthly", "yearly", "biannual"),
                  selected = "monthly"
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton(
                  inputId = "update_selected_expense",
                  label = "Add/Update Expense",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_selected_expense",
                  label = "Remove Selected Expense",
                  width = "100%"
                )
              ),
              column(
                width = 4,
                actionButton(
                  inputId = "remove_expenses",
                  label = "Remove Expenses",
                  width = "100%"
                )
              )
            )
          ),
          box(
            title = h1("Table"),
            width = 12,
            DT::DTOutput("dt_expenses")
          )
        )
      ),
      controlbar = dbControlbar(
        shinyWidgets::awesomeCheckbox(
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
