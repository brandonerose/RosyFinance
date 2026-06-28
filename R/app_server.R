#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # values ------------
  values <- reactiveValues()
  values$finances <- finances # check for missing
  output$dt_incomes <- DT::renderDT({
    make_DT_table(values$finances$data$incomes)
  })
  output$dt_expenses <- DT::renderDT({
    make_DT_table(values$finances$data$expenses)
  })
  output$dt_assets <- DT::renderDT({
    make_DT_table(values$finances$data$assets)
  })
  output$dt_debts <- DT::renderDT({
    make_DT_table(values$finances$data$debts)
  })
  output$sankey <- plotly::renderPlotly({
    values$finances$make_sankey()
  })
}
