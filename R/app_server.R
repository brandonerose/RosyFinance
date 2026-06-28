#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  values <- reactiveValues()
  if (!exists("finances")) {
    finances <- sample_dataset()
  }
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
    values$finances$make_sankey(
      yearly = input$time_mode_yearly,
      include_assets = input$time_mode_yearly,
      include_debts = input$time_mode_yearly
    )
  })
  output$treemap <- plotly::renderPlotly({
    values$finances$make_treemap(
      yearly = input$time_mode_yearly,
      include_assets = input$time_mode_yearly,
      include_debts = input$time_mode_yearly
    )
  })
}
