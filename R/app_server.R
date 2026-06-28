#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  values <- reactiveValues()
  if (!exists("finances")) {
    finances <- load_sample_finances()
  }
  values$finances <- finances
  # tables -------------
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
  # plots -------------
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
  # value boxes --------
  output$incomes_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_incomes(yearly = input$time_mode_yearly) |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Incomes",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$expenses_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_expenses(yearly = input$time_mode_yearly) |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Expenses",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$assets_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_assets() |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Assets",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$debts_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_debts() |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Debts",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  output$left_over_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_left_over(yearly = input$time_mode_yearly) |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Left Over",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$net_worth_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_net_worth() |>
        formatC(format = "d", big.mark = ","),
      subtitle = "Networth",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
}
