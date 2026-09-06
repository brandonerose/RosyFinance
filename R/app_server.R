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
    make_DT_table(values$finances$data$incomes, paging = FALSE)
  })
  output$dt_expenses <- DT::renderDT({
    make_DT_table(values$finances$data$expenses, paging = FALSE)
  })
  output$dt_assets <- DT::renderDT({
    make_DT_table(values$finances$data$assets, paging = FALSE)
  })
  output$dt_debts <- DT::renderDT({
    make_DT_table(values$finances$data$debts, paging = FALSE)
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
        clean_num(),
      subtitle = "Incomes",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$expenses_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_expenses(yearly = input$time_mode_yearly) |>
        clean_num(),
      subtitle = "Expenses",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$assets_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_assets() |> clean_num(),
      subtitle = "Assets",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$debts_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_debts() |> clean_num(),
      subtitle = "Debts",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  output$left_over_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_left_over(yearly = input$time_mode_yearly) |>
        clean_num(),
      subtitle = "Left Over",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$net_worth_box <- renderValueBox({
    valueBox(
      value = values$finances$calc_net_worth() |> clean_num(),
      subtitle = "Net Worth",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$expense_boxes <- renderUI({
    x <- make_expense_summary(values$finances$data)
    if (!input$time_mode_yearly) {
      x$yearly_amount <- x$yearly_amount / 12
    }
    fluidRow(lapply(seq_len(nrow(x)), function(i) {
      column(
        width = 3,
        valueBox(
          value = scales::dollar(x$yearly_amount[i]),
          subtitle = x$category[i],
          icon = icon("wallet"),
          width = 12
        )
      )
    }))
  })
  output$taxes_box <- renderValueBox({
    value <- values$finances$calc_taxes(yearly = input$time_mode_yearly)
    gross <- values$finances$data$incomes$gross |> sum(na.rm = TRUE)
    if (!input$time_mode_yearly) {
      gross <- gross / 12
    }
    perc <- (value / gross * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, " ETR)")
    valueBox(
      value = value,
      subtitle = "Taxes",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  output$essentials_box <- renderValueBox({
    value <- values$finances$calc_essentials(yearly = input$time_mode_yearly)
    take_home <- values$finances$data$incomes$take_home |> sum(na.rm = TRUE)
    if (!input$time_mode_yearly) {
      take_home <- take_home / 12
    }
    perc <- (value / take_home * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, " TH)")
    valueBox(
      value = value,
      subtitle = "Essentials",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  output$growth_box <- renderValueBox({
    value <- values$finances$calc_growth(yearly = input$time_mode_yearly)
    worth <- values$finances$data$assets$value |> sum(na.rm = TRUE)
    if (!input$time_mode_yearly) {
      worth <- worth / 12
    }
    perc <- (value / worth * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Asset Growth",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$interest_box <- renderValueBox({
    value <- values$finances$calc_interest(yearly = input$time_mode_yearly)
    balance <- values$finances$data$debts$value |> sum(na.rm = TRUE)
    if (!input$time_mode_yearly) {
      balance <- balance / 12
    }
    perc <- (value / balance * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Interest",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
}
