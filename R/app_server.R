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
  output$invested_box <- renderValueBox({
    appreciates <- values$finances$data$assets[which(values$finances$data$assets$growth > 0), ]
    appreciates <- appreciates[which(appreciates$category %in% c("Investment","Retirement")),]
    worth <- appreciates$value |> sum(na.rm = T)
    growth <- appreciates$yearly_growth |> sum(na.rm = T)
    if (!input$time_mode_yearly) {
      worth <- worth / 12
      growth <- growth / 12
    }
    perc <- (growth / worth * 100) |> round(1) |> paste0("%")
    value <- clean_num(worth) |> paste0(" (", perc, " Growth)")
    valueBox(
      value = value,
      subtitle = "Invested",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$contribution_box <- renderValueBox({
    appreciates <- values$finances$data$assets[which(values$finances$data$assets$growth > 0), ]
    appreciates <- appreciates[which(appreciates$category %in% c("Investment","Retirement")),]
    growth <- appreciates$yearly_growth |> sum(na.rm = T)
    contribution <- appreciates$contribution |> sum(na.rm = T)
    employer_contribution <- appreciates$employer_contribution |> sum(na.rm = T)
    contribution <- contribution + employer_contribution
    income <- values$finances$data$incomes$gross |> sum(na.rm = T)
    if (!input$time_mode_yearly) {
      growth <- growth / 12
      income <- income / 12
      contribution <- contribution / 12
    }
    perc <- (contribution / income * 100) |> round(1) |> paste0("%")
    value <- clean_num(contribution) |> paste0(" (", perc, " Income)")
    valueBox(
      value = value,
      subtitle = "Contribution",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$growth2_box <- renderValueBox({
    appreciates <- values$finances$data$assets[which(values$finances$data$assets$growth > 0), ]
    appreciates <- appreciates[which(appreciates$category %in% c("Investment","Retirement")),]
    worth <- appreciates$value |> sum(na.rm = T)
    growth <- appreciates$yearly_growth |> sum(na.rm = T)
    contribution <- appreciates$contribution |> sum(na.rm = T)
    employer_contribution <- appreciates$employer_contribution |> sum(na.rm = T)
    contribution <- contribution + employer_contribution
    if (!input$time_mode_yearly) {
      worth <- worth / 12
      growth <- growth / 12
      contribution <- contribution / 12
    }
    perc <- (growth / contribution * 100) |> round(1) |> paste0("%")
    value <- clean_num(growth) |> paste0(" (", perc, " Contribution)")
    valueBox(
      value = value,
      subtitle = "Asset Growth",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$four_percent_box <- renderValueBox({
    appreciates <- values$finances$data$assets[which(values$finances$data$assets$growth > 0), ]
    appreciates <- appreciates[which(appreciates$category %in% c("Investment","Retirement")),]
    worth <- appreciates$value |> sum(na.rm = T)
    growth <- appreciates$yearly_growth |> sum(na.rm = T)
    income <- values$finances$data$incomes$gross |> sum(na.rm = T)
    if (!input$time_mode_yearly) {
      worth <- worth / 12
      growth <- growth / 12
      income <- income / 12
    }
    supports <- worth * 0.04
    perc <- (supports / income * 100) |> round(1) |> paste0("%")
    value <- clean_num(supports) |> paste0(" (", perc, " Income)")
    valueBox(
      value = value,
      subtitle = "4% Rule",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$pre_tax_box <- renderValueBox({
    assets <- values$finances$data$assets
    total <- assets$value |> sum(na.rm = TRUE)
    assets <- assets[which(
      assets$category %in% c("Investment", "Retirement") &
        assets$contribution_tax_type == "Pre"
    ), ]
    value <- assets$value |> sum(na.rm = TRUE)
    perc <- (value / total * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Pre Tax",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$post_tax_box <- renderValueBox({
    assets <- values$finances$data$assets
    total <- assets$value |> sum(na.rm = TRUE)
    assets <- assets[which(
      assets$category %in% c("Investment", "Retirement", "Property") &
        assets$contribution_tax_type == "Post"
    ), ]
    value <- assets$value |> sum(na.rm = TRUE)
    perc <- (value / total * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Post Tax",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$wheels_box <- renderValueBox({
    assets <- values$finances$data$assets
    total <- assets$value |> sum(na.rm = TRUE)
    assets <- assets[which(assets$category == "Vehicle"),]
    value <- assets$value |> sum(na.rm = TRUE)
    perc <- (value / total * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Pre Tax",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$cash_box <- renderValueBox({
    assets <- values$finances$data$assets
    total <- assets$value |> sum(na.rm = TRUE)
    assets <- assets[which(assets$category == "Cash"),]
    value <- assets$value |> sum(na.rm = TRUE)
    perc <- (value / total * 100) |> round(1) |> paste0("%")
    value <- clean_num(value) |> paste0(" (", perc, ")")
    valueBox(
      value = value,
      subtitle = "Cash",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  # observe tables --------------
  # dt_incomes_rows_selected
  observe({
    entry <- NULL
    entry$name <- ""
    entry$gross <- 86000
    entry$take_home <- 55000
    entry$pre_tax_deductions <- 0
    if (is_something(input$dt_incomes_rows_selected)) {
      entry <- values$finances$data$incomes[input$dt_incomes_rows_selected, ]
    }
    updateTextInput(session = session,
                    inputId = "income_name",
                    value = entry$name)
    updateNumericInput(session = session,
                       inputId = "income_gross",
                       value = entry$gross)
    updateNumericInput(session = session,
                       inputId = "income_take_home",
                       value = entry$take_home)
    updateNumericInput(
      session = session,
      inputId = "income_pre_tax_deductions",
      value = entry$pre_tax_deductions
    )
  })
  # dt_expenses_rows_selected
  observe({
    entry <- NULL
    entry$name <- ""
    entry$category <- "Phone"
    entry$amount <- 150
    entry$type <- "monthly"
    if (is_something(input$dt_expenses_rows_selected)) {
      entry <- values$finances$data$expenses[input$dt_expenses_rows_selected, ]
    }
    updateTextInput(session = session,
                    inputId = "expense_name",
                    value = entry$name)
    updateTextInput(session = session,
                    inputId = "expense_category",
                    value = entry$category)
    updateNumericInput(session = session,
                       inputId = "expense_amount",
                       value = entry$amount)
    updateSelectizeInput(session = session,
                         inputId = "expense_type",
                         selected = entry$type)
  })
  # dt_assets_rows_selected
  observe({
    entry <- NULL
    entry$name <- ""
    entry$value <- 1000
    entry$growth <- 0.1
    entry$contribution <- 0
    entry$employer_contribution <- 0
    entry$contribution_tax_type <- "Post"
    entry$income_link <- ""
    entry$category
    if (is_something(input$dt_assets_rows_selected)) {
      entry <- values$finances$data$assets[input$dt_assets_rows_selected, ]
    }
    updateTextInput(session = session,
                    inputId = "asset_name",
                    value = entry$name)
    updateSelectizeInput(session = session,
                         inputId = "asset_category",
                         selected = entry$category)
    updateNumericInput(session = session,
                       inputId = "asset_value",
                       value = entry$value)
    updateNumericInput(session = session,
                       inputId = "asset_growth",
                       value = entry$growth)
    updateNumericInput(
      session = session,
      inputId = "asset_contribution",
      value = entry$contribution
    )
    updateNumericInput(
      session = session,
      inputId = "asset_employer_contribution",
      value = entry$employer_contribution
    )
    updateSelectizeInput(
      session = session,
      inputId = "asset_contribution_tax_type",
      selected = entry$contribution_tax_type
    )
    updateTextInput(session = session,
                    inputId = "asset_income_link",
                    value = entry$income_link)
  })
  # dt_debts_rows_selected
  observe({
    entry <- NULL
    entry$name <- ""
    entry$value <- 100000
    entry$interest_rate <- 0.056
    entry$payment <- 300
    if (is_something(input$dt_debts_rows_selected)) {
      entry <- values$finances$data$debts[input$dt_debts_rows_selected, ]
    }
    updateTextInput(session = session,
                    inputId = "debt_name",
                    value = entry$name)
    updateNumericInput(session = session,
                       inputId = "debt_value",
                       value = entry$value)
    updateNumericInput(
      session = session,
      inputId = "debt_interest_rate",
      value = entry$interest_rate
    )
    updateNumericInput(session = session,
                       inputId = "debt_payment",
                       value = entry$payment)
  })
  # action_buttons  ----------
  observeEvent(input$reset, ignoreInit = TRUE, {
    values$finances <- load_sample_finances()
  })
  observeEvent(input$remove_all, ignoreInit = TRUE, {
    values$finances <- NULL
    values$finances <- load_finances(BLANK_FINANCES)
    print("Removed!")
  })
  observeEvent(input$update_selected_income, ignoreInit = TRUE, {
    if (is_something(input$income_name)) {
      finances <- values$finances
      values$finances <- NULL
      values$finances <- finances$add_income(
        name = input$income_name,
        gross = input$income_gross,
        take_home = input$income_take_home,
        pre_tax_deductions = input$income_pre_tax_deductions
      )
    }
  })
  observeEvent(input$remove_selected_income, ignoreInit = TRUE, {
    if (is_something(input$dt_incomes_rows_selected)) {
      finances <- values$finances
      values$finances <- NULL
      name <- finances$data$incomes$name[input$dt_incomes_rows_selected]
      values$finances <- finances$remove_incomes(name = name)
    }
  })
  observeEvent(input$remove_incomes, ignoreInit = TRUE, {
    finances <- values$finances
    values$finances <- NULL
    values$finances <- finances$remove_incomes()
  })
  observeEvent(input$update_selected_debt, ignoreInit = TRUE, {
    if (is_something(input$debt_name)) {
      finances <- values$finances
      values$finances <- NULL
      values$finances <- finances$add_debt(
        name = input$debt_name,
        value = input$debt_value,
        interest_rate = input$debt_interest_rate,
        payment = input$debt_payment
      )
    }
  })
  observeEvent(input$remove_selected_debt, ignoreInit = TRUE, {
    if (is_something(input$dt_debts_rows_selected)) {
      finances <- values$finances
      values$finances <- NULL
      name <- finances$data$debts$name[input$dt_debts_rows_selected]
      values$finances <- finances$remove_debts(name = name)
    }
  })
  observeEvent(input$remove_debts, ignoreInit = TRUE, {
    finances <- values$finances
    values$finances <- NULL
    values$finances <- finances$remove_debts()
  })
  observeEvent(input$update_selected_asset, ignoreInit = TRUE, {
    if (is_something(input$asset_name)) {
      finances <- values$finances
      values$finances <- NULL
      values$finances <- finances$add_asset(
        name = input$asset_name,
        category = input$asset_category,
        value = input$asset_value,
        growth = input$asset_growth,
        contribution = input$asset_contribution,
        employer_contribution = input$asset_employer_contribution,
        contribution_tax_type = input$asset_contribution_tax_type,
        income_link = ifelse(
          nzchar(input$asset_income_link),
          input$asset_income_link,
          NA
        )
      )
    }
  })
  observeEvent(input$remove_selected_asset, ignoreInit = TRUE, {
    if (is_something(input$dt_assets_rows_selected)) {
      finances <- values$finances
      values$finances <- NULL
      name <- finances$data$assets$name[input$dt_assets_rows_selected]
      values$finances <- finances$remove_assets(name = name)
    }
  })
  observeEvent(input$remove_assets, ignoreInit = TRUE, {
    finances <- values$finances
    values$finances <- NULL
    values$finances <- finances$remove_assets()
  })
  observeEvent(input$update_selected_expense, ignoreInit = TRUE, {
    if (is_something(input$expense_name)) {
      finances <- values$finances
      values$finances <- NULL
      values$finances <- finances$add_expense(
        name = input$expense_name,
        category = input$expense_category,
        amount = input$expense_amount,
        type = input$expense_type
      )
    }
  })
  observeEvent(input$remove_selected_expense, ignoreInit = TRUE, {
    if (is_something(input$dt_expenses_rows_selected)) {
      finances <- values$finances
      values$finances <- NULL
      name <- finances$data$expenses$name[input$dt_expenses_rows_selected]
      values$finances <- finances$remove_expenses(name = name)
    }
  })
  observeEvent(input$remove_expenses, ignoreInit = TRUE, {
    finances <- values$finances
    values$finances <- NULL
    values$finances <- finances$remove_expenses()
  })
  observeEvent(input$finances_file, ignoreInit = TRUE, {
    req(input$finances_file)
    values$finances <- load_finances(file_path = input$finances_file$datapath)
  })
  # mod_list----
  if (app_dev()) {
    mod_list_server("input_list", values = input)
    mod_list_server("values_list", values = values)
  }
  # download ------
  volumes <- c(
    Home = fs::path_home(),
    "R Installation" = R.home(),
    shinyFiles::getVolumes()()
  )
  shinyFiles::shinyFileSave(input, "download", roots = volumes, session = session)
  observeEvent(input$download, ignoreInit = TRUE, {
    req(!is.integer(input$download))
    save_path <- shinyFiles::parseSavePath(volumes, input$download)
    values$finances$save_excel(
      dir = dirname(save_path$datapath),
      file = tools::file_path_sans_ext(basename(save_path$datapath))
    )
  })
}
