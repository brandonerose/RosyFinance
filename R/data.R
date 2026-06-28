BLANK_INCOMES <- data.frame(
  name = character(0),
  gross = integer(0),
  take_home = integer(0),
  pre_tax_deductions = integer(0)
)
BLANK_EXPENSES <- data.frame(
  name = character(0),
  category = character(0),
  amount = integer(0),
  type = character(0),
  due_month = integer(0),
  due_day = integer(0)
)
BLANK_ASSETS <- data.frame(
  name = character(0),
  value = integer(0),
  growth = numeric(0),
  contribution = integer(0),
  employer_contribution = integer(0),
  contribution_tax_type = character(0),
  income_link = character(0)
)
BLANK_DEBTS <- data.frame(
  name = character(0),
  value = integer(0),
  interest_rate = numeric(0),
  payment = integer(0)
)
BLANK_YEARS <- data.frame(
  name = character(0),
  incomes = integer(0),
  expenses = integer(0),
  assets = integer(0),
  debts = integer(0)
)
BLANK_FINANCES <- list(
  incomes = BLANK_INCOMES,
  expenses = BLANK_EXPENSES,
  assets = BLANK_ASSETS,
  debts = BLANK_DEBTS,
  years = BLANK_YEARS
)
#' @title load_finances
#' @param finances named data_list from `finances$save_excel`. If NULL will load
#'  blank. If missing (default) it will load sample. Otherwise will use provided
#'  data.
#' @param file_path Optional. if file_path provided will load from that file. If
#' provided, will ignore any finances object provided to function
#' @export
load_finances <- function(finances, file_path = NULL) {
  if(missing(finances)){
    finances <- load_sample_finances()$data
  }
  if(is.null(finances)){
    finances <- BLANK_FINANCES
  }
  FinancialData$new(finances = finances, file_path = file_path)
}
FinancialData <- R6::R6Class(
  "FinancialData",
  active = list(
    data = function(value) {
      if (!missing(value)) {
        cli_alert_danger("`data` is read only. To change use public methods")
      }
      private$project$data
    },
    transformation = function(value) {
      if (!missing(value)) {
        cli_alert_danger("`data` is read only. To change use public methods")
      }
      private$project$transformation
    }
  ),
  public = list(
    initialize = function(finances = BLANK_FINANCES, file_path = NULL) {
      if(!is.null(file_path)){
        if(file.exists(file_path)){
          finances <- REDCapSync:::excel_to_list(file_path)
        }else{
          message("`file_path` doesn't exist... ", file_path)
        }
      }
      private$project$data <- finances
      private$project$data <- transform_data(private$project$data)
      invisible(self)
    },
    add_income = function(name,
                          gross,
                          take_home,
                          pre_tax_deductions = 0) {
      # asserts
      checkmate::assert_character(name)
      # data
      private$project$data <-
        private$project$data |>
        update_entry(
          table_name = "incomes",
          entry_name = name,
          entry = transform_data_incomes(
            data.frame(
              name = name,
              gross = gross,
              take_home = take_home,
              pre_tax_deductions = pre_tax_deductions
            )
          )
        )
      invisible(self)
    },
    remove_incomes = function(name = NULL) {
      incomes <- private$project$data$incomes
      incomes <- incomes[which(incomes$name != name), ]
      private$project$data$incomes <- incomes
      invisible(self)
    },
    add_expense = function(name,
                           category,
                           amount,
                           type = "monthly",
                           due_month = NA,
                           due_day = 1) {
      # asserts
      checkmate::assert_character(name)
      checkmate::assert_choice(type, c("monthly", "yearly", "biannual"))
      # add
      private$project$data <-
        private$project$data |>
        update_entry(
          table_name = "expenses",
          entry_name = name,
          entry = transform_data_expenses(
            data.frame(
              name = name,
              category = category,
              amount = amount,
              type = type,
              due_month = due_month,
              due_day = due_day
            )
          )
        )
      invisible(self)
    },
    remove_expenses = function(name = NULL) {
      expenses <- private$project$data$expenses
      expenses <- expenses[which(expenses$name != name), ]
      private$project$data$expenses <- expenses
      invisible(self)
    },
    add_asset = function(name,
                         value,
                         growth = 0,
                         contribution = 0,
                         employer_contribution = 0,
                         contribution_tax_type = "Post",
                         income_link = NA) {
      # asserts
      checkmate::assert_character(name)
      # data
      private$project$data <-
        private$project$data |>
        update_entry(
          table_name = "assets",
          entry_name = name,
          entry = transform_data_assets(
            data.frame(
              name = name,
              value = value,
              growth = growth,
              contribution = contribution,
              employer_contribution = employer_contribution,
              contribution_tax_type = contribution_tax_type,
              income_link = income_link
            )
          )
        )
      invisible(self)
    },
    remove_assets = function(name = NULL) {
      assets <- private$project$data$assets
      assets <- assets[which(assets$name != name), ]
      private$project$data$assets <- assets
      invisible(self)
    },
    add_debt = function(name, value, interest_rate, payment) {
      # asserts
      checkmate::assert_character(name)
      # data
      private$project$data <-
        private$project$data |>
        update_entry(
          table_name = "debts",
          entry_name = name,
          entry = transform_data_debts(
            data.frame(
              name = name,
              value = value,
              interest_rate = interest_rate,
              payment = payment
            )
          )
        )
      # add
      invisible(self)
    },
    remove_debts = function(name = NULL) {
      debts <- private$project$data$debts
      debts <- debts[which(debts$name != name), ]
      private$project$data$debts <- debts
      invisible(self)
    },
    calc_incomes = function(yearly = TRUE) {
      incomes <- sum(private$project$data$incomes$take_home, na.rm = TRUE)
      if(!yearly){
        incomes <- incomes / 12
      }
      incomes
    },
    calc_expenses = function(yearly = TRUE,
                             include_debts = TRUE,
                             include_assets = TRUE) {
      expenses <- sum(private$project$data$expenses$yearly_amount, na.rm = TRUE)
      if (include_debts) {
        yearly_debt <- sum(private$project$data$debts$payment, na.rm = TRUE) * 12
        expenses <- expenses + yearly_debt
      }
      if (include_assets) {
        # yearly_growth <- sum(private$project$data$assets$yearly_growth, na.rm = TRUE)
        yearly_contribution <- sum(private$project$data$assets$contribution[which(private$project$data$assets$contribution_tax_type == "Post")], na.rm = TRUE)
        expenses <- expenses + yearly_contribution
      }
      if(!yearly){
        expenses <- expenses / 12
      }
      expenses
    },
    calc_assets = function() {
      sum(private$project$data$assets$value, na.rm = TRUE)
    },
    calc_debts= function() {
      sum(private$project$data$debts$value, na.rm = TRUE)
    },
    calc_left_over = function(yearly = TRUE,
                              include_debts = TRUE,
                              include_assets = TRUE) {
      incomes <- self$calc_incomes(yearly = yearly)
      expenses <- self$calc_expenses(yearly = yearly,
                                     include_debts = include_debts,
                                     include_assets = include_assets)
      incomes - expenses
    },
    calc_net_worth = function() {
      self$calc_assets() - self$calc_debts()
    },
    make_sankey = function(yearly = TRUE,
                           include_assets = TRUE,
                           include_debts = TRUE) {
      incomes <- private$project$data$incomes
      expenses <- private$project$data$expenses
      assets <- private$project$data$assets
      debts <- private$project$data$debts
      flow_df <- data.frame(from = character(0),
                            to = character(0),
                            value = integer(0)) |> as_tibble()
      debt_expenses <- debts[which(debts$value > 0), ]
      debt_expenses <- debt_expenses[which(debt_expenses$yearly_payment > 0), ]
      if (nrow(debt_expenses) > 0) {
        debt_expenses$category <- "Debt"
        debt_expenses$amount <- debt_expenses$payment
        debt_expenses$monthly_amount <- debt_expenses$payment
        debt_expenses$yearly_amount <- debt_expenses$payment * 12
        debt_expenses <- debt_expenses[, c("name",
                                           "category",
                                           "amount",
                                           "yearly_amount",
                                           "monthly_amount")]
        expenses <- expenses |> bind_rows(debt_expenses)
        debt_expenses$value <- debt_expenses$yearly_amount |> as.integer()
        debt_expenses$from <- debt_expenses$category
        debt_expenses$to <- debt_expenses$name
        flow_df <- flow_df |> bind_rows(debt_expenses[, c("from", "to", "value")])
      }
      if(!yearly){
        incomes$take_home <- incomes$take_home/12
        expenses$yearly_amount <- expenses$yearly_amount/12
        assets$contribution <- assets$contribution/12
        debts$yearly_payment <- debts$yearly_payment/12
        flow_df$value <- flow_df$value/12
      }
      nodes <- tibble( # need to account for same names accross tables
        name = c(
          incomes$name,
          "Combined Income",
          expenses$category,
          "Total Expenses",
          debts$name,
          assets$name,
          "Total Debt",
          "Left Over",
          "Total Assets"
        )
      )  |>
        distinct() |>
        mutate(id = row_number() - 1)
      assest_sum <- sum(assets$value)
      debt_sum <- sum(debts$value)
      expenses_sum <- sum(expenses$yearly_amount)
      income_sum <- sum(incomes$take_home)
      pre_tax_assets <- assets[which(assets$contribution_tax_type == "Pre"), ]
      post_tax_assets <- assets[which(assets$contribution_tax_type == "Post"), ]
      pre_tax <- sum(pre_tax_assets$contribution)
      post_tax <- sum(post_tax_assets$contribution)
      left_over <-  income_sum - expenses_sum - post_tax
      links <- bind_rows(
        # Income → Combined
        tibble(
          source = get_id(nodes, incomes$name),
          target = get_id(nodes, "Combined Income"),
          value  = incomes$take_home
        ),
        # Income → Asset Pre tax
        tibble(
          source = get_id(nodes, incomes$name),
          target = get_id(nodes, pre_tax_assets$name),
          value  = pre_tax_assets$contribution
        ),
        # Combined → Total Expenses
        tibble(
          source = get_id(nodes,"Combined Income"),
          target = get_id(nodes,"Total Expenses"),
          value  = expenses$yearly_amount |> sum(na.rm = TRUE)
        ),
        # Combined → Left Over
        tibble(
          source = get_id(nodes, "Combined Income"),
          target = get_id(nodes, "Left Over"),
          value  = left_over
        ),
        # # tibble(
        # #   source = get_id(nodes,"Combined Income"),
        # #   target = get_id(nodes,"Total Assets"),
        # #   value  = sum(incomes$value) |> magrittr::subtract(
        # #     expenses$value |> sum(na.rm = TRUE)
        # #   )
        # # ),
        # # Combined → post_tax_assets
        tibble(
          source = get_id(nodes, "Combined Income"),
          target = get_id(nodes, post_tax_assets$name),
          value  = post_tax_assets$contribution
        ),
        # Total Expenses → Expense categories
        tibble(
          source = get_id(nodes, "Total Expenses"),
          target = get_id(nodes, expenses$category),
          value  = expenses$yearly_amount
        ),
        # # Expense categories → Accounts (FLOW)
        tibble(
          source = get_id(nodes, flow_df$from),
          target = get_id(nodes, flow_df$to),
          value  = flow_df$value
        )
      )
      if(include_assets){
        links <- links |>
          bind_rows(
            tibble(
              source = get_id(nodes, assets$name),
              target = get_id(nodes, "Total Assets"),
              value  = assets$value |> as.integer()
            )
          )
      }
      if(include_debts){
        links <- links |>
          bind_rows(
            tibble(
              source = get_id(nodes, debts$name),
              target = get_id(nodes, "Total Debt"),
              value  = debts$value
            )
          )
      }
      plotly::plot_ly(
        type = "sankey",
        arrangement = "freeform",
        node = list(
          label = nodes$name,
          pad = 15,
          thickness = 20
        ),
        link = list(
          source = links$source,
          target = links$target,
          value  = links$value
        )
      )
    },
    make_treemap = function(yearly = TRUE,
                            include_assets = TRUE,
                            include_debts = TRUE) {
      incomes <- private$project$data$incomes
      expenses <- private$project$data$expenses
      assets <- private$project$data$assets
      debts <- private$project$data$debts
      if(!yearly){
        incomes$take_home <- incomes$take_home/12
        expenses$yearly_amount <- expenses$yearly_amount/12
        assets$contribution <- assets$contribution/12
        debts$yearly_payment <- debts$yearly_payment/12
        debts$yearly_interest <- debts$yearly_interest/12
      }
      pre_tax_assets <- assets[which(assets$contribution_tax_type == "Pre"), ]
      post_tax_assets <- assets[which(assets$contribution_tax_type == "Post"), ]
      assets$parent <- "Assests"
      assets$annual_contribution <- assets$contribution
      assets$label <- assets$name
      assets$color <- "#15BF34"
      debts$parent <- "Debt"
      debts <- debts[which(debts$value > 0),]
      debts$label <- paste0(debts$name, " - ", round(debts$interest_rate *100,1), " %")
      debts$color <- "#EB7E00"
      expenses$parent <- "Expenses"
      expenses$value <- expenses$yearly_amount
      expenses$label <- expenses$name
      expenses$color <- "#DBAA76"
      add_on <- debts[which(debts$yearly_payment > 0),]
      if(nrow(add_on)>0){
        add_on <- data.frame(
          parent = "Expenses",
          value = add_on$yearly_payment,
          label = paste0("Payments for ", add_on$name),
          color = add_on$color
        )
        expenses <- expenses |> bind_rows(add_on)
      }
      add_on <- assets[which(assets$contribution > 0 & assets$contribution_tax_type == "Post"),]
      if(nrow(add_on)>0){
        add_on <- data.frame(
          parent = "Expenses",
          value = add_on$contribution,
          label = paste0("Payments for ", add_on$name),
          color = add_on$color
        )
        expenses <- expenses |> bind_rows(add_on)
      }
      expenses_order <- expenses$value |> order(decreasing = TRUE)
      expenses <- expenses[expenses_order, ]
      incomes$parent <- "Income"
      incomes$value <- incomes$take_home
      incomes$label <- incomes$name
      incomes$color <- "#00E808"
      assest_sum <- sum(assets$value)
      debt_sum <- sum(debts$value)
      expenses_sum <- sum(expenses$value)
      income_sum <-sum(incomes$value)
      pre_tax <- sum(pre_tax_assets$contribution)
      post_tax <- sum(post_tax_assets$contribution)
      left_over <-  income_sum - expenses_sum
      annual_interest <-sum(debts$yearly_interest)
      annual_payments <-sum(debts$yearly_payment)
      #current payment
      (debt_sum/(annual_payments - annual_interest))
      # max repayment
      (debt_sum/(annual_payments - annual_interest + left_over))
      total_scale <-  income_sum
      final_df <- bind_rows(
        tibble(
          label = "Expenses",
          parent = "Income",
          value = expenses_sum
        ),
        tibble(
          label = "Left Over",
          parent = "Income",
          value = left_over
        ),
        tibble(
          label = "Income",
          parent = "Total",
          value = income_sum
        ),
        expenses %>% transmute(
          label = label,
          parent = parent,
          value = value
        )
      )
      if(include_assets){
        total_scale <- total_scale + assest_sum
        final_df <- final_df |>
          bind_rows(
            tibble(
              label = "Assests",
              parent = "Total",
              value = assest_sum
            ),
            assets %>% transmute(
              label = label,
              parent = parent,
              value = value)
          )
      }
      if(include_debts){
        total_scale <- total_scale + debt_sum
        final_df <- final_df |>
          bind_rows(
            tibble(
              label = "Debt",
              parent = "Total",
              value = debt_sum
            ),
            debts %>% transmute(
              label = label,
              parent = parent,
              value = value
            )
          )
      }
      final_df <- tibble(
        label = "Total",
        parent = "",
        value = total_scale
      ) |>  bind_rows(final_df)
      plotly::plot_ly(
        final_df,
        type = "treemap",
        labels = ~label,
        parents = ~parent,
        values = ~value,
        branchvalues = "total",
        textinfo = "label+value+percent parent"
        # color = ~color
      )
    },
    save_excel = function(dir, file = "finances"){
      self$data |>
        untransform_data() |>
        process_df_list() |>
        REDCapSync:::list_to_excel(dir = dir, file = file)
    },
    print = function(...) {
      str(self$data)
    }
  ),
  private = list(project = list(data = NULL))
)
update_entry <- function(data_list, table_name, entry_name, entry) {
  old <- data_list[[table_name]]
  new <- old[which(old$name != entry_name), ] |> bind_rows(entry)
  data_list[[table_name]] <- new
  data_list
}
transform_data_incomes <- function(incomes) {
  if(!is_something(incomes)){
    return(incomes)
  }
  # expenses <- BLANK_EXPENSES # assert
  incomes$name <- as.character(incomes$name)
  incomes$gross <- as.integer(incomes$gross)
  incomes$take_home <- as.integer(incomes$take_home)
  incomes$monthly_gross <- as.integer(incomes$gross / 12)
  incomes$monthly_amount <- as.integer(incomes$take_home / 12)
  incomes
}
transform_data_expenses <- function(expenses) {
  if(!is_something(expenses)){
    return(expenses)
  }
  expenses$name <- as.character(expenses$name)
  expenses$category <- as.character(expenses$category)
  expenses$amount <- as.integer(expenses$amount)
  expenses$type <- as.character(expenses$type)
  expenses$due_month <- as.integer(expenses$due_month)
  expenses$due_day <- as.integer(expenses$due_day)
  # expenses <- BLANK_EXPENSES # assert
  expenses$yearly_amount <- seq_along(expenses$amount) |>
    lapply(function(i) {
      amount <- expenses$amount[i]
      switch (
        expenses$type[i],
        yearly = amount,
        biannual = amount * 2,
        monthly = amount * 12
      )
    }) |> unlist() |>
    as.integer()
  expenses$monthly_amount <- as.integer(expenses$yearly_amount / 12)
  expenses
}
transform_data_assets <- function(assets) {
  if(!is_something(assets)){
    return(assets)
  }
  # expenses <- BLANK_EXPENSES # assert
  assets$name <- as.character(assets$name)
  assets$value <- as.integer(assets$value)
  assets$growth <- as.numeric(assets$growth)
  assets$contribution <- as.integer(assets$contribution)
  assets$employer_contribution <- as.integer(assets$employer_contribution)
  assets$contribution_tax_type <- as.character(assets$contribution_tax_type)
  assets$income_link <- as.character(assets$income_link)
  assets$yearly_growth <- as.integer(assets$value * assets$growth)
  assets
}
transform_data_debts <- function(debts) {
  if(!is_something(debts)){
    return(debts)
  }
  # debts assert
  debts$name <- as.character(debts$name)
  debts$value <- as.integer(debts$value)
  debts$interest_rate <- as.numeric(debts$interest_rate)
  debts$payment <- as.integer(debts$payment)
  debts$yearly_payment <- as.integer(debts$payment * 12)
  debts$yearly_interest <- as.integer(debts$value * debts$interest_rate)
  debts$monthly_interest <- debts$yearly_interest/12
  debts$years_to_payoff <- debts$name |>
    seq_along() |>
    lapply(function(i) {
      years_to_payoff(
        debt = debts$value[i],
        rate = debts$interest_rate[i],
        yearly_payment = debts$yearly_payment[i]
      )
    }) |> unlist() |>
    as.integer()
  debts
}
transform_data_years <- function(years) {
  if(!is_something(years)){
    return(years)
  }
  # years assert
  years$name <- as.character(years$name)
  years$incomes <- as.integer(years$incomes)
  years$expenses <- as.integer(years$expenses)
  years$assets <- as.integer(years$assets)
  years$debts <- as.integer(years$debts)
  years$networth <- years$assets - years$debts
  years$left_over <- years$incomes - years$expenses
  years
}
transform_data <- function(data_list) {
  data_list$incomes <- transform_data_incomes(data_list$incomes)
  data_list$expenses <- transform_data_expenses(data_list$expenses)
  data_list$assets <- transform_data_assets(data_list$assets)
  data_list$debts <- transform_data_debts(data_list$debts)
  data_list$years <- transform_data_debts(data_list$years)
  data_list
}
untransform_data <- function(data_list) {
  data_list$incomes <- data_list$incomes[,names(BLANK_INCOMES)]
  data_list$expenses <- data_list$expenses[,names(BLANK_EXPENSES)]
  data_list$assets <- data_list$assets[,names(BLANK_ASSETS)]
  data_list$debts <- data_list$debts[,names(BLANK_DEBTS)]
  all_character_cols_list(data_list)
}
all_character_cols <- function (DF) {
  as.data.frame(lapply(DF, as.character))
}
all_character_cols_list <- function (list) {
  lapply(list, all_character_cols)
}
load_sample_finances <- function() {
  x <- FinancialData$new()
  # income
  x$add_income("Income 1", 82000, 60000, 5000)
  x$add_income("Income 2", 80000, 58000, 5000)
  # expenses
  x$add_expense("Rent", "Housing", 2000)
  x$add_expense("Gym", "Health", 65)
  x$add_expense("Pet Insurance", "Health", 80)
  x$add_expense("Spotify", "Media", 17)
  # assets
  x$add_asset("Checking", 5000)
  x$add_asset("Emergency Fund", 3000)
  x$add_asset("Car 1", 6000, -0.1)
  x$add_asset("Car 2", 15000, -0.1)
  x$add_asset("Retirement 1", 11000, 0.10, 5719, 7310, "Pre", "Income 1")
  x$add_asset("Retirement 2", 40000, 0.10, 2000, 1000, "Pre", "Income 2")
  x$add_asset("IRA 1", 3000, 0.10, 7500, 0, "Post")
  x$add_asset("IRA 2", 1000, 0.10, 7500, 0, "Post")
  x$add_asset("Life Insurance Cash Value", 18000, 0.10, 276)
  # debts
  x$add_debt("Federal Student Loan 1", 350000, 0.056, 442)
  x$add_debt("Federal Student Loan 2", 135000, 0.065, 360)
  x$add_debt("Private Student Loan 2", 130000, 0.046, 1090)
  x
}
