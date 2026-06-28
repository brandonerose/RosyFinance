years_to_payoff <- function(debt, rate, yearly_payment) {
  if (yearly_payment <= rate * debt) {
    return(Inf)  # never pays off
  }
  t <- -log(1 - (rate * debt) / yearly_payment) / log(1 + rate)
  return(t)
}
tax_brackets <- list(
  single = list(
    y2025 =  tibble::tribble(
      ~lower, ~upper, ~rate,
      000000, 011925, 0.10,
      011925, 048475, 0.12,
      048475, 103350, 0.22,
      103350, 197300, 0.24,
      197300, 250525, 0.32,
      250525, 626350, 0.35,
      626350, Inf,    0.37
    ),
    y2026 =  tibble::tribble(
      ~lower, ~upper, ~rate,
      000000, 012400, 0.10,
      012400, 050400, 0.12,
      050400, 105700, 0.22,
      105700, 201775, 0.24,
      201775, 256225, 0.32,
      256225, 640600, 0.35,
      640600, Inf,    0.37
    )
  ),
  married = list(
    y2025 =  tibble::tribble(
      ~lower, ~upper, ~rate,
      000000, 023850, 0.10,
      023850, 096950, 0.12,
      096950, 206700, 0.22,
      206700, 394600, 0.24,
      394600, 501050, 0.32,
      501050, 751600, 0.35,
      751600, Inf,    0.37
    ),
    y2026 =  tibble::tribble(
      ~lower, ~upper, ~rate,
      000000, 023850, 0.10,
      024800, 100800, 0.12,
      100800, 211400, 0.22,
      211400, 403550, 0.24,
      403550, 512450, 0.32,
      512450, 768700, 0.35,
      768700, Inf,    0.37
    )
  )
)
std_deductions <- list(
  single = list(
    y2025 = 15750,
    y2026 = 16100
  ),
  married = list(
    y2025 = 31500,
    y2026 = 32200
  )
)
calc_federal_tax <- function(income,
                             year = 2026,
                             file_type = "married",
                             deduction = 10000) {
  std_deduction <- std_deductions[[file_type]][[paste0("y", year)]]
  brackets <- tax_brackets[[file_type]][[paste0("y", year)]]
  taxable <- max(0, income - std_deduction - deduction)
  tax <- purrr::map_dbl(seq_len(nrow(brackets)), function(i) {
    lower <- brackets$lower[i]
    upper <- brackets$upper[i]
    rate  <- brackets$rate[i]
    if (taxable > lower) {
      taxed_amount <- min(taxable, upper) - lower
      taxed_amount * rate
    } else {
      0
    }
  }) |> sum()
  tax
}
calc_fica <- function(income, deduction = 10000) {
  income <- income - deduction
  # Social Security (cap ~168,600)
  ss_cap <- 184500
  ss <- min(income, ss_cap) * 0.062
  # Medicare
  medicare <- income * 0.0145
  # Additional Medicare >200k
  add_med <- 250000
  addl_medicare <- ifelse(income > add_med, (income - add_med) * 0.009, 0)
  ss + medicare + addl_medicare
}
take_home_pay_year <- function(income,
                               year = 2026,
                               file_type = "married",
                               deduction = 10000) {
  fed_tax <- calc_federal_tax(
    income = income,
    year = year,
    file_type = file_type,
    deduction = deduction
  )
  fica_tax <- calc_fica(income, deduction)
  total_tax <- fed_tax + fica_tax
  message(paste0(round(total_tax / income * 100, 1), "% tax rate"))
  income - total_tax - deduction
}
take_home_pay_month <- function(income,
                                year = 2026,
                                file_type = "married",
                                deduction = 10000) {
  take_home_pay_year(
    income = income,
    year = year,
    file_type = file_type,
    deduction = deduction
  ) / 12
}
get_id <- function(nodes, x) {
  nodes$id[match(x, nodes$name)]
}
