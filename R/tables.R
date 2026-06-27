#' @noRd
make_DT_table <- function(DF,
                          editable = FALSE,
                          selection = "single",
                          paging = TRUE,
                          scrollY = FALSE,
                          searching = TRUE) {
  if (!is_something(DF)) {
    return(DT::datatable(
      data.frame(x = " ", stringsAsFactors = FALSE)[0L, , drop = FALSE],
      options = list(
        dom = "t",
        # Simplify the table appearance
        paging = FALSE,
        # Disable pagination
        ordering = FALSE # Disable ordering
      ),
      rownames = FALSE,
      colnames = " "
    ))
  }
  x <- DF |> DT::datatable(
    selection = selection,
    editable = editable,
    rownames = FALSE,
    # fillContainer = TRUE,
    # extensions = "Buttons",
    options = list(
      columnDefs = list(list(
        className = "dt-center", targets = "_all"
      )),
      paging = paging,
      pageLength = 20L,
      fixedColumns = FALSE,
      ordering = TRUE,
      scrollY = scrollY,
      scrollX = TRUE,
      # autoWidth = TRUE,
      searching = searching,
      # dom = "Bfrtip",
      # buttons = c("copy", "csv", "excel", "pdf", "print"),
      scrollCollapse = FALSE,
      stateSave = FALSE
    ),
    class = "cell-border",
    # filter = "top",
    escape = FALSE
  ) |>
    DT::formatStyle(colnames(DF), color = "#000")
  x
}
#' @noRd
make_DT_table_simple <- function(DF) {
  if (!is_something(DF)) {
    return(h3("No data available to display."))
  }
  DF |> DT::datatable() |> DT::formatStyle(colnames(DF), color = "#000")
}
