# Generate SQL expression for window function
# over("avg(x)", frame = c(-Inf, 0))
# over("avg(x)", order = "y")
# over("avg(x)", order = c("x", "y"))
# over("avg(x)")
over <- function(expr, partition = NULL, order = NULL, frame = NULL) {
  if (length(partition) == 0) {
    partition <- NULL
  }
  if (!is.null(partition)) {
    if (!is.sql(partition)) {
      partition <- ident(partition)
    }

    partition <- build_sql("PARTITION BY ",
      sql_vector(escape(partition), collapse = ", ", parens = FALSE))
  }
  if (!is.null(order)) {
    if (!is.sql(order)) {
      order <- ident(order)
    }

    order <- build_sql("ORDER BY ", sql_vector(escape(order), collapse = ", ", parens = FALSE))
  }
  if (!is.null(frame)) {
    if (is.null(order)) {
      warning(
        "Windowed expression '", expr, "' does not have explicit order.\n",
        "Please use arrange() to make determinstic.",
        call. = FALSE
        )
    }

    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    frame <- build_sql("ROWS ", frame)
  }

  over <- sql_vector(compact(list(partition, order, frame)), parens = TRUE)
  sql <- build_sql(expr, " OVER ", over)

  sql
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) stop("from must be less than to", call. = FALSE)

  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) return("CURRENT ROW")
    paste(val(x), dir(x))
  }

  if (to == 0) {
    sql(bound(from))
  } else {
    sql(paste0("BETWEEN ", bound(from), " AND ", bound(to)))
  }
}
