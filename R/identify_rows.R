#' Identify rows with a match
#'
#' @md
#' @export
#' @description
#' Filter rows which values satisfy the specified conditions. The functionality is identical to [dplyr::filter()] combined with [dplyr::if_any()] or [dplyr::if_all()], but it used the 'data.table' package `vignette("datatable-intro", package = "data.table")` for data.frame method, and has regular regular expression support for remote database tables. The motivation is to take away some pain when working with databases which often do not support regular expression and 'LIKE' operator with multiple string patterns.
#'
#' @param data Data.frames or remote tables (e.g., from [dbplyr::tbl_sql()])
#' @param vars An expression passing to [dplyr::select()]. It can be quoted/unquoted column names, or helper functions, such as [dplyr::starts_with()].
#' @param match One of "in", "start", "regex", "like", "between", and "glue_sql". It determines how values would be matched. The operations under each type:
#'  - "in": var %in% vals (This is default)
#'  - "regex": stringr::str_detect(var, vals). For remote tables, unique values in vars are collected locally before matching (may be slow).
#'  - "like": stringr::str_like(var, vals). For remote tables, WHERE var LIKE val.
#'  - "start": same as regex or LIKE with modified vals, e.g., "^val1|^val2" or "va1%|val2%"
#'  - "between": dplyr::between(var, val1, val2)
#'  - "glue_sql": For remote table only, this gives full control of the WHERE clause using dplyr::filter(dbplyr::sql(glue::glue_sql(...)))
#' @param vals Depending on `match`, it takes different input:
#'  - "in": a vector of values (numeric/character/Date)
#'  - "start": a vector of numeric/character that would be modified into a regex or LIKE pattern string by adding "^" in front or "%" at the end
#'  - "regex"/"like": a string of the expression
#'  - "between": a vector of numeric or date with exactly two elements, e.g., c(lower, upper)
#'  - "glue_sql": a string of a SQL WHERE clause, which will be passed to [glue::glue_sql()]. See examples for detail.
#' @param if_all A logical for whether combining the predicates (if multiple columns were selected by vars) with AND instead of OR. Default is FALSE, e.g., var1 in vals OR var2 in vals.
#' @param verbose A logical for whether printing explanation and result overview for the query. Default is fetching from options. Use `options(healthdb.verbose = FALSE)` to suppress once and for all. Result overview is not for remote tables as the query is not executed immediately, thus no result is available for summary without adding an extra run (may be slow) of the query.
#' @param query_only A logical for whether keeping the output as remote table (Default TRUE) or downloading the query result as a tibble (FALSE). The argument is ignored when the input data is a data.frame/tibble.
#' @param ... For remote table method only. Additional arguments passing to [glue::glue_sql()] for parameterized queries.
#'
#' @return A data.frame or tbl_sql object depending on the input.
#'
#' @examples
#' #applying to data.frame; both sepal length and width in range 3-5
#' identify_row(iris, starts_with("Sepal"), "between", c(3, 5), if_all = TRUE)
#'
#' #applying to remote table; species starts with se or ends with ca
#' iris_db <- dbplyr::memdb_frame(iris)
#' identify_row(iris_db, Species, "like", c("se%", "%ca"))
#'
#' #using glue_sql to write the WHERE clause
#' #use {`vars`} to refer to the variables selected by vars
#' #supply additional values required in the query through '...'
#' #note that if you use LIKE here, you cannot supply multiple patterns in what
#' identify_row(iris_db, Species, "glue_sql",
#'  "{`vars`} LIKE {what}",
#'   what = "se%")
#'
#' #add * after a vector
#' identify_row(iris_db, Species, "glue_sql",
#'  "{`vars`} IN ({what*})",
#'  what = c("setosa", "virginica"))
identify_row <- function(data, vars, match = c("in", "start", "regex", "like", "between", "glue_sql"), vals, if_all = FALSE, verbose = getOption("healthdb.verbose"), query_only = TRUE, ...) {
  rlang::check_required(vars)
  rlang::check_required(vals)
  UseMethod("identify_rows")
}

identify_rows <- identify_row
