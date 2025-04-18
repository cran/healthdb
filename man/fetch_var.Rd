% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_var.R
\name{fetch_var}
\alias{fetch_var}
\title{Get variables from multiple tables with common ID columns}
\usage{
fetch_var(data, keys, linkage, ...)
}
\arguments{
\item{data}{A data.frame or remote table (tbl_sql) which must be an object and not from a pipe. It would be used as the x argument in left_join().}

\item{keys}{A vector of quoted/unquoted variable names, or 'tidyselect' expression (see \code{\link[dplyr:select]{dplyr::select()}}). These variables must be present in \code{data} and would be used as the \code{by} argument in left_join(). The y tables must have a subset of these if not all.}

\item{linkage}{A list of formulas in the form of "from_tab ~ get_vars|by_keys":
\itemize{
\item source table on the left-hand-side
\item variables on the right-hand-side
\item If a source table does not have all the variables in \code{keys}, use "|" on RHS to specify the subset of \code{keys} to be used.
}

For example, given \code{keys} has 3 variables,
list(
y1 ~ tidyselect_expr1,
y2 ~ tidyselect_expr2|key1 + key2)

meaning:
\enumerate{
\item from table y1 get variables picked by the tidyselect expression matching on all 3 keys;
\item from table y2 get variables matching on only key1 and key2.
}}

\item{...}{Additional arguments, e.g., \code{copy = TRUE}, passing to left_join().}
}
\value{
A data.frame or remote table containing all original columns of x and new variables matched from other tables based on the specified linkage.
}
\description{
This function fetches variables from different tables that linked by common IDs. It calls \code{\link[dplyr:mutate-joins]{dplyr::left_join()}} multiple times with various source tables (y argument of the join) to gather variables. It is not meant to replace left_join() but simplify syntax for the situation that you started off a table of study sample and wanted to gather covariates from different sources linked by common client IDs, which is often the case when working with healthcare databases.
\strong{Caution}: this function is intended for one-to-one joins only because it could be problematic when we do not know which source caused a one-to-many join and changed the number of rows. For data.frame input, an error will be given when one-to-many joins were detected. However, such checking could be an expensive operation on remote source. Therefore, for database input, the result will not be checked.
}
\examples{
# make toy data
size <- 30
n <- 10
df1 <- data.frame(
  id = sample(1:n, size = size, replace = TRUE),
  service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2022-01-31"), by = 1),
    size = size
  )
) \%>\%
  dplyr::mutate(year = lubridate::year(service_dt))
df2 <- data.frame(
  id = rep(1:n, size / n), year = rep(2020:2022, each = n),
  status_1 = sample(0:1, size = size, replace = TRUE),
  status_2 = sample(0:1, size = size, replace = TRUE)
)
df3 <- data.frame(id = 1:n, sex = sample(c("F", "M"), size = n, replace = TRUE))

# simple joins
# note that for left_join(df1, df2), boths keys have to be used,
# otherwise, error as the relation would not be one-to-one
fetch_var(df1,
  keys = c(id, year),
  linkage = list(
    df2 ~ starts_with("s"), # match both keys without '|'
    df3 ~ sex | id
  ) # match by id only; otherwise failed because df3 has no year
)

# example if some y is remote
# make df2 as database table
db2 <- dbplyr::tbl_memdb(df2)

fetch_var(df1,
  keys = c(id, year),
  linkage = list(
    db2 ~ starts_with("s"),
    df3 ~ sex | id
  ),
  copy = TRUE # pass to left_join for forced collection of remote table
)
}
