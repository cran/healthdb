## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("healthdb")

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(dbplyr)
library(lubridate)
library(glue)
library(purrr)
library(healthdb)

## -----------------------------------------------------------------------------
# make_test_dat() makes either a toy data.frame or database table in memory with known number of rows that satisfy the query we will show later
claim_db <- make_test_dat(vals_kept = c("303", "304", "305", "291", "292", glue("30{30:59}"), glue("29{10:29}"), noise_val = c("999", "111")), type = "database")

# this is a database table
# note that in-memory SQLite database stores dates as numbers
claim_db %>% head()

## -----------------------------------------------------------------------------
hosp_df <- make_test_dat(vals_kept = c(glue("F{10:19}"), glue("F{100:199}"), noise_val = "999"), type = "data.frame")

# this is a local data.frame/tibble
hosp_df %>% head()

# convert Date to numeric to be consistent with claim_db
hosp_df <- hosp_df %>% 
  mutate(dates = julian(dates))

## ----eval=FALSE---------------------------------------------------------------
# ## not run
# claim_db %>%
#   # identify the target codes
#   filter(if_any(starts_with("diagx"), ~ str_like(., c("291%", "292%", "303%", "304%", "305%")))) %>%
#   # each clnt has at least 2 records on different dates
#   group_by(clnt_id) %>%
#   # the n_distinct step is mainly for reducing computation in the next step
#   filter(n_distinct(dates) >= 2) %>%
#   # any two dates within one year?
#   filter((max(dates) - min(dates)) <= 365)
# ## end

## -----------------------------------------------------------------------------
result1 <- claim_db %>%
  identify_row(
vars = starts_with("diagx"),
match = "start",
vals = c(291:292, 303:305)
  )

## -----------------------------------------------------------------------------
result2 <- result1 %>%
  exclude(
excl = identify_row(claim_db, starts_with("diagx"), "in", "111"),
by = "clnt_id"
  )

## -----------------------------------------------------------------------------
result3 <- result2 %>% restrict_n(
  clnt_id = clnt_id,
  n_per_clnt = 2,
  count_by = dates,
  # here we use filter mode to remove records that failed the restriction
  mode = "filter"
)

## -----------------------------------------------------------------------------
result4 <- result3 %>% restrict_date(
  clnt_id = clnt_id,
  date_var = dates,
  n = 2,
  within = 365,
  uid = uid,
  # here we use flag mode to flag records that met the restriction instead of removing those
  mode = "flag"
)

## -----------------------------------------------------------------------------
# Class of result4
class(result4)

# execute query and download the result
result_df <- result4 %>% collect()

# Number of rows in source
nrow(claim_db %>% collect())

# Number of rows in the current result
nrow(result_df)

## -----------------------------------------------------------------------------
# make two look up tables
age_tab <- data.frame(
  clnt_id = 1:50,
  age = sample(1:90, 50),
  sex = sample(c("F", "M"), 50, replace = TRUE)
)
address_tab <- data.frame(
  clnt_id = rep(1:50, 5), year = rep(2016:2020, each = 50),
  area_code = sample(0:200, 50, replace = TRUE)
)

# get year from dates for matching

result_df <- result_df %>% mutate(year = lubridate::year(as.Date(dates, origin = "1970-01-01")))

# note that keys must be present in all tables
fetch_var(result_df,
          keys = c(clnt_id, year),
          linkage = list(
            # the formula means from_table ~ get_variable
            # |clnt_id means matching on clnt_id only
            age_tab ~ c(age, sex) | clnt_id,
            address_tab ~ area_code
          )
) %>%
  select(uid, clnt_id, dates, age, sex, area_code) %>% 
  head()

## -----------------------------------------------------------------------------
# build the full definition of SUD
sud_def <- build_def(
  # name of definition
  def_lab = "SUD",
  # place holder names for sources
  src_labs = c("claim", "hosp"),
  def_fn = define_case, # you could alter it and supply your own function
  # below are argumets of define_case
  fn_args = list(
    # if length = 1, the single element will be use for every source
    vars = list(starts_with("diagx")),
    match = "start", # match ICD starts with vals
    vals = list(c(291:292, 303:305), glue("F{10:19}")),
    clnt_id = clnt_id,
    n_per_clnt = c(2, 1),
    date_var = dates,
    within = c(365, NULL),
    uid = uid,
    mode = "flag"
  )
)

sud_def

## -----------------------------------------------------------------------------
sud_def$fn_call

## -----------------------------------------------------------------------------
# execute the definition
result_list <- sud_def %>%
  execute_def(with_data = list(
    claim = claim_db,
    hosp = hosp_df
  ))

## -----------------------------------------------------------------------------
# view the results
purrr::walk(result_list, ~ head(.) %>% print())

## -----------------------------------------------------------------------------
bind_source(result_list,
  # output_name = c(names in the list elements)
  src = "src",
  uid = "uid",
  clnt_id = "clnt_id",
  flag = c("flag_restrict_date", NA),
  # force_proceed is needed to collect remote tables to local memory
  force_proceed = TRUE
)

## -----------------------------------------------------------------------------
pool_case(result_list,
          def = sud_def,
          # your could skip summary with output_lvl = "raw"
          output_lvl = "clnt",
          # include records only from sources having valid records, see function documentation for more detail and other options
          include_src = "has_valid",
          force_proceed = TRUE)

