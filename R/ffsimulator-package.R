#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#'
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table data.table
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data `%||%` .env
#' @importFrom stats median
#' @importFrom utils str
## usethis namespace: end
NULL

## data.table-related stuff
globalVariables(".")
.datatable.aware <- TRUE
