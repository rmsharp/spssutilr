# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
#' Converts factors to integer or numeric where possible within a dataframe.
#'
#' Takes a dataframe and converts any column to type integer if the column is
#' of type factor with integers as the factor names and to type numeric if the
#' column is of type factor with non-integer numerics as the factor names.
#'
#' @param df dataframe to be converted.
#' @export
factor_to_integer_or_numeric <- function(df) {
  for (i in seq_along(df)) {
    if (class(df[ , i]) == "factor") {
      if (!any(is.na(suppressWarnings(as.integer(levels(df[ , i])))))) {
        df[, i] <- as.integer(levels(df[, i])[df[ , i]])
      } else if (!any(is.na(suppressWarnings(as.numeric(levels(df[ , i])))))) {
        df[ , i] <- as.numeric(levels(df[, i])[df[ , i]])
      }
    }
  }
  df
}
#' Returns a named character vector with the column names as the vector
#' names and column descriptions as the vector values.
#'
#' @param df dataframe provided by \code{rio::import()}.
#' @import rio
#' @export
get_column_label <- function(df) {
  col_label <- attr(rio::gather_attrs(df), "label")

  col_desc <- as.character(col_label)
  names(col_desc) <- names(col_label)
  col_desc
}
#' Returns the column factor descriptions as a list from the dataframe
#' provided by \code{rio::import()}.
#'
#' @param df dataframe provided by \code{rio::import()}.
#' @import rio
#' @export
get_column_factor_desc <- function(df) {
  attr(rio::gather_attrs(df), "labels")
}
# #' Converts factor type variables of SPSS origin to R factors
# #'
# #' @param df dataframe provided by \code{rio::import()}.
# #' @export
# make_factors <- function(df) {
#   tmp_df <- df
#   for (i in seq_along(df)) {
#     if (!is.null(desc)) {
#       tmp_df[i] <- levels(df[[i]])[df[[i]]]
#     }
#   }
#   tmp_df
# }
#' Helper function for \code{rm_null_obs()} that tests whether an object is
#' either NULL a list of NULLs.
#'
#' Written by Josh O'Brien to handle a NULL values and a list filled with
#' NULL values.
#' @param x list that may contain NULL values or other lists with NULL values.
is_null_ob <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all NULL objects.
#'
#' Written by Josh O'Brien to handle a NULL values and a list filled with
#' NULL values. Found on
#' http://stackoverflow.com/questions/26539441/r-remove-null-elements-from-list-of-lists,
#' which was addapted from a function by G. Grothendieck (GKX Group,
#' GKX Associates Inc., tel: 1-877-GKX-GROUP;
#' http://stackoverflow.com/questions/25081263/is-there-a-super-fast-way-to-remove-all-null-values-in-a-list-and-all-sublists/25081427#25081427).
#' Josh's adaptation replaced \code{is.null()} with \code{is_null_ob}.
#'
#' @param x list that may contain NULL values or other lists with NULL values.
#' @export
rm_null_obs <- function(x) {
  x <- Filter(Negate(is_null_ob), x)
  lapply(x, function(x) if (is.list(x)) rm_null_obs(x) else x)
}
#' Converts SPSS \code{.sav} file to a list containing all of the information
#' in the \code{.sav} file.
#'
#' This is not a general purpose function as it handles a column (117) that
#' has values that do not correspond to the expected values represented by
#' the list of factors.
#'
#' A list is returned containing \code{column_label}, which is a named
#' character vector where the names are the column names of the dataframe
#' and the values are the column descriptions; \code{converted_list},
#' which is a list containing all of the columns in the original \code{.sav}
#' file; \code{column_length}, which is a named integer vector containing the
#' length of each column in \code{converted_list}; and \code{converted_df},
#' which is a dataframe containing all of the columns with values.
#'
#' @param sav_file fully qualified file name of the SPSS \code{.sav} file.
#' @param data_fix_fn optional function that replaces a column in the
#' dataframe provided by \code{rio::import()}.
#' This in needed when the original data set has a column that does not adhere
#' to standard datafrome structure.
#'
#' @import rio
#' @export
sav_to_df <- function(sav_file, data_fix_fn) {
  tmp_df <- rio::import(sav_file)
  if (!is.null(data_fix_fn))
    tmp_df <- data_fix_fn(tmp_df)
  column_label <- get_column_label(tmp_df)
  factor_desc <- get_column_factor_desc(tmp_df)
  tmp_list <- list()
  for (i in seq_along(tmp_df)) {
    if (is.null(factor_desc[[i]])) {
      tmp_list[names(tmp_df)[i]] <- list(as.factor(tmp_df[[i]]))
    } else {
      #tmp <- factor_desc[[i]]
      offset <- 1 - min(attr(tmp_df[ , i], "labels"))
      tmp_list[names(tmp_df)[i]] <- list(as.factor(names(
        attr(tmp_df[ , i], "labels")[as.numeric(tmp_df[ , i]) + offset])))
      # tmp_list[names(tmp_df)[i]] <-
      #   list(as.factor(names(sapply(as.numeric(tmp_df[ , i]),
      #                               function(x) {tmp[tmp == x]}))))
    }
  }
  column_length <- sapply(tmp_list, function(x) length(unlist(x)))
  converted_list <- lapply(tmp_list, function(x) levels(x)[x])
  #converted_list <- lapply(tmp_list, function(x) unlist(x))
  converted_df <- lapply(seq_along(converted_list), function(i) {
    if (column_length[i] > 0) converted_list[[i]]})
  converted_df <- rm_null_obs(converted_df)
  names(converted_df) <- names(column_length[column_length > 0])
  converted_df <- data.frame(converted_df)
  converted_df <- factor_to_integer_or_numeric(converted_df)
  list(column_labels = column_label, converted_list = converted_list,
       column_length = column_length, converted_df = converted_df)
}
