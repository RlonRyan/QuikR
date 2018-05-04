# ==================================================
# tablediff.R
# --------------------------------------------------
#
# Table diff functions.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' Internal function handling single-row diffs.
#'
.table.diff.row.internal <- function(i, V1, V2, digits) {
  # Convert to dataframe.
  V1 <- as.data.frame(V1);
  V2 <- as.data.frame(V2);

  # Remove non-numeric values.
  V1 <- dplyr::select_if(V1, is.numeric);
  V2 <- dplyr::select_if(V2, is.numeric);

  # Remove row names.
  rownames(V1) <- NULL;
  rownames(V2) <- NULL;

  # Compute delta.
  rd <- (V1 - V2);

  # Compute column names.
  cn <- colnames(rd);

  # If colnames is nothing, generate alternative.
  if(is.null(cn) || (length(cn) < ncol(rd))) {
    cn <- paste("V", 1:ncol(rd), sep='');
  }

  # Set column names.
  colnames(V1) <- cn;
  colnames(V2) <- cn;
  colnames(rd) <- cn;

  # Compute sums.
  sV1 <- rowSums(abs(V1));
  sV2 <- rowSums(abs(V2));
  sV3 <- rowSums(abs(rd));

  # Round values.
  sV1 <- round(sV1, digits);
  sV2 <- round(sV2, digits);
  sV3 <- round(sV3, digits);

  # Round Columns
  V1 <- round(V1, digits);
  V2 <- round(V2, digits);
  rd <- round(rd, digits);

  # Bind into single result table.
  rra <- c(rownum=i, type='actual',   delta=sV1, V1);
  rre <- c(rownum=i, type='expected', delta=sV2, V2);
  rrd <- c(rownum=i, type='delta',    delta=sV3, rd);

  # Bind into single matrix.
  result <- rbind.data.frame(rra, rre, rrd, stringsAsFactors = FALSE);
  rownames(result) <- 1:3;

  # Return the result.
  return(result);
}

#'
#' Computes the difference of two values, with the given precision.
#'
#' @param a the actual value.
#' @param e the expected value.
#' @param digits the number of decimal places to round values to.
#' @param incl.a if the acutal   value for each row should be included in the result.
#' @param incl.e if the expected value for each row should be included in the result.
#' @param incl.d if the delta    value for each row should be included in the result.
#' @param drop if non-differing rows should be dropped from the result.
#'
#' @name table.diff
#'
#' @export
#' @rdname table.diff
#'
table.diff <- function(a, e, digits=4, incl.a=FALSE, incl.e=FALSE, incl.d=TRUE, drop=FALSE) {
  # Convert to dataframes.
  a <- as.data.frame(a);
  e <- as.data.frame(e);

  # Dataframe to store difference.
  diff <- data.frame();

  # Iterate over each row.
  for (i in 1:nrow(a)) {
    # Get the two rows.
    rowa <- a[i,];
    rowe <- e[i,];

    # Compute row difference.
    rdiff <- .table.diff.row.internal(i, rowa, rowe, digits);

    # If not dropping, bind the row.
    if (!isTRUE(drop) || (rdiff[3,'delta'] > 0)) {
      # If including row a, bind the row.
      if(isTRUE(incl.a)) {
        diff <- rbind(diff, rdiff[1,]);
      }

      # If including row e, bind the row.
      if(isTRUE(incl.e)) {
        diff <- rbind(diff, rdiff[2,]);
      }

      # If including row d, bind the row.
      if(isTRUE(incl.d)) {
        diff <- rbind(diff, rdiff[3,]);
      }
    }

  }

  # Correct row numbers.
  rownames(diff) <- 1:nrow(diff);

  # Return result.
  return(diff);
}

#'
#' Diffs two tables and opens the result in a viewer.
#'
#' @export
#' @rdname table.diff
#'
table.vdiff <- function(a, e, ...) {
    # Compute names.
    nam.a <- deparse(substitute(a));
    nam.e <- deparse(substitute(e));

    # Combine names.
    nam <- sprintf("%s vs. %s", nam.a, nam.e);

    # Compute difference.
    diff <- table.diff(a, e, ...);

    # Open Viewer
    View(diff, nam);
}
