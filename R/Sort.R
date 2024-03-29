#[export]
Sort <- function(x, descending = FALSE, partial = NULL, stable = FALSE, na.last = NULL, parallel = FALSE) {
  if (stable) {
    .Call(Rfast_stable_sort, x, descending, parallel)
  }
  else if (!is.null(partial)) {
    .Call(Rfast_partial_sort, x, partial, descending, parallel)
  } else if (is.character(x)) {
    .Call(Rfast_sort_string, x, descending, parallel)
  } else {
    if (identical(na.last, FALSE)) {
      .Call(Rfast_Sort_na_first, x, descending, parallel)
    } else {
      .Call(Rfast_Sort, x, descending, na.last, parallel)
    }
  }
}

#[export]
Sort.int <- function(x) {
  .Call(Rfast_sort_int, x)
}

#[export]
rowSort <- function(x, descending = FALSE, stable = FALSE, parallel = FALSE,cores = 0) {
  .Call(Rfast_sort_mat, x, descending, TRUE, stable, parallel,cores)
}

#[export]
colSort <- function(x, descending = FALSE, stable = FALSE, parallel = FALSE,cores = 0) {
  .Call(Rfast_sort_mat, x, descending, FALSE, stable, parallel,cores)
}

#[export]
sort_mat <- function(x, by.row = FALSE, descending = FALSE, stable = FALSE, parallel = FALSE) {
  if (by.row) {
    .Defunct(paste("rowSort(x, descending = ",descending," stable = ",stable," parallel = ",parallel,")", "Rfast"))
  }else{
    .Defunct(paste("colSort(x, descending = ",descending," stable = ",stable," parallel = ",parallel,")", "Rfast"))
  }
}

#[export]
sort_cor_vectors <- function(x, base, stable = FALSE, descending = FALSE) {
  x[Rfast::Order(base, stable, descending)]
}