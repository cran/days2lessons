#' Lista lecțiilor repartizate în câte o aceeași zi
#'
#' Asociază fiecărei zile, setul lecțiilor repartizate în acea zi 
#' (în format lung, 'prof/cls')
#'
#' @param DZ distribuția pe zile a tuturor lecțiilor
#' @return Listă conținând pentru fiecare zi, lecțiile acelei zile
#' @export
#'
less2days <- function(DZ)
    lapply(split(DZ, ~ zl), function(L) L[, 1:2])  # fără 'zl'

