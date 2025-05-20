#' Repartizarea pe zile a tuplajelor
#'
#' Din distribuția pe zile a tuturor lecțiilor se deduc zilele în care
#' au fost repartizate tuplajele existente.
#'
#' @param DZ distribuția pe zile returnată de 'mount_days()'.
#' @param TP setul tuplajelor, dacă există.
#' @return setul tuplajelor, completat cu un câmp conținând ziua (sau zilele)
#'     în care a fost repartizat fiecare.
#' @export
#'
days2tupl <- function(DZ, TP) {
    DL <- TP %>% distinct() %>% mutate(zl = "")
    for(i in 1:nrow(DL)) {
        Pr <- strsplit(DL[i, 1], " ")[[1]]
        Cl <- strsplit(DL[i, 2], " ")[[1]]
        LL <- purrr::pmap(list(Pr, Cl), function(P, K)
                  DZ %>% filter(.data$prof == P & .data$cls == K) %>% 
                     pull(.data$zl) 
        )
        DL[i, 3] <- paste0(Reduce(intersect, LL), collapse= " ")
    }
    DL
}

