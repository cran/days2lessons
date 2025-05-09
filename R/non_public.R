## Generează (aleatoriu) o repartizare pe zile a tuplajelor și o explicitează,
#  împerechind în ordinea lor, profesorii și clasele din fiecare tuplaj
#  (amintim că tuplajele implică numai profesori care au și ore proprii).
#
explain_tpl <- function(Tpl) {
    if(is.null(Tpl)) return(NULL)
    nr <- nrow(Tpl)
    TP <- Tpl %>% 
          mutate(zl = factor(rep_len(sample(Zile), length.out = nr), 
                             levels = Zile, ordered = TRUE))
    map_dfr(1:nr, function(i) {
        vpr <- strsplit(TP[i, 1], " ")[[1]]
        vcl <- strsplit(TP[i, 2], " ")[[1]]
        data.frame(prof = vpr, cls = vcl, zl = TP[i, 3])
    })
}


## Pentru repartizarea (omogenă) pe zile a lecțiilor din afara tuplajelor,
# avem de ținut seama dacă profesorul este sau nu, implicat în cuplaje 
# (profesorul nu poate cumula în vreo zi, mai mult de 6, excepțional 7, ore).
#
who_affects_whom <- function(Lss) {
    Prof <- Lss$prof %>% unique()
    sgp <- Prof[nchar(Prof) == 3]
    if(length(sgp) == length(Prof)) 
        return(NULL)  # nu există cuplaje (deci nici cumul de ore/zi)
    # de la cine cumulează ore, cel care este angajat în cuplaje
    Tw1 <- map(sgp, function(P) {
        gp <- Prof[which(grepl(P, Prof))]
        if(length(gp) > 1) setdiff(gp, P)
    }) %>% setNames(sgp) %>% purrr::compact()
    # invers: pe cine influențează cu ore, cuplajele
    cup <- Prof[nchar(Prof) == 6]
    Tw2 <- lapply(cup, function(PP) {
        p12 <- vector("character")
        p1 <- substr(PP, 1, 3)
        p2 <- substr(PP, 4, 6)
        for(p in c(p1, p2))
            if(p %in% sgp) p12 <- c(p12, p, Tw1[[p]])
        # dacă există -- cuplajele "externe" (în care niciun membru 
        # nu are ore proprii) NU influențează alți profesori/cuplaje
        setdiff(p12, PP) %>% unique()
    }) %>% setNames(cup) %>% compact()
    list(Tw1, Tw2)
}


