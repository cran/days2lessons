#' Montează zilele pe lecții, echilibrat
#'
#' Repartizarea pe zile decurge separat: lecțiile celor neimplicați în 
#' cuplaje (plus dacă există, cuplajele "externe"), respectiv ale cuplajelor 
#' și membrilor acestora; în final, se adaugă repartizarea lecțiilor din
#' tuplaje (dacă există). 
#'
#' @param LSS Setul lecțiilor profesorilor și cuplajelor.
#' @param TPL Setul tuplajelor.
#' @param Dfh Diferența maximă între maximul și minimul de ore/zi la clase.
#' @return O repartiție pe zile a tuturor lecțiilor, relativ echilibrată.
#' @export  
#'
#' @examples
#' \donttest{
#'     R123 <- mount_days(LSS, TPL, Dfh = 3)
#' }

mount_days <- function(LSS, TPL = NULL, Dfh = 2) {
    # Profesorul implicat în cuplaje (dacă acestea există) cumulează orele
    # proprii cu cele ale cuplajelor în care este angajat
    Tw1 <- Tw2 <- NULL
    whw <- who_affects_whom(LSS)
    if(!is.null(whw)) {
        Tw1 <- whw[[1]]; Tw2 <- whw[[2]]
    }
    
    # Următoarele două funcții (interne lui 'mount_days()') folosesc
    # variabile definite în 'mount_days()' (mai sus, sau în partea finală).
     
    # 'mount_days_tandem()'
    #     repartizează pe zile lecțiile cuplajelor și membrilor acestora
    #     (furnizate în parametrul 'LS2')
    mount_days_tandem <- function(LS2) { 
        if(is.null(Tw1)) return(NULL)
        W <- list()
        nw <- names(Tw1)
        W[[nw[1]]] <- c(nw[1], Tw1[[nw[1]]])
        U <- Tw1[[nw[1]]]
        for(n in nw[-1]) {
            W[[n]] <- c(n, setdiff(Tw1[[n]], U))
            U <- union(U, Tw1[[n]])
        }
        # Distribuie lecțiile proprii ale profesorului și lecțiile 
        # cuplajelor din care acesta face parte. Pentru aceasta, se dispun
        # lecțiile în ordinea claselor și se etichetează liniile respective,
        # repetând de sus în jos, pe o nouă coloană, o aceeași permutare
        # (aleatorie) de zile. 
        # (matricea internă 'PERM' are drept coloane permutările de 1:5).
        assign_zl_twins <- function(P) {
            lss <- LS2 %>% filter(.data$prof %in% W[[P]])
            nr <- nrow(lss)
            lss %>% arrange(.data$cls) %>%
                mutate(zl = rep_len(PERM[, sample(120)], nr))
        }

        map_dfr(names(W), assign_zl_twins) %>%
            mutate(zl = factor(.data$zl, labels = Zile))
    }  # END mount_days_tandem()
    
    # 'mount_days_single()'
    #     repartizează pe zile lecțiile profesorilor ne-implicați în cuplaje
    #     (furnizate în parametrul 'LS1')
    # Se instituie matricea 'ZH' în care coloanele sunt numite după 
    # profesorii respectivi și vor înregistra pe parcurs (într-o copie a ei,
    # 'Zore'), numărul de ore plasate succesiv profesorilor în fiecare zi. 
    # Apoi, se partiționează lecțiile după clasă și pentru fiecare clasă 
    # (într-o ordine aleatorie) se invocă funcția internă 'labels_to_cls()'; 
    # în final (eventual, după o serie de reluări, până ce se reușește trecerea
    # prin toate clasele) se îmbină repartizările rezultate astfel pe lecțiile
    # fiecărei clase.
    mount_days_single <- function(LS1) {
        LS1$prof <- factor(LS1$prof, ordered = TRUE)
        ZH <- matrix(data = rep(0L, 5*nlevels(LS1$prof)), nrow = 5, 
                     byrow=TRUE, dimnames = list(1:5, levels(LS1$prof)))
        
        # Montează coloana zilelor alocate lecţiilor unei aceleiaşi clase.
        # Se dispun lecțiile clasei după profesori și se caută o permutare
        # de zile a cărei aplicare consecutivă pe lecțiile respective asigură
        # că până la momentul curent, distribuțiile pe zile ale lecțiilor
        # profesorilor respectivi (cumulând cu cele reflectate deja în 'Zore')
        # sunt cvasi-omogene (cu diferență <= două ore, între zile).
        # În caz de succes, se actualizează valorile din 'Zore' corespunzătoare
        # profesorilor clasei și se returnează repartizarea făcută clasei.
        labels_to_class <- function(Q) {
            nr <- nrow(Q)
            Qpr <- unique(Q$prof)
            for(h in 1:200) {  # încercări de alocare (schimbând ordini) 
                Q <- Q %>%  
                    arrange(match(.data$prof,
                                  sample(unique(.data$prof))), .data$prof)
                for(j in sample(120)) {  # pentru toate ordonările de zile
                    S <- Q %>% mutate(zl = rep_len(PERM[, j], nr)) 
                    M <- as.matrix(table(S[c('zl', 'prof')])) + Zore
                    appr <- TRUE
                    # acceptă diferențe de maximum 2 ore între zile
                    for(p in Qpr) {  
                        oz <- M[, p]
                        if(any(sapply(oz, '-', oz) > 2)) {
                            appr <- FALSE
                            break
                        }
                    }
                    if(appr) {   
                        Zore <<- M  # actualizează numărul de ore pe zi
                        return(S)  # alocare aprobată
                    }
                }
            }  # rezultă 'NULL' dacă încercările de alocare au eșuat
        }  # END 'labels_to_class()'

        FRM <- split(LS1, ~ cls)  # partiționează lecțiile pe clase
        lstCls <- names(FRM)
        # aplică aleatoriu 'labels_to_class()', până "trec" toate clasele 
        Lds <- vector("list", length(lstCls)) %>% 
               setNames(lstCls)  # va stoca repartițiile pe zile ale claselor
        repeat {
            sem <- TRUE  # semnalează succesul sau eșecul alocărilor
            Zore <- ZH  # (re)iniţializează matricea alocărilor pe zile
            lstCls <- sample(lstCls)  # ordonează aleatoriu clasele
            for(K in lstCls) {  # distribuie pe zile lecţiile fiecărei clase
                W <- labels_to_class(FRM[[K]])   ##  ; cat("*")
                if(is.null(W)) {   ##  ; cat("/ ")
                    sem <- FALSE  # semnalează eșecul alocării la clasa curentă
                    break  # abandonează restul claselor
                }
                Lds[[K]] <- W  # salvează distribuția lecțiilor clasei curente
            }
            if(sem) break  # 'Lds' are distribuții pe toate clasele
        }
        ##  cat("\n")
        bind_rows(Lds) %>% 
            mutate(zl = factor(.data$zl, labels = Zile))
    }  # END 'mount_days_single()'

    # Subseturile de lecții, pentru funcțiile interne 'mount_days_single()' și
    # respectiv pentru 'mount_days_tandem()'.
    LS1 <- LSS %>% filter(! .data$prof %in% union(names(Tw1), names(Tw2)))
    LS2 <- anti_join(LSS, LS1, by=c('prof', 'cls'))
    # 'LS1' cuprinde lecțiile celor care nu-s membri în vreun cuplaj,
    # plus eventual, cuplajele "externe" (ai căror membri nu au ore proprii).
    # 'LS2' cuprinde lecțiile cuplajelor și membrilor cu ore proprii.
    
    R1 <- mount_days_single(LS1)
    # În 'R1', lecțiile fiecărei clase sunt distribuite omogen pe zile;
    # invocăm repetat 'mount_days_tandem()' și 'explain_tpl()' până ce
    # reunind cele trei repartiții, pentru toate clasele rezultă distribuții
    # cvasi-omogene pe zile.
    # Se poate folosi 'Dfh'=3 (în loc de valoarea implicită 2), obținând
    # rezultatele mai rapid (și poate... mai sigur); dar atunci, probabil că
    # va trebui să se ajusteze interactiv repartiția rezultată, pentru a evita
    # totuși, distribuții neomogene apărute la unele clase.
    while(TRUE) {
        R2 <- mount_days_tandem(LS2)
        R3 <- explain_tpl(TPL)
        R123 <- rbind(R1, R2, R3)
        dfh <- apply(table(R123[c('cls', 'zl')]), 1, function(w) 
                     diff(range(w))) %>% as.vector()
        if(all(dfh <= Dfh)) break
    }
    R123  # o repartiție pe zile cvasi-omogenă, a tuturor lecțiilor
}  # END 'mount_days()'
