#' Lecții (inclusiv cuplaje) pentru o săptămână de lucru într-o școală.
#' @format
#' set numit 'LSS' de 870 lecții prof|cls (cu 66 profesori, din care 4 cuplaje
#'     și cu 32 clase). Pe săptămână, profesorii din 'LSS' au între 1 și 28 de
#'     lecții, iar clasele între 25 și 31 de ore.
#' \describe{
#'     \item{prof}{Cod de 3 sau 6 caractere, reprezentând un profesor,
#'         respectiv un cuplaj (doi profesori, pe grupe ale unei clase);
#'         primele două litere abreviază disciplina pe care este încadrat
#'         profesorul, iar cifra care urmează indexează profesorii
#'         de pe aceeași disciplină}
#'     \item{cls}{Clasa (două sau trei caractere) la care profesorul are de
#'         făcut o lecție} 
#' }
"LSS"
#'
#' Exemplu de tuplaje; lecțiile dintr-un tuplaj au a se desfășura simultan.
#' Atenție: considerăm că niciun profesor "extern" (fără ore proprii, ci 
#' numai în cuplaje) NU poate face parte din vreun tuplaj.
#' @format
#' set numit 'TPL', conținând 27 de tuplaje asociate setului 'LSS'. Un tuplaj 
#'     va angaja într-un același moment (zi și oră), doi (sau trei) profesori
#'     și două (respectiv, trei) clase (după anumite criterii, elevii reuniți
#'     ai claselor respective sunt redistribuiți ad-hoc în noi "clase", în
#'     locul și cu numele celor inițiale). De separat cu câte un spațiu, 
#'     profesorii, respectiv clasele tuplajului.
#' \describe{
#'     \item{prof}{Profesorii care trebuie să intre în același moment, la câte
#'         una dintre clasele respective}
#'     \item{cls}{Clasele la care profesorii din tuplaj au de făcut câte
#'         o lecție, în câte o aceeași zi (și oră).}
#'}
"TPL"

