## code to prepare `dataraw` dataset goes here

compo <- get_compo(compo = c("COTYCONS", "COTYCON2"), inforce = TRUE, contexte_choisi = 1)

usethis::use_data(compo, internal = TRUE)
