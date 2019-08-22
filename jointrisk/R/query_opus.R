data("compo")

#' @export
#' @importFrom data.table setDT dcast setnames
#' @importFrom ROracle dbGetQuery
get_risks <- function(con) {

  query <- paste0("

    SELECT POAS.POAS_NO,
           VEPC.PROD_CODE,
           POAS.INTE_NO,
           PRCH.PRCH_NO,
           VEPC.VEPC_ID,
           coalesce(ATGL.ATGL_CODE_INFO, cast(DEOB.ATGL_ID_DNM as varchar(15))) QUESTION,
           DEOB.DEOB_VAL REPONSE,
           DEOB.VAAT_ID_VAL

    FROM SOUS_TBL_POL_ASS             POAS,
         SOUS_TBL_PERD_COUVRT_POL_ASS PCPA,
         SOUS_TBL_COMBINSN_TRANSCT    COTR,
         SOUS_TBL_DETAIL_COMBINSN     DECT,
         SOUS_TBL_PERD_COUVRT         PECO,
         SOUS_TBL_VPROD_CHOISI        VEPC,
         SOUS_TBL_PROD_CHOISI         PRCH,
         SOUS_TBL_DETAIL_OBT          DEOB,
         PILO_TBL_ATTR_GLOB           ATGL

    WHERE
         POAS.VAAT_ID_LIG_AFF = 4
     AND POAS.VAAT_ID_CATEG_AFF = 2
     AND PCPA.POAS_ID = POAS.POAS_ID
     AND PCPA.VAAT_ID_TYPE_STATUT_DNM_STSO = 9
     AND sysdate BETWEEN PCPA.PCPA_DT_VIG AND NVL(PCPA.PCPA_DT_EXPIR, sysdate)
     AND COTR.PCPA_ID = PCPA.PCPA_ID
     AND COTR.COTR_IND_STA_PRCH_DNM = 'O'
     AND COTR.VAAT_ID_TYPE_STATUT_DNM_STSO = 9
     AND DECT.COTR_ID = COTR.COTR_ID
     AND DECT.DECT_IND_ACTIF = 'O'
     AND PECO.PECO_ID = DECT.PECO_ID
     AND sysdate BETWEEN PECO.PECO_DT_VIG AND NVL(PECO.PECO_DT_EXPIR, sysdate)
     AND VEPC.VEPC_ID = DECT.VEPC_ID_DNM
     AND VEPC.PROD_CODE IN ('MPF', 'MD', 'MB', 'MCO')
     AND PRCH.PRCH_ID = VEPC.PRCH_ID
     AND DEOB.VEPC_ID = VEPC.VEPC_ID
     AND DEOB.ATGL_ID_DNM IN (140, 959, 971, 1083, 1084, 1092, 9045, 9046, 9400, 9406, 9408, 9413, 10762, 10763, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
     AND ATGL.ATGL_ID = DEOB.ATGL_ID_DNM

  ")

  dt <- dbGetQuery(con, query)
  setDT(dt, key = "VEPC_ID")

  # Check that there is only one VEPC_ID per INTE_NO/POAS_NO/PRCH_NO combination
  if (length(unique(dt$VEPC_ID)) > nrow(unique(dt[,list(INTE_NO, POAS_NO, PRCH_NO)]))) {
    dups <- dt[, list(COUNT = length(unique(VEPC_ID))), by = list(INTE_NO, POAS_NO, PRCH_NO)][COUNT > 1]
    dups <- paste0(dups[,paste0(INTE_NO, ", ", POAS_NO, ", ", PRCH_NO)], collapse = "\n")
    warning(paste0("Multiple VEPC_ID exist for the following combination :\nINTE_NO, POAS_NO, PRCH_NO\n", dups, "Only the max VEPC_ID per combination will be kept."))
    keep <- dt[, list(VEPC_ID = max(VEPC_ID)), by = list(INTE_NO, POAS_NO, PRCH_NO)][,list(VEPC_ID)]
    dt <- dt[keep]
  }

  dt <- dcast(dt, INTE_NO + POAS_NO + PRCH_NO + PROD_CODE ~ QUESTION, value.var = "REPONSE")
  setnames(dt, "14367", "COMAUTBA", skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "SUTOOCCO", "RVEXTBET", "RVEXTBOI", "RVEXTPAP", "RVEXTBRI", "RVEXTALU", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.numeric), .SDcols = numcol])

  set(dt, j = "REVETEME", value = ifelse(rowSums(dt[,list(RVEXTBET, RVEXTBRI)], na.rm = TRUE) > 59, "O", "N"))

  dt[compo$COTYCONS,
     on = list(MURSTRUC = MURSTRUC,
               PLANCHER = PLANCHER,
               TOITSTRU = TOITSTRU,
               REVETEME = REVETEME),
     `:=`(TYPECONS = TYPECONS)]

  dt[compo$COTYCON2,
     on = list(RESAUFEU = RESAUFEU),
     `:=`(TYCONS2 = TYCONS2)]

  return(dt)

}


# Usage temporaire

library(ROracle)
library(data.table)
# la liste de connectString est dans U:\ccap\asge\sqlnet\TNSNAMES.ORA
con <- dbConnect(Oracle(),
                 Sys.getenv("USERNAME"),
                 Sys.getenv("USERNAME"),
                 dbname = "(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL=TCP)(HOST = opus_intg.bd.capitale.qc.ca)(PORT = 1710)))(CONNECT_DATA = (SERVICE_NAME = intg.capitale.qc.ca)(SERVER = DEDICATED)))",
                 bigint = "integer")

system.time(dt <- get_risks(con))

# il reste a peut-etre renommer les variables et valider les donnees
#       DETAIL    ID CODE_INFO                                       NOM_ABRG DATATYPE
#  1: AFFECTAT   140  AFFECTAT Affectation principale ***(imprimé au contrat)   CARACT
#  2: MURSTRUC   959  MURSTRUC                    Murs structure construction     CODE
#  3: TOITSTRU   971  TOITSTRU                             Toiture(structure)     CODE
#  4: SUPERREZ  1083  SUPERREZ                 Superficie du  rez-de-chaussee   ENTIER
#  5: SUTOOCCO  1084  SUTOOCCO             Superficie totale occupee commerce   ENTIER
#  6: UMESSUP2  1092  UMESSUP2                      Unite mesure=superficie 2     CODE
#  7: RVEXTBET  9045  RVEXTBET      Revet.ext.beton/bloc beton/briq.solide= %   ENTIER
#  8: RVEXTBOI  9046  RVEXTBOI   Revet.ext.bois/stucco/vinyle/autre déclin= %   ENTIER
#  9: RVEXTPAP  9400  RVEXTPAP   Revetement exterieur(papier  brique/aucun)=%   ENTIER
# 10: PLANCHER  9406  PLANCHER                       Plancher(s) construction   CARACT
# 11: RVEXTBRI  9408  RVEXTBRI          Revetement exterieur  brique/pierre=%   ENTIER
# 12: RVEXTALU  9413  RVEXTALU     Revetement  ext.  aluminium/métal/acier= %   ENTIER
# 13: TYPECONS 10762  TYPECONS                              Type construction   ENTIER
# 14: REVETEME 10763  REVETEME                        (conv. tar.) revêtement     CODE
# 15: RISASGRB 14218  RISASGRB                    Risque assuré - classe bien   CARACT
# 16: PRINCFUS 14219  PRINCFUS                      Protection incendie (fus)   ENTIER
# 17: MTTOTRAS 14220  MTTOTRAS                 Montant total du risque assuré   ENTIER
# 18:    14367 14367      <NA>             Communicant avec un autre bâtiment   CARACT
# 19: RESAUFEU 14491  RESAUFEU  Rés. au feu/incombus.(locataire 150 000 et -)   CARACT
# 20: PREGEOCO 14650  PREGEOCO                 Précision geocode - commercial   CARACT
# 21: LONGICOM 14660  LONGICOM                         Longitude - commercial   CARACT
# 22: LATITCOM 14661  LATITCOM                          Latitude - commercial   CARACT
#       DETAIL    ID CODE_INFO                                       NOM_ABRG DATATYPE
