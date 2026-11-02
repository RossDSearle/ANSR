


getSiteLocation <- function(siteAsList){

  srid <- siteAsList$geometry[[1]]
  bits <- stringr::str_split(srid, '[(]')
  bits2 <- stringr::str_remove(bits[[1]][2], '[)]')
  bits3 <- stringr::str_split(bits2, ' ')[[1]]
  ol <- list()
  ol$X <- as.numeric(bits3[1])
  ol$Y <- as.numeric(bits3[2])
  return(ol)
}



makeSitesLocationTable <- function(df){

  dt <- data.table::data.table(df)
  unique_combos <- unique(dt, by = c("SiteID", "SRID", "Longitude", 'Latitude'))[, c('SiteID', 'SRID', 'Longitude', 'Latitude')]
  return(unique_combos)
}



summariseOutputs <- function(df) {

  # Ensure input is a tibble/data.frame to work seamlessly with dplyr
  df <- tibble::as_tibble(df)

  # 1. Calculate overall counts using count()
  totCnts  <- df |>dplyr::count(Property, name = "count")
  siteCnts <- df |> dplyr::count(SiteID, Property, name = "count")

  ol <- list()
  ol$Totals$ANSISPropertyCounts <- totCnts
  ol$Sites$ANSISPropertyCounts  <- siteCnts

  # 2. Handle conditional counts for UsedProcedure
  meths <- df |> dplyr::filter(!is.na(UsedProcedure))

  if (nrow(meths) > 0) {

    totMethCnts <- meths |>
      dplyr::count(Property, UsedProcedure, name = "count") |>
      dplyr::arrange(Property, UsedProcedure)

    siteMethCnts <- meths |>
      dplyr::count(SiteID, Property, UsedProcedure, name = "count") |>
      dplyr::arrange(SiteID, Property, UsedProcedure)

    ol$Totals$LabMethodCounts <- totMethCnts
    ol$Sites$LabMethodCounts  <- siteMethCnts
  }

  return(ol)
}




