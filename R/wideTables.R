

# makeWideTable
#
#' Format the ANSIS response into a wide table
#' @param ANSISDataObject An ANSIS R object
#' @param propertyType Return data for either 'SiteVisit', 'Horizons' or 'Lab'
#' @param widenBy The ANSIS field to widen the table by. One of 'Properties' or 'usedProcedure'
#' @param filterOn to be implemented
# #' @param decode return either coded or decoded values (default=F)

#' @details  You need to specify a parameter value for either 'propertyType' or 'properties'.
#' The data frame contains SiteID, location, depths and soil property fields
#' @author Ross Searle
#' @return dataframe
#' @export


makeWideTable <- function(ANSISDataObject, propertyType=NULL, widenBy='usedProcedure', filterOn=NULL){

  indf <- ANSISDataObject$Data

  if(propertyType=='Lab'){

    df_filtered <- indf |> filter((!is.na(UsedProcedure) & UsedProcedure != "") & !is.na(depthUpper))

      if(widenBy=='usedProcedure'){
        odf <- expandByUsedProcedure(ANSISFlatDF=df_filtered)
      }else{
        odf <- expandByPropertiesLab(ANSISFlatDF=df_filtered)
      }
  }else if(propertyType=='Horizons'){

      df_filtered <- indf |> filter((is.na(UsedProcedure) | UsedProcedure == "") & !is.na(depthUpper))
      odf <- expandByPropertiesProfile(ANSISFlatDF=df_filtered)
  } else if(propertyType=='SiteVisit'){

      df_filtered <- indf |> filter((is.na(UsedProcedure) | UsedProcedure == "") & is.na(depthUpper))
      odf <- expandByPropertiesSite(ANSISFlatDF=df_filtered)

  } else{

    stop('Please specify a type parameter of one of "Lab", "Profile" oe "Site"')
  }

return(odf)

}


expandByPropertiesSite <- function(ANSISFlatDF){

  df <- ANSISFlatDF

  id_cols <- c(
    "SiteID", "projectAuthority", "projectLabel", "AnsisSiteID",
    "SRID", "Longitude", "Latitude", "endAtTime"

  )

  df_combined <- df %>%
    distinct(across(all_of(c(id_cols, "Property", "Value")))) %>%
    pivot_wider(
      id_cols     = all_of(id_cols),
      names_from  = Property,
      values_from = c(Value),
      names_glue  = "{Property}_{.value}",
      values_fn   = ~ first(na.omit(.x)) # Fills with NA when property value is missing
    )

  return(df_combined)

}




expandByPropertiesProfile <- function(ANSISFlatDF){

  df <- ANSISFlatDF

  id_cols <- c(
    "SiteID", "projectAuthority", "projectLabel", "AnsisSiteID",
    "SRID", "Longitude", "Latitude", "endAtTime", "depthUpper", "depthLower"

  )

  df_combined <- df %>%
    distinct(across(all_of(c(id_cols, "Property", "Value")))) %>%
    pivot_wider(
      id_cols     = all_of(id_cols),
      names_from  = Property,
      values_from = c(Value),
      names_glue  = "{Property}_{.value}",
      values_fn   = ~ first(na.omit(.x)) # Fills with NA when property value is missing
    )

  return(df_combined)

}

expandByPropertiesLab <- function(ANSISFlatDF){

 df <- ANSISFlatDF

id_cols <- c(
  "SiteID", "projectAuthority", "projectLabel", "AnsisSiteID",
  "SRID", "Longitude", "Latitude", "endAtTime", "depthUpper", "depthLower"

)

df_combined <- df %>%
  distinct(across(all_of(c(id_cols, "Property", "Value", "Unit", "UsedProcedure")))) %>%
  pivot_wider(
    id_cols     = all_of(id_cols),
    names_from  = Property,
    values_from = c(Value, Unit, UsedProcedure),
    names_glue  = "{Property}_{.value}",
    values_fn   = ~ first(na.omit(.x)) # Fills with NA when property value is missing
  )

return(df_combined)

}

expandByUsedProcedure <- function(ANSISFlatDF){

  df <- ANSISFlatDF

    id_cols <- c(
      "SiteID", "projectAuthority", "projectLabel", "AnsisSiteID",
      "SRID", "Longitude", "Latitude", "endAtTime", "depthUpper", "depthLower"
    )

    # 4. Pivot wider using valid UsedProcedure values as column names
    df_wide <- df %>%
      distinct(across(all_of(c(id_cols, "UsedProcedure", "Value")))) %>%
      pivot_wider(
        id_cols     = all_of(id_cols),
        names_from  = UsedProcedure,
        values_from = Value,
        values_fn   = ~ first(na.omit(.x))
      )
return(df_wide)
}
