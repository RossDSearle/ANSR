

generateANSISSchemaMappingTable <- function(){

    aprops <- data.table::data.table(apiPropertyDefinitions2())
    aprops$jcode <- basename(aprops$code)

    physchemSchemaTable <- data.table::data.table(getANSISPhysChemProcedures())
    properties <- data.table::data.table(getANSISProerties())

    ansisPhysChemMappings <- merge(aprops, physchemSchemaTable, by.x='name', by.y='title')
    labMethods <- getCSISLabMethods()
    bits <- stringr::str_split(ansisPhysChemMappings$usedProcedure, ':')
    cds <- sapply(bits, function (x) x[2])
    ansisPhysChemMappings$p2 <- cds


    pc1 <- merge(ansisPhysChemMappings, labMethods, by.x='p2', by.y='LABM_CODE')
    bits <- stringr::str_split(pc1$usedProcedure, ':')
    cds <- sapply(bits, function (x) x[2])
    pc1$LabCode <- cds
    pc2 <- data.frame(pc1$Definition_Name, pc1$name, pc1$code, pc1$jcode, pc1$unit, pc1$usedProcedure, pc1$LabCode, pc1$LABM_NAME, attType='PhysChem')


    colnames(pc2) <- c('ANSIS_ID', 'ANSIS_Property', 'ANSIS_Path', 'ANSIS_Code', 'Unit', 'usedProcedure', 'labCode', 'Description', 'Group')

    ansisSiteLayersMappings <- merge(
      transform(aprops, jcode = tolower(jcode)),
      transform(properties, Definition_Name = tolower(Definition_Name)),
      by.x='jcode', by.y='Definition_Name'
    )

    sl1 <- data.frame(ansisSiteLayersMappings$Definition_Name, ansisSiteLayersMappings$name, ansisSiteLayersMappings$code, ansisSiteLayersMappings$jcode, unit='',
                      usedProcedure='', labCode='', ansisSiteLayersMappings$Description, Group='SiteVisit/Layer')
    colnames(sl1) <- c('ANSIS_ID', 'ANSIS_Property', 'ANSIS_Path', 'ANSIS_Code', 'Unit', 'usedProcedure', 'labCode', 'Description', 'Group')

    ANSISMappings <- rbind(pc2, sl1)

    return(ANSISMappings)
}



apiPropertyDefinitions2 <- function(outFile=NULL){

  url <- "https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/definitions"
  #output_file <- "site_catalogue_all_fields.csv"

  # 1. Fetch and parse the JSON data
  json_data <- tryCatch({
    jsonlite::fromJSON(url, simplifyVector = FALSE, )
  }, error = function(e) {
    stop("Failed to download or parse the JSON. Please check the URL or your network connection.")
  })

  # 2. Locate the definitions list
  if ("$defs" %in% names(json_data)) {
    defs_list <- json_data[["$defs"]]
  } else if ("definitions" %in% names(json_data)) {
    defs_list <- json_data[["definitions"]]
  } else if ("components" %in% names(json_data) && "schemas" %in% names(json_data$components)) {
    defs_list <- json_data$components$schemas
  } else {
    # If no standard wrapper is found, assume the root object is the list of definitions
    defs_list <- json_data
  }

  # 3. Flatten all dynamic fields into columns
  # enframe() puts the definition names in one column and the data in a list column
  odf <- tibble::enframe(defs_list, name = "Definition_Name", value = "details") |>
    # unnest_wider() automatically creates a column for every key it finds in 'details'
   tidyr::unnest_wider(details, names_repair = "unique")

  # 4. Clean up the data for CSV export
  # CSVs cannot hold R lists, so we convert any nested objects or arrays into JSON strings
  odf_clean <- odf |>
    dplyr::mutate(across(everything(), ~ sapply(.x, function(v) {
      if (is.null(v) || length(v) == 0) {
        return(NA_character_)
      }
      # If the value is a nested list or array, convert it to a JSON string representation
      if (is.list(v) || length(v) > 1) {
        return(as.character(jsonlite::toJSON(v, auto_unbox = TRUE)))
      }
      # Otherwise, ensure it is a simple character string
      return(as.character(v))
    })))

  # 5. Export the data frame to a CSV file
  #write.csv(odf_clean, output_file, row.names = FALSE, na = "")



  if(!is.null(outFile)){
    write.csv(odf, outFile, row.names = FALSE, na = "")
    cat(sprintf("Successfully parsed %d definitions and saved to %s\n", nrow(odf_clean), outFile))
    shell(outFile, wait = F)
  }

  return(odf_clean)

}



getCSISLabMethods <- function(){
  url <- 'https://soils.csiro.au/data/natsoil-api/lab-methods'
  labMethods <-  jsonlite::fromJSON(url )

  return(labMethods)

}



getANSISProerties <- function(outFile=NULL){



  # Use the RAW URL
  url <- "https://raw.githubusercontent.com/ANZSoilData/def-au-schema-json/main/schema/domain/2023-07-31/properties.json"
  #output_file <- "parsed_properties_defs.csv"

  # 1. Fetch and parse the JSON data
  json_data <- tryCatch({
    jsonlite::fromJSON(url, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Failed to parse the JSON. Check the URL or network connection.")
  })

  # 2. Extract the top-level $defs which contains the categories (_att, _obs, _rel)
  main_defs <- json_data[["$defs"]]

  # 3. Map over each category to extract its nested properties
  odf <- purrr::map_dfr(names(main_defs), function(cat_name) {
    cat_group <- main_defs[[cat_name]]

    # Navigate to the nested $defs within the category
    prop_defs <- cat_group[["$defs"]]

    # If the category doesn't have a nested $defs, skip it safely
    if (is.null(prop_defs)) return(NULL)

    # Map over each individual property inside the category's $defs
    purrr::map_dfr(names(prop_defs), function(prop_name) {
      p <- prop_defs[[prop_name]]

      # Helper function to safely extract values
      safe_extract <- function(x, key) {
        val <- purrr::pluck(x, key)
        if (is.null(val)) {
          return(NA_character_)
        }
        # Collapse arrays into a single comma-separated string
        paste(unlist(val), collapse = ", ")
      }

     tibble::tibble(
        Category = cat_name,
        Definition_Name = prop_name,
        ID = safe_extract(p, "@id"),
        Title = safe_extract(p, "title"),
        Description = safe_extract(p, "description"),
        Type = safe_extract(p, "type"),
        Reference = safe_extract(p, "$ref") # Added to capture $ref for fields like 'confidence'
      )
    })
  })


  if(!is.null(outFile)){
    write.csv(odf, outFile, row.names = FALSE, na = "")
    cat(sprintf("Successfully extracted %d definitions and saved to %s\n", nrow(outFile), save))
    shell(outFile, wait = F)

  }

  return(odf)
}




getANSISPhysChemProcedures <- function(outFile=NULL){


  paths <- c('https://raw.githubusercontent.com/ANZSoilData/def-au-schema-json/main/schema/domain/2023-07-31/properties-physical.json',
             "https://raw.githubusercontent.com/ANZSoilData/def-au-schema-json/main/schema/domain/2023-07-31/properties-chemical.json")

  odf <- data.frame()
  outfile <- "c:/temp/PhysicalAndChemical_properties_procedures.csv"

  for (i in 1:length(paths)) {

    json_data <- jsonlite::fromJSON(paths[i], simplifyVector = FALSE)

    # Extract the $defs list which contains the property definitions
    defs <- json_data[["$defs"]]

    # Map over each definition to extract the title, units, and usedProcedure array
    parsed_data <- purrr::map_dfr(defs, function(x) {

      # Extract the title
      prop_title <- x$title

      # Navigate to the usedProcedure enum array within the allOf structure safely using pluck
      procedures <- purrr::pluck(x, "allOf", 1, "properties", "usedProcedure", "enum")

      # Handle cases where the procedure list might be empty or missing
      if (is.null(procedures)) {
        procedures <- list(NA_character_)
      }

      # Navigate to the unit enum array safely using pluck.
      # If 'result' is a boolean instead of a list, pluck will return NULL instead of throwing an error.
      units <- purrr::pluck(x, "allOf", 1, "properties", "result", "properties", "unit", "enum")

      # Handle cases where the unit list might be empty or missing
      if (is.null(units)) {
        unit_str <- NA_character_
      } else {
        # Collapse multiple valid units into a single string (e.g., "unit:PERCENT, unit:MilliGM-PER-KiloGM")
        unit_str <- paste(unlist(units), collapse = ", ")
      }

      # Create a tibble; this automatically repeats the title and unit for each procedure
      tibble::tibble(
        title = prop_title,
        unit = unit_str,
        usedProcedure = unlist(procedures)
      )
    })

    odf <- rbind(odf, parsed_data)

  }

  if(!is.null(outFile)){
    write.csv(odf, outFile, row.names = FALSE)
    shell(outFile, wait=F)
  }

  return(odf)
}



