
#' @importFrom methods new
#' @importFrom data.table data.table :=

# Define data.table awareness at top-level scope
.datatable.aware <- TRUE

# Create an internal environment to hold package state/constants without polluting .GlobalEnv
ANSR_env <- new.env(parent = emptyenv())

.onAttach <- function(lib, pkg) {

  # 1. Store constants in internal package environment
  ANSR_env$Constants <- list(
    ANSISAPIurlDev  = "",
    ANSISAPIurlProd = "https://apim-ansis-app-prod-ac.azure-api.net",
    ANSISguiURL     = "https://portal.ansis.net/"
  )

  # 2. Lazily / safely initialize network data without breaking offline attachment
  tryCatch({
    if (curl::has_internet()) {
      ANSR_env$DataSets <- list(
        mappingInfo = generateANSISSchemaMappingTable(),
        Providers   = apiCatalogueSummary(),
        Enums       = getEnums()
      )
    }
  }, error = function(e) {
    # Suppress network initialization failures during offline load
    NULL
  })

  # 3. Capture asciiProfile output if it uses cat() internally
  ascii_txt <- paste(utils::capture.output(asciiProfile()), collapse = "\n")

  # 4. Build startup message string safely
  header  <- crayon::bold(crayon::green("\n\n#########     Welcome to the ANSR Package     ##########\n\n"))
  body    <- crayon::bold(crayon::blue(
    "This package is designed for interacting with the Australian National Soil Information System (ANSIS).\n",
    "Before any of the API functions will work you need to authorise access using the apiAuthoriseMe() function\n",
    "using your ANSIS account Username and Password. You also need to supply a DataStore path.\n\n"
  ))
  link    <- crayon::magenta("Visit https://ansis.net\n\n")

  msg <- paste0(ascii_txt, "\n", header, body, link)

  # 5. Emit single suppressable startup message
  packageStartupMessage(msg)
}


# Quiet R CMD check NOTES regarding tidyverse unquoted column names
utils::globalVariables(c(
  "AnsisSiteID", "CodesTable", "Definition_Name", "Latitude", "Longitude",
  "Property", "PropertyIndex", "PropertyPath", "SRID", "SiteID", "Unit",

  "UsedProcedure", "Value", "attr_type", "base_path", "clean_path",
  "coords", "depthLower", "endAtTime", "fieldDepth", "depthUpper", "id", "path", "sourceFile",
  "hierarchyPath", "site_idx", "visit_idx", "prof_idx", "layer_idx", "details", 'jcode', 'fieldName',
  'projectName', 'projectLabel', 'projectAuthority', 'subProperty', 'usedProcedure', 'value'
))
