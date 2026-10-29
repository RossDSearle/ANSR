#' Get ANSIS Data
#'
#' @import data.table
#' @param queryName A name for the query. Default is NULL.
#' @param description A description of the query.
#' @param minx Western extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxx Eastern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param miny Southern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxy Northern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param ansisProperties ANSIS property elements to query on.
#' @param parseAll Logical. Parse all properties or only filtered ones. Default is TRUE.
#' @param startYear Starting year of date range to return data for.
#' @param endYear Finishing year of date range to return data for.
#' @param provider The Data Provider to return data for.
#' @param sites A list of ANSIS Site IDs to return data for.
#' @param numCPUs Number of CPU cores for parallel parsing.
#' @param overwrite Overwrite existing cached query file. Default is FALSE.
#' @param verbose Logical. Print detailed progress logs. Default is FALSE.
#' @param viewSummaries Logical. View parsing summaries. Default is FALSE.
#' @details Main function to retrieve data from the ANSIS Data System.
#' @author Ross Searle
#' @return ANSIS Data Object
#' @export
apiGetANSISData <- function(queryName = NULL, description = NULL, ansisProperties = NULL, parseAll = TRUE,
                            minx = NULL, maxx = NULL, miny = NULL, maxy = NULL,
                            startYear = NULL, endYear = NULL, provider = NULL, sites = NULL, numCPUs = NULL,
                            overwrite = FALSE, verbose = FALSE, viewSummaries = FALSE) {

  if (!checkIfAuthorised()) {
    base::warning("User is not authorised. Please run apiAuthoriseMe() first.", call. = FALSE)
    return(NULL)
  }

  qry <- apiSendQuery(minx = minx, maxx = maxx, miny = miny, maxy = maxy,
                      ansisProperties = ansisProperties, startYear = startYear,
                      endYear = endYear, provider = provider, sites = sites)

  if (base::is.null(qry) || base::is.null(qry$reqID)) {
    base::warning("Failed to submit query to ANSIS server.", call. = FALSE)
    return(NULL)
  }

  reqID <- qry$reqID
  status <- apiQueryStatus_Monitor(reqID)

  if (base::is.null(status) || status != "Completed") {
    base::warning(base::sprintf("Query %s did not complete successfully.", reqID), call. = FALSE)
    return(NULL)
  }

  of <- getOutputFileName(queryName = queryName, overwrite = overwrite)
  if (base::is.null(of)) return(NULL)

  zipFile <- apiDownloadQueryData(reqID, outZipFile = of)
  if (base::is.null(zipFile) || !base::file.exists(zipFile)) {
    base::warning("No data files were downloaded.", call. = FALSE)
    return(NULL)
  }

  if (parseAll) {
    parseProps <- NULL
  } else {
    ansisPropertyRecs <- getAnsisPropertyInfo(ansisProperties = ansisProperties)
    parseProps <- if (!base::is.null(ansisPropertyRecs)) base::unique(ansisPropertyRecs$ANSIS_Code) else NULL
  }

  res <- base::tryCatch({
    parseANSISJsonParallel3(zipFile = zipFile, numCPUs = numCPUs,
                            propertiesToParse = parseProps, verbose = verbose,
                            parseAll = parseAll, viewSummaries = viewSummaries)
  }, error = function(e) {
    base::warning(base::sprintf("Failed to parse JSON response: %s", e$message), call. = FALSE)
    return(NULL)
  })

  if (!base::is.null(res) && base::is.list(res)) {
    res$QueryParameters <- qry
  }

  return(res)
}


#' Get ANSIS API Documentation
#' @details Opens the documentation PDF of the ANSIS API.
#' @author Ross Searle
#' @export
ANSIS_APIDocs <- function() {
  pdf_path <- base::system.file("extdata", "ANSIS_HTTP_API.pdf", package = "ANSR")

  if (!base::file.exists(pdf_path)) {
    base::warning("ANSIS_HTTP_API.pdf documentation file not found in package.", call. = FALSE)
    return(invisible(FALSE))
  }

  if (base::Sys.info()["sysname"] == "Linux") {
    base::cat("\nDocumentation PDF available at: ", crayon::red(pdf_path), "\n")
  } else {
    utils::browseURL(pdf_path)
  }
  return(invisible(TRUE))
}


#' Open a demo R Script
#' @details Opens a demo script demonstrating 'ANSR' package functionality.
#' @author Ross Searle
#' @export
ANSIS_Demos <- function() {
  demo_path <- base::system.file("extdata", "ANSR_Demo.R", package = "ANSR")

  if (!base::file.exists(demo_path)) {
    base::warning("ANSR_Demo.R file not found in package.", call. = FALSE)
    return(invisible(FALSE))
  }

  if (base::Sys.info()["sysname"] == "Linux") {
    lns <- base::readLines(demo_path, warn = FALSE)
    base::cat(base::paste0(lns, collapse = "\n"))
    base::cat(crayon::bold(crayon::red("\n\nCopy and paste the code above into an R script file.\n\n")))
  } else {
    utils::file.show(demo_path, title = "ANSR Demo Script")
  }
  return(invisible(TRUE))
}


#' Opens the ANSIS Website
#' @author Ross Searle
#' @export
ANSIS_OpenWebsite <- function() {
  utils::browseURL("https://ansis.net/")
}


#' Authorise ANSIS Session
#' @param username ANSIS account username
#' @param password ANSIS account password
#' @param DataStorePath Directory path for local data storage.
#' @author Ross Searle
#' @export
apiAuthoriseMe <- function(username, password, DataStorePath) {
  if (base::missing(username) || base::missing(password) || base::missing(DataStorePath)) {
    base::stop("Arguments 'username', 'password', and 'DataStorePath' are all required.", call. = FALSE)
  }

  tkn <- apiGenerateToken(username, password, verbose = TRUE)

  if (base::is.null(tkn)) {
    base::cat(base::paste0("\n", crayon::bold(crayon::red("Authorisation was not successful")),
                           "\n\nUnable to retrieve authentication response from ANSIS server.\n\n"))
    return(invisible(FALSE))
  }

  if (!base::is.null(tkn$error)) {
    base::cat(base::paste0("\n", crayon::bold(crayon::red(tkn$error)), "\n\n",
                           tkn$error_description, "\nPlease check your username and password.\n\n"))
    return(invisible(FALSE))
  }

  if (base::is.null(tkn$access_token) || base::is.null(tkn$expires_in)) {
    base::cat(base::paste0("\n", crayon::bold(crayon::red("Authorisation failed")),
                           "\n\nInvalid token response returned by server.\n\n"))
    return(invisible(FALSE))
  }

  expire <- base::Sys.time() + base::as.numeric(tkn$expires_in)

  base::tryCatch({
    if (!base::dir.exists(DataStorePath)) base::dir.create(DataStorePath, recursive = TRUE, showWarnings = FALSE)
    base::dir.create(base::file.path(DataStorePath, "ANSISDataObjects"), recursive = TRUE, showWarnings = FALSE)
    base::dir.create(base::file.path(DataStorePath, "RawJSONResponses"), recursive = TRUE, showWarnings = FALSE)
  }, error = function(e) {
    base::stop(base::sprintf("Failed to create storage directories at '%s': %s", DataStorePath, e$message), call. = FALSE)
  })

  authANSIS <<- methods::new("Authorisation",
                             usr = username,
                             pwd = password,
                             Authorised = TRUE,
                             Token = tkn$access_token,
                             TokenExpiry = expire,
                             DataStorePath = DataStorePath)

  tokenTime <- base::as.numeric(difftime(authANSIS@TokenExpiry, base::Sys.time(), units = "hours"))
  base::cat(base::paste0("\n", crayon::bold(crayon::green("Authorisation successful")),
                         "\n\nThis authorisation will remain valid for about ",
                         base::round(tokenTime, 1), " hours.\n\n"))
  return(invisible(TRUE))
}


#' Generate an Authorisation token
#' @param user ANSIS account username
#' @param pwd ANSIS account password
#' @param verbose Logical. Return raw token object or access token string.
#' @author Ross Searle
#' @export
apiGenerateToken <- function(user, pwd, verbose = FALSE) {
  if (verbose) base::cat("Authorising user....\n")

  url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/Authorise")

  resp <- base::tryCatch({
    httr::GET(url, httr::add_headers(username = user, password = pwd), httr::timeout(30))
  }, error = function(e) {
    base::warning(base::sprintf("Authentication HTTP request failed: %s", e$message), call. = FALSE)
    return(NULL)
  })

  if (base::is.null(resp) || httr::http_error(resp)) {
    base::warning(base::sprintf("Authentication HTTP error (status %s)", httr::status_code(resp)), call. = FALSE)
    return(NULL)
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  auth <- base::tryCatch({
    jsonlite::fromJSON(jsn)
  }, error = function(e) {
    base::warning("Failed to parse token response JSON.", call. = FALSE)
    return(NULL)
  })

  if (verbose) {
    return(auth)
  } else {
    return(auth$access_token)
  }
}


#' ANSIS Catalogue Summary
#' @author Ross Searle
#' @export
apiCatalogueSummary <- function() {
  url <- base::paste0(Constants@ANSISAPIurlProd, "/site-catalogue/v1/catalogue-summary")

  resp <- base::tryCatch({
    httr::GET(url = url, httr::timeout(30))
  }, error = function(e) {
    base::warning(base::sprintf("HTTP request failed: %s", e$message), call. = FALSE)
    return(data.frame())
  })

  if (httr::http_error(resp)) {
    base::warning(base::sprintf("HTTP Error %s when fetching catalogue summary.", httr::status_code(resp)), call. = FALSE)
    return(data.frame())
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  smry <- base::tryCatch({
    jsonlite::fromJSON(jsn, simplifyDataFrame = TRUE)
  }, error = function(e) {
    base::warning("Failed to parse catalogue summary JSON.", call. = FALSE)
    return(data.frame())
  })

  return(smry)
}


#' ANSIS Provider Catalogue
#' @param providerNames List of ANSIS Provider names.
#' @param includeProperties Include ANSIS Soil properties list.
#' @param outputFormat 'dataframe' or 'json'
#' @param ViewIt View raw JSON output.
#' @author Ross Searle
#' @export
apiProviderCatalogue <- function(providerNames = NULL, includeProperties = FALSE, outputFormat = "dataframe", ViewIt = FALSE) {

  if (base::is.null(providerNames)) {
    prvs <- apiCatalogueSummary()
    if (base::nrow(prvs) == 0 || !("id" %in% base::names(prvs))) return(data.frame())
    providerNames <- prvs$id
  }

  cdf <- data.frame()
  raw_jsons <- base::list()

  for (i in base::seq_along(providerNames)) {
    p <- providerNames[i]
    base::cat(base::paste0("Getting catalogue information for ", p, "....\n"))

    url <- base::paste0(Constants@ANSISAPIurlProd, "/site-catalogue/v1/provider/", p)
    resp <- base::tryCatch({
      httr::GET(url = url, httr::timeout(30))
    }, error = function(e) NULL)

    if (base::is.null(resp) || httr::http_error(resp)) next

    jsn <- httr::content(resp, "text", encoding = "UTF-8")
    raw_jsons[[p]] <- jsn
    jdf <- base::tryCatch({ jsonlite::fromJSON(jsn) }, error = function(e) NULL)

    if (!base::is.null(jdf) && !base::is.null(jdf$properties)) {
      n <- base::nrow(jdf)
      yrs <- jdf$properties$years
      sy <- base::sapply(yrs, function(x) if (base::length(x) >= 1) x[1] else NA)
      ey <- base::sapply(yrs, function(x) if (base::length(x) >= 2) x[2] else NA)

      locs <- jdf$geometry$coordinates
      lng <- base::sapply(locs, function(x) if (base::length(x) >= 1) x[1] else NA)
      lat <- base::sapply(locs, function(x) if (base::length(x) >= 2) x[2] else NA)

      odf <- data.frame(
        Provider = jdf$properties$provider,
        Project = jdf$properties$project,
        SiteID = jdf$properties$site,
        Restricted = jdf$properties$isRestricted,
        Cached = jdf$properties$isHydrated,
        StartYear = sy,
        EndYear = ey,
        Longitude = lng,
        Latitude = lat,
        stringsAsFactors = FALSE
      )

      if (includeProperties && !base::is.null(jdf$properties$properties)) {
        odf$Properties <- base::sapply(jdf$properties$properties, function(x) base::paste0(x, collapse = ", "))
      }

      cdf <- base::rbind(cdf, odf)
    }
  }

  if (ViewIt && base::length(raw_jsons) > 0 && base::requireNamespace("listviewer", quietly = TRUE)) {
    listviewer::jsonedit(raw_jsons)
  }

  if (outputFormat == "dataframe") {
    return(cdf)
  } else {
    return(raw_jsons)
  }
}


#' ANSIS Property Definitions
#' @author Ross Searle
#' @export
apiPropertyDefinitions <- function() {
  url <- base::paste0(Constants@ANSISAPIurlProd, "/site-catalogue/v1/definitions")
  resp <- base::tryCatch({ httr::GET(url, httr::timeout(30)) }, error = function(e) NULL)

  if (base::is.null(resp) || httr::http_error(resp)) {
    base::warning("Failed to fetch property definitions.", call. = FALSE)
    return(data.frame())
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  defs <- base::tryCatch({ jsonlite::fromJSON(jsn) }, error = function(e) NULL)

  if (base::is.null(defs)) return(data.frame())

  odf <- data.frame()
  for (i in 0:4) {
    key <- base::as.character(i)
    if (!key %in% base::names(defs)) next

    n1 <- defs[[key]]
    propType <- n1$name

    if (propType %in% base::c("Layer/Horizon", "Profile")) {
      if (!base::is.null(n1$members)) {
        for (k in base::seq_along(n1$members)) {
          m_key <- n1$members[k]
          n2 <- defs[[m_key]]
          if (!base::is.null(n2)) {
            df <- data.frame(PropType = propType, Property = propType, Name = n2$name, ANSISCode = m_key, stringsAsFactors = FALSE)
            odf <- base::rbind(odf, df)
          }
        }
      }
    } else {
      if (!base::is.null(n1$members)) {
        for (j in base::seq_along(n1$members)) {
          p_key <- n1$members[j]
          n2 <- defs[[p_key]]
          if (!base::is.null(n2)) {
            prop <- n2$name
            if (!base::is.null(n2$members)) {
              for (k in base::seq_along(n2$members)) {
                sub_key <- n2$members[k]
                n3 <- defs[[sub_key]]
                if (!base::is.null(n3)) {
                  df <- data.frame(PropType = propType, Property = prop, Name = n3$name, ANSISCode = sub_key, stringsAsFactors = FALSE)
                  odf <- base::rbind(odf, df)
                }
              }
            }
          }
        }
      }
    }
  }
  return(odf)
}


getOutputFileName <- function(queryName, overwrite = FALSE) {
  if (!base::exists("authANSIS") || base::is.null(authANSIS@DataStorePath)) {
    base::stop("Authorisation object missing. Please authorise session.", call. = FALSE)
  }

  inDir <- base::file.path(authANSIS@DataStorePath, "RawJSONResponses")
  if (!base::dir.exists(inDir)) base::dir.create(inDir, recursive = TRUE, showWarnings = FALSE)

  if (base::is.null(queryName) || queryName == "") {
    fls <- base::list.files(inDir, pattern = "\\.zip$", recursive = FALSE, full.names = FALSE)
    idxs <- base::which(base::grepl("^ansisQueryJsonFiles_", fls))

    if (base::length(idxs) > 0) {
      efls <- fls[idxs]
      xf <- stringr::str_remove(efls, "\\.zip$")
      nums <- base::suppressWarnings(base::as.numeric(stringr::str_remove(xf, "^ansisQueryJsonFiles_")))
      nums <- nums[!base::is.na(nums)]
      nextNum <- if (base::length(nums) > 0) base::max(nums) + 1 else 1
      outJsonZipFile <- base::file.path(inDir, base::sprintf("ansisQueryJsonFiles_%d.zip", nextNum))
    } else {
      outJsonZipFile <- base::file.path(inDir, "ansisQueryJsonFiles_1.zip")
    }
  } else {
    outJsonZipFile <- base::file.path(inDir, base::sprintf("%s.zip", queryName))
    if (base::file.exists(outJsonZipFile)) {
      if (overwrite) {
        base::unlink(outJsonZipFile)
      } else {
        if (base::interactive()) {
          killit <- utils::readline(prompt = crayon::red("A query zip file with this name exists. Overwrite? (Y/n): "))
          if (base::toupper(killit) == "Y") {
            base::unlink(outJsonZipFile)
          } else {
            base::stop("Query cancelled. Please choose another query name.", call. = FALSE)
          }
        } else {
          base::stop("Output zip file already exists and overwrite = FALSE.", call. = FALSE)
        }
      }
    }
  }
  return(outJsonZipFile)
}


makeQueryObject <- function(ansisProperty = NULL, ansisID = NULL, labCode = NULL,
                            minx = NULL, maxx = NULL, miny = NULL, maxy = NULL,
                            startYear = NULL, endYear = NULL, provider = NULL, sites = NULL) {

  usr <- if (base::exists("authANSIS")) authANSIS@usr else "Unknown"

  queryObject <- base::list(
    Time = base::Sys.time(),
    User = usr,
    Query = base::list(
      Bdy = base::list(
        Minx = if (base::is.null(minx)) "" else minx,
        Maxx = if (base::is.null(maxx)) "" else maxx,
        Miny = if (base::is.null(miny)) "" else miny,
        Maxy = if (base::is.null(maxy)) "" else maxy
      ),
      ansisProperty = if (base::is.null(ansisProperty)) "" else ansisProperty,
      ansisID = if (base::is.null(ansisID)) "" else ansisID,
      LabCode = if (base::is.null(labCode)) "" else labCode,
      StartYear = if (base::is.null(startYear)) "" else startYear,
      EndYear = if (base::is.null(endYear)) "" else endYear,
      Provider = if (base::is.null(provider)) "" else provider,
      Sites = if (base::is.null(sites)) "" else sites
    )
  )
  return(queryObject)
}


#' Send a Query to the ANSIS API
#' @author Ross Searle
#' @export
apiSendQuery <- function(ansisProperties = NULL, minx = NULL, maxx = NULL, miny = NULL, maxy = NULL,
                         startYear = NULL, endYear = NULL, provider = NULL, sites = NULL) {

  if (!checkIfAuthorised()) return(NULL)

  if (base::exists("validateQueryParameters") && !validateQueryParameters(ansisProperties, minx, maxx, miny, maxy, startYear, endYear, provider, sites)) {
    return(NULL)
  }

  ids <- NULL
  if (!base::is.null(ansisProperties)) {
    ansisPropertyRecs <- getAnsisPropertyInfo(ansisProperties)
    if (!base::is.null(ansisPropertyRecs) && "ANSIS_ID" %in% base::names(ansisPropertyRecs)) {
      ids <- base::unique(ansisPropertyRecs$ANSIS_ID)
    }
  }

  qryDF <- makeQuery(minx = minx, maxx = maxx, miny = miny, maxy = maxy, ansisIDs = ids,
                     startYear = startYear, endYear = endYear, provider = provider, sites = sites)

  if (base::is.null(qryDF) || !("QueryField" %in% base::names(qryDF))) {
    base::warning("Failed to construct query payload dataframe.", call. = FALSE)
    return(NULL)
  }

  qryJSON <- qryDF[qryDF$QueryField == "ANSISQueryJSON", ]$QueryValue

  base::cat("Sending query to the ANSIS server....\n")
  url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/create-query-request")

  resp <- base::tryCatch({
    httr::POST(url = url, body = qryJSON,
               httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)),
               httr::timeout(60))
  }, error = function(e) {
    base::warning(base::sprintf("Failed to send query: %s", e$message), call. = FALSE)
    return(NULL)
  })

  if (base::is.null(resp) || httr::http_error(resp)) {
    base::warning(base::sprintf("Query request failed with status %s", httr::status_code(resp)), call. = FALSE)
    return(NULL)
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  parts <- stringr::str_split(jsn, ": ")[[1]]
  reqID <- if (base::length(parts) >= 2) parts[2] else NULL

  if (base::is.null(reqID) || reqID == "") {
    base::warning(base::sprintf("Server returned invalid request ID: %s", jsn), call. = FALSE)
    return(NULL)
  }

  return(base::list(reqID = reqID, queryParameters = qryDF))
}


#' Monitor an ANSIS Query Status
#' @param reqID Request ID
#' @param max_wait_sec Maximum time to monitor before timing out (default: 1800s / 30m)
#' @author Ross Searle
#' @export
apiQueryStatus_Monitor <- function(reqID, max_wait_sec = 1800) {

  if (!checkIfAuthorised()) return(NULL)

  inProgress <- TRUE
  pb <- base::c("\\", "|", "/", "-")
  iter <- 1
  start_time <- base::Sys.time()

  base::cat("\rQuerying ANSIS Server  [-] : Number of data chunks generated = 0")

  while (inProgress) {
    base::Sys.sleep(1)

    elapsed <- base::as.numeric(difftime(base::Sys.time(), start_time, units = "secs"))
    if (elapsed > max_wait_sec) {
      base::cat("\n\nQuery monitoring timed out after ", max_wait_sec, " seconds.\n")
      return("Timeout")
    }

    base::cat(base::sprintf("\rQuerying ANSIS Server  [%s] : Number of data chunks generated = ", pb[iter]))
    iter <- iter + 1
    if (iter > 4) {
      iter <- 1
      url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/get-query-request-status?requestId=", reqID)

      resp <- base::tryCatch({
        httr::GET(url = url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(30))
      }, error = function(e) NULL)

      if (!base::is.null(resp) && !httr::http_error(resp)) {
        jsn <- httr::content(resp, "text", encoding = "UTF-8")
        status <- base::tryCatch({ jsonlite::fromJSON(jsn, simplifyDataFrame = FALSE) }, error = function(e) NULL)

        if (!base::is.null(status) && !base::is.null(status$sdrRequest)) {
          base::cat(base::length(status$sdrRequest$files))

          if (status$sdrRequest$status == "Completed") {
            base::cat("\n\nQuery completed...\n\n")
            inProgress <- FALSE
            return("Completed")
          } else if (status$sdrRequest$status %in% base::c("Error", "Failed", "Cancelled")) {
            base::cat(base::sprintf("\n\nQuery terminated with status: %s\n\n", status$sdrRequest$status))
            return(status$sdrRequest$status)
          }
        }
      }
    }
  }
  return("Completed")
}


#' Get the Status of all ANSIS Queries
#' @author Ross Searle
#' @export
apiQueryStatus_All <- function(statusTypes = NULL) {

  if (!checkIfAuthorised()) return(NULL)

  valid_stats <- base::c("Processing", "Completed", "Queued", "Error")
  stat_url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/get-all-query-requests-statuses")

  resp <- base::tryCatch({
    httr::GET(url = stat_url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(30))
  }, error = function(e) NULL)

  if (base::is.null(resp) || httr::http_error(resp)) {
    base::warning("Failed to fetch query statuses from server.", call. = FALSE)
    return(data.frame())
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  if (jsn == "[]" || jsn == "") {
    base::cat("\nNo queries in the queue.\n\n")
    return(data.frame(id = character(), status = character()))
  }

  rq <- base::tryCatch({ jsonlite::fromJSON(jsn, simplifyDataFrame = TRUE) }, error = function(e) NULL)

  if (base::is.null(rq) || base::nrow(rq) == 0) return(data.frame(id = character(), status = character()))

  df <- data.frame(id = rq$id, status = rq$sdrRequest$status, stringsAsFactors = FALSE)

  if (!base::is.null(statusTypes)) {
    if (base::all(statusTypes %in% valid_stats)) {
      return(df[df$status %in% statusTypes, ])
    } else {
      base::cat(base::sprintf("Valid status filters are: (%s)\n\n", base::paste0(valid_stats, collapse = ", ")))
      return(df)
    }
  }
  return(df)
}


#' Get an ANSIS Query Status
#' @author Ross Searle
#' @export
apiQueryStatus_Single <- function(reqID, verbose = FALSE) {

  if (!checkIfAuthorised()) return(NULL)

  stat_url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/get-query-request-status?requestId=", reqID)
  resp <- base::tryCatch({
    httr::GET(url = stat_url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(30))
  }, error = function(e) NULL)

  if (base::is.null(resp) || httr::http_error(resp)) return(NULL)

  jsn <- httr::content(resp, "text", encoding = "UTF-8")
  rq <- base::tryCatch({ jsonlite::fromJSON(jsn, simplifyDataFrame = TRUE) }, error = function(e) NULL)

  if (base::is.null(rq)) return(NULL)

  if (verbose) {
    return(rq)
  } else {
    return(rq$sdrRequest$status)
  }
}


#' Delete an ANSIS Query
#' @author Ross Searle
#' @export
apiDeleteQuery <- function(reqID) {

  if (!checkIfAuthorised()) return(NULL)

  url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/cancel-query-request?requestId=", reqID)
  resp <- base::tryCatch({
    httr::POST(url = url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(30))
  }, error = function(e) NULL)

  if (base::is.null(resp) || httr::http_error(resp)) return(NULL)

  return(httr::content(resp, "text", encoding = "UTF-8"))
}


#' Delete all ANSIS Queries
#' @author Ross Searle
#' @export
apiDeleteAllQueries <- function() {

  queries <- apiQueryStatus_All()
  if (base::is.null(queries) || base::nrow(queries) == 0) {
    base::cat("\nNo queries to delete.\n")
    return(invisible(0))
  }

  if (base::requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "Deleting queries :what [:bar] :percent in :elapsed",
      total = base::nrow(queries), clear = FALSE, width = 100, show_after = 1
    )
    for (i in 1:base::nrow(queries)) {
      pb$tick(tokens = base::list(what = stringr::str_pad(i, 20, "left")))
      apiDeleteQuery(queries$id[i])
    }
  } else {
    for (i in 1:base::nrow(queries)) {
      apiDeleteQuery(queries$id[i])
    }
  }

  base::cat(base::sprintf("\nDeleted %d queries.\n", base::nrow(queries)))
  return(invisible(base::nrow(queries)))
}


#' Download Query Data
#' @author Ross Searle
#' @export
apiDownloadQueryData <- function(reqID, outZipFile = NULL) {

  if (!checkIfAuthorised()) return(NULL)

  qStat <- apiQueryStatus_Single(reqID = reqID, verbose = TRUE)
  if (base::is.null(qStat) || base::is.null(qStat$sdrRequest$files)) {
    base::warning("No files returned for this request ID.", call. = FALSE)
    return(NULL)
  }

  fls <- qStat$sdrRequest$files
  if (base::length(fls) == 0) return(NULL)

  base::cat("Downloading data from ANSIS server....\n")

  outDir <- base::tempfile(pattern = "tmp_ansis_")
  base::dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  # Ensure temp directory cleanup on exit
  base::on.exit(base::unlink(outDir, recursive = TRUE), add = TRUE)

  for (i in base::seq_along(fls)) {
    if (base::length(fls) > 10) {
      base::cat(base::sprintf("\rDownloading %d of %d", i, base::length(fls)))
    }
    fn <- base::file.path(outDir, base::sprintf("%s.json", fls[i]))

    if (!base::file.exists(fn)) {
      dl_url <- base::paste0(Constants@ANSISAPIurlProd, "/ansis-external-api/query-requests/v2/download-response?fileId=", fls[i])
      resp <- base::tryCatch({
        httr::GET(url = dl_url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(60))
      }, error = function(e) NULL)

      if (!base::is.null(resp) && !httr::http_error(resp)) {
        jsn <- httr::content(resp, "text", encoding = "UTF-8")
        base::writeLines(jsn, con = fn, useBytes = TRUE)
      }
    }
  }

  base::cat("\nZipping JSON files....\n")

  # Safe working directory toggle using on.exit
  old_wd <- base::getwd()
  base::setwd(outDir)
  base::on.exit(base::setwd(old_wd), add = TRUE)

  files_to_zip <- base::list.files(path = ".", full.names = FALSE)
  if (base::length(files_to_zip) > 0 && !base::is.null(outZipFile)) {
    zip::zip(zipfile = outZipFile, files = files_to_zip)
    return(outZipFile)
  }

  return(NULL)
}


#' Get a DSM Suitable Data Table
#' @author Ross Searle
#' @export
getDSMtable <- function(ansisObject = NULL, propertyCode = NULL) {

  if (base::is.null(propertyCode) || propertyCode == "") {
    base::stop("You must specify a valid 'propertyCode'.", call. = FALSE)
  }

  if (base::is.null(ansisObject)) {
    base::stop("You must supply an 'ansisObject'.", call. = FALSE)
  }

  base::cat("\nGenerating DSM Table....\n")
  dsm <- makeDSMTable(ansisObject, propertyCode)
  return(dsm)
}


#' Get a single site
#' @author Ross Searle
#' @export
getSingleSite <- function(providerID, siteID, target_fields = NULL, format = "ANSISDataObject") {

  if (!checkIfAuthorised()) return(NULL)

  if (base::missing(providerID) || base::missing(siteID)) {
    base::stop("Both 'providerID' and 'siteID' must be specified.", call. = FALSE)
  }

  url <- base::paste0(Constants@ANSISAPIurlProd, "/sdr-public/v1/SingleSite?provider=", providerID, "&site=", siteID)

  resp <- base::tryCatch({
    httr::GET(url = url, httr::add_headers(Authorization = base::paste0("Bearer ", authANSIS@Token)), httr::timeout(30))
  }, error = function(e) NULL)

  if (base::is.null(resp) || httr::http_error(resp)) {
    base::warning(base::sprintf("Failed to retrieve single site data (HTTP status %s).", httr::status_code(resp)), call. = FALSE)
    return(NULL)
  }

  jsn <- httr::content(resp, "text", encoding = "UTF-8")

  if (format == "JSON") {
    return(jsn)
  } else if (format == "ANSISDataObject") {
    jf <- base::tempfile(pattern = "ANSIS_Site_", fileext = ".json")
    base::on.exit(base::unlink(jf), add = TRUE)

    base::writeLines(jsn, con = jf, useBytes = TRUE)
    ol <- parseANSISJsonSingleSite3(jsnFileName = jf, propertiesToParse = target_fields)
    return(ol)
  } else {
    base::stop("Invalid format specified. Must be 'JSON' or 'ANSISDataObject'.", call. = FALSE)
  }
}
