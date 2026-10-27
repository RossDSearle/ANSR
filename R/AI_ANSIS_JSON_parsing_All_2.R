library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(foreach)
library(doSNOW)
library(parallel)

# 1. Main function to process a single JSON file (Helpers nested inside)
process_json_fileAll <- function(file_path) {

  # =========================================================================
  # HELPER FUNCTIONS (Nested inside the worker so parallel cores can see them)
  # =========================================================================
  strip_unitsAll <- function(v) {
    if (is.na(v)) return(v)
    split_v <- strsplit(as.character(v), ",")[[1]]
    return(trimws(split_v[1]))
  }

  extract_single_nodeAll <- function(x) {
    if (!is.list(x)) {
      return(tibble::tibble(Value = as.character(x), Unit = NA_character_, UsedProcedure = NA_character_))
    }
    flat <- unlist(x)
    if (is.null(names(flat))) {
      return(tibble::tibble(Value = paste(flat, collapse = ", "), Unit = NA_character_, UsedProcedure = NA_character_))
    }
    unit_idx <- grep("unit|uom", names(flat), ignore.case = TRUE)
    unit_val <- if (length(unit_idx) > 0) paste(flat[unit_idx], collapse = ", ") else NA_character_
    proc_idx <- grep("procedure", names(flat), ignore.case = TRUE)
    proc_val <- if (length(proc_idx) > 0) paste(flat[proc_idx], collapse = ", ") else NA_character_
    val_idx <- grep("result|hasValue|^value$", names(flat), ignore.case = TRUE)

    if (length(val_idx) > 0) {
      val_val <- paste(flat[val_idx], collapse = ", ")
    } else {
      leftover <- flat
      exclude_idx <- c(proc_idx, unit_idx)
      if (length(exclude_idx) > 0) leftover <- flat[-exclude_idx]
      leftover <- leftover[leftover != ""]
      val_val <- if (length(leftover) > 0) paste(leftover, collapse = ", ") else NA_character_
    }
    return(tibble::tibble(
      Value = as.character(val_val),
      Unit = as.character(unit_val),
      UsedProcedure = as.character(proc_val)
    ))
  }

  extract_fieldsAll <- function(node) {
    if (is.null(node) || length(node) == 0) {
      return(tibble::tibble(Value = NA_character_, Unit = NA_character_, UsedProcedure = NA_character_))
    }
    if (is.list(node) && is.null(names(node))) {
      return(purrr::map_dfr(node, extract_single_nodeAll))
    } else {
      return(extract_single_nodeAll(node))
    }
  }
  # =========================================================================

  json_data <- tryCatch(jsonlite::fromJSON(file_path, simplifyVector = FALSE),
                        error = function(e) return(NULL))

  if (is.null(json_data) || is.null(json_data$data)) return(tibble::tibble())

  purrr::imap_dfr(json_data$data, function(site, idx_site) {

    scoped_id_flat <- unlist(site$scopedIdentifier)
    scoped_id <- if (length(scoped_id_flat) > 0) paste(rev(scoped_id_flat), collapse = "_") else NA_character_

    geom_node <- purrr::pluck(site, "geometry", "result", .default = purrr::pluck(site, "geometry", .default = NA_character_))
    geom_str <- paste(unlist(geom_node), collapse = " ")

    srid_val <- NA_character_
    lon_val <- NA_character_
    lat_val <- NA_character_

    if (grepl("SRID=[0-9]+;POINT", geom_str)) {
      srid_val <- sub(".*SRID=([0-9]+).*", "\\1", geom_str)
      coords_str <- sub(".*POINT\\s*\\((.*?)\\).*", "\\1", geom_str)
      coords_split <- strsplit(trimws(coords_str), "\\s+")[[1]]
      if (length(coords_split) >= 2) {
        lon_val <- coords_split[1]
        lat_val <- coords_split[2]
      }
    }

    purrr::imap_dfr(site$siteVisit, function(visit, idx_visit) {
      end_time <- purrr::pluck(visit, "endedAtTime", .default = NA_character_)

      purrr::imap_dfr(visit$soilProfile, function(profile, idx_prof) {
        purrr::imap_dfr(profile$soilLayer, function(layer, idx_layer) {

          d_upper <- paste(extract_fieldsAll(layer$depthUpper)$Value, collapse = ", ")
          d_lower <- paste(extract_fieldsAll(layer$depthLower)$Value, collapse = ", ")

          d_upper <- gsub("[^0-9.-]", "", d_upper)
          d_lower <- gsub("[^0-9.-]", "", d_lower)

          obs_fields <- setdiff(names(layer), c("depthUpper", "depthLower", "type", "@type"))

          purrr::map_dfr(obs_fields, function(field_name) {

            extracted <- extract_fieldsAll(layer[[field_name]])
            if (nrow(extracted) == 0) return(tibble::tibble())

            extracted$Value <- purrr::map_chr(extracted$Value, strip_unitsAll)

            hierarchy_path <- sprintf("data[%s]/siteVisit[%s]/soilProfile[%s]/soilLayer[%s]/%s",
                                      idx_site, idx_visit, idx_prof, idx_layer, field_name)

            base_info <- tibble::tibble(
              sourceFile = basename(file_path),
              hierarchyPath = hierarchy_path,
              scopedIdentifier = as.character(scoped_id),
              SRID = srid_val,
              Longitude = lon_val,
              Latitude = lat_val,
              endAtTime = as.character(end_time),
              depthUpper = as.character(d_upper),
              depthLower = as.character(d_lower),
              fieldName = as.character(field_name)
            )

            dplyr::bind_cols(base_info, extracted)
          })
        })
      })
    })
  })
}


# 2. Main execution function utilizing doSNOW
ANSIS_JSON_parsing_All <- function(inDir, num_cores){

  json_files <- list.files(path = inDir, full.names = TRUE, recursive = FALSE)
  total_files <- length(json_files)

  if (total_files == 0) {
    message("No JSON files found in the specified directory.")
    return(tibble::tibble())
  }

  # Set up and register the parallel cluster
  num_cores <- min(total_files, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(num_cores)
  doSNOW::registerDoSNOW(cl)

  message(sprintf("Processing %d files across %d parallel workers...", total_files, num_cores))

  # Set up the txtProgressBar and link it to doSNOW options
  pb <- txtProgressBar(min = 0, max = total_files, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  `%dopar%` <- foreach::`%dopar%`

  # Execute parallel loop with foreach
  # .packages ensures required packages are loaded on each worker
  parsed_robust_df <- foreach::foreach(
    file = json_files,
    .combine = dplyr::bind_rows,
    .packages = c("jsonlite", "purrr", "dplyr", "tibble"),
    .export = c("process_json_fileAll"),
    .options.snow = opts
  ) %dopar% {
    process_json_fileAll(file)
  }

  # Close progress bar and shut down the cluster
  close(pb)
  stopCluster(cl)

  ansisData <- parsed_robust_df

  return(ansisData)
}
