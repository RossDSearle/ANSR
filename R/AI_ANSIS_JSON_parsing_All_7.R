
# Helper 1: Recursively prune nested JSON list before flattening
prune_json_tree <- function(node, target_pattern) {
  if (is.atomic(node) || is.null(node) || length(node) == 0) return(node)

  if (is.list(node)) {
    nms <- names(node)

    # Unnamed list (JSON array)
    if (is.null(nms)) {
      res <- lapply(node, function(x) prune_json_tree(x, target_pattern))
      if (length(res) == 0) return(NULL)
      keep <- vapply(res, function(x) !is.null(x) && length(x) > 0, FUN.VALUE = logical(1))
      res <- res[keep]
      return(if (length(res) > 0) res else NULL)
    }

    # Named list (JSON object)
    res <- vector("list", length(node))
    keep <- logical(length(node))

    for (i in seq_along(node)) {
      nm <- nms[i]
      if (is.na(nm) || nm == "") next

      # Keep sub-tree if element name matches target pattern
      if (grepl(target_pattern, nm, ignore.case = TRUE)) {
        res[[i]] <- node[[i]]
        keep[i] <- TRUE
      } else {
        # Otherwise recursively inspect child branches
        child_pruned <- prune_json_tree(node[[i]], target_pattern)
        if (!is.null(child_pruned) && length(child_pruned) > 0) {
          res[[i]] <- child_pruned
          keep[i] <- TRUE
        }
      }
    }

    res <- res[keep]
    names(res) <- nms[keep]
    return(if (length(res) > 0) res else NULL)
  }

  return(node)
}

# Helper 2: Fast vector-based JSON list flattener
flatten_all_nodes_fast <- function(node) {
  paths <- character(1000)
  vals <- character(1000)
  count <- 0

  walk <- function(curr_node, curr_path) {
    if (is.atomic(curr_node) || is.null(curr_node)) {
      count <<- count + 1
      if (count > length(paths)) {
        length(paths) <<- length(paths) * 2
        length(vals) <<- length(vals) * 2
      }
      paths[count] <<- curr_path
      vals[count] <<- if (is.null(curr_node)) NA_character_ else paste(curr_node, collapse = ", ")
      return()
    }

    if (is.list(curr_node)) {
      nms <- names(curr_node)
      if (is.null(nms)) nms <- seq_along(curr_node)

      for (i in seq_along(curr_node)) {
        child_name <- nms[i]
        new_path <- if (curr_path == "") as.character(child_name) else paste0(curr_path, ".", child_name)
        walk(curr_node[[i]], new_path)
      }
    }
  }

  walk(node, "")
  if (count == 0) return(tibble::tibble(path = character(), value = character()))
  tibble::tibble(path = paths[1:count], value = vals[1:count])
}

# 1. Main function to process a single JSON file
process_json_fileAll <- function(file_path, target_fields = NULL) {

  json_data <- tryCatch(
    jsonlite::fromJSON(file_path, simplifyVector = FALSE),
    error = function(e) return(NULL)
  )

  if (is.null(json_data) || length(json_data) == 0) return(tibble::tibble())

  # Extract Project Lookup Table from top-level 'included'
  project_lookup <- tibble::tibble(
    id = character(),
    projectName = character(),
    projectLabel = character(),
    projectAuthority = character()
  )

  if (is.list(json_data) && !is.null(json_data$included) && !is.null(json_data$included$projects)) {
    project_lookup <- purrr::map_dfr(json_data$included$projects, function(p) {
      pid <- if (!is.null(p$id)) as.character(p$id) else NA_character_
      pname <- if (!is.null(p$name)) as.character(p$name) else NA_character_
      plabel <- if (!is.null(p$label)) as.character(p$label) else NA_character_

      pauth <- NA_character_
      if (!is.null(p$scopedIdentifier)) {
        if (is.list(p$scopedIdentifier) && !is.null(p$scopedIdentifier$authority)) {
          pauth <- as.character(p$scopedIdentifier$authority)
        } else if (is.character(p$scopedIdentifier) && "authority" %in% names(p$scopedIdentifier)) {
          pauth <- as.character(p$scopedIdentifier["authority"])
        }
      }

      tibble::tibble(
        id = pid,
        projectName = pname,
        projectLabel = plabel,
        projectAuthority = pauth
      )
    }) |> dplyr::distinct(id, .keep_all = TRUE)
  }

  # Exclude top-level '$schema', 'meta', 'included', and 'include' fields
  if (is.list(json_data) && !is.null(names(json_data))) {
    json_data <- json_data[!names(json_data) %in% c("$schema", "meta", "included", "include")]
  }

  # PRE-FLATTENING PRUNING: Discard unneeded JSON sub-trees prior to flattening
  if (!is.null(target_fields) && length(target_fields) > 0) {
    required_metadata <- c("scopedIdentifier", "geometry", "endedAtTime", "depthUpper", "depthLower", "relatedProject")
    keep_fields <- unique(c(required_metadata, target_fields))
    target_pattern <- paste0("(?i)^(", paste(keep_fields, collapse = "|"), ")$")

    json_data <- prune_json_tree(json_data, target_pattern)
    if (is.null(json_data) || length(json_data) == 0) return(tibble::tibble())
  }

  # FAST FLATTENING
  flat_df <- flatten_all_nodes_fast(json_data)
  if (nrow(flat_df) == 0) return(tibble::tibble())

  parsed_df <- flat_df |>
    dplyr::mutate(
      sourceFile = basename(file_path),
      hierarchyPath = path,
      site_idx = as.integer(stringr::str_extract(path, "(?<=data\\.)\\d+")),
      visit_idx = as.integer(stringr::str_extract(path, "(?<=siteVisit\\.)\\d+")),
      prof_idx = as.integer(stringr::str_extract(path, "(?<=soilProfile\\.)\\d+")),
      layer_idx = as.integer(stringr::str_extract(path, "(?<=soilLayer\\.)\\d+")),

      clean_path = stringr::str_remove_all(path, "(?i)\\.result"),

      attr_type = dplyr::case_when(
        stringr::str_detect(clean_path, "(?i)\\.(unit|uom)(\\.\\d+)?$") ~ "Unit",
        stringr::str_detect(clean_path, "(?i)\\.?(used)?procedure(\\.\\d+)?$") ~ "UsedProcedure",
        stringr::str_detect(clean_path, "(?i)\\.(value|hasValue)(\\.\\d+)?$") ~ "Value",
        TRUE ~ "Value"
      ),

      base_path = stringr::str_remove(clean_path, "(?i)\\.(unit|uom|procedure|usedProcedure|value|hasValue)(\\.\\d+)?$"),
      fieldName = stringr::str_remove(base_path, "^.*(data|siteVisit|soilProfile|soilLayer)\\.\\d+\\.")
    )

  wide_df <- parsed_df |>
    dplyr::select(sourceFile, hierarchyPath, site_idx, visit_idx, prof_idx, layer_idx, base_path, fieldName, attr_type, value) |>
    dplyr::group_by(sourceFile, site_idx, visit_idx, prof_idx, layer_idx, base_path, fieldName) |>
    dplyr::summarise(
      hierarchyPath = first(hierarchyPath),
      Value = paste(value[attr_type == "Value"], collapse = ", "),
      Unit = paste(value[attr_type == "Unit"], collapse = ", "),
      UsedProcedure = paste(value[attr_type == "UsedProcedure"], collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Value = dplyr::if_else(Value == "", NA_character_, Value),
      Unit = dplyr::if_else(Unit == "", NA_character_, Unit),
      UsedProcedure = dplyr::if_else(UsedProcedure == "", NA_character_, UsedProcedure)
    )

  # Extract AnsisSiteID
  scoped_ids <- flat_df |>
    dplyr::filter(stringr::str_detect(path, "(?i)scopedIdentifier.*value$")) |>
    dplyr::mutate(site_idx = as.integer(stringr::str_extract(path, "(?<=data\\.)\\d+"))) |>
    dplyr::group_by(site_idx) |>
    dplyr::summarise(AnsisSiteID = paste(value, collapse = ", "), .groups = "drop")

  # Resolve relatedProject UUIDs against project_lookup
  rel_projects <- flat_df |>
    dplyr::filter(stringr::str_detect(path, "relatedProject")) |>
    dplyr::mutate(site_idx = as.integer(stringr::str_extract(path, "(?<=data\\.)\\d+"))) |>
    dplyr::filter(!is.na(value) & value != "") |>
    tidyr::separate_rows(value, sep = ",\\s*") |>
    dplyr::left_join(project_lookup, by = c("value" = "id")) |>
    dplyr::group_by(site_idx) |>
    dplyr::summarise(
      projectName = paste(na.omit(unique(projectName)), collapse = ", "),
      projectLabel = paste(na.omit(unique(projectLabel)), collapse = ", "),
      projectAuthority = paste(na.omit(unique(projectAuthority)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      projectName = dplyr::if_else(projectName == "", NA_character_, projectName),
      projectLabel = dplyr::if_else(projectLabel == "", NA_character_, projectLabel),
      projectAuthority = dplyr::if_else(projectAuthority == "", NA_character_, projectAuthority)
    )

  geoms <- flat_df |>
    dplyr::filter(stringr::str_detect(path, "geometry\\.result|geometry$")) |>
    dplyr::mutate(
      site_idx = as.integer(stringr::str_extract(path, "(?<=data\\.)\\d+")),
      SRID = stringr::str_extract(value, "(?<=SRID=)\\d+"),
      coords = stringr::str_extract(value, "(?<=POINT\\s?\\().*?(?=\\))"),
      Longitude = stringr::str_split_i(coords, "\\s+", 1),
      Latitude = stringr::str_split_i(coords, "\\s+", 2)
    ) |>
    dplyr::select(site_idx, SRID, Longitude, Latitude) |> dplyr::distinct()

  end_times <- flat_df |>
    dplyr::filter(stringr::str_detect(path, "endedAtTime")) |>
    dplyr::mutate(
      site_idx = as.integer(stringr::str_extract(path, "(?<=data\\.)\\d+")),
      visit_idx = as.integer(stringr::str_extract(path, "(?<=siteVisit\\.)\\d+"))
    ) |>
    dplyr::select(site_idx, visit_idx, endAtTime = value) |> dplyr::distinct()

  depths <- wide_df |>
    dplyr::filter(fieldName %in% c("depthUpper", "depthLower")) |>
    dplyr::select(site_idx, visit_idx, prof_idx, layer_idx, fieldName, Value) |>
    tidyr::pivot_wider(names_from = fieldName, values_from = Value)

  if (!"depthUpper" %in% names(depths)) depths$depthUpper <- NA_character_
  if (!"depthLower" %in% names(depths)) depths$depthLower <- NA_character_

  depths <- depths |>
    dplyr::mutate(
      depthUpper = stringr::str_remove_all(depthUpper, "[^0-9.-]"),
      depthLower = stringr::str_remove_all(depthLower, "[^0-9.-]")
    )

  final_df <- wide_df |>
    dplyr::left_join(scoped_ids, by = "site_idx") |>
    dplyr::left_join(rel_projects, by = "site_idx") |>
    dplyr::left_join(geoms, by = "site_idx") |>
    dplyr::left_join(end_times, by = c("site_idx", "visit_idx")) |>
    dplyr::left_join(depths, by = c("site_idx", "visit_idx", "prof_idx", "layer_idx")) |>
    dplyr::mutate(
      fieldDepth = stringr::str_count(hierarchyPath, "\\.") + 1,
      SiteID = paste(projectAuthority, projectLabel, AnsisSiteID, sep = "_"),
      Property = stringr::str_extract(fieldName, "^[^.]+"),
      PropertyIndex = stringr::str_extract(fieldName, "(?<=\\.)\\d+(?=\\.|$)"),
      subProperty = dplyr::case_when(
        !is.na(PropertyIndex) & stringr::str_detect(fieldName, "^[^.]+\\.\\d+\\.") ~ stringr::str_replace(fieldName, "^[^.]+\\.\\d+\\.", ""),
        is.na(PropertyIndex) & stringr::str_detect(fieldName, "\\.") ~ stringr::str_replace(fieldName, "^[^.]+\\.", ""),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::rename(PropertyPath = fieldName) |>
    dplyr::select(
      fieldDepth, SiteID, projectAuthority, projectLabel, AnsisSiteID, SRID, Longitude, Latitude,
      endAtTime, depthUpper, depthLower, PropertyPath, Property, PropertyIndex,
      subProperty, Value, Unit, UsedProcedure, projectName, sourceFile, hierarchyPath
    ) |>
    dplyr::filter(
      !PropertyPath %in% c("depthUpper", "depthLower", "relatedProject", "relatedVisitor", "geometry", "srid", "SRID", "$schema"),
      !stringr::str_detect(PropertyPath, "(?i)(^|\\.)(@?type|ansisType|id|included|organizations|persons|projects|scopedIdentifier)"),
      !stringr::str_detect(hierarchyPath, "(?i)(^|\\.)(included|scopedIdentifier)(\\.|$)")
    )

  # Final case-insensitive check against target_fields
  if (!is.null(target_fields) && length(target_fields) > 0) {
    targets_lower <- tolower(target_fields)
    final_df <- final_df |>
      dplyr::filter(tolower(Property) %in% targets_lower | tolower(PropertyPath) %in% targets_lower)
  }

  return(final_df)
}

# 2. Main execution function utilizing doSNOW
ANSIS_JSON_parsing_All <- function(inDir, target_fields = NULL, num_cores = NULL){

  json_files <- list.files(path = inDir, full.names = TRUE, recursive = FALSE)
  total_files <- length(json_files)

  if (total_files == 0) return(tibble::tibble())

  if (is.null(num_cores)) num_cores <- min(total_files, max(1, parallel::detectCores() - 1))
  cl <- parallel::makeCluster(num_cores)
  doSNOW::registerDoSNOW(cl)

  pb <- txtProgressBar(min = 0, max = total_files, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  `%dopar%` <- foreach::`%dopar%`

  parsed_robust_df <- foreach::foreach(
    file = json_files,
    .combine = dplyr::bind_rows,
    .packages = c("jsonlite", "purrr", "dplyr", "tibble", "stringr", "tidyr"),
    .export = c("process_json_fileAll", "prune_json_tree", "flatten_all_nodes_fast"),
    .options.snow = opts
  ) %dopar% {
    process_json_fileAll(file, target_fields = target_fields)
  }

  close(pb)
  parallel::stopCluster(cl)

  return(parsed_robust_df)
}



ANSIS_JSON_parsing_SingleSite <- function(jsnFileName, target_fields = NULL){

  singleDF <- process_json_fileAll(jsnFileName, target_fields = target_fields)
  return(singleDF)
}




