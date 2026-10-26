
library(jsonlite)
library(parallel)
library(pbapply)

# # Define the directory containing your JSON files (use "." for the current directory)
# input_directory <- "C:/temp/ansis"
# output_file <- "c:/temp/parsed_soil_data_combined.csv"
#
# input_files <- list.files(path = input_directory,  full.names = TRUE)

# 1. Define the worker function that processes a single file
process_file <- function(file_path) {

  csv_data <- list()
  counter <- 1

  # Helper functions to safely extract characters
  safe_char <- function(x) if (is.null(x)) NA_character_ else as.character(x)

  get_used_procedure <- function(p) {
    if (is.null(p)) return(NA_character_)
    if (is.character(p)) return(p)
    if (is.list(p)) {
      if ("href" %in% names(p)) return(as.character(p$href))
      if ("value" %in% names(p)) return(as.character(p$value))
      return(as.character(p[[1]])) # Fallback for nested lists
    }
    return(as.character(p))
  }

  # Read the JSON file safely
  json_data <- tryCatch({
    fromJSON(file_path, simplifyVector = FALSE)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(json_data) || is.null(json_data$data)) return(NULL)

  for (site in json_data$data) {

    # Extract and split Geometry/SRID into X and Y
    geometry_str <- safe_char(site$geometry$result)
    x_coord <- NA_character_
    y_coord <- NA_character_

    if (!is.na(geometry_str)) {
      match_coords <- regmatches(geometry_str, regexec("([+-]?[0-9]*\\.?[0-9]+)\\s+([+-]?[0-9]*\\.?[0-9]+)", geometry_str))
      if (length(match_coords[[1]]) >= 3) {
        x_coord <- match_coords[[1]][2]
        y_coord <- match_coords[[1]][3]
      }
    }

    # Extract and concatenate scopedIdentifier value and authority
    scoped_id <- NA_character_
    if (!is.null(site$scopedIdentifier) && length(site$scopedIdentifier) > 0) {
      val <- safe_char(site$scopedIdentifier[[1]]$value)
      auth <- safe_char(site$scopedIdentifier[[1]]$authority)

      if (!is.na(val) && !is.na(auth)) {
        scoped_id <- paste(val, auth, sep = "_")
      } else if (!is.na(val)) {
        scoped_id <- val
      } else if (!is.na(auth)) {
        scoped_id <- auth
      }
    }

    for (visit in site$siteVisit) {
      for (profile in visit$soilProfile) {
        for (layer in profile$soilLayer) {

          depth_upper <- safe_char(layer$depthUpper$result$value)
          depth_lower <- safe_char(layer$depthLower$result$value)

          exclude_keys <- c("type", "boundaryDistinctness", "designation", "depthUpper", "depthLower")

          for (prop_key in names(layer)) {
            if (!(prop_key %in% exclude_keys)) {
              prop_val <- layer[[prop_key]]

              # Handle properties as arrays
              if (is.list(prop_val) && is.null(names(prop_val))) {
                for (item in prop_val) {
                  if (is.list(item) && "result" %in% names(item)) {
                    res_obj <- item$result
                    res_val <- if (is.list(res_obj) && "value" %in% names(res_obj)) res_obj$value else res_obj
                    proc_val <- get_used_procedure(item$usedProcedure)

                    csv_data[[counter]] <- data.frame(
                      source_file = basename(file_path),
                      x = x_coord,
                      y = y_coord,
                      scopedIdentifier_Value_Authority = scoped_id,
                      depthUpper = depth_upper,
                      depthLower = depth_lower,
                      property = prop_key,
                      usedProcedure = proc_val,
                      result_value = safe_char(res_val),
                      stringsAsFactors = FALSE
                    )
                    counter <- counter + 1
                  }
                }
              }
              # Handle properties as single dictionary objects
              else if (is.list(prop_val) && "result" %in% names(prop_val)) {
                res_obj <- prop_val$result
                res_val <- if (is.list(res_obj) && "value" %in% names(res_obj)) res_obj$value else res_obj
                proc_val <- get_used_procedure(prop_val$usedProcedure)

                csv_data[[counter]] <- data.frame(
                  source_file = basename(file_path),
                  x = x_coord,
                  y = y_coord,
                  scopedIdentifier_Value_Authority = scoped_id,
                  depthUpper = depth_upper,
                  depthLower = depth_lower,
                  property = prop_key,
                  usedProcedure = proc_val,
                  result_value = safe_char(res_val),
                  stringsAsFactors = FALSE
                )
                counter <- counter + 1
              }
            }
          }
        }
      }
    }
  }

  if (length(csv_data) > 0) {
    return(do.call(rbind, csv_data))
  } else {
    return(NULL)
  }
}

# # 2. Execute parallel processing
# if (length(input_files) > 0) {
#   num_cores <- max(1, detectCores() - 1)
#
#   cat(sprintf("Found %d JSON files. Starting parallel processing across %d cores...\n",
#               length(input_files), num_cores))
#
#   cl <- makeCluster(num_cores)
#
#   clusterEvalQ(cl, {
#     library(jsonlite)
#   })
#
#   results_list <- pblapply(input_files, process_file, cl = cl)
#
#   stopCluster(cl)
#
#   final_df <- do.call(rbind, results_list)
#
#   if (!is.null(final_df) && nrow(final_df) > 0) {
#     write.csv(final_df, output_file, row.names = FALSE, na = "")
#     cat(sprintf("\nSuccessfully processed %d records. Saved to %s\n",
#                 nrow(final_df), output_file))
#   } else {
#     cat("\nNo data was extracted from the provided files.\n")
#   }
#
# } else {
#   cat("No JSON files found in the specified directory.\n")
# }
# shell(output_file, wait = F)
