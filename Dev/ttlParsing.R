fetch_ansis_properties_github <- function(
    ttl_url = "https://raw.githubusercontent.com/ANZSoilData/def-au-scm/main/rdf/scm.ttl",
    cache_file = tempfile(fileext = ".ttl"),
    use_cache = FALSE
) {
  if (use_cache && file.exists(cache_file)) {
    message("Loading TTL from cache: ", cache_file)
  } else {
    message("Downloading TTL from GitHub raw URL: ", ttl_url)
    download.file(ttl_url, destfile = cache_file, quiet = TRUE)
  }
  ttl_text <- paste(readLines(cache_file), collapse = "\n")
  # Split text by each concept/collection block starting with a URI
  blocks <- str_split(ttl_text, "(?=<http://anzsoil.org/def/au/scm/)", simplify = FALSE)[[1]]
  # Filter out any empty or irrelevant blocks
  blocks <- blocks[nzchar(blocks)]
  # Parse each block
  parsed <- lapply(blocks, function(block) {
    # Extract the URI (subject)
    uri <- str_extract(block, "<http://anzsoil.org/def/au/scm/[^>]+>")
    if (is.na(uri)) return(NULL)
    # Extract code (last part of URI)
    code <- sub(".*/", "", uri)
    code <- gsub("[<>]", "", code)
    # Extract skos:prefLabel literal (English)
    label <- str_match(block, 'skos:prefLabel\\s+"([^"]+)"@en')[,2]
    if (is.na(label)) return(NULL)
    # Extract rdf:type (optional)
    type <- str_match(block, 'a\\s+([^;\\s]+)')[,2]
    tibble(code = code, label = label, uri = gsub("[<>]", "", uri), type = type)
  })
  df <- bind_rows(parsed)
  # Optional: filter only skos:Concept or skos:Collection
  df <- df %>% filter(type %in% c("skos:Concept", "skos:Collection"))
  return(df)
}

Properties.tbl <- fetch_ansis_properties_github()