## Download and untar file from Zenodo
zenodo_dl <- function(id) {
  jx <- jsonlite::fromJSON(paste0("https://zenodo.org/api/records/", id))
  ne_or <- function(z, or) tryCatch(if (!is.null(z) && nzchar(z)) z else or, error = function(e) or)
  ## collection size
  csize <- tryCatch(as.numeric(format(sum(jx$files$filesize, na.rm = TRUE)/1024^3, digits = 1)), error = function(e) NULL)
  doi <- ne_or(jx$doi, ne_or(jx$metadata$doi, NULL))
  doc_url <- if (length(doi) > 0) paste0("https://doi.org/", doi) else paste0("https://zenodo.org/record/", id)
  file_name <- jx$files$filename
  source_url = jx$files$links$download
  download.file(url = source_url, destfile = file_name)
  untar(file_name)
}
