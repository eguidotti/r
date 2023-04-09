#' Import tabular data into SQL
#'
#' Create SQL tables from \code{data.table}, \code{data.frame}, or flat files.
#'
#' @param conn a \code{DBIConnection} object, as returned by \code{\link[DBI]{dbConnect}}.
#' @param table \code{data.table}, \code{data.frame}, path to a tabular data file, or path to the directory containing tabular data files.
#' @param name table name. Used only when \code{data} is a \code{data.table} or path to file.
#' @param pattern an optional regular expression. Only file names into the \code{data} folder which match the regular expression will be used.
#' @param index character vector of column names to use as index.
#' @param overwrite if \code{TRUE}, an existing table of the same name will be overwritten. This argument doesn't change behavior if the table does not exist yet.
#' @param verbose if \code{TRUE}, print on progress.
#' @param na.strings a character vector of strings which are to be interpreted as \code{NA} values by \code{\link[data.table]{fread}}.
#' @param ... additional arguments passed to \code{\link[data.table]{fread}}.
#'
#' @returns
#' \code{NULL}
#'
#' @examples
#' \dontrun{
#' library(RSQLite)
#' conn <- dbConnect(RSQLite::SQLite(), "crsp.db")
#' path <- "path/to/crsp/sazYYYYMM_r"
#' dbInit(conn, path)
#' }
#'
#' @export
#'
dbImport <- function(
    conn, 
    table, 
    name = NULL,
    pattern = "\\.(rds|csv|gz)$", 
    index = NULL,
    overwrite = FALSE, 
    verbose = TRUE,  
    na.strings = "", 
    ...) {
  
  if(is.character(table) && dir.exists(table)){

    for(file in list.files(path = table, pattern = pattern, full.names = TRUE)){
      
      dbImportTable(
        conn, 
        table = file, 
        name = gsub("\\..*$", "", basename(file)),
        index = index,
        overwrite = overwrite, 
        verbose = verbose, 
        na.strings = na.strings, 
        ...
      )
      
    }

  }
  
  else {
    
    dbImportTable(
      conn, 
      table = table, 
      name = name,
      index = index,
      overwrite = overwrite, 
      verbose = verbose, 
      na.strings = na.strings, 
      ...
    )
    
  }

}

#' Import Table
#'
#' @param conn \code{DBIConnection} object, as returned by \code{\link[DBI]{dbConnect}}.
#' @param table \code{data.table}, \code{data.frame}, or path to a data file.
#' @param name the table name.
#' @param index character vector of column names to use as index.
#' @param overwrite if \code{TRUE}, an existing table of the same name will be overwritten. This argument doesn't change behavior if the table does not exist yet.
#' @param verbose if \code{TRUE}, print on progress.
#' @param na.strings character vector of strings which are to be interpreted as \code{NA} values by \code{\link[data.table]{fread}}.
#' @param ... additional arguments passed to \code{\link[data.table]{fread}}.
#'
#' @returns
#' \code{NULL}
#'
dbImportTable <- function(
    conn, 
    table, 
    name,
    index,
    overwrite, 
    verbose, 
    na.strings, 
    ...) {
  
  exists <- dbExistsTable(conn, name = name)
  
  if(verbose) cat(sprintf("Table: %s\n", name))
  
  if(exists & !overwrite){
    
    if(verbose) cat(
      "  -> skipped: table already exists and overwrite=FALSE.\n"
    )
    
  }
  
  else {
    
    if(is.character(table)){
      
      if(verbose) cat("  ->  reading file...\n")
      
      if(endsWith(table, ".rds"))
        table <- readRDS(table)
      
      else
        table <- fread(
          file = table, 
          na.strings = na.strings, 
          showProgress = FALSE, 
          ...)
      
    }
    
    else if(is.data.table(table) & is.null(index)){
      
      index <- key(table)
      
    }
    
    if(verbose) cat("  ->  sanitizing data...\n")
    
    table <- data.frame(table)
    index <- tolower(index)
    colnames(table) <- tolower(colnames(table))
    
    for(i in colnames(table))
      if(inherits(table[,i], 'Date'))
        table[,i] <- as.integer(format(table[,i], format = "%Y%m%d"))
    
    if("itemlongdesc" %in% colnames(table))
      table$itemlongdesc <- gsub("[\x80-\xff]", "", table$itemlongdesc)
    
    if(verbose) cat(sprintf(
      "  ->  %s table...\n", 
      ifelse(exists, "overwriting", "creating")
    ))
    
    dbWriteTable(
      conn, 
      name = name, 
      value = table, 
      overwrite = overwrite, 
      row.names = FALSE
    )
    
    if(!is.null(index)){
      
      cols <- paste(intersect(index, colnames(table)), collapse = ", ")
      
      if(cols != ""){
        
        if(verbose) cat("  ->  creating index...\n")
        
        dbExecute(conn, sprintf(
          "CREATE INDEX idx_%s ON %s (%s)", 
          name, name, cols
        ))  
        
      }
      
    }
    
    if(verbose) cat("  ->  done!\n")
    
  }

}
