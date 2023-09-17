# Run whenever you need to start the table from scratch again

source('libs.R')

#' Re-initialise the House Prices SCD table
#'
#' Run when you need to initialise or re-initialise the SCD table. Will ask if
#' you want to delete the table if it already exists.
#'
#' @param data_override Dataframe to load into the DB instead of doing a fresh
#'   scrape. Requires a `timestamp` column to build SCD artifacts from.
#' @param force Boolean, whether to ask to delete table if it exists
scd_table_init <- function(data_override = NULL, force = FALSE) {
  
  # Function defns ----
  
  # Generate CREATE TABLE query based on contents of this table
  generate_create_table_sql <- function(df, table_name, schema = "public") {
    
    # Extract column names and types
    col_names <- names(df)
    col_types <- sapply(df, function(col) class(col)[1])
    
    # Map R data types to PostgreSQL data types
    type_mapping <- list(
      "numeric"   = "NUMERIC",
      "integer"   = "INTEGER",
      "character" = "TEXT",
      "factor"    = "TEXT",
      "Date"      = "DATE",
      "POSIXct"   = "TIMESTAMP WITH TIME ZONE",
      "logical"   = "BOOLEAN"
    )
    
    sql_types <- sapply(col_types, function(type) type_mapping[[type]])
    
    # Generate column definitions
    col_definitions <- paste0(col_names, " ", sql_types, collapse = ", ")
    
    # Generate SQL statement
    create_table_sql <- glue("CREATE TABLE {schema}.{table_name} ({col_definitions});")
    
    return(create_table_sql)
    
  }
  
  
  # Check if table exists ----

  log_info('Instantiating DB connection')
  
  # Instantiate connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = 'localhost',
    port = 5433,
    user = 'postgres',
    password = '',
    dbname = 'house_prices'
  )
  
  matching_tables <- dbGetQuery(
    con,
    "
      SELECT * 
      FROM information_schema.tables
      WHERE table_schema = 'public' AND table_name = 'cbr_house_prices'
    "
  )
  
  log_info('Checking the existence of this table')
  
  if (nrow(matching_tables) > 0) {
    
    if (!force) {
      
      usr_input <- askYesNo('Table cbr_house_prices already exists, delete and reinitialise?')
      if (!usr_input | is.na(usr_input)) stop('User chose to stop reinitialisation')
      
    }
    
    log_info('cbr_house_prices table found, deleting table')
    dbExecute(con, 'DROP TABLE cbr_house_prices')
    
  }
  
  
  # Load data ----
  log_info('Getting data')
  
  # Set up Slowly Changing Dimensions (Type 2) attributes columns using the
  # timestamp
  table_init_payload_raw <- if (is.null(data_override)) {
    bind_rows(
      allhomes_scraper(),
      domain_scraper()
    )
  } else data_override
  
  table_init_payload <- table_init_payload_raw %>% 
    mutate(
      extracted  = as_datetime(timestamp, tz = 'Australia/Sydney'), 
      start_date = as_datetime(timestamp, tz = 'Australia/Sydney'), 
      end_date   = as_datetime(NA_Date_,  tz = 'Australia/Sydney'),
      active     = TRUE,
      .keep      = 'unused'
    )
  
  create_table_house_prices <- generate_create_table_sql(table_init_payload, "cbr_house_prices")
  
  
  # Create table ----
  
  log_info('Creating table in DB')
  
  dbExecute(
    con, 
    create_table_house_prices
  )
  
  log_info('Writing...')
  
  # Write data to table
  dbWriteTable(
    con, 
    Id(schema = "public", table = "cbr_house_prices"), 
    table_init_payload, 
    append = TRUE, 
    overwrite = FALSE
  )
  
  log_success('Table written, disconnecting from DB')
  
  # Disconnect
  dbDisconnect(con)
  
}

