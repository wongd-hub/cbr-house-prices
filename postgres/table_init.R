# Load data ----
# Set up Slowly Changing Dimensions (Type 2) attributes columns using the
# timestamp
table_init_payload <- allhomes_scraper(i_max = 5) %>% 
  rename(start_date = timestamp) %>% 
  mutate(end_date = as_datetime(NA_Date_), active = TRUE)

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
    "POSIXct"   = "TIMESTAMP",
    "logical"   = "BOOLEAN"
  )
  
  sql_types <- sapply(col_types, function(type) type_mapping[[type]])
  
  # Generate column definitions
  col_definitions <- paste0(col_names, " ", sql_types, collapse = ", ")
  
  # Generate SQL statement
  create_table_sql <- sprintf("CREATE TABLE %s.%s (%s);", schema, table_name, col_definitions)
  
  return(create_table_sql)
  
}

create_table_house_prices <- generate_create_table_sql(table_init_payload, "cbr_house_prices")

# Create connection and create table ----

con <- dbConnect(
  RPostgres::Postgres(),
  host = 'localhost',
  port = 5433,
  user = 'postgres',
  password = '',
  dbname = 'house_prices'
)

dbExecute(
  con, 
  create_table_house_prices
)


# Write data to table
dbWriteTable(
  con, 
  Id(schema = "public", table = "cbr_house_prices"), 
  table_init_payload, 
  append = TRUE, 
  overwrite = FALSE
)

# Disconnect
dbDisconnect(con)