# TODO: What happens when there's missing cols? Create a new col or will the write handle this for us?

#' Update PostgreSQL DB
#' 
#' @param data Data to check against DB
#' @param source Data source to limit updates to
update_db <- function(
  data   = NULL,
  source = NULL
) {
  
  log_info('[DB*] Beginning database update process')
  
  if (is.null(data)) stop('No new data provided')
  
  hash_contents <- function(.data) .data %>%
    mutate(
      contents_hash = paste(
        bathrooms, bedrooms, parking, eer, 
        lead_agent_name, agency_name, auction, price
      ) %>% 
        str_remove_all(' ') %>% 
        toupper() %>% 
        md5() %>% 
        as.character()
    )
  
  generate_scd_attrs <- function(.data) .data %>% 
    mutate(
      extracted  = timestamp, 
      start_date = timestamp, 
      end_date   = with_tz(as_datetime(NA_Date_), "Australia/ACT"),
      active     = TRUE,
      .keep      = 'unused'
    )

  data <- data %>% 
    {if (is.null(source)) . else . %>% filter(source == source)} %>% 
    hash_contents()
  
  extracted_timestamp <- unique(data$timestamp)[[1]] - seconds(1)
  
  # Return list of changed, unchanged, new, but not deleted (we dont do anything with these)
 
  # Get current table ----
  
  # Instantiate connection
  log_info('[DB*]   Instantiating DB connection, hashing contents')
  
  con <- dbConnect(
    RPostgres::Postgres(),
    host = 'localhost',
    port = 5433,
    user = 'postgres',
    password = '',
    dbname = 'house_prices'
  )
  
  # Get active records in table
  db_table <- tbl(con, 'cbr_house_prices') %>% 
    filter(active) %>% 
    {if (is.null(source)) . else . %>% filter(source == source)} 
  
  working_copy <- db_table %>% 
    collect() %>% 
    hash_contents()
  
  # Comparisons ----
  
  log_info('[DB*]   Compare row contents and determine updates required')
  
  # Pull updated rows
  row_diff <- full_join(
    working_copy %>% select(hash_id, contents_hash),
    data %>% select(hash_id, contents_hash),
    suffix = c('_current', '_new'),
    by = 'hash_id'
  )
  
  # Filter only for new records/differences. Do not worry about
  # deletions/missing records since we're just getting a feed of updated data
  # - not what the whole dataset should be.
  #
  # For new rows, set start/end/active
  new_rows <- data %>%
    inner_join(
      row_diff %>% 
        filter(is.na(contents_hash_current) & !is.na(contents_hash_new)) %>% 
        select(hash_id), 
      by = 'hash_id'
    ) %>% 
    generate_scd_attrs()
  
  # For updated rows, we'll need to update the start/end/active in the DB table
  upd_rows <- data %>% 
    inner_join(
      row_diff %>% 
        filter(contents_hash_current != contents_hash_new) %>% 
        select(hash_id), 
      by = 'hash_id'
    ) %>% 
    generate_scd_attrs()
    
  
  # Update existing IDs, then update and end existing rows
  if (nrow(upd_rows) > 0) {
    
    log_info(glue('[DB*]   Existing rows to update: {nrow(upd_rows)}'))
    
    existing_row_update <- glue("
      UPDATE cbr_house_prices
      SET end_date = '{extracted_timestamp}', active = FALSE
      WHERE hash_id IN ({
        paste0(shQuote(upd_rows$hash_id, type = 'sh'), collapse = ', ')
      })
    ")
    
    dbExecute(con, existing_row_update)
    
  }
  
  # Add new rows
  if (nrow(new_rows) > 0) {
    
    log_info(glue('[DB*]   New rows to add: {nrow(new_rows)}'))
    
    dbWriteTable(
      con, 
      Id(schema = "public", table = 'cbr_house_prices'), 
      new_rows %>% select(-contents_hash), 
      append = TRUE, 
      overwrite = FALSE
    )
    
  }

  log_info('[DB*]   Disconnecting from DB')
  
  dbDisconnect(con)
  
  log_success('[DB*] Database update complete')
   
}
