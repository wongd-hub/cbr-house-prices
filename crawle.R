# TODO: Can we generalise the allhomes scraper? Pass in:
#  - Base URL, subpage URL
#  - XML base/sub-queries
# Might be difficult. AllHomes had special stuff that needed separate scraping.
# Make sub-functions that you can switch out to handle this?

#' Scrape Domain
#'
#' @param baseurl Base URL of site
#' @param start_page Which page to start from
#' @param n_pages Number of pages to scrape
#' @param hardstop Max number of pages to scrape, handy if setting `n_pages` to
#'   `Inf` but still not wanting function to go forever
#' @param forced_delay Whether to force sleep for 30 seconds every 5 iterations
#' @param debug Whether to run browser if potentially problematic rows exist
domain_scraper <- function(
    baseurl      = "https://www.domain.com.au", 
    start_page   = NULL, 
    n_pages      = Inf, 
    hardstop     = 50,
    forced_delay = FALSE,
    
    # Dev/debugging
    debug        = FALSE,
    html_ovrride = NULL
) {
  
  # Setup ----
  log_info('[DM] Beginning Domain scrape')
  
  # For testing purposes, set up a few variables
  if (!is.null(html_ovrride)) n_pages <- 1
  
  # Politely introduce bot to AllHomes website
  session <- if (is.null(html_ovrride)) bow(baseurl, force = T) else NA
  
  # Iterators
  start_page <- coalesce(start_page, 1)
  page_sublink <- glue('sold-listings/canberra-act/?excludepricewithheld=1&page={start_page}')
  iterator <- 1
  result_table <- list()
  
  # Loop across pages until there are no more
  while (!is.na(page_sublink) & iterator <= n_pages) {
    
    if (forced_delay & iterator %% 5 == 0) {
      log_info(glue('[DM]   5 iterations since last sleep, sleeping for 30...'))
      Sys.sleep(30)
    }
    
    log_info(glue('[DM]   Iteration {iterator}, scraping {baseurl}/{page_sublink}...'))
    
    # Agree a change in route with the server, then scrape
    search_page_html <- if (is.null(html_ovrride)) {
      nod(session, page_sublink, verbose = T) %>% scrape()
    } else {
      read_html(html_ovrride)
    }
    
    
    # Will the `break` in the page population check properly figure out what while loop to break from?
    
    etc
    result_table[[i]] <- result$results
    page_sublink <- result$page_sublink
    
    iterator <- iterator + 1
    
  }
  
  log_success('[DM] Domain scrape complete')
  
  bind_rows(result_table) %>% 
    mutate(timestamp = now("Australia/Sydney"))
  
}
