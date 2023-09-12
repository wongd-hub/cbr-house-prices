# TODO: Can we generalise the allhomes scraper? Pass in:
#  - Base URL, subpage URL
#  - XML base/sub-queries
# Might be difficult. AllHomes had special stuff that needed separate scraping.
# Make sub-functions that you can switch out to handle this?

# TODO: Still missing one record on page 3

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
    
    
    # Check if page is populated ----
    
    # If this image appears on screen, there are no results at this page. End
    # the while loop.
    no_results_img <- search_page_html %>% 
      {if (is.null(.)) NA else html_nodes(., 'h1[data-testid=error-page__message-header]')}
    
    if (length(no_results_img) > 0 | is.null(search_page_html)) {
      # TODO: We don't want a warning to break the scrape; get next route and
      # move onwards
      log_info('[DM]     No results found on page - ending scrape')
      break
    }
    
    # Information extraction ----

    # Our base query gets us our house results and navigates internally to where
    # the information is. This navigation was built by inspecting the structure
    # of each card in Chrome DevTools
    base_query <- "div[data-testid^=listing-card-wrapper]"
    
    relevant_html <- search_page_html %>%
      html_nodes(base_query)
    
    # From the base query, we add these extra selectors on to pull out
    # easy-to-access relevant information
    sub_queries <- list(
      price         = 
        " p[data-testid=listing-card-price]",
      address       = 
        " span[data-testid=address-line1]",
      locality      = 
        " span[data-testid=address-line2] > span:nth-of-type(1)",
      state         = 
        " span[data-testid=address-line2] > span:nth-of-type(2)",
      postcode      = 
        " span[data-testid=address-line2] > span:nth-of-type(3)",
      abode_type    =
        "div[data-testid=listing-card-features-wrapper] > div:nth-of-type(2)"
    )
    
    log_info(glue('[DM]     {length(relevant_html)} entries found'))
    
    # Do all scraping within one map to handle if certain search result cards
    # are missing whole sets of attributes
    log_info('[DM]     Extracting property details')
    
    house_details <- relevant_html %>%
      map(~{

        search_result <- .x

        ## Easily queryable details ----
        base_details <- names(sub_queries) %>%
          map(~{
            
            search_result %>%
              html_nodes(sub_queries[[.x]]) %>%
              html_text() %>%
              trimws() %>%
              {if (.x == 'address') str_remove(., ',\\s$') else .} %>%
              {if (.x == 'locality') str_to_title(.) else .}
            
          }) %>%
          set_names(names(sub_queries)) %>%
          unlist() %>% enframe() %>% 
          pivot_wider(names_from = name, values_from = value) %>% 
          # Manipulating the price string to pull relevant information
          mutate(
            price    = if ("price"    %in% names(.)) str_extract(price, '(?<=\\$)[0-9,]+') %>%
              str_remove_all(',') %>%
              as.numeric() else NA_real_,
            address  = if ("address"  %in% names(.)) str_remove(address, ',\\s$') else NA_character_,
            locality = if ("locality" %in% names(.)) str_to_title(locality) else NA_character_,
            source   = 'domain'
          )

        ## Other attributes ----
        # Auction/Private Treaty tag
        auction_or_negotiation <- html_nodes(search_result, "div[data-testid=listing-card-tag]") %>% 
          html_text()
        
        attributes_container <- search_result %>% 
          html_nodes('
            div[data-testid=listing-card-features-wrapper] 
            span[data-testid="property-features-text-container"]
          ') %>% 
          html_text()
        
        # Pull last word - the attribute name
        attribute_titles <- attributes_container %>% 
          str_extract('\\w+$') %>% 
          str_remove('s$')
        
        missing_idx <- which(is.na(attribute_titles))
        
        # If there is a missing attribute title, ignore that attribute; this
        # is generally the land area which isn't consistently present
        if (length(missing_idx) > 0) attribute_titles <- attribute_titles[-missing_idx]
        
        # Placeholder in case no attributes exist
        if (length(attribute_titles) == 0) 
          attributes_container <- c(Bed = NA_real_, Bath = NA_real_, Parking = NA_real_)
        
        # Pull first 'word' - the number (be general here since this can also
        # take the form of a dash)
        attribute_values <- attributes_container %>% 
          str_extract('^\\w+') %>% 
          {if (length(missing_idx) > 0) .[-missing_idx] else .} %>% 
          as.numeric() %>% 
          {if (length(attribute_titles) == 0) . else set_names(., attribute_titles)}
          
        
        ## Agency details ----
        agent_details <- html_nodes(search_result, "div[data-testid=listing-card-branding] div > span") %>% 
          html_text()
        
        attribute_details <- tibble(
          enframe(attribute_values) %>% 
            pivot_wider(names_from = name, values_from = value),
          auction         = str_detect(auction_or_negotiation, '(?i)auction'),
          by_negotiation  = str_detect(auction_or_negotiation, '(?i)private treaty|negotiation'),
          sold_date       = str_extract(auction_or_negotiation, '\\d+ \\w+ \\d{4}') %>% 
            dmy() %>% 
            as_datetime('Australia/Sydney'),
          lead_agent_name = ifelse(length(agent_details) == 0, NA_character_, agent_details[[1]]),
          agency_name     = ifelse(length(agent_details) == 0, NA_character_, agent_details[[2]])
        )
        
        # Output in tibble to row-bind
        bind_cols(
          base_details,
          attribute_details
        )
        
      }) %>% 
      bind_rows() %>% 
      rename_all(.funs = tolower) %>% 
      rename(
        bedrooms  = bed,
        bathrooms = bath
      )
    
    if (
      debug & (house_details %>% filter(is.na(price)) %>% nrow() > 0)
    ) browser()
    

    ## Save ----
    log_info('[DM]     Performing checks and saving....')
    
    # Save to overall table
    result_table[[iterator]] <- house_details %>% 
      mutate(
        hash_id = paste(address, locality, state, postcode) %>% 
          str_remove_all(' ') %>% 
          toupper() %>% 
          md5() %>% 
          as.character()
      ) %>% 
      select(
        any_of(c(
          'hash_id', 'price', 'sold_date', 'abode_type', 'address', 'locality', 'state', 'postcode', 'bathrooms', 
          'bedrooms', 'parking', 'eer', 'lead_agent_name', 'agency_name', 'auction', 'by_negotiation', 'source'
        ))
      )
    
    
    # Get next route ----
    log_info('[DM]     Getting next route')
    
    pagination_buttons <- search_page_html %>% html_nodes('a[data-testid=paginator-navigation-button]')
    
    page_sublink <- if (length(pagination_buttons) == 1 & start_page == 1 & iterator == 1) {
      
      # This is the first page, go to the next page
      pagination_buttons[[1]] %>% html_attr('href')
      
    } else if (length(pagination_buttons) == 2) {
      
      # A normal page; the second button will be the next page
      pagination_buttons[[2]] %>% html_attr('href')
      
    } else {
      
      log_info('[DM]       No next page, ending scrape')
      NA
      
    }
    
    iterator <- iterator + 1
    
  }
  
  log_success('[DM] Domain scrape complete')
  
  bind_rows(result_table) %>% 
    mutate(timestamp = now("Australia/Sydney"))
  
}
