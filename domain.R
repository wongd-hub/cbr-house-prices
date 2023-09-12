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
    
    # search_page_html %>% 
    #   html_nodes(base_query) %>% 
    #   map(~{
    #     
    #     search_result <- .x
    #     
    #     ## Deets
    #      names(sub_queries) %>% 
    #       map_dfc(~{
    #         
    #         search_result %>% 
    #           html_nodes(sub_queries[[.x]]) %>% 
    #           html_text() %>% 
    #           trimws() %>% 
    #           {if (.x == 'address') str_remove(., ',\\s$') else .} %>% 
    #           {if (.x == 'locality') str_to_title(.) else .}
    #         
    #       }) %>% 
    #       set_names(names(sub_queries)) %>% 
    #       # Manipulating the price string to pull relevant information
    #       mutate(
    #         price          = str_extract(price, '(?<=\\$)[0-9,]+') %>% 
    #           str_remove_all(',') %>% 
    #           as.numeric(),
    #         source         = 'domain'
    #       )
    #      
    #      ## 
    #   })
    
    
    ## Price/Location details ----
    log_info('[DM]     Extracting price/location details')
    
    # From the base query, we add these extra selectors on to pull out relevant
    # information
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
        " span[data-testid=address-line2] > span:nth-of-type(3)"
    )
    
    # Loop across details, extract, and store in table
    suppressMessages({
      house_details <- names(sub_queries) %>% 
        map_dfc(~{
          
          search_page_html %>% 
            html_nodes(paste(base_query, '> div:nth-of-type(2)', sub_queries[[.x]])) %>% 
            html_text() %>% 
            trimws() %>% 
            {if (.x == 'address') str_remove(., ',\\s$') else .} %>% 
            {if (.x == 'locality') str_to_title(.) else .}
          
        }) %>% 
        set_names(names(sub_queries)) %>% 
        # Manipulating the price string to pull relevant information
        mutate(
          price          = str_extract(price, '(?<=\\$)[0-9,]+') %>% 
            str_remove_all(',') %>% 
            as.numeric(),
          source         = 'domain'
        )
    })
    
    if (
      debug & (house_details %>% 
               filter(is.na(price)) %>% 
               nrow() > 0)
    ) browser()
    
    ## Beds/Baths/etc attributes ----
    log_info('[DM]     Extracting beds/baths/etc attributes & agent details')
    
    # Pulling out bed/bath/etc attributes
    house_attribute_nodes <- search_page_html %>% 
      # div id __domain_group/APP_ROOT
      html_nodes(base_query)
    
    #  Attributes
    house_attributes <- house_attribute_nodes %>% 
      map(~{
        
        ## Attributes ----
        # Auction/Private Treaty tag
        auction_or_negotiation <- html_nodes(.x, "div[data-testid=listing-card-tag]") %>% 
          html_text()
        
        # Type of abode
        abode_type <- .x %>% 
          html_nodes('
            div[data-testid=listing-card-features-wrapper] > 
            div:nth-of-type(2)
          ') %>% 
          html_text() # Can be Apartment / Unit / Flat - update allhomes handling 
        
        attributes_container <- .x %>% 
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
        
        # Pull first 'word' - the number (be general here since this can also
        # take the form of a dash)
        attribute_values <- attributes_container %>% 
          str_extract('^\\w+') %>% 
          {if (length(missing_idx) > 0) .[-missing_idx] else .} %>% 
          as.numeric() %>% 
          set_names(attribute_titles)

        ## Agency details ----
        agent_details <- html_nodes(.x, "div[data-testid=listing-card-branding] div > span") %>% 
          html_text()
        
        # Output in tibble to row-bind
        tibble(
          enframe(attribute_values) %>% 
            pivot_wider(names_from = name, values_from = value),
          auction         = str_detect(auction_or_negotiation, '(?i)auction'),
          by_negotiation  = str_detect(auction_or_negotiation, '(?i)private treaty|negotiation'),
          abode_type      = abode_type,
          sold_date       = str_extract(auction_or_negotiation, '\\d+ \\w+ \\d{4}') %>% 
            dmy() %>% 
            as_datetime('Australia/Sydney'),
          lead_agent_name = ifelse(length(agent_details) == 0, NA, agent_details[[1]]),
          agency_name     = ifelse(length(agent_details) == 0, NA, agent_details[[2]])
        )
        
      }) %>% 
      bind_rows() %>% 
      rename_all(.funs = tolower) %>% 
      rename(
        bedrooms  = bed,
        bathrooms = bath
      )
    
    
    ## Save ----
    log_info('[DM]     Performing checks and saving....')
    
    # Save to overall table as long as results are valid
    if (
      c(
        nrow(house_details), 
        nrow(house_attributes)
      ) %>% 
      unique() %>% 
      length() == 1
    ) {
      result_table[[iterator]] <- bind_cols(house_details, house_attributes) %>% 
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
    } else {
      stop(glue('[DM] Not all details/attributes/agent subtables have equal length for {page_sublink}'))
    }
    
    
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
