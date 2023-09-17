# TODO: Put all the scraping logic into the one map to avoid issues with missing data

#' Scrape AllHomes
#'
#' @param baseurl Base URL of site
#' @param start_page Which page to start from
#' @param n_pages Number of pages to scrape
#' @param forced_delay Whether to force sleep for 30 seconds every 5 iterations
#' @param debug Whether to run browser if potentially problematic rows exist
allhomes_scraper <- function(
  baseurl      = "https://www.allhomes.com.au", 
  start_page   = NULL, 
  n_pages      = Inf, 
  forced_delay = FALSE,
  
  # Dev/debugging
  debug        = FALSE,
  html_ovrride = NULL
) {
  
  # Setup ----
  log_info('[AH] Beginning AllHomes scrape')
  
  # For testing purposes, set up a few variables
  if (!is.null(html_ovrride)) n_pages <- 1
  
  # Politely introduce bot to AllHomes website
  allhomes_session <- if (is.null(html_ovrride)) bow(baseurl, force = T) else NA
  
  # Iterators
  start_page <- coalesce(start_page, 1)
  page_sublink <- glue('/sold/search?page={start_page}&region=canberra-act')
  iterator <- 1
  result_table <- list()
  
  # Loop across pages until there are no more
  while (!is.na(page_sublink) & iterator <= n_pages) {
    
    if (forced_delay & iterator %% 5 == 0) {
      log_info(glue('[AH]   5 iterations since last sleep, sleeping for 30...'))
      Sys.sleep(30)
    }
    
    log_info(glue('[AH]   Iteration {iterator}, scraping {baseurl}/{page_sublink}...'))
    
    # Agree a change in route with the server, then scrape
    search_page_html <- if (is.null(html_ovrride)) {
      nod(allhomes_session, page_sublink, verbose = T) %>% 
        scrape()
    } else {
      read_html(html_ovrride)
    }
    
    # TODO: Start sub-function from here and pass in search_page_html, debug

    # Check if page is populated ----
    
    # If this image appears on screen, there are no results at this page. End
    # the while loop.
    no_results_img <- search_page_html %>% 
      html_nodes('img[alt="No properties match your search right now"]')
    
    if (length(no_results_img) > 0) {
      log_info('[AH]     No results found on page - ending scrape')
      break
    }
    
    
    # Information extraction ----
    # Pull nodes relevant to search results then start extracting
    search_results <- search_page_html %>% 
      # div id __domain_group/APP_ROOT
      html_nodes("div[id='__domain_group/APP_ROOT']") %>% 
      # grabbing nodes by position
      html_nodes("
        section:nth-of-type(2) > 
        div:nth-of-type(2) > 
        section:nth-of-type(1) > 
        section:nth-of-type(1) > 
        div:nth-of-type(1) > 
        div:nth-of-type(1) > 
        div:nth-of-type(1) >
        div
      ")
    
    # There are popups interspersed between house search results, we can target
    # the house results only by looking for the most frequently occurring div
    # class inside the results container. We then use this to pull out only the
    # house results.
    div_classes <- search_results %>% 
      html_attr('class') %>% 
      table()
    
    house_card_class <- names(div_classes)[which.max(div_classes)]
    
    # Our base query gets us our house results and navigates internally to where
    # the information is. This navigation was built by inspecting the structure
    # of each card in Chrome DevTools
    base_query <- glue("
      div[id='__domain_group/APP_ROOT'] 
      div.{house_card_class} > 
      div:nth-of-type(1) > 
      div:nth-of-type(2) > 
      div:nth-of-type(2)
    ")
    
    relevant_html <- search_page_html %>%
      html_nodes(base_query)
    
    ## Price/Location details ----
    log_info('[AH]     Extracting price/location details')
    
    # From the base query, we add these extra selectors on to pull out relevant
    # information
    sub_queries <- list(
      price         = 
        "div > div > div:nth-of-type(1)",
      address       = 
        "span[itemprop=streetAddress]",
      locality      = 
        "span[itemprop=addressLocality]",
      state         = 
        "span[itemprop=addressRegion]",
      postcode      = 
        "span[itemprop=postalCode]"
    )
    
    log_info(glue('[AH]     {length(relevant_html)} entries found'))
    
    
    # Do all scraping within one map to handle if certain search result cards
    # are missing whole sets of attributes
    log_info('[AH]     Extracting property details')
    
    house_details <- relevant_html %>% 
      map(~{
        
        search_result <- .x
        
        ## Easily queryable details ----
        base_details <- names(sub_queries) %>%
          map(~{
            
            search_result %>%
              html_nodes(sub_queries[[.x]]) %>%
              {if (.x == 'price') .[[1]] else .} %>% 
              html_text() %>%
              trimws()
            
          }) %>%
          set_names(names(sub_queries)) %>%
          unlist() %>% enframe() %>% 
          pivot_wider(names_from = name, values_from = value) %>% 
          # Manipulating the price string to pull relevant information
          mutate(
            auction        = if ("price" %in% names(.)) str_detect(price, '(?i)auction'),
            by_negotiation = if ("price" %in% names(.)) str_detect(price, '(?i)by negotiation'),
            price          = if ("price" %in% names(.)) str_extract(price, '(?<=\\$)[0-9,]+') %>% 
              str_remove_all(',') %>% 
              as.numeric(),
            source         = 'allhomes'
          )
        
        ## Other attributes ----
        house_attr_node <- html_nodes(.x, "div > div:nth-of-type(3)") 
        
        attribute_titles <- html_nodes(house_attr_node, 'svg title') %>% 
          html_text()
        
        attribute_values <- html_nodes(house_attr_node, 'span > span:nth-of-type(2)') %>% 
          html_text() %>% 
          set_names(tolower(attribute_titles))
        
        
        ## Final table ----
        attribute_details <- tibble(
          enframe(attribute_values) %>% 
            pivot_wider(names_from = name, values_from = value),
          abode_type      = html_nodes(house_attr_node, 'div > span:nth-of-type(1)') %>% 
            html_text(),
          lead_agent_name = html_nodes(search_result, 'div[data-test-id=agency-details-name]') %>% 
            html_text(),
          agency_name     = html_nodes(search_result, 'div[data-test-id=agency-details-type] span#agencyName') %>% 
            html_text()
        )
        
        
        bind_cols(
          base_details,
          attribute_details
        )
        
      }) %>% 
      bind_rows()

    if (
      debug & (house_details %>% 
        filter(is.na(price) & !by_negotiation & !auction) %>% 
        nrow() > 0)
    ) browser()
    
    
    ## Save ----
    log_info('[AH]     Performing checks and saving....')
    
    # Save to overall table as long as results are valid
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
    log_info('[AH]     Getting next route')
    
    pagination_buttons <- search_page_html %>% html_nodes('a[data-testid=paginator-navigation-button]')
    
    page_sublink <- if (length(pagination_buttons) == 1 & start_page == 1 & iterator == 1) {
      
      # This is the first page, go to the next page
      pagination_buttons[[1]] %>% html_attr('href')
      
    } else if (length(pagination_buttons) == 2) {
      
      # A normal page; the second button will be the next page
      pagination_buttons[[2]] %>% html_attr('href')
      
    } else {
      
      log_info('[AH]       No next page, ending scrape')
      NA
      
    }
    
    # TODO: Return iteration table and next sublink
    
    iterator <- iterator + 1
    
  }
  
  log_success('[AH] AllHomes scrape complete')
  
  bind_rows(result_table) %>% 
    mutate(timestamp = now("Australia/Sydney"))
  
}
