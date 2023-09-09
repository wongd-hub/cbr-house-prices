#' Scrape AllHomes
#'
#' @param baseurl Base URL of site
#' @param start_page Which page to start from
#' @param n_pages Number of pages to scrape
#' @param hardstop Max number of pages to scrape, handy if setting `n_pages` to
#'   `Inf` but still not wanting function to go forever
allhomes_scraper <- function(
  baseurl = "https://www.allhomes.com.au", 
  start_page = NULL, 
  n_pages = Inf, 
  hardstop = 50
) {
  
  # Setup ----
  log_info('[AH] Beginning AllHomes scrape')
  
  # Politely introduce bot to AllHomes website
  allhomes_session <- bow(baseurl, force = T)
  
  # Iterators
  start_page <- coalesce(start_page, 1)
  page_sublink <- glue('/sold/search?page={start_page}&region=canberra-act')
  iterator <- 1
  result_table <- list()
  
  # Loop across pages until there are no more
  while ((!is.na(page_sublink) | hardstop > 50) & iterator <= n_pages) {
    
    if (iterator %% 5 == 0) {
      log_info(glue('[AH]   5 iterations since last sleep, sleeping for 30...'))
      Sys.sleep(30)
    }
    
    log_info(glue('[AH]   Iteration {iterator}, scraping {baseurl}/{page_sublink}...'))
    
    # Agree a change in route with the server, then scrape
    search_page_html <- nod(allhomes_session, page_sublink, verbose = T) %>% 
      scrape()
    
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
      div:nth-of-type(2) > 
      div:nth-of-type(1) > 
      div:nth-of-type(1) 
    ")
    
    ## Price/Location details ----
    log_info('[AH]     Extracting price/location details')
    
    # From the base query, we add these extra selectors on to pull out relevant
    # information
    sub_queries <- list(
      price         = 
        "> div:nth-of-type(1)",
      address       = 
        "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > div:nth-of-type(1) > span[itemprop=streetAddress]",
      locality      = 
        "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=addressLocality]",
      state         = 
        "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=addressRegion]",
      postcode      = 
        "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=postalCode]"
    )
    
    # Loop across details, extract, and store in table
    suppressMessages({
      house_details <- names(sub_queries) %>% 
        map_dfc(~{
          
          search_page_html %>% 
            html_nodes(paste(base_query, sub_queries[[.x]])) %>% 
            html_text()
          
        }) %>% 
        set_names(names(sub_queries)) %>% 
        # Manipulating the price string to pull relevant information
        mutate(
          auction = str_detect(price, '(?i)auction'),
          price   = str_extract(price, '(?<=\\$)[0-9,]+') %>% 
            str_remove_all(',') %>% 
            as.numeric(),
          source = 'allhomes'
        )
    })
    
    ## Beds/Baths/etc attributes ----
    log_info('[AH]     Extracting beds/baths/etc attributes')
    
    # Pulling out bed/bath/etc attributes
    house_attribute_nodes <- search_page_html %>% 
      # div id __domain_group/APP_ROOT
      html_nodes(paste(base_query, "> div:nth-of-type(3)"))
    
    #  Attributes
    house_attributes <- house_attribute_nodes %>% 
      map(~{
        
        attribute_titles <- html_nodes(.x, 'svg title') %>% 
          html_text()
        
        attribute_values <- html_nodes(.x, 'span > span:nth-of-type(2)') %>% 
          html_text() %>% 
          set_names(attribute_titles)
        
        c(
          attribute_values,
          abode_type = html_nodes(.x, 'div > span:nth-of-type(1)') %>% html_text()
        )
        
      }) %>% 
      bind_rows() %>% 
      rename_all(.funs = tolower) %>% 
      mutate(across(c(bathrooms, eer, bedrooms, parking), as.numeric))
    
    ## Agent details ----
    log_info('[AH]     Extracting agent details')
    
    house_agent_detail_nodes <- search_page_html %>% 
      # div id __domain_group/APP_ROOT
      html_nodes(glue("
        div[id='__domain_group/APP_ROOT'] 
        div.{house_card_class} > 
        div:nth-of-type(1) > 
        div:nth-of-type(2) > 
        div:nth-of-type(2) >
        div:nth-of-type(2)
      "))
    
    house_agent_details <- house_agent_detail_nodes %>% 
      map_dfr(~{
        tibble(
          lead_agent_name = .x %>% html_nodes('div[data-test-id=agency-details-name]') %>% html_text(),
          agency_name     = .x %>% html_nodes('div[data-test-id=agency-details-type] span#agencyName') %>% html_text()
        )
      })
    
    ## Save ----
    log_info('[AH]     Performing checks and saving....')
    
    # Save to overall table as long as results are valid
    if (
      c(
        nrow(house_details), 
        nrow(house_attributes), 
        nrow(house_agent_details)
      ) %>% 
      unique() %>% 
      length() == 1
    ) {
      result_table[[iterator]] <- bind_cols(house_details, house_attributes, house_agent_details) %>% 
        mutate(
          hash_id = paste(address, locality, state, postcode) %>% 
            str_remove_all(' ') %>% 
            toupper() %>% 
            md5() %>% 
            as.character()
        ) %>% 
        select(
          hash_id, price, abode_type, address, locality, state, postcode, bathrooms, 
          bedrooms, parking, eer, lead_agent_name, agency_name, auction, source
        )
    } else {
      stop(glue('[AH] Not all details/attributes/agent subtables have equal length for {page_sublink}'))
    }
  
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
    
    iterator <- iterator + 1
    
  }
  
  log_success('[AH] AllHomes scrape complete')
  
  bind_rows(result_table) %>% 
    mutate(timestamp = now("Australia/Sydney"))
  
}
