allhomes_scraper <- function(baseurl = "https://www.allhomes.com.au/sold/search?region=canberra-act") {
  
  log_info('Beginning AllHomes scrape')
  log_info(glue('  Scraping {baseurl}'))
  
  search_page_html <- bow(baseurl, force = TRUE) %>% 
    scrape()
  
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
  
  div_classes <- search_results %>% 
    html_attr('class') %>% 
    table()
  
  house_card_class <- names(div_classes)[which.max(div_classes)]
  
  base_query <- glue("
    div[id='__domain_group/APP_ROOT'] 
    div.{house_card_class} > 
    div:nth-of-type(1) > 
    div:nth-of-type(2) > 
    div:nth-of-type(2) > 
    div:nth-of-type(1) > 
    div:nth-of-type(1) 
  ")
  
  sub_queries <- list(
    price         = "> div:nth-of-type(1)",
    address       = "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > div:nth-of-type(1) > span[itemprop=streetAddress]",
    locality      = "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=addressLocality]",
    state         = "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=addressRegion]",
    postcode      = "> div:nth-of-type(2) > a:nth-of-type(1) > div:nth-of-type(1) > h2:nth-of-type(1) > span:nth-of-type(1) > span[itemprop=postalCode]"
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
      mutate(
        auction = str_detect(price, '(?i)auction'),
        price   = str_extract(price, '(?<=\\$)[0-9,]+') %>% 
          str_remove_all(',') %>% 
          as.numeric(),
        source = 'allhomes'
      )
  })

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
        house_type = html_nodes(.x, 'div > span:nth-of-type(1)') %>% html_text()
      )
      
      
    }) %>% 
    bind_rows() %>% 
    rename_all(.funs = tolower)
  
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
  
  if (
    c(
      nrow(house_details), 
      nrow(house_attributes), 
      nrow(house_agent_details)
    ) %>% 
    unique() %>% 
    length() == 1
  ) {
    results_table <- bind_cols(house_details, house_attributes, house_agent_details)
  } else {
    stop('Not all details/attributes/agent subtables have equal length')
  }
  
  results_table
  
  
  
  
}


  
