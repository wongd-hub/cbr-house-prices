test_that(
  'AllHomes scraper works',
  {
    
    static_html <- 'testthat/allhomes_scraper/allhomes_search_results.html'
    test_scrape <- allhomes_scraper(html_ovrride = static_html)
    
    # Count number of NA prices
    expect_equal(
      test_scrape %>% filter(is.na(price)) %>% nrow(),
      25
    )
    
    # Count number of auctions
    expect_equal(
      test_scrape %>% filter(auction) %>% nrow(),
      11
    )
    
    # Count number of by negotiations
    expect_equal(
      test_scrape %>% filter(by_negotiation) %>% nrow(),
      25
    )
    
    # Count total results
    expect_equal(
      test_scrape %>% nrow(),
      50
    )
    
  }
)
