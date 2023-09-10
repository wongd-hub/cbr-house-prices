source('libs.R')

allhomes_results <- allhomes_scraper()

update_db(allhomes_results, 'allhomes')