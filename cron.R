source('libs.R')

if (F) {
  
  cron_add(
    '~/Documents/Projects/cbr-houses/orchestration.R',
    frequency = 'monthly',
    at = '7:23AM'
  )
  
}
