# for questions about this app, please contact
# Murphy Zhang: yz4732@columbia.edu

# the necessary packages
library(leaflet)
library(RPostgres)
library(scales)
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# connection to the wc_24b_04 database----
# replace with your server credentials
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'italydb',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25060,
  user = 'italy', 
  password = 'xxxx_xxxxxx-xxxx-xxxxxxx'
)

# champ vector
champ = c(1934,1938,1982,2006)

# city vector
city = c('Miami', 'Atlanta', 'Houston')

# mth vector
mth = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# team tibble----
team = tibble(
  first = c(
    'Murphy',
    'Shuqing',
    'Shuyi',
    'Guangyi',
    'Haohang',
    'Yue',
    'Yueyang'
  ),
  last = c(
    'Zhang',
    'Yang',
    'Zuo',
    'Gao',
    'Li',
    'Pan',
    'Xu'
  )
)

linkedin = c(
  'https://www.linkedin.com/in/yumeng-zhang-855901203/',
  'https://www.linkedin.com/in/shuqing-yang/',
  'https://www.linkedin.com/in/shuyi-zuo-a530b8237/'
)

# text shortener function----
txtShort = function(x, n) {
  t = x
  if (str_length(x) > n) {
    t = paste0(str_sub(x,1,n-3), '...')
  }
  return(t)
}