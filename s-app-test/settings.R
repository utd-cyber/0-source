install.packages('rsconnect')
install.packages("shiny")
library(pacman)
p_load(rsconnect)

# https://82az14-utd0cyber.shinyapps.io/app-test/
rsconnect::setAccountInfo(name='82az14-utd0cyber',
                          token='535CB02A3523B56DA8E1277DEAEA1DE8',
                          secret='pdKcMEWZYCIixHFY5D/6RyzlC8NkzymqHhRpzn25')

rsconnect::deployApp('app-test')

#
p_load(shiny, tidytext, tm)
