library(tidyverse)
library(openxlsx)

readxl::read_xlsx('datos/pronosticos_banxico.xlsx', 
                  sheet = 'prons Banxico', range = paste0('A', 2, ':D',10))


prons_banxico <- list()

for(i in 0:44) {

  prons_banxico[[i+1]] <- readxl::read_xlsx('datos/pronosticos_banxico.xlsx', 
                                            sheet = 'prons Banxico', 
                                            range = paste0('A', 2 + i*12, ':D',10 + i*12)) %>% 
    mutate(intercept = 1, pib = log(pib), Q1 = 1, Q2 = 0, Q3 = 0, Q4 = 0) %>% 
    dplyr::select(intercept, pib, inpc, Q1, Q2, Q3, Q4)
}

prons_banxico %>% write_rds('cache/variables/prons_banxico.rds')

