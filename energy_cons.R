energy_cons <- read_xlsx('data/Australian Energy Statistics 2022 - Consumption.xlsx', sheet=4, skip=4)
colnames(energy_cons) <- c("year", 'agriculutre', 'mining', 'manufacturing', 'electricity_generation', 'construction', 'transport', 'water', 'commerical', 'residential', 'other', 'total')
energy_cons <- energy_cons %>%
  filter(str_detect(energy_cons$year, pattern=regex('^20(08|09)|20(1|2)[0-9]{1}')))

energy_cons$year <- as.ordered(energy_cons$year)
energy_cons$total <- as.numeric(energy_cons$total)
energy_cons[c(2:11)] <- sapply(energy_cons[c(2:11)], function(c,total) { as.numeric(c)/total }, energy_cons$total)
energy_cons <- energy_cons %>% select(-c(total,other))
energy_cons[is.na(energy_cons)] <- 0

energy_cons <- energy_cons %>%
  mutate( services = electricity_generation + water,
          industry = agriculutre + mining + manufacturing + construction, 
          .keep='unused')

energy_con_plt <- energy_cons %>% pivot_longer(cols=2:length(colnames(energy_cons)), names_to='source', values_to='amount')

save(energy_con_plt, file='energy_cons.RData')

