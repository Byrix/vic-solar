energy_prod <- read_xlsx('data/Australian Energy Statistics 2022 - Production.xlsx', sheet=6, skip=4, n_max=20)
colnames(energy_prod) <- c('type', colnames(energy_prod)[2:14])

energy_prod <- energy_prod %>% filter(!is.na(energy_prod$type))

energy_prod <- energy_prod %>%
  pivot_longer(names_to='year', values_to='values', cols=2:14) %>%
  pivot_wider(names_from='type', values_from='values')

energy_prod$year <- as.ordered(energy_prod$year)
energy_prod[c(2:18)] <- sapply(energy_prod[c(2:18)], as.numeric)

energy_prod[is.na(energy_prod)] <- 0
energy_prod <- energy_prod %>% select(1, 3:7, 10:16,18)
colnames(energy_prod) <- c('year', 'coal_black', 'coal_brown', 'gas', 'oil', 'other', 'bagasse', 'biogas', 'wind', 'hydro', 'solar_large', 'solar_small', 'geothermal', 'total')

energy_prod <- energy_prod %>%
  mutate( coal = coal_black + coal_brown + other, 
          oil_gas = oil + gas,
          renewable = bagasse + biogas + wind + hydro + solar_large + geothermal + solar_small,
          .keep='unused')

temp <- energy_prod %>% filter(year=='2014-15')
goal <- cbind(temp[3:4]/2)
goal$renewable <- temp$total - (goal$coal + goal$oil_gas)
goal$total = temp$total
goal$year <- '2029-30'
goal <- energy_prod %>% filter(year=='2020-21') %>% rbind(goal)
goal <- goal %>% select(-total) %>% pivot_longer(cols=2:4, names_to='source', values_to='amount')

for (year in 2022:2029) {
  goal <- rbind(goal, data.frame(year=paste0(year-1,'-',year-2000), source=NA, amount=NaN))
}

energy_prod_plt <- energy_prod %>% select(-total) %>% pivot_longer(cols=2:4, names_to='source', values_to='amount')

energy_prod_plt$amount <- energy_prod_plt$amount * 3.6
goal$amount <- goal$amount * 3.6

save(energy_prod_plt, goal, file='energy_prod.RData')

