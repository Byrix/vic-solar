energy_prod <- read_xlsx('data/Australian Energy Statistics 2022 - Production.xlsx', sheet=4, skip=4)
colnames(energy_prod) <- c("year", 'coal_black', 'coal_brown', 'oil', 'gas_lpg', 'gas_nat', 'gas_eth', 'biofuel', 'wood', 'bagasse', 'hydro', 'wind', 'solar', 'solar_water')
energy_prod <- energy_prod %>%
  filter(str_detect(energy_prod$year, pattern=regex('^20(08|09)|20(1|2)[0-9]{1}')))

energy_prod$year <- as.ordered(energy_prod$year)
energy_prod[c(2:14)] <- sapply(energy_prod[c(2:14)], as.numeric)

energy_prod[is.na(energy_prod)] <- 0

energy_prod <- energy_prod %>% 
  mutate( coal = coal_black + coal_brown,
          gas_oil = gas_lpg + gas_nat + gas_eth + oil,
          renewable = biofuel + bagasse + hydro + wind + solar_water + wood + solar,
          .keep='unused' ) %>%
  mutate( total = coal + gas_oil + renewable ) %>%
  mutate( coal = coal/total, 
          gas_oil = gas_oil/total,
          renewable = renewable/total)
energy_plt <- energy_prod %>% select(-total) %>% pivot_longer(cols=2:4, names_to='source', values_to='amount')

temp <- energy_prod %>% filter(year=='2020-21')
goal <- cbind(temp[2:3]/2)
goal$renewable <- 1 - (goal$coal + goal$gas_oil)
goal$year <- '2029-30'
goal <- rbind(goal, select(temp, -total))
goal <- goal %>% pivot_longer(cols=1:3, names_to='source', values_to='amount')

for (year in 2022:2029) {
  goal <- rbind(goal, data.frame(year=paste0(year-1,'-',year-2000), source=NA, amount=NaN))
}

save(energy_plt, goal, file='energy_prod.RData')

