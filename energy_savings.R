energy_cons <- read_xlsx('data/Australian Energy Statistics 2022 - Consumption.xlsx', sheet=4, skip=4)
colnames(energy_cons) <- c("year", 'agriculutre', 'mining', 'manufacturing', 'electricity_generation', 'construction', 'transport', 'water', 'commerical', 'residential', 'other', 'total')
energy_cons <- energy_cons %>%
  filter(str_detect(energy_cons$year, pattern=regex('^2019')))

energy_cons$year <- as.ordered(energy_cons$year)
for (i in 2:11) { energy_cons[i] <- as.numeric(energy_cons[i]) }

he <- energy_cons %>% filter(year=='2019-20') %>% select('residential')
he <- deframe(he)

solar_emissions <- data.frame(solar=1,
                              energy_pj=0)

for (i in seq(0,0.9,0.1)) {
  df <- data.frame(solar=i, energy_pj=(1-i)*he)
  solar_emissions <- rbind(solar_emissions, df)
}

solar_emissions <- solar_emissions %>% mutate(energy_kwh=energy_pj*277800000)
solar_emissions$emissions <- c(0, 51476.434, 46328.791, 41181.148, 36033.504, 30855.861, 25738.217, 20590.574, 15422.930, 10295.287, 5147.643)

save(solar_emissions, file='data/solar_emissions.RData')
