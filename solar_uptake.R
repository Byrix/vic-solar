# SOLAR PANEL STUFF
df <- read_csv('data/uptake/Postcode data for small-scale installations 2009 - SGU-Solar.csv') %>%
  select(c(1, 2, seq(4, 14, 2), seq(16, 38, 2)))
colnames(df) <- c('postcode', 'prev', 'p-jan', 'p-feb', 'p-mar', 'p-apr', 'p-may', 'p-jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr', 'may', 'jun')

df <- df %>%
  filter(str_detect(postcode, pattern=regex('^3'))) %>%
  mutate(inst = jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,
         prev = prev + `p-jan` + `p-feb` + `p-mar` + `p-apr` + `p-may` + `p-jun`,
         .keep='unused')

uptake <- data.frame(year='2008-09', installed=sum(df$inst), total=sum(df$prev)+sum(df$inst))

for (file in list.files('data/uptake', pattern=regex('20(1|2)'))) { 
  year <- as.numeric(str_extract(file, pattern='[:digit:]{4}'))
  
  df <- read_csv(paste0('data/uptake/',file)) %>%
    select(c(1, seq(16, 38, 2)))
  colnames(df) <- c('postcode', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr', 'may', 'jun')
  
  df <- df %>%
    filter(str_detect(postcode, pattern=regex('^3'))) %>%
    mutate(inst = jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, .keep='unused')
  
  new_entry <- data.frame(year=paste0(year-1,'-',year-2000), 
                          installed=sum(df$inst), 
                          total=sum(df$inst)+tail(uptake$total,1))
  uptake <- rbind(uptake, new_entry)
}


# HOUSING STUFF
df <- read_xlsx('data/housing/12. Housing occupancy costs, states and territories.xlsx', sheet=7, skip=4, n_max=61-4) %>% 
  select(1, 13:18)
colnames(df) <- c('type', '2009-10', '2011-12', '2013-14', '2015-16', '2017-18', '2019-20')
df <- df %>%
  filter(type %in% c('Separate house', 'Semi-detached, row or terrace house, townhouse', 'Estimated number of households')) %>%
  pivot_longer(cols=2:7, names_to='year', values_to='values') %>%
  pivot_wider(names_from='type', values_from='values')

colnames(df) <- c('year', 'separate', 'townhouse', 'num_houses')
housing <- df %>%
  mutate( separate = separate/100, 
          townhouse = townhouse/100, 
          num_houses = num_houses*1000 ) %>%
  mutate( separate = num_houses * separate, 
          townhouse = num_houses * townhouse ) %>%
  mutate( dwellings = separate + townhouse ) %>%
  select(1, 5)

# PUTTING THAT STUFF TOGETHER
uptake <- left_join(uptake, housing, by='year') %>% 
  mutate( per_houses = total/dwellings,
          growth = installed/total)

save(uptake, file="uptake.RData")

