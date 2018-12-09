#!/usr/bin/env Rscript 

# Loads the raw data, cleaning and transforming it for analysis, then saves the
# clean data to ../data/clean/.

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# custom functions
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))


############
# CLEAN DATA
############


#====
# FDI
#====

fdi <- read.csv('./data/raw/FDI_FLOW_PARTNER_28102018214703504.csv',
                stringsAsFactors=FALSE)
colnames(fdi) <- tolower(colnames(fdi))

# we are only interested in outflow, from the U.S. to other countries
fdi <- fdi %>% 
    filter(reporting.country == 'United States',
           type.of.fdi == 'Outward',
           cur == 'USD')

aggregations <- c('ACP countries', 'AFRICA', 'African ACP countries', 'AMERICA', 'ASEAN countries', 'ASIA', 'BALTIC COUNTRIES', 'BLEU', 'Caribbean ACP countries', 'CENTRAL AMERICA', 'EU15', 'EU25', 'EUROPE', 'Other territories', 'GULF ARABIAN COUNTRIES', 'MERCOSUR', 'Mediterranean Basin countries', 'NAFTA', 'NEAR AND MIDDLE EAST', 'NICs1', 'NICs2A', 'NICs2LA', 'NORTH AFRICA', 'NORTH AMERICA', 'OCEANIA & POLAR REGIONS', 'OECD', 'OPEC countries', 'OTHER AFRICAN COUNTRIES (Excluding North Africa)', 'OTHER ASIAN COUNTRIES (Excluding Near and Middle East)', 'Other Central/Eastern Europe', 'NEAR AND MIDDLE EAST COUNTRIES OTHER THAN GULF STATES', 'Pacific ACP countries', 'SOUTH AMERICA', 'TOTAL WORLD', 'EUROPE Unallocated', 'EUROPE (Excluding OECD countries)', 'AFRICA Unallocated', 'North Africa - Unallocated', 'Africa excluding North African countries- Unallocated', 'AMERICA Unallocated', 'Central America - Unallocated', 'CENTRAL AMERICA (Excluding OECD countries)', 'South America - Unallocated', 'ASIA Unallocated', 'ASIA (Excluding OECD countries)', 'Near and Middle East - Unallocated', 'Gulf Arabian countries - Unallocated', 'Near and Middle East countries excluding Gulf states - Unallocated', 'Other Asian countries - Unallocated', 'OTHER ASIAN COUNTRIES (Excluding Near and Middle East and OECD countries)', 'Oceania & Polar Regions - Unallocated', 'OCEANIA & POLAR REGIONS (Excluding OECD countries)', 'TOTAL WORLD Unallocated', 'TOTAL WORLD (Excluding OECD countries)', 'OECD - Unallocated', 'NORTH AMERICA (Excluding OECD countries)', 'AMERICA (Excluding OECD countries)', 'Economic zones', 'EU27', 'SOUTH AMERICA (Excluding OECD countries)', 'NEAR AND MIDDLE EAST (Excluding OECD countries)', 'CIS countries', 'EFTA')

fdi <- fdi %>% filter(partner.country %not in% aggregations)

us_fdi <- fdi %>%
    select(year, partner.country, value) %>%
    mutate(value = value * 1000000) %>%
    arrange(year, partner.country)
colnames(us_fdi) <- c('year', 'country', 'fdi')
rm(fdi)

us_fdi$country <- recode(us_fdi$country, 'NewZealand' = 'New Zealand',
                                         'SriLanka' = 'Sri Lanka',
                                         'Cambodia (Kampuchea)' = 'Cambodia',
                                         'Timor_Leste' = 'Timor-Leste',
                                         'Holy See (Vatican City State)' = 'Holy See',
                                         'Papua New Guine' = 'Papua New Guinea',
                                         'Viet Nam' = 'Vietnam')


write.table(us_fdi, './data/clean/fdi.tsv', row.names=FALSE, sep='\t')



#===============
# Arms transfers
#===============

# not really an Excel file!
arms_raw <- read.csv('./data/raw/TIV-Export-USA-2003-2013.csv.xls', skip=10, 
                     header=TRUE)
arms_raw$Total <- NULL
colnames(arms_raw)[1] <- 'country'

arms_exports <- arms_raw %>%
    gather(year, arms_exports, X2003:X2013, na.rm=TRUE) %>%
    mutate(year=as.integer(str_remove(year, 'X'))) %>%
    arrange(country, year)
colnames(arms_exports)[3] <- 'arms_exports'
rm(arms_raw)

library(plyr)
arms_exports$country <- recode(arms_exports$country, 
                                  'DR Congo' = 'Congo, the Democratic Republic of the', 
                                  'Macedonia (FYROM)' = 'Macedonia, the Former Yugoslav Republic of',
                                  'Serbia' = 'Serbia, Republic of',
                                  'South Korea' = 'Korea, Republic of (South Korea)',
                                  'Taiwan' = 'Taiwan, Province of China',
                                  'UAE' = 'United Arab Emirates',
                                  "Côte d'Ivoire" = "Cote d'Ivoire")

write.table(arms_exports, './data/clean/arms_exports.tsv', row.names=FALSE, sep='\t')



#===========
# Population
#===========

pop_raw <- read_xlsx('./data/raw/WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx',
                     skip=16, col_names=TRUE) %>%
    select(3, `2003`:`2013`)
colnames(pop_raw)[1] <- 'country'

pop_aggs <- c('WORLD', 'More developed regions', 'Less developed regions', 'Least developed countries', 'Less developed regions, excluding least developed countries', 'Less developed regions, excluding China', 'High-income countries', 'Middle-income countries', 'Upper-middle-income countries', 'Lower-middle-income countries', 'Low-income countries', 'Sub-Saharan Africa', 'AFRICA', 'Middle Africa', 'Eastern Africa', 'Western Sahara', 'Southern Africa', 'ASIA', 'Eastern Asia', 'Southern Asia', 'South-Eastern Asia', 'EUROPE', 'Eastern Europe', 'Northern Europe', 'Southern Europe', 'Western Europe', 'NORTHERN AMERICA', 'OCEANIA', 'Central Asia', 'LATIN AMERICA AND THE CARIBBEAN', 'South-Central Asia', 'Western Africa', 'Northern Africa')

pop <- pop_raw %>% 
    filter(country %not in% pop_aggs) %>%
    gather(year, population, `2003`:`2013`) %>%
    mutate(population = population * 1000,
           year = as.numeric(year))
rm(pop_raw)

pop$country <- recode(pop$country, 'United Republic of Tanzania' = 'Tanzania, United Republic of',
                                   'Democratic Republic of the Congo' = 'Congo, the Democratic Republic of the',
                                   'Saint Helena' = 'St. Helena',
                                   'China, Taiwan Province of China' = 'Taiwan, Province of China',
                                   "Dem. People's Republic of Korea" = "Korea, Dem. People's Republic of (North Korea)",
                                   'Republic of Korea' = 'Korea, Republic of (South Korea)',
                                   'Iran (Islamic Republic of)' = 'Iran, Islamic Republic of',
                                   'Republic of Moldova'  = 'Moldova, Republic of',
                                   'Serbia' = 'Serbia, Republic of',
                                   'TFYR Macedonia' = 'Macedonia, the Former Yugoslav Republic of',
                                   'Saint Kitts and Nevis' = 'St. Kitts and Nevis',
                                   'Saint Lucia' = 'St. Lucia',
                                   'Saint Vincent and the Grenadines' = 'St. Vincent and the Grenadines',
                                   'Bolivia (Plurinational State of)' = 'Bolivia',
                                   'Venezuela (Bolivarian Republic of)' = 'Venezuela',
                                   'United States of America' = 'United States',
                                   'Montenegro' = 'Montenegro, Republic of')

write.table(pop, './data/clean/population.tsv', row.names=FALSE, sep='\t')



#========
# Conflict
#========

### THIS NEEDS TO BE BROKEN UP

conflict <- read.csv('./data/raw/ucdp-prio-acd-181.csv', 
                     stringsAsFactors=FALSE) %>%
    select(conflict_id, location, year) %>%
    mutate(conflict = 1) %>%
    arrange(conflict_id, location, year)
colnames(conflict)[2] <- 'country'

conflict$country <- recode(conflict$country, 'Cambodia (Kampuchea)' = 'Cambodia (Kampuchea)',
                                             'Iran' = 'Iran, Islamic Republic of',
                                             'Laos' = "Lao People's Democratic Republic",
                                             'Russia (Soviet Union)' = 'Russian Federation',
                                             'Myanmar (Burma)' = 'Myanmar',
                                             'United States of America' = 'United States',
                                             'Moldova' = 'Moldova, Republic of',
                                             'Serbia (Yugoslavia)' = 'Serbia, Republic of',
                                             'Moldova' = 'Moldova, Republic of',
                                             'Macedonia, FYR' = 'Macedonia, the Former Yugoslav Republic of',
                                             'Montenegro' = 'Montenegro, Republic of')

write.table(conflict, './data/clean/conflict.tsv', row.names=FALSE, sep='\t')



#============
# Regime type
#============

regime <- read_xls('./data/raw/p4v2017.xls') %>%
    filter(year >= 2003,
           year <= 2013) %>%
    select(country, year, polity2) %>%
    arrange(country, year)

regime$country <- recode(regime$country, 'Iran' = 'Iran, Islamic Republic of',
                         'Korea North' = "Korea, Dem. People's Republic of (North Korea)",
                         'Korea South' = 'Korea, Republic of (South Korea)',
                         'Laos' = "Lao People's Democratic Republic",
                         'Macedonia' = 'Macedonia, the Former Yugoslav Republic of',
                         'Moldova' = 'Moldova, Republic of',
                         'Montenegro' = 'Montenegro, Republic of',
                         'Myanmar (Burma)' = 'Myanmar',
                         'Russia' = 'Russian Federation',
                         'Serbia' = 'Serbia, Republic of',
                         'Slovak Republic' = 'Slovakia',
                         'Syria' = 'Syrian Arab Republic',
                         'Taiwan' = 'Taiwan, Province of China',
                         'Tanzania' = 'Tanzania, United Republic of',
                         'UAE' = 'United Arab Emirates',
                         'Bosnia' = 'Bosnia and Herzegovina',
                         'Congo Brazzaville' = 'Congo',
                         'Congo Kinshasa' = 'Congo, the Democratic Republic of the',
                         'Dominica' = 'Dominican Republic')
                         
         
write.table(regime, './data/clean/regime.tsv', row.names=FALSE, sep='\t')



#==========
# Alliances
#==========

alliances <- read.csv('./data/raw/alliance_v4.1_by_dyad_yearly.csv',
                      stringsAsFactors=FALSE) %>%
    filter(state_name1 == 'United States of America',
           year >= 2003,
           year <= 2013) %>%
    select(state_name2, year) %>%
    mutate(alliance = 1)
colnames(alliances)[1] <- 'country'

alliances$country = recode(alliances$country, 'Antigua & Barbuda' = 'Antigua and Barbuda',
                                               'South Korea' = 'Korea, Republic of (South Korea)')

write.table(alliances, './data/clean/alliances.tsv', row.names=FALSE, sep='\t')



#=========
# Distance
#=========

countries <- read.table('./data/raw/iisystem.dat', sep='\t',
                        stringsAsFactors=FALSE)

distance <- read.csv('./data/raw/capdist.csv', stringsAsFactors=FALSE) %>%
    filter(ida == 'USA') %>%
    inner_join(countries, by=c('idb'='V2')) %>%
    select(V3, kmdist)

colnames(distance) <- c('country', 'km_dist')
rm(countries)

distance$country <- recode(distance$country, 'Serbia' = 'Serbia, Republic of',
                                             'Montenegro' = 'Montenegro, Republic of',
                                             'Moldova' = 'Moldova, Republic of',
                                             'Germany (Prussia)' = 'Germany',
                          'Rumania' = 'Romania',
                          'Russia (Soviet Union)' = 'Russian Federation',
                          'Belarus (Byelorussia)' = 'Belarus',
                          'Congo, Democratic Republic of (Zaire)' = 'Congo, the Democratic Republic of the',
                          'Tanzania/Tanganyika' = 'Tanzania, United Republic of',
                          'Zimbabwe (Rhodesia)' = 'Zimbabwe', 
                          'Madagascar (Malagasy)' = 'Madagascar',
                          'Iran (Persia)' = 'Iran, Islamic Republic of',
                          'Turkey (Ottoman Empire)' = 'Turkey',
                          'Syria' = 'Syrian Arab Republic',
                          'Yemen (Arab Republic of Yemen)' = 'Yemen',
                          'Korea, Republic of' = 'Korea, Republic of (South Korea)',
                          'Myanmar (Burma)' = 'Myanmar',
                          'Sri Lanka (Ceylon)' = 'Sri Lanka',
                          'Cambodia (Kampuchea)' = 'Cambodia',
                          'Laos' = "Lao People's Democratic Republic",
                          'Vietnam, Democratic Republic of' = 'Vietnam',
                          'Brunei' = 'Brunei Darussalam',
                          'Bosnia-Herzegovina' = 'Bosnia and Herzegovina',
                          'Macedonia (Former Yugoslav Republic of)' = 'Macedonia, the Former Yugoslav Republic of',
                          'Burkina Faso (Upper Volta)' = 'Burkina Faso')


distance[nrow(distance) + 1,] = list('Afghanistan', 11132)
distance[nrow(distance) + 1,] = list('Bahrain', 10951)
distance[nrow(distance) + 1,] = list('China', 11139)
distance[nrow(distance) + 1,] = list('Italy', 7212)
distance[nrow(distance) + 1,] = list('Kazakhstan', 9536)
distance[nrow(distance) + 1,] = list("Korea, Dem. People's Republic of (North Korea)", 11035)
distance[nrow(distance) + 1,] = list('Kuwait', 10518)
distance[nrow(distance) + 1,] = list('Kyrgyzstan', 10475)
distance[nrow(distance) + 1,] = list('Mongolia', 10350)
distance[nrow(distance) + 1,] = list('Oman', 11655)
distance[nrow(distance) + 1,] = list('Qatar', 11091)
distance[nrow(distance) + 1,] = list('Suriname', 4278)
distance[nrow(distance) + 1,] = list('Taiwan, Province of China', 12639)
distance[nrow(distance) + 1,] = list('Tajikistan', 10715)
distance[nrow(distance) + 1,] = list('Turkmenistan', 10321)
distance[nrow(distance) + 1,] = list('United Arab Emirates', 11339)
distance[nrow(distance) + 1,] = list('United Kingdom', 5894)
distance[nrow(distance) + 1,] = list('United States', 0)
distance[nrow(distance) + 1,] = list('Uzbekistan', 10458)

write.table(distance, './data/clean/distance.tsv', row.names=FALSE, sep='\t')



#====
# GDP
#====

# PERC GROWTH IS FUCKED UP!

gdp <- read.csv('./data/raw/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10203569.csv',
                stringsAsFactors=FALSE, skip=4) %>%
    select(Country.Name, X2002:X2013) %>%
    gather(year, gdp, X2002:X2013) %>%
    mutate(year = as.numeric(str_remove(year, 'X'))) %>%
    arrange(Country.Name, year)
colnames(gdp) <- c('country', 'year', 'gdp')

gdp <- gdp %>% 
    group_by(country) %>% 
    dplyr::mutate(gdp_l1 = lag(gdp, n=1, default=NA, order_by=country)) %>%
    dplyr::mutate(gdp_perc_growth = (gdp - gdp_l1) / gdp_l1) %>%
    filter(year >= 2003, year <= 2013) %>%
    select(country, year, gdp, gdp_perc_growth)

gdp$country <- recode(gdp$country, 
    'Bahamas, The' = 'Bahamas',
    'Congo, Dem. Rep.' = 'Congo, the Democratic Republic of the',
    'Congo, Rep.' = 'Congo',
    'Egypt, Arab Rep.' = 'Egypt',
    'Gambia, The' = 'Gambia',
    'Iran, Islamic Rep.' = 'Iran, Islamic Republic of',
    'Korea, Dem. People’s Rep.' = "Korea, Dem. People's Republic of (North Korea)",
    'Korea, Rep.' = 'Korea, Republic of (South Korea)',
    'Kyrgyz Republic' = 'Kyrgyzstan',
    'Lao PDR' = "Lao People's Democratic Republic",
    'Macedonia, FYR' = 'Macedonia, the Former Yugoslav Republic of',
    'Moldova' = 'Moldova, Republic of',
    'Montenegro' = 'Montenegro, Republic of',
    'Serbia' = 'Serbia, Republic of',
    'Slovak Republic' = 'Slovakia',
    'Tanzania' = 'Tanzania, United Republic of',
    'Venezuela, RB' = 'Venezuela',
    'Yemen, Rep.' = 'Yemen')

write.table(gdp, './data/clean/gdp.tsv', sep='\t', row.names=FALSE)



##############
# COMBINE DATA
##############

df <- us_fdi %>%
    inner_join(pop, by=c('country', 'year')) %>%
    left_join(arms_exports, by=c('country', 'year')) %>%
    left_join(conflict,  by=c('country', 'year')) %>%
    left_join(regime, by=c('country', 'year')) %>%
    left_join(alliances, by=c('country', 'year')) %>%
    left_join(distance, by=c('country')) %>%
    left_join(gdp, by=c('country', 'year')) %>%
    select(-conflict_id) %>%
    arrange(country, year)

# Fill in missing variables to indicate absense of exports, etc.
df <- df %>%
    mutate(fdi = replace_na(fdi, 0),
           arms_exports = replace_na(arms_exports, 0),
           conflict = replace_na(conflict, 0),
           alliance = replace_na(alliance, 0)) %>%
    unique



#################################################
# Remove small countries missing significant data
#################################################

small_countries <- c("Andorra", "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Belize", "Bermuda", "Bosnia and Herzegovina", "Brunei Darussalam", 'Cayman Islands', "Côte d'Ivoire", "Dominica", "French Polynesia", "Gibraltar", "Greenland", "Grenada", "Holy See", "Iceland", "Kiribati", "Liechtenstein", "Maldives", "Malta", "Marshall Islands", "Nauru", "Palau", "Samoa", "San Marino", "Sao Tome and Principe", "Seychelles", "Somalia", "St. Helena", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Timor-Leste", "Tonga", "Tuvalu", "Vanuatu")

df <- df %>%
    filter(country %not in% small_countries)



####################
# Fill in some holes
####################

# Regime type
# Failed/Occupied -- Sudan, Afghanistan, Libya View(regime[is.na(regime$polity2),])
# Autocracy: -10 to -6
# Closed anocracy: -5 to 0
# Open anocracy: 1 to 5
# Democracy: 6 to 9
# Full democracy: 10

df$regime_type <- NA
df$regime_type[df$polity2 >= -10 & df$polity2 <= -5] <- 'autocracy'
df$regime_type[df$polity2 >= -5 & df$polity2 <= -0] <- 'closed anocracy'
df$regime_type[df$polity2 >= 1 & df$polity2 <= 5] <- 'open anocracy'
df$regime_type[df$polity2 >= 6 & df$polity2 <= 9] <- 'democracy'
df$regime_type[df$polity2 == 10] <- 'full democracy'

df$regime_type[df$country == 'Sudan' & df$year == 2012] <- 'conflict/occupied'
df$regime_type[df$country == 'Afghanistan'] <- 'conflict/occupied'
df$regime_type[df$country == 'Iraq' & (df$year == 2003 | df$year == 2004 |df$year == 2005 |df$year == 2006 |df$year == 2007 |df$year == 2008 |df$year == 2009)] <- 'conflict/occupied'
df$regime_type[df$country == 'Lebanon' & (df$year == 2003 | df$year == 2004)] <- 'conflict/occupied'

df$polity2 <- NULL



################################
# Remove remaining missing cases
# - Note this missing GDP data mostly, 35 entries: North Korea, Swaziland,
# Syria, and Taiwan, plus a couple of years for Iraq and Eritrea
########################

df <- df[complete.cases(df), ]

# Exclude U.S. case since that's what we're interested in
df <- filter(df, country != 'United States')

write.table(df, './data/clean/master_dataset.tsv', row.names=FALSE, sep='\t')

