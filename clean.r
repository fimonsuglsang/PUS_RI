
library(tidyverse)
library(osfr)
set.seed(1337)

# Barometer data ----------------------------------------------------------

# extract of only relevant variables.
# full data on https://search.gesis.org/research_data/ZA7782?doi=10.4232/1.13884
load("data/barom.rdata")

rawbarom %>% 
  transmute(
    country = as_factor(tnscntry) %>% str_remove( " \\(..\\)$"),
    country = case_when(country == "EG" | country == "WG" ~ "Germany", .default = country),
    gender = as_factor(d10), 
    
    edu = case_when(
      d8>= 3 & d8<= 15 ~ "-15",
      d8>= 16 & d8<= 19 ~ "16-19",
      d8>= 20 & d8<= 24 ~ "24-29",
      d8>= 25 & d8<= 29 ~ "25-29",
      d8>= 30 & d8<= 90 ~ "30-",
      d8== 98 ~ "still studying",
    ),
    edu = fct_relevel(edu, "-15","16-19","24-29","25-29","30-","still studying"),
    age = d11,
    
    pol = case_when(d1 < 11 ~ d1),
    rel = case_when(d90_1 < 11 ~ d90_1)
  ) -> background

rawbarom %>% 
  transmute(
    across(c(qa20_1:qa20_6, qa20_8:qa20_9), 
           \(x) case_when(str_detect(as_factor(x), "CORRECT") == T ~ 1, 
                          str_detect(as_factor(x), "Don't know") == F ~ 0
           ))) %>%  
  mirt::mirt(itemtype = "3PL", optimizer = "SANN") %>% 
  mirt::fscores() %>% 
  as_tibble() %>% 
  rename(scilit = F1) -> lit

rawbarom %>% 
  select(matches("qa10_\\d"), matches("qa11_\\d")) %>% 
  mutate(across(everything(), \(x) case_when(as_factor(x) != "Don't know (SPONTANEOUS)" ~ x))) %>% 
  mutate(across(everything(), \(x) case_when(as_factor(x) != "Other (SPONTANEOUS)" ~ x))) %>% 
  haven::zap_labels() %>% 
  mutate(
    qa10_1n2 = case_when(
      is.na(qa10_1) == F ~ qa10_1, #different wordings, taken as one
      is.na(qa10_2) == F ~ qa10_2
    ),
    qa11_4n5 = case_when(
      is.na(qa11_4) == F ~ qa11_4, #different wordings, taken as one
      is.na(qa11_5) == F ~ 6-qa11_5 #reversed above
    )
  ) %>% 
  #reversing attitude items to have positive always be high
  mutate(across(c(qa10_1n2, qa10_3, qa10_4, qa10_5, qa10_6), \(x) 6-x)) %>% 
  select(-qa10_1, -qa10_2, -qa11_4, -qa11_5) %>% 
  relocate(qa10_1n2, 1) -> atts

psych::fa(atts, 2) %>% loadings()

#getting factor scores, and merging to data
psych::factor.scores(atts,psych::fa(atts, 2))$scores %>% 
  as_tibble() %>% 
  rename(res = MR1, prom = MR2) -> atts

#getting engagment data
rawbarom %>% 
  sjlabelled::label_to_colnames() %>% 
  select(matches("S&T ENGAGEMENT: ")) %>% 
  mutate(across(everything(), \(x) case_when(as_factor(x) != "Don't know (SPONTANEOUS)" ~ x))) %>% 
  mutate(across(everything(), \(x) 5-x)) %>% 
  
  mirt::mirt(itemtype = "graded") %>% 
  mirt::fscores() %>% 
  as_tibble() %>% 
  transmute(engage = F1) -> engage


barom <- bind_cols(background, lit, atts, engage) %>% 
  mutate(country = str_replace(country, "Czech Republic", "Czechia")) %>%
  mutate(country = str_replace(country, "Luxemburg", "Luxembourg"))



# expert evaluation (sops) ------------------------------------------------


# extract of only relevant variables.
# full data on https://osf.io/xb9rk/files/osfstorage
load("data/sopsdata.rdata")


sops <- sops %>%  
  mutate(across(c(reswkenv:respubcom, impwkenv:imppubcom), ~6-.)) %>% 
  mutate(RI = rowMeans(select(., reswkenv:respubcom), na.rm = T)) %>% 
  
  mutate(across(qrpcite:qrpethapp, ~case_when(. > 1 ~ .-2))) %>% 
  mutate(QRP = rowMeans(select(., qrpcite:qrpethapp), na.rm = T)) %>% 
  
  mutate(country = as_factor(country)) %>% 
  mutate(country = str_replace(country, "UK", "United Kingdom")) %>%  

  summarise(RI_mean = mean(RI, na.rm = T), QRP_mean = mean(QRP, na.rm = T), .by = country)


# country level data ---------------------------------------------------------------

country <- read_csv("data/wbgdp.csv", skip = 4) %>% 
  select(country = `Country Name`, gdppc = `2021`) %>% 
  mutate(country = str_replace(country, "North Macedonia", "Macedonia")) %>% 
  mutate(country = str_replace(country, "Slovak Republic", "Slovakia")) %>% 
  
  left_join(
    read_csv("data/wbrnd.csv", skip = 4) %>% 
      select(country = `Country Name`, rnd = `2021`) %>% 
      mutate(country = str_replace(country, "North Macedonia", "Macedonia")) %>% 
      mutate(country = str_replace(country, "Slovak Republic", "Slovakia"))
  ) %>% 
  
  left_join(
    read_csv("data/wbedu.csv", skip = 4) %>% 
      select(country = `Country Name`, espend = `2021`) %>% 
      mutate(espend = case_when(country == "Greece" ~ 3.38, .default = espend)) %>% 
      mutate(country = str_replace(country, "North Macedonia", "Macedonia")) %>% 
      mutate(country = str_replace(country, "Slovak Republic", "Slovakia"))
  ) %>% 
  
  left_join(
    read_csv("./data/vdem.csv") %>% 
      filter(year == 2021) %>% 
      select(country = "country_name", dem = "v2x_polyarchy") %>% 
      mutate(country = str_replace(country, "Czech Republic", "Czechia"))
  )

#merging

clist <- left_join(barom, left_join(sops, country)) %>% 
  filter(!is.na(RI_mean)) %>% 
  count(country) %>% 
  select(country)

merged <- left_join(
  
  barom %>% 
    semi_join(clist) %>% 
    mutate(across(c(pol:engage), \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T))),
  
  left_join(sops, country) %>% 
    semi_join(clist) %>% 
    mutate(across(c(RI_mean:dem), \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)))
)

save(merged, file = "merged.rdata")


