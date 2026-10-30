

library(tidyverse)

load("data/barom.rdata")

rawbarom %>% 
  select(matches("qa10_\\d"), matches("qa11_\\d"), country = tnscntry) %>% 
  mutate(country = haven::as_factor(country)) %>% 
  mutate(across(everything(), \(x) case_when(haven::as_factor(x) != "Don't know (SPONTANEOUS)" ~ x))) %>% 
  mutate(across(everything(), \(x) case_when(haven::as_factor(x) != "Other (SPONTANEOUS)" ~ x))) %>% 
  haven::zap_labels() %>% 
  mutate(
    qa10_1n2 = coalesce(qa10_1, qa10_2),        # different wordings, taken as one
    qa11_4n5 = coalesce(qa11_4, 6 - qa11_5)     # qa11_5 reversed to match qa11_4
  ) %>% 
  # reversing attitude items so positive is always high
  mutate(across(c(qa10_1n2, qa10_3, qa10_4, qa10_5, qa10_6), \(x) 6 - x)) %>% 
  select(-qa10_1, -qa10_2, -qa11_4, -qa11_5) %>% 
  rename(
    "better lives"          = qa10_1n2,
    "informed citizens"     = qa10_3,
    "natural resources"     = qa10_4,
    "opportunities"         = qa10_5,
    "AI = jobs"             = qa10_6,
    "science v. faith"      = qa10_7,
    "human rights"          = qa10_8,
    "life change too fast"  = qa10_9,
    "dangerous knowledge"   = qa10_10,
    "special interests"     = qa11_1,
    "specific knowledge"    = qa11_2,
    "too complex"           = qa11_3,
    "intervene in politics" = qa11_4n5,
    "misuse by others"      = qa11_6
  ) -> atts

atts %>% 
  select(-country) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  psych::fa(nfactors = 2) %>% 
  # getting loadings as tibble
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  
  # Promise = the factor on which "better lives" loads more strongly
  mutate(
    swap = abs(MR1[var == "better lives"]) > abs(MR2[var == "better lives"]),
    P = if_else(swap, MR1, MR2),
    R = if_else(swap, MR2, MR1),
    # fix factor signs using reference items
    Promise      = P * sign(P[var == "opportunities"]),
    Reservations = R * sign(R[var == "dangerous knowledge"])
  ) %>% 
  
  # plotting positions of indicators in two dimensional space
  ggplot(aes(group = var, x = Reservations, y = Promise)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = var), 
    seed = 123, 
    force = 10,
    min.segment.length = 0
  ) +
  jtools::theme_nice() +
  coord_cartesian(xlim = c(-.22, .8), ylim = c(-.2, .8)) +
  ylab("Promise") +
  xlab("Reservations")

ggsave("plots/a_factor.png")



atts %>% 
  reframe(
    pick(everything()) %>% 
      cor(use = "pairwise.complete.obs") %>% 
      psych::fa(nfactors = 2) %>% 
      loadings() %>% 
      unclass() %>% 
      as_tibble(rownames = "var"),
    .by = country
  ) %>% 
  mutate(
    swap = abs(MR1[var == "opportunities"]) > abs(MR2[var == "opportunities"]),
    Promise      = if_else(swap, MR1, MR2),
    Reservations = if_else(swap, MR2, MR1),
    .by = country
  ) %>% 
  mutate(var = fct_reorder(var, Promise, mean) %>% fct_rev()) %>% 
  ggplot(aes(group = var, x = Reservations, y = Promise)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point() +
  jtools::theme_nice() +
  scale_y_continuous(position = "right") +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(~var, ncol = 3)


ggsave("plots/a_factor_countries.png")


