

library(specr)

# multiverse analyses (takes a long time, consider loading object instead)
# #outcome: promise, predicter: RI
multi_prom_RI <- merged %>%
  setup(y = "prom", x = "RI_mean", model = "lmer",
        controls = c("QRP_mean", "gdppc", "rnd", "espend", 
                     "dem", "age", "gender", "edu", 
                     "scilit", "pol", "rel"),
        add_to_formula = "(1 | country)") %>%
  specr()

save(multi_prom_RI, file = "multi_prom_RI.rdata")

# #outcome: promise, predicter: QRP
multi_prom_QRP <- merged %>%
  setup(y = "prom", x = "QRP_mean", model = "lmer",
        controls = c("RI_mean", "gdppc", "rnd", "espend", 
                     "dem", "age", "gender", 
                     "edu", "scilit", "pol", "rel"),
        add_to_formula = "(1 | country)") %>%
  specr()

save(multi_prom_QRP, file = "multi_prom_QRP.rdata")

# #outcome: reservations, predicter: RI
multi_res_RI <- merged %>%
  setup(y = "res", x = "RI_mean", model = "lmer",
        controls = c("QRP_mean", "gdppc", "rnd", "espend", 
                     "dem", "age", "gender", "edu", 
                     "scilit", "pol", "rel"),
        add_to_formula = "(1 | country)") %>%
  specr()

save(multi_res_RI, file = "multi_res_RI.rdata")

# #outcome: reservations, predicter: QRP
multi_res_QRP <- merged %>%
  setup(y = "res", x = "QRP_mean", model = "lmer",
        controls = c("RI_mean", "gdppc", "rnd", "espend", 
                     "dem", "age", "gender", "edu", 
                     "scilit", "pol", "rel"),
        add_to_formula = "(1 | country)") %>%
  specr()

save(multi_res_QRP, file = "multi_res_QRP.rdata")



load("multi_prom_RI.rdata")
load("multi_prom_QRP.rdata")
load("multi_res_RI.rdata")
load("multi_res_QRP.rdata")

bind_rows(
  multi_res_RI$data,
  multi_res_QRP$data,
  multi_prom_RI$data,
  multi_prom_QRP$data
) %>% 
  mutate(
    main = case_when(
      formula == "prom ~ QRP_mean + RI_mean + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 | country)" ~ T,
      formula == "prom ~ RI_mean + QRP_mean + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 | country)" ~ T,
      formula == "res ~ QRP_mean + RI_mean + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 | country)" ~ T,
      formula == "res ~ RI_mean + QRP_mean + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 | country)" ~ T
    )) %>% 
  
  mutate(rank = rank(estimate, ties.method = "first"), .by = c(x, y)) %>%
  ggplot() +
  aes(
    rank, estimate, ymin = conf.low, ymax = conf.high,
    color = conf.low > 0 | conf.high < 0
  ) +
  geom_pointrange(size = .1, linewidth = .2, alpha = .5) +
  geom_pointrange(data = . %>% filter(main == T), color = "#d6604d", size = .1, linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray70", "black")) +
  facet_grid(y ~ x, scales = "free", space = "free_x") +
  jtools::theme_nice(legend.pos = "none")

ggsave("multi.png")


