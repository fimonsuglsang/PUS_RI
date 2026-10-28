

library(tidyverse)
library(lme4)
library(broom.mixed)

load("merged.rdata")
load("regs.rdata")


expand_grid(
  outcome = c("prom", "res"),
  dropped = c("none", merged$country %>% unique() %>% as.character())
) %>%
  mutate(
    fit = map2(outcome, dropped, \(o, d) {
      message("Refitting ", o, ", dropping: ", d)
      
      regs[[o]] %>%
        update(data = filter(merged, as.character(country) != d)) %>%
        tidy(effects = "fixed", conf.int = TRUE)
    })
  ) %>%
  unnest(fit) -> sens

save(sens, file = "sens.rdata")


load("sens.rdata")

sens %>%
  filter(term %in% c("RI_mean", "QRP_mean")) %>%
  mutate(term = str_remove(term, "_mean")) %>% 
  ggplot(aes(x = estimate, y = dropped)) +
  geom_pointrange(
    data = . %>% filter(dropped != "none"),
    aes(xmin = conf.low, xmax = conf.high),
    size = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(outcome ~ term, scales = "free_x") +
  labs(x = "Coefficient when country is dropped", y = "Dropped country") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.y = element_text(hjust = 1))

ggsave("plots/a_sensitivity.png")


