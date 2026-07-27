

library(tidyverse)

load("merged.rdata")
load("regs.rdata")
load("preds.rdata")
load("regs_lit.rdata")
load("preds_lit.rdata")
load("regs_pol.rdata")
load("preds_pol.rdata")

# inspection --------------------------------------------------------------

rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe" | name %in% c("Cyprus", "N. Cyprus", "Turkey")) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 2)) %>%
  dplyr::mutate(var = rep(c("RI_mean", "QRP_mean"), times = dplyr::n() / 2)) %>%
  sf::st_as_sf() %>%
  left_join(
    merged %>%
      select(RI_mean, QRP_mean, country) %>%
      distinct() %>%
      pivot_longer(1:2, names_to = "var"),
    by = c("name" = "country", "var")
  ) %>%
  mutate(
    var = recode(var, RI_mean = "Research Integrity", QRP_mean = "Questionable Research Practices"),
    fill_value = ifelse(var == "Questionable Research Practices", -value, value)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = fill_value), color = NA) +
  geom_sf(
    data = rnaturalearth::ne_download(
      scale = "medium", type = "boundary_lines_land",
      category = "cultural", returnclass = "sf"
    ) %>% filter(!FEATURECLA %in% c("Disputed (please verify)", "Indefinite")),
    color = "white", linewidth = 0.3
  ) +
  scale_fill_gradient(low = "#d6604d", high = "#2166ac",
                      na.value = "#e8e6e1", guide = "none") +
  coord_sf(xlim = c(-25, 35), ylim = c(33, 72), expand = FALSE) +
  facet_grid(.~var) +
  jtools::theme_nice() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text = element_blank())

ggsave("plots/fig1.png")

merged %>% 
  select(country, RI_mean, QRP_mean) %>% 
  summarise(
    "Research Integrity (Research Environment)" = mean(RI_mean, na.rm = T),
    "Questionable Research Practices (Researcher)" = mean(QRP_mean, na.rm = T),
    .by = country
  ) %>% 
  ggplot() +
  aes(y = `Questionable Research Practices (Researcher)`, x = `Research Integrity (Research Environment)`) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  ggrepel::geom_text_repel(
    aes(label = country), 
    seed = "123", 
    force = 10,
    min.segment.length = 0,
    size = 3
  ) +
  jtools::theme_nice()

merged %>% 
  select(country, res, prom, RI_mean, QRP_mean) %>% 
  summarise(
    "Attitude (Reservation)" = mean(res, na.rm = T),
    "Attitude (Promise)" = mean(prom, na.rm = T),
    "Research Integrity" = mean(RI_mean, na.rm = T),
    "Questionable Research Practices" = mean(QRP_mean, na.rm = T),
    .by = country
  ) %>% 
  pivot_longer(2:3, names_to = "att", values_to = "att_v") %>% 
  pivot_longer(2:3, names_to = "ri", values_to = "ri_v") %>%
  ggplot() +
  aes(y = att_v, x = ri_v) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  ggrepel::geom_text_repel(
    aes(label = country), 
    seed = "1337", 
    force = 10,
    min.segment.length = 0,
    size = 3
  ) +
  coord_cartesian(ylim = c(-1,1), xlim = c(-2,2)) +
  ylab("") +
  xlab("") +
  facet_grid(att~ri) +
  jtools::theme_nice()

ggsave("plots/fig2.png")

# regressions (base) -------------------------------------------------------

preds %>% 
  mutate(
    pred = str_replace(pred, "QRP", "Questionable Research Practices"),
    pred = str_replace(pred, "RI", "Research Integrity"),
    outc = str_replace(outc, "prom", "Attitude (Promise)"),
    outc = str_replace(outc, "res", "Attitude (Reservation)")
  ) %>% 
  filter(measure == "mean", type == "both") %>% 
  mutate(pred0 = approx(x, predicted, xout = 0)$y, .by = c(pred, model, outc, measure, type)) %>% 
  mutate(across(c(predicted, conf.low, conf.high), \(x) x-pred0)) %>% 
  
  ggplot() +
  aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_ribbon(aes(group = model), alpha = 0.15, fill = "grey60") +
  geom_line(aes(color = outc), linewidth = 1) +
  ylab("") +
  xlab("") +
  scale_color_manual(values = c("#2166ac", "#d6604d"), guide = "none") +
  coord_cartesian(ylim = c(-.5,.5), xlim = c(-2,2)) +
  facet_grid(outc~pred, scale = "free_y", space = "free") +
  jtools::theme_nice(legend.pos = "none") +
  theme(legend.title = element_blank())

ggsave("plots/fig3.png")

modelsummary::modelsummary(
  list(
    "prom" = regs$prom_mean,
    "prom" = regs$prom_mean_ri,
    "prom" = regs$prom_mean_qrp,
    "res" = regs$res_mean,
    "res" = regs$res_mean_ri,
    "res" = regs$res_mean_qrp
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/a4.docx")


# regressions (lit interaction) -------------------------------------------

bind_rows(
  preds_lit %>% mutate(inter = "Knowledge\n(higher = dashed)"),
  preds_pol %>% mutate(inter = "Ideology\n(right = dashed)")
) %>% 
  mutate(
    pred = str_replace(pred, "QRP", "Questionable Research Practices"),
    pred = str_replace(pred, "RI", "Research Integrity")
  ) %>% 
  filter(measure == "mean", type == "both", outc == "res") %>% 
  mutate(pred0 = approx(x, predicted, xout = 0)$y, .by = c(pred, model, inter, outc, measure, type)) %>% 
  mutate(across(c(predicted, conf.low, conf.high), \(x) x-pred0)) %>% 

  ggplot() +
  aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_ribbon(aes(group = paste0(model, inter, group, outc)), alpha = 0.15, fill = "grey60") +
  geom_line(aes(linetype = group), color = "#d6604d", linewidth = 1) +
  ylab("") +
  xlab("") +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  coord_cartesian(ylim = c(-.5,.5), xlim = c(-2,2)) +
  facet_grid(inter~pred) +
  jtools::theme_nice(legend.pos = "none") +
  theme(legend.title = element_blank())

ggsave("plots/fig4.png")

modelsummary::modelsummary(
  list(
    "prom" = regs_lit$prom_mean,
    "prom" = regs_lit$prom_mean_ri,
    "prom" = regs_lit$prom_mean_qrp,
    "res" = regs_lit$res_mean,
    "res" = regs_lit$res_mean_ri,
    "res" = regs_lit$res_mean_qrp
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/a5.docx")

modelsummary::modelsummary(
  list(
    "prom" = regs_pol$prom_mean,
    "prom" = regs_pol$prom_mean_ri,
    "prom" = regs_pol$prom_mean_qrp,
    "res" = regs_pol$res_mean,
    "res" = regs_pol$res_mean_ri,
    "res" = regs_pol$res_mean_qrp
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/a6.docx")



