

library(tidyverse)

load("merged.rdata")
load("regs.rdata")
load("preds.rdata")

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

ggsave("plots/fig3.png")


# regressions -------------------------------------------------------

preds %>% 
  mutate(
    pred = str_replace(pred, "QRP", "Questionable Research Practices"),
    pred = str_replace(pred, "RI", "Research Integrity"),
    outc = str_replace(outc, "prom", "Attitude (Promise)"),
    outc = str_replace(outc, "res", "Attitude (Reservation)")
  ) %>% 
  filter(type == "both") %>% 
  mutate(pred0 = approx(x, predicted, xout = 0)$y, .by = c(pred, model, outc, type)) %>% 
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

ggsave("plots/fig4.png")

# as table, in appendix

modelsummary::modelsummary(
  list(
    "prom" = regs$prom,
    "prom" = regs$prom_ri,
    "prom" = regs$prom_qrp,
    "res" = regs$res,
    "res" = regs$res_ri,
    "res" = regs$res_qrp
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/a4.docx")

