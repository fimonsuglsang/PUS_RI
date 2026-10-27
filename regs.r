

library(tidyverse)
library(lme4)
library(ggeffects)

load("merged.rdata")

regs <- list()

# prom
# the main regression with both indicators
regs[["prom"]] <- lmer(
  prom ~ RI_mean + QRP_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)
# only RI
regs[["prom_ri"]] <- lmer(
  prom ~ RI_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)
# only QRPs
regs[["prom_qrp"]] <- lmer(
  prom ~ QRP_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)

# res
# the main regression with both indicators
regs[["res"]] <- lmer(
  res ~ RI_mean + QRP_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)
# only RI
regs[["res_ri"]] <- lmer(
  res ~ RI_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)
# only QRPs
regs[["res_qrp"]] <- lmer(
  res ~ QRP_mean + gdppc + rnd + espend + dem + 
    age + gender + edu +
    scilit + pol + rel + (1 | country),
  data = merged, control = lmerControl(optimizer = "bobyqa")
)


save(regs, file = "regs.rdata")


names(regs) %>%
  imap(\(model, i) {
    message("Prediction ", i, " of ", length(regs), ": ", model)

    outc <- str_remove(model, "_.*$")
    type <- if (str_detect(model, "_")) str_remove(model, "^[^_]+_") else "both"

    bind_rows(
      if (type %in% c("both", "ri")) {
        ggpredict(regs[[model]], terms = "RI_mean [all]", typical = "mean_mode") %>%
          as_tibble() %>%
          mutate(pred = "RI")
      },
      if (type %in% c("both", "qrp")) {
        ggpredict(regs[[model]], terms = "QRP_mean [all]", typical = "mean_mode") %>%
          as_tibble() %>%
          mutate(pred = "QRP")
      }
    ) %>%
      mutate(model = model, outc = outc, type = type)
  }) %>%
  list_rbind() -> preds


save(preds, file = "preds.rdata")
