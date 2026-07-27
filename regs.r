

library(tidyverse)
library(lme4)
library(ggeffects)


regs <- list()
i = 0

for(outc in c("prom", "res")){
  for(pred in c("_mean", "_median")){
    
    i = i + 1
    message(paste0("Regression ", i, ": ", paste0(outc, pred)))
    
    # full model (RI + QRP)
    regs[[paste(outc, gsub("^_", "", pred), sep = "_")]] <- lmer(
      as.formula(
        paste0(outc, " ~ RI", pred, " + QRP", pred,
               " + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 + scilit + pol + rel | country)")
      ),
      data = merged,
      control = lmerControl(optimizer = "bobyqa")
    )
    
    # solo models
    for(solo in c("RI", "QRP")){
      i = i + 1
      
      solo_term <- paste0(solo, pred)
      tag <- paste(outc, gsub("^_", "", pred), tolower(gsub("_", "", solo)), sep = "_")
      message(paste0("Regression ", i, ": ", tag))
      
      regs[[tag]] <- lmer(
        as.formula(
          paste0(outc, " ~ ", solo_term,
                 " + gdppc + rnd + espend + dem + age + gender + edu + scilit + pol + rel + (1 + scilit + pol + rel | country)")
        ),
        data = merged,
        control = lmerControl(optimizer = "bobyqa")
      )
    }
  }
}

save(regs, file = "regs.rdata")



preds <- imap_dfr(names(regs), ~ {
  
  message(paste0("Prediction ", .y, " of ", length(regs), ": ", .x))
  
  outc    <- sub("_.*$", "", .x)
  suffix  <- sub("^[^_]+_", "", .x)
  measure <- sub("_(ri|qrp)$", "", suffix)
  type    <- if(grepl("_", suffix)) sub("^[^_]+_", "", suffix) else "both"
  
  bind_rows(
    if(type %in% c("both", "ri")) ggpredict(
      regs[[.x]], terms = paste0("RI_", measure, " [all]"), typical = "mean_mode"
    ) %>% as_tibble() %>% mutate(pred = "RI", model = .x),
    
    if(type %in% c("both", "qrp")) ggpredict(
      regs[[.x]], terms = paste0("QRP_", measure, " [all]"), typical = "mean_mode"
    ) %>% as_tibble() %>% mutate(pred = "QRP", model = .x)
  ) %>%
    mutate(outc = outc, measure = measure, type = type)
})

save(preds, file = "preds.rdata")
