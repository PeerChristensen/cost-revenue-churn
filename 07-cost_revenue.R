# Kapacity case (churn)

# script 7: cost revenue

library(tidyverse)
library(scales)

theme_set(theme_minimal() +
            theme(plot.background = element_rect(fill="black"),
                  panel.grid = element_line(colour="grey20"),
                  strip.text = element_text(colour="snow",size = 14),
                  axis.text = element_text(colour="snow",size=14),
                  legend.text = element_text(colour="snow"),
                  legend.title = element_text(colour="snow"))
)

preds <- read_csv("predictions.csv") %>%
  mutate_if(is.character,factor)

results <- NULL
for ( i in seq(0,1,0.01)) {
  
  preds_i <- preds %>% mutate(predict = if_else(Yes > i,"Yes","No"))

  tp <- preds_i %>% filter(predict == "Yes" & Churn == "Yes") %>% nrow()
  fp <- preds_i %>% filter(predict == "Yes" & Churn == "No") %>% nrow()
  tn <- preds_i %>% filter(predict == "No" & Churn == "No") %>% nrow()
  fn <- preds_i %>% filter(predict == "No" & Churn == "Yes") %>% nrow()
  
  discount <- 100
  
  tp_val <- 2500 - discount
  fp_val <- - discount
  tn_val <- 0
  fn_val <- -2500
  
  payoff <- (tp * tp_val) + (fp * fp_val) + (fn * fn_val) + (tn * tn_val)
  
  result <- tibble(threshold = i, payoff)
  
  results <- rbind(results,result)
}

ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }

max_point <- results %>% filter(payoff == max(payoff))

results %>%
  ggplot(aes(x=threshold,y=payoff)) +
  geom_line(color="#dc3912",size = 1) +
  scale_y_continuous(labels = ks)
ggsave("img/cost_rev.png")
  

preds_i <- preds %>% mutate(predict = if_else(Yes > 0.02,"Yes","No"))
