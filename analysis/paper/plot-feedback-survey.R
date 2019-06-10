
library(tidyverse)
feedback <- read_csv(here::here("analysis/data/raw_data/Replication report feedback Survey Student Analysis Report.csv"))

# --- tidy-the-variable-names ---
feedback <-
  feedback %>%
  rename_all(list(~str_squish(str_remove(., "\\d*:"))))

# --- extract-yes-no-questions ---
feedback_yn <-
  feedback %>%
  select(6, 8)

# --- extract-likert-scale-questions ---
feedback_likert <-
  feedback %>%
  select(seq(10, 19, 2)) %>%
  mutate_all(as.factor)  %>%
  mutate_all(tolower) %>%
  as.data.frame()

mylevels <- c('strongly disagree', 'disagree', 'neutral', 'agree', 'strongly agree')

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(feedback_likert)) {
  feedback_likert[,i] <- factor(feedback_likert[,i], levels=mylevels)
}

# --- plot-likert ---
require(likert)
feedback_likert_out <- likert(feedback_likert)
plot_l <- plot(feedback_likert_out,
               wrap = 30,
               legend.position = "top",
               base_size = 10)

# --- plot-y-n
plot_y <-
feedback_yn %>%
  gather(variable, value) %>%
  group_by(variable, value) %>%
  tally() %>%
  mutate(response = ifelse(str_detect(value, "No"), "No", "Yes")) %>%
  mutate(question = str_wrap(variable, 30)) %>%
  ggplot(aes(question,
             n,
             fill = response)) +
  geom_col() +
  labs(x = "",
       y = "Number of responses") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  scale_fill_grey()

# --- combine-plots
library(cowplot)
plot_grid(plot_l,
          plot_y,
          ncol =1,
          rel_heights = c(4, 1))

ggsave('analysis/figures/feedback_likert_yn_plot.png',
                  height = 8,
                  width = 12)


# --- likert-cor
library(corrr)
feedback_likert_out$results %>%
  gather(variable, value, -Item) %>%
  mutate(Item = str_wrap(Item, 20)) %>%
  spread(Item, value) %>%
  select(-1) %>%
  correlate(method = "spearman") %>%
  rplot() +
  scale_size(range = c(5, 20))

ggsave('analysis/figures/feedback_likert_corr.png',
       height = 8,
       width = 10)




