

## ---- prepare-the-feedback-data
library(tidyverse)
feedback <- read_csv(here::here("analysis/data/raw_data/Replication report feedback Survey Student Analysis Report.csv"))

# --- tidy-the-variable-names ---
feedback <-
  feedback %>%
  rename_all(list(~str_squish(str_remove(., "\\d*:"))))

# extract-yes-no-questions ---
feedback_yn <-
  feedback %>%
  select(6, 8)

# add period to every sentence
feedback_yn_names <- names(feedback_yn)
feedback_yn_names <- str_remove_all(feedback_yn_names, "\\.")
feedback_yn_names <-   str_glue('{feedback_yn_names}.')
names(feedback_yn) <- feedback_yn_names

# extract-likert-scale-questions ---
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

# add period to every sentence
feedback_likert_names <- names(feedback_likert)
feedback_likert_names <- str_remove_all(feedback_likert_names, "\\.")
feedback_likert_names <-   str_glue('{feedback_likert_names}.')
names(feedback_likert) <- feedback_likert_names


## ---- plot-the-feedback-data
#  plot-likert ---
require(likert)
feedback_likert_out <- likert(feedback_likert)

plot_l <- plot(feedback_likert_out,
               wrap = 30,
               text.size = 2,
               legend.position = "top") +
  theme(text=element_text(size=4),
        axis.text=element_text(size=4),
        legend.text = element_text(size = 5.5))

# plot-y-n
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
  theme_minimal(base_size = 6) +
  scale_fill_grey()

# combine-plots
library(cowplot)
feedback_plots <-
plot_grid(plot_l,
          plot_y,
          ncol =1,
          rel_heights = c(4, 1))

ggsave(plot = feedback_plots,
       here::here('analysis/figures/feedback_likert_yn_plot.jpg'),
                  width = 5.75,
       dpi = 300)


# likert-cor
library(corrr)
feedback_likert_out_plot <-
feedback_likert_out$results %>%
  gather(variable, value, -Item) %>%
  mutate(Item = str_wrap(Item, 20)) %>%
  spread(Item, value) %>%
  select(-1) %>%
  correlate(method = "spearman") %>%
  shave(upper = FALSE) %>%
  rplot() +
  scale_size(range = c(5, 20)) +
  theme(axis.text=element_text(size=6))

ggsave(plot = feedback_likert_out_plot,
       here::here('analysis/figures/feedback_likert_corr.jpg'),
       width = 5.75,
       h = 5.75,
       dpi = 300)




