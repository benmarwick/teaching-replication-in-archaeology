
## ---- prepare-the-rubric-data
library(tidyverse)
library(ggbeeswarm)

#' read in the data
# what a mess
grading_rubric <- readxl::read_excel(here::here("analysis/data/raw_data/grading-rubric.xlsx"))

#' clean the data
criteria <-
  grading_rubric %>%
  select(1) %>%
  # get every second row
  filter(row_number() %% 2 == 1) %>%
  slice(1:12) %>%
  pull()

criteria_short <- c(
"Content: submission includes Rmd file, Data file, and Word file"
,"Content: maximum of 500 words"
,"Content: minimum of 4 scholarly items in the reference list"
,"Intro: has clear statement of the purpose of the report"
,"Intro: has background to debate"
,"Intro: has names, locations, and basic chronology of sites"
,"Methods: identify the specific results you will replicate"
,"Methods: describe the specific methods that you used to validate these results"
,"Results: includes 1-2 original plots & description of these"
, "Conclusion:state whether the author’s claims appear to be robust, unreliable, etc"
, "Style: Use efficient and succinct sentences, no direct quotes"
, "Style: Use commas and apostrophes correctly, and spell consistently"
)

# clean up the scores from canvas, one col per student
clean_and_ratio_the_score <-
  function(x) {
    str_remove_all(x, "/|pts") %>%
      str_squish() %>%
      str_split(" ") %>%
      unlist() %>%
      parse_number() %>%
      enframe() %>%
      spread(name, value) %>%
      mutate(prop = `1` / `2`) %>%
      pull(prop) %>%
      as.numeric()
  }

#
scores <-
grading_rubric %>%
  select(contains("Ratings")) %>%
  # get every second row
  filter(row_number() %% 2 == 0) %>%
  mutate(criteria = criteria_short) %>%
  gather(student, score, -criteria) %>%
  mutate(prop = map_dbl(score, clean_and_ratio_the_score))

## ---- plot-the-rubric-data
rubric_data_plot <-
scores %>%
  mutate(criteria = str_wrap((criteria), 40)) %>%
  ggplot(aes(reorder(criteria, prop),
             prop)) +
  geom_quasirandom(alpha = 0.3,
              size = 5) +
  stat_summary(fun.y=mean,
               geom="point",
               shape="\U007C",
               size=15,
               colour="red") +
  labs(y = "Proportion of full score of each criterion",
       x = "Criterion") +
  theme_minimal(base_size = 8) +
  coord_flip()

ggplot2::ggsave(plot = rubric_data_plot,
       here::here('analysis/figures/rubric_score_distribution.jpg'),
       width = 5.75,
       dpi = 300)

# details on figure size
# electronically as high-resolution TIFF, EPS, or JPG files for best print reproduction.
# https://documents.saa.org/container/docs/default-source/doc-publications/style-guide/saa-style-guide_updated-july-2018c5062f7e55154959ab57564384bda7de.pdf?sfvrsn=8247640e_6
# A minimum of 300 dpi
# The maximum dimensions of a published figure are 5.75 inches (ca. 15 cm) by 8 inches (20.3 cm).


