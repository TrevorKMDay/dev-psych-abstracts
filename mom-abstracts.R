setwd("G:/My Drive/Research/parent-reliability/abstracts/")

library(tidyverse)
select <- dplyr::select

list.files()


load_data <- function(tsv, string) {

  columnnames <- c("info", "title", "authors", "author_info", "abstract", "CR",
                   "doi_pmid", "coi")

  data <- read.delim(tsv, header = FALSE)

  data %>%
    setNames(columnnames[1:ncol(.)]) %>%
    separate(info, into = c("index", "info"), sep = ". ", extra = "merge",
             convert = TRUE) %>%
    mutate(
      year = str_extract(info, paste(string, "....")) %>%
              str_remove(string) %>%
              as.numeric()
    ) %>%
    return()

}

extract_text <- function(data) {

  data %>%
    select(index, abstract) %>%
    mutate(
      abstract = tolower(abstract),
      moms     = str_detect(abstract, "mother"),
      dads     = str_detect(abstract, "father"),
      care     = str_detect(abstract, "caregiver"),
      parent   = str_detect(abstract, "parent")
    ) %>%
    return()

}

to_venn <- function(x, sub = 1:4) {

  x <- list(
          mom = x$index[x$moms],
          dad = x$index[x$dads],
          caregiver = x$index[x$care],
          parent = x$index[x$parent]
        )

  return(x[sub])

}

################################################################################
# Child Dev

child_dev <- load_data("ChildDev.tsv", "Child Dev.") %>%
  extract_text()

child_dev2 <- child_dev %>%
  filter(moms | dads) %>%
  group_by(moms, dads) %>%
  dplyr::summarize(
    n = n()
  ) %>%
  mutate(
    journal = "ChildDev"
  )

child_dev %>%
  to_venn(1:2) %>%
  ggVennDiagram()

################################################################################
# Dev Sci

dev_sci <- load_data("DevSci.tsv", "Dev Sci.") %>%
  extract_text()

dev_sci2 <- dev_sci %>%
  filter(moms | dads) %>%
  group_by(moms, dads) %>%
  dplyr::summarize(
    n = n()
  ) %>%
  mutate(
    journal = "DevSci"
  )

dev_sci %>%
  to_venn() %>%
  ggVennDiagram()

dev_psych <- load_data("DevPsych.tsv", "Dev Psych.") %>%
  extract_text()

dev_psych2 <- dev_psych %>%
  filter(moms | dads) %>%
  group_by(moms, dads) %>%
  dplyr::summarize(
    n = n()
  ) %>%
  mutate(
    journal = "DevPsych"
  )

dev_psych %>%
  to_venn() %>%
  ggVennDiagram()

################################################################################

mom_dad_lut <- tibble(
  moms = c(TRUE, TRUE, FALSE),
  dads = c(TRUE, FALSE, TRUE),
  val  = c("mom_and_dad", "mom_only", "dad_only")
)

all2 <- bind_rows(child_dev2, dev_psych2, dev_sci2) %>%
  left_join(mom_dad_lut)

all2_sum <- all2 %>%
  group_by(journal) %>%
  dplyr::summarize(
    sum = sum(n)
  )

all2 <- left_join(all2, all2_sum) %>%
  mutate(
    p = n / sum
  )

ggplot(all2, aes(x = val, y = p * 100, fill = journal)) +
  geom_histogram(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  labs(x = "Abstract content", y = "% papers mentioning mom|dad")
