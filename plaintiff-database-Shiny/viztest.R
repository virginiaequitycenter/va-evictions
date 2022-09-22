library(tidyverse)
library(plotly)
library(ggthemes)
library(paletteer)

# read data ----

# data for full period
plaintiff_dat <- read.csv('plaintiff-aggregated-data.txt', colClasses = 'character')

# Make certain variables numeric so that sorting (e.g, cases hi-->lo) works appropriately
plaintiff_dat <- plaintiff_dat %>% 
  mutate(cases_filed = as.numeric(cases_filed),
         cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
         plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_years, .before = def_zips)

# plaintiff_dat$pla_1_zip <- ifelse(is.na(plaintiff_dat$pla_1_zip), 'NA', plaintiff_dat$pla_1_zip)


# data by quarter
monthly_plaintiff_dat <- read.csv('monthly-plaintiff-aggregated-data.txt', colClasses = 'character')

monthly_plaintiff_dat <- monthly_plaintiff_dat %>% 
  mutate(cases_filed = as.numeric(cases_filed),
         cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
         plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_month, .before = def_zips)

# quarterly_plaintiff_dat$pla_1_zip <- ifelse(is.na(quarterly_plaintiff_dat$pla_1_zip), 'NA', quarterly_plaintiff_dat$pla_1_zip)


# Possible visuals ----

## timeline by quarter ----
# a. summing across selections

# make jurisdiction selectable
# make outcome selectable
psum <- monthly_plaintiff_dat %>% 
  filter(court_name %in% c("Albemarle General District Court",
                           "Charlottesville General District Court",
                           "Fluvanna General District Court",
                           "Greene General District Court",
                           "Louisa General District Court",
                           "Nelson General District Court")) %>%
  group_by(filing_month) %>% 
  summarize(cases_filed = sum(cases_filed)) %>% 
  ggplot(aes(x = filing_month, y = cases_filed, group = 1)) +
  geom_point(color = "tan4") +
  geom_line(color = "tan4") +
  labs(x = "Year-Month", y = "") +
  theme(axis.text.x = element_text(angle = 45)) 

ggplotly(psum)

# AND/OR
# b. different lines for each selection

# make jurisdiction selectable
# make outcome selectable

ncolors <- n_distinct(monthly_plaintiff_dat$cour)
psep <- monthly_plaintiff_dat %>% 
  filter(court_name %in% c("Albemarle General District Court",
                           "Charlottesville General District Court",
                           "Fluvanna General District Court",
                           "Greene General District Court",
                           "Louisa General District Court",
                           "Nelson General District Court")) %>%
  group_by(filing_month, court_name) %>% 
  summarize(cases_filed = sum(cases_filed)) %>% 
  ggplot(aes(x = filing_month, y = cases_filed, group = court_name, color = court_name)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = colorRampPalette(paletteer_dynamic("cartography::multi.pal", 10))(ncolors)) +
  labs(x = "Year-Month", y = "") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none") 
 
ggplotly(psep, tooltip = c("x", "y", "group"))

# c. all three outcomes together (summed across selection)
# show all outcomes (summed)
psum <- monthly_plaintiff_dat %>% 
  filter(court_name %in% c("Albemarle General District Court",
                           "Charlottesville General District Court",
                           "Fluvanna General District Court",
                           "Greene General District Court",
                           "Louisa General District Court",
                           "Nelson General District Court")) %>%
  group_by(filing_month) %>% 
  summarize(cases_filed = sum(cases_filed),
            cases_filed_ns = sum(cases_filed_excluding_all_but_final_serial),
            cases_eviction = sum(plaintiff_judgments)) %>% 
  pivot_longer(cols = -filing_month, names_to = "outcome_type", values_to = "outcome") %>% 
  
  ggplot(aes(x = filing_month, y = outcome, 
             group = outcome_type, color = outcome_type)) +
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("nord::lake_superior") +
  labs(x = "Year-Month", y = "") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none")

ggplotly(psum, tooltip = c("x", "y", "group"))



## dotplot by jursidction/full period ----
# a. one outcome
# make jurisdiction selectable
# make outcome selectable
pdot <- plaintiff_dat %>% 
  filter(court_name %in% c("Albemarle General District Court",
                           "Charlottesville General District Court",
                           "Fluvanna General District Court",
                           "Greene General District Court",
                           "Louisa General District Court",
                           "Nelson General District Court")) %>%
  mutate(court_name = str_remove(court_name, "General District Court")) %>% 
  group_by(court_name) %>% 
  summarize(cases_filed = sum(cases_filed)) %>% 
  ggplot(aes(y = fct_reorder(court_name, cases_filed), 
             x = cases_filed,
             court = court_name)) +
  geom_segment(aes(x = 0, xend = cases_filed, 
                   y = fct_reorder(court_name, cases_filed), yend = fct_reorder(court_name, cases_filed)),
               color = "gray") +
  geom_point(size = 2, color = "tan4") +
  labs(x = "Number of Cases Filed", y = "") +
  theme(axis.text.y = element_text(size = 2)) +
  theme_classic()

ggplotly(pdot, tooltip = c("x", "court"))

# b. all outcomes
pdot <- plaintiff_dat %>% 
  filter(court_name %in% c("Albemarle General District Court",
                           "Charlottesville General District Court",
                           "Fluvanna General District Court",
                           "Greene General District Court",
                           "Louisa General District Court",
                           "Nelson General District Court")) %>%
  mutate(court_name = str_remove(court_name, "General District Court")) %>% 
  group_by(court_name) %>% 
  summarize(cases_filed = sum(cases_filed),
            cases_filed_ns = sum(cases_filed_excluding_all_but_final_serial),
            cases_eviction = sum(plaintiff_judgments)) %>% 
  pivot_longer(cols = -court_name, names_to = "outcome_type", values_to = "outcome") %>% 
  
  ggplot(aes(y = fct_reorder(court_name, outcome), 
             x = outcome,
             color = outcome_type,
             court = court_name)) +
  geom_segment(aes(x = 0, xend = outcome, 
                   y = fct_reorder(court_name, outcome), yend = fct_reorder(court_name, outcome)),
               color = "gray") +
  geom_point(size = 2) +
  scale_color_paletteer_d("nord::lake_superior") +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 2),
        legend.position = "bottom") 

ggplotly(pdot, tooltip = c("x", "color", "court"))
# fix legend 


## map? ----
# not sure this will be helpful with localities selected
