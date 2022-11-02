library(tidyverse)
library(plotly)
library(ggthemes)
library(paletteer)
library(scales)
#install.packages("nord")
library(nord)
library(DT)
show_col(nord_palettes$lake_superior)
pal_lake_superior <- c("#c87d4b", "#324b64")

# read data ----

# data for full period
#plaintiff_dat <- read.csv('plaintiff-database-Shiny/plaintiff-aggregated-data.txt', colClasses = 'character')
plaintiff_dat <- read.csv('plaintiff-aggregated-data.txt', colClasses = 'character')

# Make certain variables numeric so that sorting (e.g, cases hi-->lo) works appropriately
plaintiff_dat <- plaintiff_dat %>% 
  mutate(cases_filed = as.numeric(cases_filed),
         cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
         plaintiff_judgments = as.numeric(plaintiff_judgments),
         eviction_percent = round((plaintiff_judgments/cases_filed)*100,1),
         serial_cases = cases_filed - cases_filed_excluding_all_but_final_serial) %>% 
  relocate(filing_years, .before = def_zips)

# plaintiff_dat$pla_1_zip <- ifelse(is.na(plaintiff_dat$pla_1_zip), 'NA', plaintiff_dat$pla_1_zip)

datatable(plaintiff_dat)

# data by year
#yearly_plaintiff_dat <- read.csv('plaintiff-database-Shiny/yearly-plaintiff-aggregated-data.txt', colClasses = 'character')
yearly_plaintiff_dat <- read.csv('yearly-plaintiff-aggregated-data.txt', colClasses = 'character')

yearly_plaintiff_dat <- yearly_plaintiff_dat %>% 
  mutate(cases_filed = as.numeric(cases_filed),
         cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
         plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_year, .before = def_zips)

# data by month
#monthly_plaintiff_dat <- read.csv('plaintiff-database-Shiny/monthly-plaintiff-aggregated-data.txt', colClasses = 'character')
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
  # filter(court_name %in% c("Albemarle General District Court",
  #                          "Charlottesville General District Court",
  #                          "Fluvanna General District Court",
  #                          "Greene General District Court",
  #                          "Louisa General District Court",
  #                          "Nelson General District Court")) %>%
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

# d. Evictions and Cases (2 outcomes) together (summed across selection) ----
# show all outcomes (summed)
psum <- monthly_plaintiff_dat %>% 
  group_by(filing_month) %>% 
  summarize(cases_filed = sum(cases_filed),
            cases_eviction = sum(plaintiff_judgments)) %>%
  pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
               names_prefix = "cases_") %>% 
  mutate(Outcome = ifelse(Outcome == "filed", "Cases", "Evictions"),
         Outcome = factor(Outcome, levels = c("Evictions", "Cases"))) %>%
  
  ggplot(aes(x = filing_month, y = Number, 
             group = Outcome, color = Outcome)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = pal_lake_superior,
                     labels = c("Evictions", "Cases"),
                     name = "") +
  labs(x = "Year-Month", y = "") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "bottom") 
psum

ggplotly(psum, tooltip = c("x", "y", "group"))%>% 
  layout(legend = list(orientation = "h", x = .3, y = 10))
# fix tooltips: filing_month

# Yearly Evictions and Cases (2 outcomes) together (summed across selection) ----
# show all outcomes (summed)
psum_year <- yearly_plaintiff_dat %>% 
  group_by(filing_year) %>% 
  summarize(Cases = sum(cases_filed),
            Evictions = sum(plaintiff_judgments)) %>%
  mutate(cases = "Cases", evictions = "Evictions") %>% 

  ggplot() +
  geom_col(aes(x = filing_year, y = Cases, fill = cases)) +
  geom_col(aes(x = filing_year, y = Evictions, fill = evictions),
           width = 0.70) +
  scale_fill_manual(values = pal_lake_superior[c(2,1)],
                     labels = c("Cases", "Evictions"),
                     name = "") +
  labs(x = "Year", y = "") +
  theme(legend.position = "bottom") 
psum_year

ggplotly(psum_year, tooltip = c("x", "y"))%>% 
  layout(legend = list(orientation = "h"))
# fix tooltips: filing_month



## dotplot by jursidction/full period ----
# a. one outcome
# make jurisdiction selectable
# make outcome selectable
pdot <- plaintiff_dat %>% 
  # filter(court_name %in% c("Albemarle General District Court",
  #                          "Charlottesville General District Court",
  #                          "Fluvanna General District Court",
  #                          "Greene General District Court",
  #                          "Louisa General District Court",
  #                          "Nelson General District Court")) %>%
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
pdot

ggplotly(pdot, tooltip = c("x", "court"))


# b. all outcomes ----
# palette

pdot <- plaintiff_dat %>% 
  # filter(court_name %in% c("Albemarle General District Court",
  #                          "Charlottesville General District Court",
  #                          "Fluvanna General District Court",
  #                          "Greene General District Court",
  #                          "Louisa General District Court",
  #                          "Nelson General District Court")) %>%
  mutate(court_name = str_remove(court_name, "General District Court"),
         court_name = str_trim(court_name)) %>% 
  group_by(court_name) %>% 
  summarize(cases_filed = sum(cases_filed),
            cases_eviction = sum(plaintiff_judgments)) %>% 
  pivot_longer(cols = -court_name, names_to = "Outcome", values_to = "Number",
               names_prefix = "cases_") %>% 
  mutate(Outcome = ifelse(Outcome == "filed", "Cases", "Evictions"),
         Outcome = factor(Outcome, levels = c("Evictions", "Cases"))) %>% 
  
  ggplot(aes(y = fct_reorder(court_name, Number), 
             x = Number,
             color = Outcome,
             label = Outcome,
             court = court_name)) +
  geom_segment(aes(x = 0, xend = Number, 
                   y = fct_reorder(court_name, Number), yend = fct_reorder(court_name, Number)),
               color = "gray") +
  geom_point(size = 2) +
  scale_color_manual(values = pal_lake_superior,
                     labels = c("Evictions", "Cases"),
                     name = "") +
  scale_x_continuous(expand = expansion(mult = c(0,.05))) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(size = 2),
        legend.position = "bottom") +
  theme_classic()
pdot

ggplotly(pdot, tooltip = c("x", "label", "color", "court")) %>% 
  layout(legend = list(orientation = "h"))
# fix tooltips: court_name


## map? ----
# not sure this will be helpful with localities selected
