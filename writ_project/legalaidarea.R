# Legal Aid Service Areas
# 2024-04-19
# mpc
# area list/map: https://evictionhelpline.org/legal-aid-map/


# Libaries ----
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)


# Get localities/fips ----
va_localities <- get_acs(
  geography = "county",
  state = "51",
  variables = "B01001_001",
  survey = "acs5",
  year = 2022
)

va_localities <- va_localities %>% 
  select(GEOID, NAME, pop_est = estimate)


# Identify legal aid areas ----
lsnv <- c("51013", "51510", "51610", "51059", "51107", 
          "51153","51685", "51600", 51683) 
# Fairfax city, Manassas City not listed on page but shown on map

law <- c("51179", "51177", "51099", "51033", "51193", 
         "51133", "51159", "51103", "51057", "51097", 
         "51101", "51047", "51137", "51113", "51157", 
         "51061", "51630")

brls <- c("51043", "51069", "51187", "51171", "51139", 
          "51165", "51015", "51790", "51820", "51163", 
          "51530", "51091", "51017", "51005", "51660", 
          "51678", "51580", "51840") 
# Covington, Winchester not listed on page but shown on map
# removed listed localities that overlapped with roanoke valley area

cvlas <- c("51079", "51003", "51125", "51109", "51065", 
           "51075", "51085", "51087", "51145", "51041", 
           "51053", "51570", "51670", "51149", "51181", 
           "51127", "51036", "51760", "51540", "51730")
# Petersburg not listed on page but shown on map

lasev <- c("51119", "51073", "51115", "51830", "51650", 
           "51710", "51700", "51735", "51740", "51199", 
           "51810", "51550", "51001", "51131", "51095")

lasrv <- c("51045", "51023", "51019", "51067", "51770", 
           "51161", "51775")

svlas <- c("51071", "51121", "51063", "51155", "51035", 
           "51021", "51197", "51077", "51185", "51173", 
           "51167", "51027", "51191", "51520", "51195", 
           "51169", "51105", "51720", "51750", "51640",
           "51051") 
# Dickenson not listed on page, but shown on map

vlas <- c("51141", "51089", "51590", "51143", "51031", 
          "51083", "51037", "51009", "51011", "51029", 
          "51147", "51117", "51111", "51049", "51007", 
          "51135", "51025", "51081", "51183", "51175", 
          "51093", "51800", "51595", "51620", "51680", 
          "51690")


# Assign legal aid areas ----
va_localities <- va_localities %>% 
  mutate(legal_aid_service_area = case_when(
    GEOID %in% lsnv ~ "Legal Services of Northern Virginia",
    GEOID %in% law ~ "Legal Aid Works",
    GEOID %in% brls ~ "Blue Ridge Legal Services",
    GEOID %in% cvlas ~ "Central Virginia Legal Aid Society",
    GEOID %in% lasev ~ "Legal Aid Society of Eastern Virginia",
    GEOID %in% lasrv ~ "Legal Aid Society of Roanoke Valley",
    GEOID %in% svlas ~ "Southwest Virginia Legal Aid Society",
    GEOID %in% vlas ~ "Virginia Legal Aid Society"))


# Write file ----
write_csv(va_localities, "data/legal_aid_service_areas.csv")


# Check/Compare to web page map ----
va_localities_sf <- counties(state = "51", cb = TRUE, year = 2022)

va_localities_sf <- va_localities_sf %>% 
  left_join(va_localities, by = "GEOID")

ggplot(va_localities_sf) +
  geom_sf(aes(fill = legal_aid_service_area), alpha = 1/2) +
  scale_fill_manual(values = c("blue", "green", "turquoise", 
                               "yellow", "pink","orange",
                               "red","purple"),
                    guide = "none") +
  theme_void()
                               
