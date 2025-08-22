# -------------------------------
# Project Description: Github actions crime map
#
# Creation Date:   2025-08-20
# -------------------------------
# Load Data --------------------------------------------------------------------------------------------------

library(tidyverse)
library(sf)

dc_tracts <- sf::read_sf("https://hub.arcgis.com/api/v3/datasets/95f1885eff9a4c089012a872c851c50a_8/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")

dc_25 <- read_csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1", col_types = cols(CCN = col_character()))

dc_data <- bind_rows(dc_25)

df_01 <- dc_data %>%
  separate(REPORT_DAT, c("date","time"), sep = " ") |>
  mutate(date = as.Date(date, "%Y/%m/%d"),
         police_department = "Washington Police Department",
         ori = "DCMPD0000",
         GEOID = "1150000") |>
  filter(date > "2025-08-9")

dc_crime_shapefile <- sf::st_as_sf(df_01, coords = c(x = "LONGITUDE", y = "LATITUDE"), crs = "WGS84")
dc_tracts_01 <- sf::st_as_sf(dc_tracts, crs = "NAD83")

tract_crimes <- sf::st_join(dc_crime_shapefile, dc_tracts_01)

tract_crimes_01 <- tract_crimes |>
  # count(OFFENSE)|>
  filter(OFFENSE %in% c("ASSAULT W/DANGEROUS WEAPON", "HOMICIDE", "ROBBERY", "SEX ABUSE")) |>
  group_by(date, GEOID.y) |>
  tally() |>
  select(-geometry)

tract_crimes_02 <- tract_crimes_01 |>
  group_by(GEOID = GEOID.y) |>
  summarise(
    recent_incidents = sum(n),
  ) |>
  mutate(
    GEOID = str_pad(GEOID, side = "left", pad = "0", width = 12)
  ) |>
  tibble() |>
  select(-geometry)

# ---------- Write to Google Sheets (Overwrite the target tab) ----------
sa_key_path <- Sys.getenv("GCP_SA_KEY_PATH"); stopifnot(nzchar(sa_key_path))
googlesheets4::gs4_auth(path = sa_key_path)

sheets_id <- Sys.getenv("SHEETS_ID"); stopifnot(nzchar(sheets_id))
ss <- googlesheets4::as_sheets_id(sheets_id)

props   <- googlesheets4::sheet_properties(ss)
gid_env <- Sys.getenv("SHEETS_GID")  # "1811629747"
tab_env <- Sys.getenv("SHEETS_TAB")  # optional override by name

sheet_name <- if (nzchar(tab_env)) {
  tab_env
} else if (nzchar(gid_env)) {
  gid_num <- suppressWarnings(as.integer(gid_env))
  found <- props$name[match(gid_num, props$id)]
  if (length(found) == 1 && !is.na(found)) found else "crime_map"
} else {
  "crime_map"
}

# Hard replace the worksheet contents
if (sheet_name %in% googlesheets4::sheet_names(ss)) {
  googlesheets4::sheet_delete(ss, sheet = sheet_name)
}
googlesheets4::sheet_add(ss, sheet = sheet_name)

googlesheets4::range_write(ss, data = tract_crimes_02, sheet = sheet_name, range = "A1")

