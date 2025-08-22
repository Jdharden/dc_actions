# -------------------------------
# Project Description: A github actions script to
# pull data in and save it to a google sheet
#
#
# Creation Date:   2025-08-16
# -------------------------------
library(tidyverse)

dc_16 <- read_csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv", col_types = cols(CCN = col_character()))
dc_17 <- read_csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv", col_types = cols(CCN = col_character()))
dc_18 <- read_csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv", col_types = cols(CCN = col_character()))
dc_19 <- read_csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv", col_types = cols(CCN = col_character()))
dc_20 <- read_csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv", col_types = cols(CCN = col_character()))
dc_21 <- read_csv("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.csv", col_types = cols(CCN = col_character()))
dc_22 <- read_csv("https://opendata.arcgis.com/api/v3/datasets/f9cc541fc8c04106a05a1a4f1e7e813c_4/downloads/data?format=csv&spatialRefId=4326", col_types = cols(CCN = col_character()))
dc_23 <- read_csv("https://opendata.arcgis.com/api/v3/datasets/89561a4f02ba46cca3c42333425d1b87_5/downloads/data?format=csv&spatialRefId=4326&where=1%3D1", col_types = cols(CCN = col_character()))
dc_24 <- read_csv("https://opendata.arcgis.com/api/v3/datasets/c5a9f33ffca546babbd91de1969e742d_6/downloads/data?format=csv&spatialRefId=4326&where=1%3D1", col_types = cols(CCN = col_character()))
dc_25 <- read_csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1", col_types = cols(CCN = col_character()))

dc_data <- bind_rows(dc_16, dc_17, dc_18, dc_19, dc_20,
          dc_21, dc_22, dc_23, dc_24, dc_25)

dc_data |>
  count(OFFENSE)

df_01 <- dc_data %>%
  separate(REPORT_DAT, c("date","time"), sep = " ") |>
  filter(OFFENSE %in% c("HOMICIDE","ROBBERY","SEX ABUSE", "ASSAULT W/DANGEROUS WEAPON")) |>
  mutate(date = as.Date(date, "%Y/%m/%d"),
         police_department = "Washington Police Department",
         ori = "DCMPD0000",
         GEOID = "1150000") |>
  group_by(month = month(date), day = day(date), year = year(date), date) |>
  tally() |>
  group_by(year) |>
  mutate(
    crime = cumsum(n)
  ) |>
  arrange(year, month)

# --- scaffold with y/m/d dates ---
base_year   <- 2025
other_years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018)
years       <- sort(c(base_year, other_years))
fmt         <- "%m/%d/%Y"

cal <- tibble(
  date_base = seq.Date(as.Date(sprintf("%d-01-01", base_year)),
                       as.Date(sprintf("%d-12-31", base_year)), by = "day")
) %>%
  mutate(
    month = month(date_base),
    day   = day(date_base),
    date  = format(date_base, fmt)
  ) %>%
  select(-date_base)

for (y in other_years) {
  cal[[paste0("date_", y)]] <- format(make_date(y, cal$month, cal$day), fmt)  # NA for invalid dates
}


# --- pivot cumulative counts so each year is a column ---
cum_wide <- df_01 %>%
  filter(year %in% years) %>%
  transmute(month, day, year, cumulative = crime) %>%
  pivot_wider(
    names_from  = year,
    values_from = cumulative,
    names_prefix = "cumulative_",
    names_sort  = TRUE
  )

# --- final table (dates like your screenshot + cumulative columns by year) ---
out <- cal %>%
  left_join(cum_wide, by = c("month","day")) %>%
  select(
    date,
    all_of(paste0("date_", other_years)),   # e.g., date_2023, date_2022
    starts_with("cumulative_")
  )

# ======== WRITE TO GOOGLE SHEETS ========

# Expect these env vars from GitHub Actions:
# - GCP_SA_KEY_PATH : path to service-account JSON (written by the workflow)
# - SHEETS_ID       : spreadsheet ID
# - SHEETS_GID      : (optional) gid of target tab; if missing, SHEETS_TAB or "out" is used
# - SHEETS_TAB      : (optional) explicit tab name override

sa_key_path <- Sys.getenv("GCP_SA_KEY_PATH")
if (!nzchar(sa_key_path)) stop("GCP_SA_KEY_PATH is not set.")
gs4_auth(path = sa_key_path)

ss <- as_sheets_id(Sys.getenv("SHEETS_ID"))
if (!nzchar(as.character(ss))) stop("SHEETS_ID is not set.")

props <- googlesheets4::sheet_properties(ss)
gid_env <- Sys.getenv("SHEETS_GID")
tab_env <- Sys.getenv("SHEETS_TAB")

sheet_name <- if (nzchar(tab_env)) {
  tab_env
} else if (nzchar(gid_env)) {
  gid_num <- suppressWarnings(as.integer(gid_env))
  found <- props$name[match(gid_num, props$id)]
  if (length(found) == 1 && !is.na(found)) found else "out"
} else {
  "out"
}

# Ensure sheet exists, then overwrite its contents
if (!(sheet_name %in% googlesheets4::sheet_names(ss))) {
  googlesheets4::sheet_add(ss, sheet = sheet_name)
} else {
  googlesheets4::sheet_clear(ss, sheet = sheet_name)
}

googlesheets4::range_write(ss, data = out, sheet = sheet_name)



