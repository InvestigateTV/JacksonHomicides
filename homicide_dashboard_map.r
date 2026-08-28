library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(stringr)
library(jsonlite)
library(leaflet)
library(readxl)
library(htmltools)

# =============================================================================
# DATA LOAD
# =============================================================================

HOMICIDE_DATA_URL <- Sys.getenv("HOMICIDE_DATA_URL")

if (identical(HOMICIDE_DATA_URL, "")) {
  stop("HOMICIDE_DATA_URL environment variable is not set. ",
       "In GitHub Actions this comes from a repository secret; locally, set it ",
       "with Sys.setenv(HOMICIDE_DATA_URL = \"...\") before sourcing this script.")
}

WARDS_PATH <- "data/JacksonWardsNew.json"
CITY_BOUNDARY_PATH <- "data/City_Boundaries.json"
CCID_BOUNDARY_PATH <- "data/Old_CCID_2024.json"

homicide_data_path <- tempfile(fileext = ".csv")
download.file(HOMICIDE_DATA_URL, destfile = homicide_data_path, mode = "wb", quiet = TRUE)
JacksonHomicides <- read.csv(homicide_data_path)

homicides_sf <- st_as_sf(JacksonHomicides, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

Wards <- read_sf(WARDS_PATH) |>
  rename(
    Pop_18Plus   = `_18P_Pop`,
    White_18Plus = `_18P_Wht`,
    Black_18Plus = `_18P_Blk`
  )

city_boundary_sf <- read_sf(CITY_BOUNDARY_PATH) |> st_as_sf()
CCID_boundary_sf <- read_sf(CCID_BOUNDARY_PATH) |> st_as_sf()

homicides_sf <- homicides_sf |> mutate(WardKey = toupper(WardKey))

# =============================================================================
# INCIDENT-LEVEL TABLE (one row per homicide, not per victim)
# =============================================================================

homicides_incidents <- homicides_sf |>
  st_drop_geometry() |>
  group_by(UUID) |>
  summarise(
    Date                  = first(Date),
    Year                  = first(Year),
    Month                 = first(Month),
    Day                   = first(Day),
    WardKey               = first(WardKey),
    Ward                  = first(Ward),
    Precinct              = first(Precinct),
    City                  = first(City),
    State                 = first(State),
    Address               = first(Address),
    Street                = first(Street),
    Premises.Type         = first(Premises.Type),
    Circumstance          = first(Circumstance),
    Investigating.Agency  = first(Investigating.Agency),
    Case.Status           = first(Case.Status),
    Arrest.made           = first(Arrest.made),
    Latitude              = first(Latitude),
    Longitude             = first(Longitude),
    victim_count          = n(),
    .groups = "drop"
  ) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

homicides_incidents_sf <- homicides_incidents |>
  filter(!is.na(Latitude), !is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(homicides_sf), remove = FALSE) |>
  st_transform(crs = 4326)

CCID_boundary_sf <- st_transform(CCID_boundary_sf, crs = 4326)
city_boundary_sf <- st_transform(city_boundary_sf, crs = 4326)

# =============================================================================
# CONSTANTS
# =============================================================================

all_wardkeys  <- Wards$District |> unique() |> sort()
all_years     <- seq(min(homicides_incidents$Year, na.rm = TRUE), max(homicides_incidents$Year, na.rm = TRUE))
TODAY         <- Sys.Date()
CURRENT_YEAR  <- year(TODAY)
PREVIOUS_YEAR <- CURRENT_YEAR - 1
DOY_CUTOFF    <- yday(TODAY)

color_lookup <- c(
  "High Decrease" = "#2166AC",
  "Decrease"      = "#92C5DE",
  "Stable"        = "#D2B48C",
  "Increase"      = "#F4A582",
  "High Increase" = "#B2182B",
  "No Data"       = "#BDBDBD"
)
legend_order <- c("High Increase", "Increase", "Stable", "Decrease", "High Decrease", "No Data")

# =============================================================================
# CITY-WIDE TRENDS (static, filter-independent panel)
# =============================================================================

citywide_ytd_current  <- homicides_incidents |>
  filter(Year == CURRENT_YEAR, yday(Date) <= DOY_CUTOFF) |>
  nrow()

citywide_ytd_previous <- homicides_incidents |>
  filter(Year == PREVIOUS_YEAR, yday(Date) <= DOY_CUTOFF) |>
  nrow()

citywide_pct_change <- case_when(
  citywide_ytd_previous == 0 & citywide_ytd_current == 0 ~ NA_real_,
  citywide_ytd_previous == 0 & citywide_ytd_current  > 0 ~ Inf,
  TRUE ~ (citywide_ytd_current - citywide_ytd_previous) / citywide_ytd_previous
)

citywide_pct_change_label <- case_when(
  is.na(citywide_pct_change)       ~ "No Change (0 to 0)",
  is.infinite(citywide_pct_change) ~ "Large Increase",
  TRUE ~ paste0(ifelse(citywide_pct_change >= 0, "+", ""), round(citywide_pct_change * 100, 1), "%")
)

current_month_label <- format(TODAY, "%B %Y")

current_month_count <- homicides_incidents |>
  filter(Year == CURRENT_YEAR, month(Date) == month(TODAY)) |>
  nrow()

last_30_days_count <- homicides_incidents |>
  filter(Date >= (TODAY - 29), Date <= TODAY) |>
  nrow()

five_year_avg_years <- seq(CURRENT_YEAR - 5, CURRENT_YEAR - 1)
current_month_num <- month(TODAY)
current_day_num   <- day(TODAY)

five_year_month_to_date_counts <- sapply(five_year_avg_years, function(Y) {
  homicides_incidents |>
    filter(
      Year == Y,
      month(Date) == current_month_num,
      day(Date) <= current_day_num
    ) |>
    nrow()
})

five_year_avg_label <- round(mean(five_year_month_to_date_counts), 1)
month_to_date_label <- paste0(format(TODAY, "%B"), " (to date)")

# =============================================================================
# KEY TRENDS CHART DATA (static, filter-independent; feeds the "Key Trends" tab)
# =============================================================================

CITYWIDE_POPULATION <- 141196

# --- Cumulative Homicides by Year: one running-total series per year, ---
# --- indexed by day-of-year (1-366) so years overlay on a shared x-axis. ---
cumulative_by_year <- lapply(all_years, function(Y) {
  year_days <- if (Y == CURRENT_YEAR) DOY_CUTOFF else 366

  daily_counts <- homicides_incidents |>
    filter(Year == Y) |>
    mutate(doy = yday(Date)) |>
    count(doy, name = "n")

  running <- integer(year_days)
  for (i in seq_len(year_days)) {
    day_n <- daily_counts$n[daily_counts$doy == i]
    running[i] <- if (length(day_n) == 0) 0L else day_n
  }
  running <- cumsum(running)

  list(
    year = as.character(Y),
    is_current = (Y == CURRENT_YEAR),
    final_total = as.integer(tail(running, 1)),
    values = as.integer(running)
  )
})
names(cumulative_by_year) <- as.character(all_years)

today_doy_label <- DOY_CUTOFF

# --- Homicides by Year: incident counts stacked by agency bucket + rate/100k ---
agency_bucket_expr <- function(agency) {
  case_when(
    agency == "JPD"            ~ "JPD",
    agency == "Capitol Police" ~ "Capitol Police",
    TRUE                       ~ "Other Agencies"
  )
}

homicides_by_year_agency <- homicides_incidents |>
  filter(Year %in% all_years) |>
  mutate(agency_bucket = agency_bucket_expr(Investigating.Agency)) |>
  count(Year, agency_bucket, name = "n")

homicides_by_year_summary <- lapply(all_years, function(Y) {
  rows <- homicides_by_year_agency |> filter(Year == Y)
  get_n <- function(bucket) {
    v <- rows$n[rows$agency_bucket == bucket]
    if (length(v) == 0) 0L else as.integer(v)
  }
  total_incidents <- get_n("JPD") + get_n("Capitol Police") + get_n("Other Agencies")
  list(
    year           = as.character(Y),
    jpd            = get_n("JPD"),
    capitol_police = get_n("Capitol Police"),
    other_agencies = get_n("Other Agencies"),
    total_incidents = total_incidents,
    rate_per_100k  = round((total_incidents / CITYWIDE_POPULATION) * 100000, 1)
  )
})

# =============================================================================
# STAGE A: Per-year YoY velocity/color
#   - CURRENT_YEAR: YTD vs YTD (same day-of-year cutoff)
#   - Completed past years: full year vs full year
# =============================================================================

full_year_counts <- homicides_incidents |>
  filter(!is.na(WardKey)) |>
  count(WardKey, Year, name = "full_year_count")

ytd_counts <- homicides_incidents |>
  filter(!is.na(WardKey), Year %in% c(CURRENT_YEAR, PREVIOUS_YEAR)) |>
  filter(yday(Date) <= DOY_CUTOFF) |>
  count(WardKey, Year, name = "ytd_count")

velocity_by_year_list <- lapply(all_years, function(Y) {
  is_current <- (Y == CURRENT_YEAR)

  if (is_current) {
    this_year_count  <- ytd_counts |> filter(Year == Y)     |> select(WardKey, count = ytd_count)
    prior_year_count <- ytd_counts |> filter(Year == Y - 1) |> select(WardKey, count = ytd_count)
  } else {
    this_year_count  <- full_year_counts |> filter(Year == Y)     |> select(WardKey, count = full_year_count)
    prior_year_count <- full_year_counts |> filter(Year == Y - 1) |> select(WardKey, count = full_year_count)
  }

  tibble(WardKey = all_wardkeys) |>
    left_join(this_year_count  |> rename(count_this  = count), by = "WardKey") |>
    left_join(prior_year_count |> rename(count_prior = count), by = "WardKey") |>
    mutate(
      Year        = Y,
      count_this  = replace_na(count_this, 0L),
      count_prior = replace_na(count_prior, 0L),
      has_prior_year_data = (Y - 1) %in% all_years,
      pct_change = case_when(
        !has_prior_year_data               ~ NA_real_,
        count_prior == 0 & count_this == 0 ~ NA_real_,
        count_prior == 0 & count_this  > 0 ~ Inf,
        TRUE ~ (count_this - count_prior) / count_prior
      ),
      velocity_category = case_when(
        is.na(pct_change)                      ~ "No Data",
        pct_change == Inf | pct_change >  0.20 ~ "High Increase",
        pct_change >  0.05                     ~ "Increase",
        pct_change >= -0.05                    ~ "Stable",
        pct_change >= -0.20                    ~ "Decrease",
        TRUE                                   ~ "High Decrease"
      )
    )
})

ward_velocity_by_year <- bind_rows(velocity_by_year_list) |>
  mutate(fill_color = unname(color_lookup[velocity_category]))

# =============================================================================
# Ward popup builder (per year)
# =============================================================================

build_ward_popup <- function(w, Y) {
  is_current <- (Y == CURRENT_YEAR)
  period_label <- if (is_current) "YTD" else "Full Year"

  with(w, paste0(
    "<strong>", Name, "</strong><br/>",
    Y, " Homicides (", period_label, "): ", count_this, "<br/>",
    Y - 1, " Homicides (", period_label, "): ", count_prior, "<br/>",
    "Year over Year Change: ", case_when(
      is.na(pct_change) & !has_prior_year_data ~ "No prior year data",
      is.na(pct_change)                        ~ "No Change (0 to 0)",
      is.infinite(pct_change)                  ~ "Large Increase",
      TRUE ~ paste0(round(pct_change * 100, 1), "%")
    )
  ))
}

# =============================================================================
# Build one ward sf object per year (velocity/color/popup joined on)
# =============================================================================

wards_by_year <- lapply(all_years, function(Y) {
  w <- Wards |>
    left_join(ward_velocity_by_year |> filter(Year == Y), by = c("District" = "WardKey")) |>
    st_transform(crs = 4326)
  w$popup_html <- build_ward_popup(w, Y)
  w
})
names(wards_by_year) <- as.character(all_years)

# =============================================================================
# Serialize each year's ward layer to a GeoJSON string
# (write-to-tempfile-then-read-as-text avoids the geojsonsf/.rs.* RStudio issue)
# =============================================================================

ward_geojson_by_year <- lapply(all_years, function(Y) {
  w <- wards_by_year[[as.character(Y)]]

  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(
    w[, c("Name", "fill_color", "popup_html")],
    dsn = tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE
  )
  geo_str <- paste(readLines(tmp, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  unlink(tmp)
  geo_str
})
names(ward_geojson_by_year) <- as.character(all_years)

# =============================================================================
# Victim detail rollup (per UUID) - identified vs "Unidentified [Race] [Sex] Victim"
# =============================================================================

victim_detail_by_uuid <- homicides_sf |>
  st_drop_geometry() |>
  mutate(
    age_display  = ifelse(is.na(Age)  | Age  %in% c("N/A", "Unknown"), "Unknown", Age),
    sex_word     = case_when(Sex == "M" ~ "Male", Sex == "F" ~ "Female", TRUE ~ NA_character_),
    race_word    = ifelse(is.na(Race) | Race %in% c("N/A", "Unknown"), NA_character_, Race),
    sex_display  = ifelse(is.na(sex_word),  "Sex Unknown",  sex_word),
    race_display = ifelse(is.na(race_word), "Race Unknown", race_word),
    is_unidentified = str_detect(coalesce(Victim, ""), regex("unidentified", ignore_case = TRUE)),
    victim_label = case_when(
      is_unidentified & !is.na(race_word) & !is.na(sex_word) ~ paste0("Unidentified ", race_word, " ", sex_word, " Victim"),
      is_unidentified & is.na(race_word)  & !is.na(sex_word) ~ paste0("Unidentified ", sex_word, " Victim"),
      is_unidentified & !is.na(race_word) & is.na(sex_word)  ~ paste0("Unidentified ", race_word, " Victim"),
      TRUE ~ Victim
    ),
    victim_line = ifelse(
      is_unidentified,
      victim_label,
      paste0(victim_label, " (Age ", age_display, ", ", sex_display, ", ", race_display, ")")
    )
  ) |>
  group_by(UUID) |>
  summarise(victim_details = paste(victim_line, collapse = "<br/>"), .groups = "drop")

# =============================================================================
# Incident-level GeoJSON per year (multi-year toggling)
# =============================================================================

incident_geojson_by_year <- lapply(all_years, function(Y) {
  inc <- homicides_incidents_sf |>
    filter(Year == Y) |>
    left_join(victim_detail_by_uuid, by = "UUID")

  if (nrow(inc) == 0) {
    return(NULL)
  }

  inc$case_status <- ifelse(is.na(inc$Case.Status) | inc$Case.Status == "", "Unsolved", inc$Case.Status)
  inc$is_solved <- inc$case_status == "Solved"

  inc$popup_html <- with(inc, paste0(
    "<strong>", format(Date, "%B %d, %Y"), " | ", Circumstance, "</strong><hr>",
    "Victim", ifelse(victim_count > 1, "s", ""), ": ", victim_details, "<br/>",
    "Agency: ", Investigating.Agency, "<br/>",
    "Case Status: ", case_status
  ))
  inc$agency <- inc$Investigating.Agency
  inc$circumstance <- inc$Circumstance

  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(
    inc[, c("popup_html", "agency", "circumstance", "case_status", "is_solved")],
    dsn = tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE
  )
  geo_str <- paste(readLines(tmp, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  unlink(tmp)
  geo_str
})
names(incident_geojson_by_year) <- as.character(all_years)

# =============================================================================
# Gray fallback ward layer, shown when no consecutive year pair is selected
# =============================================================================

no_comparison_wards <- wards_by_year[[as.character(all_years[1])]] |>
  mutate(
    fill_color = unname(color_lookup["No Data"]),
    popup_html = paste0(
      "<strong>", Name, "</strong><br/>",
      "Select two consecutive years to see year-over-year trends."
    )
  )

tmp <- tempfile(fileext = ".geojson")
sf::st_write(
  no_comparison_wards[, c("Name", "fill_color", "popup_html")],
  dsn = tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE
)
no_comparison_geojson <- paste(readLines(tmp, encoding = "UTF-8", warn = FALSE), collapse = "\n")
unlink(tmp)

# =============================================================================
# Year checkbox control HTML
# =============================================================================

all_years_desc <- sort(all_years, decreasing = TRUE)

year_checkbox_rows <- paste0(
  "<label style='display:block; font-weight:normal;'>",
  "<input type='checkbox' class='year-toggle' value='", all_years_desc, "'",
  ifelse(all_years_desc == CURRENT_YEAR, " checked", ""),
  "> ", all_years_desc,
  "</label>",
  collapse = ""
)

year_control_html <- paste0("
<div class='sidebar-section'>
  <strong>Years Shown</strong><br/>
  <div class='sidebar-scroll'>
  ", year_checkbox_rows, "
  </div>
</div>
")

# =============================================================================
# Incident summary records (all incidents, including non-geocoded)
# =============================================================================

incident_summary_df <- homicides_incidents |>
  filter(Year %in% all_years) |>
  transmute(
    Year = as.character(Year),
    Agency = Investigating.Agency,
    Circumstance = Circumstance,
    Latitude = Latitude,
    Longitude = Longitude,
    VictimCount = victim_count
  )

incident_summary_records <- lapply(seq_len(nrow(incident_summary_df)), function(i) {
  list(
    Year = incident_summary_df$Year[i],
    Agency = incident_summary_df$Agency[i],
    Circumstance = incident_summary_df$Circumstance[i],
    Latitude = incident_summary_df$Latitude[i],
    Longitude = incident_summary_df$Longitude[i],
    VictimCount = incident_summary_df$VictimCount[i]
  )
})

# =============================================================================
# Victim-level summary records (drives "Detailed Breakdowns" charts and the
# "Victims" table tab; both are responsive to the same Year/Agency/
# Circumstance/Search filters as the map).
# =============================================================================

age_group_bucket <- function(age_chr) {
  age_num <- suppressWarnings(as.numeric(age_chr))
  case_when(
    is.na(age_num)          ~ "Unknown",
    age_num <= 17            ~ "0-17",
    age_num >= 18 & age_num <= 29 ~ "18-29",
    age_num >= 30 & age_num <= 49 ~ "30-49",
    age_num >= 50            ~ "50+",
    TRUE                      ~ "Unknown"
  )
}

# NOTE: homicides_sf already carries Year/WardKey/Investigating.Agency/
# Circumstance/Latitude/Longitude at the victim-row level (each victim row
# for a given UUID shares the same incident-level values from the raw CSV),
# so no join against homicides_incidents is needed here - joining on UUID
# while also having those same column names present on both sides would
# collide and get column-suffixed (e.g. Year.x/Year.y), which is what
# caused the "object 'Year' not found" error.
victim_summary_df <- homicides_sf |>
  st_drop_geometry() |>
  filter(Year %in% all_years) |>
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    AgeDisplay = ifelse(is.na(Age) | Age %in% c("N/A", "Unknown", ""), "Unknown", Age),
    AgeGroup = age_group_bucket(Age),
    RaceDisplay = ifelse(is.na(Race) | Race %in% c("", "N/A"), "Unknown", Race),
    CaseStatusDisplay = ifelse(is.na(Case.Status) | Case.Status == "", "Unsolved", Case.Status),
    DisplayName = ifelse(is.na(Victim) | Victim == "", "Unidentified Victim", Victim),
    CoverageUrl = ifelse(is.na(URL) | URL == "", NA_character_, URL)
  ) |>
  transmute(
    UUID = UUID,
    Year = as.character(Year),
    Agency = Investigating.Agency,
    Circumstance = Circumstance,
    WardKey = WardKey,
    Latitude = Latitude,
    Longitude = Longitude,
    Age = AgeDisplay,
    AgeGroup = AgeGroup,
    Race = RaceDisplay,
    CaseStatus = CaseStatusDisplay,
    Name = DisplayName,
    Date = format(Date, "%m/%d/%Y"),
    Address = Address,
    CoverageUrl = CoverageUrl
  )

victim_summary_records <- lapply(seq_len(nrow(victim_summary_df)), function(i) {
  list(
    UUID = victim_summary_df$UUID[i],
    Year = victim_summary_df$Year[i],
    Agency = victim_summary_df$Agency[i],
    Circumstance = victim_summary_df$Circumstance[i],
    WardKey = victim_summary_df$WardKey[i],
    Latitude = victim_summary_df$Latitude[i],
    Longitude = victim_summary_df$Longitude[i],
    Age = victim_summary_df$Age[i],
    AgeGroup = victim_summary_df$AgeGroup[i],
    Race = victim_summary_df$Race[i],
    CaseStatus = victim_summary_df$CaseStatus[i],
    Name = victim_summary_df$Name[i],
    Date = victim_summary_df$Date[i],
    Address = victim_summary_df$Address[i],
    CoverageUrl = victim_summary_df$CoverageUrl[i]
  )
})

# =============================================================================
# Agency/Circumstance filter control HTML (populated dynamically by JS)
# =============================================================================

agency_control_html <- "
<div class='sidebar-section sidebar-section-small'>
  <strong>Agency</strong><br/>
  <div id='agency-checkboxes' class='sidebar-scroll sidebar-scroll-small'></div>
</div>
"

circumstance_control_html <- "
<div class='sidebar-section sidebar-section-small'>
  <strong>Circumstance</strong><br/>
  <div id='circumstance-checkboxes' class='sidebar-scroll sidebar-scroll-small'></div>
</div>
"

trends_control_html <- paste0("
<div class='sidebar-section'>
  <strong>City-Wide Trends</strong><br/>
  <div class='trends-row'>
    <span>", CURRENT_YEAR, " Homicide Incidents to Date:</span>
    <span class='trends-value'>", citywide_ytd_current, "</span>
  </div>
  <div class='trends-row'>
    <span>Change vs. Last Year:</span>
    <span class='trends-value'>", citywide_pct_change_label, "</span>
  </div>
  <div class='trends-row'>
    <span>", current_month_label, ":</span>
    <span class='trends-value'>", current_month_count, "</span>
  </div>
  <div class='trends-row'>
    <span>Last 30 Days:</span>
    <span class='trends-value'>", last_30_days_count, "</span>
  </div>
  <div class='trends-row'>
    <span>", month_to_date_label, " 5 Year Average:</span>
    <span class='trends-value'>", five_year_avg_label, "</span>
  </div>
</div>
")

days_since_last_homicide <- as.integer(TODAY - max(homicides_incidents$Date, na.rm = TRUE))

header_html <- paste0("
<div class='dashboard-header'>
  <div class='header-main'>
    <div class='header-title'>
      <div class='header-title-main'>Jackson's Homicides</div>
      <div class='header-title-sub'>A Public Safety Tracker</div>
    </div>
  </div>
  <div class='header-right'>
    <img src='visuals/wlbtinv.png' class='header-logo' alt='WLBT3 Investigates'>
  </div>
  <div class='header-stats'>
    <div class='header-stat header-stat-count'>", CURRENT_YEAR, " Count: ", citywide_ytd_current, "</div>
    <div class='header-stat header-stat-days'>", days_since_last_homicide, " Days Since Last Homicide</div>
  </div>
</div>
")

# =============================================================================
# Search control HTML (address/zip + radius + go/clear)
# =============================================================================

search_control_html <- "
<div class='sidebar-section'>
  <strong>Search for a Location:</strong><br/>
  <input type='text' id='search-address' placeholder='Address or Zip Code...'>
  <select id='search-radius'>
    <option value='0.25'>0.25 Mile</option>
    <option value='0.5'>0.5 Mile</option>
    <option value='1' selected>1 Mile</option>
    <option value='3'>3 Miles</option>
    <option value='5'>5 Miles</option>
  </select>
  <div id='search-error'></div>
  <div id='search-matched'></div>
  <button id='search-go-btn'>Go</button>
  <button id='search-clear-btn'>Clear Search &amp; Reset Map</button>
</div>
"

# =============================================================================
# Charts/Tabs HTML (Key Trends / Detailed Breakdowns / Victims, below the map)
# =============================================================================

charts_html <- "
<div class='dashboard-charts'>
  <div class='charts-tabs'>
    <button class='charts-tab-btn active' data-tab='key-trends'>Key Trends</button>
    <button class='charts-tab-btn' data-tab='detailed-breakdowns'>Detailed Breakdowns</button>
    <button class='charts-tab-btn' data-tab='victims'>Victims</button>
  </div>

  <div class='charts-tab-panel active' id='tab-key-trends'>
    <div class='charts-grid charts-grid-2col'>
      <div class='chart-card'>
        <div class='chart-card-title'>Cumulative Homicides by Year</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-cumulative-year'></canvas></div>
      </div>
      <div class='chart-card'>
        <div class='chart-card-title'>Homicides by Year</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-homicides-year'></canvas></div>
      </div>
    </div>
  </div>

  <div class='charts-tab-panel' id='tab-detailed-breakdowns'>
    <div class='charts-grid'>
      <div class='chart-card'>
        <div class='chart-card-title'>Victims by Age Group</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-age-group'></canvas></div>
      </div>
      <div class='chart-card'>
        <div class='chart-card-title'>Victims by Race</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-race'></canvas></div>
      </div>
      <div class='chart-card'>
        <div class='chart-card-title'>Homicides by Circumstance</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-circumstance'></canvas></div>
      </div>
      <div class='chart-card'>
        <div class='chart-card-title'>Homicides by Agency</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-agency'></canvas></div>
      </div>
      <div class='chart-card'>
        <div class='chart-card-title'>Homicides by Ward</div>
        <div class='chart-card-canvas-wrap'><canvas id='chart-ward'></canvas></div>
      </div>
    </div>
  </div>

  <div class='charts-tab-panel' id='tab-victims'>
    <div class='victims-table-wrap'>
      <div class='victims-search-row'>
        <input type='text' id='victims-search' placeholder='Search by name or address...'>
        <span class='victims-count-label' id='victims-count-label'>0 victims shown</span>
      </div>
      <div class='victims-table-scroll'>
        <table class='victims-table'>
          <thead>
            <tr>
              <th>Name</th><th>Age</th><th>Date</th><th>Address</th><th>Status</th><th>Coverage</th>
            </tr>
          </thead>
          <tbody id='victims-table-body'></tbody>
        </table>
      </div>
      <div style='font-size:11px; color:#888; margin-top:8px;'>
        Reflects the filters and any search radius currently applied to the map. Names appear as recorded by investigating agencies.
      </div>
    </div>
  </div>
</div>
"

# =============================================================================
# Legend HTML
# =============================================================================

velocity_rows <- paste0(
  "<span style='display:inline-block; width:14px; height:14px; background:",
  unname(color_lookup[legend_order]),
  "; border:1px solid #999; vertical-align:middle; margin-right:6px;'></span>",
  c("High Increase (>20%)", "Increase (5-20%)", "Stable (+/- 5%)",
    "Decrease (5-20%)", "High Decrease (>20%)", "No Data"),
  collapse = "<br/>"
)

legend_html <- paste0("
<style>
.custom-legend-box {
  background:white; padding:8px 10px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; line-height:1.6; transform: scale(0.8); transform-origin: top left;
}
.leaflet-control.custom-legend-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
.map-summary-box {
  background:white; padding:8px 10px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:14px; line-height:1.3; width:170px; max-width:170px;
  word-wrap:break-word; overflow-wrap:break-word; text-align:center;
  transform: scale(0.8); transform-origin: top right;
}
.map-summary-box .summary-block-left { text-align:left; }
.summary-count { font-size:24px; font-weight:bold; }
.summary-subcount { font-size:12px; color:#333; margin-top:2px; }
.summary-location-note { color:#2c7be5; font-size:13px; margin-bottom:4px; }
.summary-table { width:100%; border-collapse:collapse; font-size:13px; margin-top:2px; }
.summary-table th { font-size:11px; font-weight:normal; color:#666; text-align:right; }
.summary-table th:first-child { text-align:left; }
.summary-table td { padding:1px 0; }
.leaflet-control.map-summary-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
</style>
<div class='custom-legend-box'>
  <strong>Legend</strong><br/>
  <span style='display:inline-block; width:14px; height:14px; border-radius:50%; background:#B22222; border:1px solid #8B0000; vertical-align:middle; margin-right:6px;'></span>
  Homicide (Unsolved)<br/>
  <span style='display:inline-block; width:14px; height:14px; border-radius:50%; background:#1a2b48; border:1px solid #0d1a2e; vertical-align:middle; margin-right:6px;'></span>
  Homicide (Solved)<br/>
  <span style='display:inline-block; width:20px; height:0; border-top:3px solid #000000; vertical-align:middle; margin-right:6px;'></span>
  City Limits<br/>
  <span style='display:inline-block; width:20px; height:0; border-top:3px solid #A020F0; vertical-align:middle; margin-right:6px;'></span>
  CCID Boundary<br/>
  <hr style='margin:6px 0;'>
  <strong>Year Over Year Trend</strong><br/>
  ", velocity_rows, "
</div>
")

# =============================================================================
# Bounding box (used to constrain address search to the map's coverage area)
# =============================================================================

map_bbox <- st_bbox(Wards)

# =============================================================================
# Render payload for the JS side
# =============================================================================

render_payload <- list(
  wards            = ward_geojson_by_year,
  incidents        = incident_geojson_by_year,
  no_comparison    = no_comparison_geojson,
  current_year     = as.character(CURRENT_YEAR),
  incident_summary = incident_summary_records,
  victim_summary   = victim_summary_records,
  cumulative_by_year      = cumulative_by_year,
  homicides_by_year       = homicides_by_year_summary,
  today_doy               = today_doy_label,
  bbox = list(
    west  = unname(map_bbox["xmin"]),
    south = unname(map_bbox["ymin"]),
    east  = unname(map_bbox["xmax"]),
    north = unname(map_bbox["ymax"])
  )
)

# =============================================================================
# CONSOLIDATED MAP
#   Pane order (low -> high): wardsPane < cityPane < incidentsPane
#   Ward layer and incident layers are both drawn dynamically via
#   onRender()/L.geoJSON(), swapped per the selected year(s)/filters.
#   Boundaries are non-interactive so they can never intercept a click
#   meant for a ward polygon underneath.
#   The YoY status text is a plain HTML element living OUTSIDE the Leaflet
#   map (in the outer page, absolutely positioned against .dashboard-map),
#   not a Leaflet addControl(). This avoids Leaflet's .leaflet-top pane
#   re-asserting its own sizing/positioning on re-render, which is what
#   broke the previous bottom-anchored mobile attempt.
# =============================================================================

map <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 16, zoomControl = FALSE)) |>
  addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(attribution = "© OpenStreetMap")) |>
  addMapPane("wardsPane",     zIndex = 650) |>
  addMapPane("cityPane",      zIndex = 670) |>
  addMapPane("ccidPane",      zIndex = 680) |>
  addMapPane("incidentsPane", zIndex = 690) |>
  addPolygons(
    data = city_boundary_sf, color = "#000000", fillOpacity = 0, weight = 2,
    options = pathOptions(pane = "cityPane", interactive = FALSE)
  ) |>
  addPolygons(
    data = CCID_boundary_sf, color = "#A020F0", fillOpacity = 0, weight = 3, opacity = 1,
    options = pathOptions(pane = "ccidPane", interactive = FALSE)
  ) |>
  addControl(html = legend_html, position = "topleft", className = "custom-legend-wrap") |>
  addControl(
    html = "<div id='map-summary' class='map-summary-box'>Loading summary...</div>",
    position = "topright",
    className = "map-summary-wrap"
  ) |>
  htmlwidgets::onRender(
    "
    function(el, x, data) {
      var map = this;

      L.control.zoom({ position: 'bottomright' }).addTo(map);
      map.attributionControl.setPrefix(false);

      window.wardDataByYear = data.wards;
      window.incidentDataByYear = data.incidents;
      window.currentWardLayer = null;
      window.incidentLayersByYear = {};

      window.wardsRenderer = L.svg({ pane: 'wardsPane' });
      window.incidentsRenderer = L.svg({ pane: 'incidentsPane' });

      window.drawWardYear = function(year) {
        if (window.currentWardLayer) {
          map.removeLayer(window.currentWardLayer);
          window.currentWardLayer = null;
        }

        var geoText = window.wardDataByYear[String(year)];
        if (!geoText) {
          console.error('No ward data found for year', year);
          return;
        }
        var geo = JSON.parse(geoText);

        window.currentWardLayer = L.geoJSON(geo, {
          pane: 'wardsPane',
          renderer: window.wardsRenderer,
          style: function(feature) {
            var p = feature.properties || {};
            return {
              fillColor: p.fill_color || '#BDBDBD',
              fillOpacity: 0.65,
              color: '#444444',
              opacity: 1,
              weight: 1.5
            };
          },
          onEachFeature: function(feature, layer) {
            var p = feature.properties || {};
            if (p.popup_html) { layer.bindPopup(p.popup_html); }
            if (p.Name) {
              layer.bindTooltip(String(p.Name), {
                pane: 'incidentsPane',
                direction: 'top',
                sticky: true
              });
            }
          }
        });

        window.currentWardLayer.addTo(map);
      };

      window.getOrBuildIncidentLayer = function(year) {
        var key = String(year);

        if (window.incidentLayersByYear[key]) {
          return window.incidentLayersByYear[key];
        }

        var geoText = window.incidentDataByYear[key];
        if (!geoText) {
          return null;
        }
        var geo = JSON.parse(geoText);

        var layer = L.geoJSON(geo, {
          pane: 'incidentsPane',
          pointToLayer: function(feature, latlng) {
            var p = feature.properties || {};
            var solved = p.is_solved === true || p.is_solved === 'true' || p.is_solved === 1;
            return L.circleMarker(latlng, {
              pane: 'incidentsPane',
              renderer: window.incidentsRenderer,
              radius: 4,
              color: solved ? '#0d1a2e' : '#8B0000',
              fillColor: solved ? '#1a2b48' : '#B22222',
              fillOpacity: 0.9,
              weight: 1,
              stroke: true
            });
          },
          onEachFeature: function(feature, layer) {
            var p = feature.properties || {};
            if (p.popup_html) { layer.bindPopup(p.popup_html); }
          }
        });

        window.incidentLayersByYear[key] = layer;
        return layer;
      };

      window.getCheckedValues = function(selector) {
        var checked = [];
        document.querySelectorAll(selector).forEach(function(cb) {
          checked.push(cb.value);
        });
        return checked;
      };

      window.getCheckedYears = function() {
        var checked = [];
        window.getCheckedValues('.year-toggle:checked').forEach(function(v) {
          var n = parseInt(v, 10);
          if (!isNaN(n) && checked.indexOf(n) === -1) {
            checked.push(n);
          }
        });
        return checked;
      };

      window.buildFilterCheckboxes = function(containerId, className, values, priorChecked) {
        var container = document.getElementById(containerId);
        if (!container) return;

        var html = values.map(function(v) {
          var isChecked = priorChecked.hasOwnProperty(v) ? priorChecked[v] : true;
          return '<label style=\"display:block; font-weight:normal;\">' +
            '<input type=\"checkbox\" class=\"' + className + '\" value=\"' + v + '\"' +
            (isChecked ? ' checked' : '') + '> ' + v + '</label>';
        }).join('');

        container.innerHTML = html;
      };

      window.refreshFilterOptions = function() {
        var checkedYears = window.getCheckedYears().map(String);

        var priorAgencies = {};
        document.querySelectorAll('.agency-toggle').forEach(function(cb) {
          priorAgencies[cb.value] = cb.checked;
        });

        var priorCircumstances = {};
        document.querySelectorAll('.circumstance-toggle').forEach(function(cb) {
          priorCircumstances[cb.value] = cb.checked;
        });

        var relevant = data.incident_summary.filter(function(r) {
          return checkedYears.indexOf(r.Year) !== -1;
        });

        var agencySet = {};
        var circumstanceSet = {};
        relevant.forEach(function(r) {
          agencySet[r.Agency] = true;
          circumstanceSet[r.Circumstance] = true;
        });

        var agencies = Object.keys(agencySet).sort();
        var circumstances = Object.keys(circumstanceSet).sort();

        window.buildFilterCheckboxes('agency-checkboxes', 'agency-toggle', agencies, priorAgencies);
        window.buildFilterCheckboxes('circumstance-checkboxes', 'circumstance-toggle', circumstances, priorCircumstances);

        document.querySelectorAll('.agency-toggle, .circumstance-toggle').forEach(function(cb) {
          cb.addEventListener('change', window.onAnyFilterChange);
        });
      };

      window.haversineMiles = function(lat1, lng1, lat2, lng2) {
        var R = 3958.8;
        var dLat = (lat2 - lat1) * Math.PI / 180;
        var dLng = (lng2 - lng1) * Math.PI / 180;
        var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
          Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
          Math.sin(dLng / 2) * Math.sin(dLng / 2);
        var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        return R * c;
      };

      window.isWithinSearchBuffer = function(lat, lng) {
        if (!window.searchState.active) { return true; }
        var d = window.haversineMiles(lat, lng, window.searchState.lat, window.searchState.lng);
        return d <= window.searchState.radiusMiles;
      };

      window.refreshIncidentVisibility = function() {
        var checkedYears = window.getCheckedYears().map(String);
        var checkedAgencies = window.getCheckedValues('.agency-toggle:checked');
        var checkedCircumstances = window.getCheckedValues('.circumstance-toggle:checked');

        checkedYears.forEach(function(year) {
          var layer = window.getOrBuildIncidentLayer(year);
          if (!layer) return;
          if (!map.hasLayer(layer)) { layer.addTo(map); }
        });

        Object.keys(window.incidentLayersByYear).forEach(function(year) {
          var layer = window.incidentLayersByYear[year];
          var yearIsChecked = checkedYears.indexOf(year) !== -1;

          if (!yearIsChecked) {
            if (map.hasLayer(layer)) { map.removeLayer(layer); }
            return;
          }

          layer.eachLayer(function(sub) {
            var p = sub.feature.properties || {};
            var latlng = sub.getLatLng();
            var matches =
              checkedAgencies.indexOf(p.agency) !== -1 &&
              checkedCircumstances.indexOf(p.circumstance) !== -1 &&
              window.isWithinSearchBuffer(latlng.lat, latlng.lng);

            sub.setStyle({
              opacity: matches ? 1 : 0,
              fillOpacity: matches ? 0.9 : 0
            });
            if (sub._path) {
              sub._path.style.pointerEvents = matches ? 'auto' : 'none';
            }
          });
        });
      };

      window.findMostRecentConsecutivePair = function(checkedYears) {
        var sorted = checkedYears.slice().sort(function(a, b) { return b - a; });

        for (var i = 0; i < sorted.length; i++) {
          var y = sorted[i];
          if (sorted.indexOf(y - 1) !== -1) {
            return y;
          }
        }

        var currentYearNum = parseInt(data.current_year, 10);
        if (sorted.indexOf(currentYearNum) !== -1) {
          return currentYearNum;
        }

        return null;
      };

      window.drawNoComparisonWards = function() {
        if (window.currentWardLayer) {
          map.removeLayer(window.currentWardLayer);
          window.currentWardLayer = null;
        }

        var geo = JSON.parse(data.no_comparison);

        window.currentWardLayer = L.geoJSON(geo, {
          pane: 'wardsPane',
          renderer: window.wardsRenderer,
          style: function(feature) {
            var p = feature.properties || {};
            return {
              fillColor: p.fill_color || '#BDBDBD',
              fillOpacity: 0.65,
              color: '#444444',
              opacity: 1,
              weight: 1.5
            };
          },
          onEachFeature: function(feature, layer) {
            var p = feature.properties || {};
            if (p.popup_html) { layer.bindPopup(p.popup_html); }
            if (p.Name) {
              layer.bindTooltip(String(p.Name), {
                pane: 'incidentsPane',
                direction: 'top',
                sticky: true
              });
            }
          }
        });

        window.currentWardLayer.addTo(map);
      };

      window.updateYoyStatusText = function(text) {
        var el = document.getElementById('yoy-status');
        if (el) { el.innerHTML = text; }
      };

      window.updateMapSummary = function() {
        var checkedYears = window.getCheckedYears().map(String);
        var checkedAgencies = window.getCheckedValues('.agency-toggle:checked');
        var checkedCircumstances = window.getCheckedValues('.circumstance-toggle:checked');

        var records = data.incident_summary.filter(function(r) {
          var passesFilters = checkedYears.indexOf(r.Year) !== -1 &&
            checkedAgencies.indexOf(r.Agency) !== -1 &&
            checkedCircumstances.indexOf(r.Circumstance) !== -1;

          if (!passesFilters) { return false; }

          if (window.searchState.active) {
            if (r.Latitude === null || r.Longitude === null ||
                typeof r.Latitude === 'undefined' || typeof r.Longitude === 'undefined') {
              return false;
            }
            return window.isWithinSearchBuffer(r.Latitude, r.Longitude);
          }

          return true;
        });

        var byYear = {};
        var byAgency = {};
        var totalVictims = 0;
        records.forEach(function(r) {
          var vc = r.VictimCount || 1;
          totalVictims += vc;

          if (!byYear[r.Year]) { byYear[r.Year] = { incidents: 0, victims: 0 }; }
          byYear[r.Year].incidents += 1;
          byYear[r.Year].victims += vc;

          if (!byAgency[r.Agency]) { byAgency[r.Agency] = { incidents: 0, victims: 0 }; }
          byAgency[r.Agency].incidents += 1;
          byAgency[r.Agency].victims += vc;
        });

        var currentYearNum = parseInt(data.current_year, 10);

        var yearLabels = checkedYears.sort(function(a, b) { return b - a; });
        var yearDisplayLabels = yearLabels.map(function(y) {
          return (parseInt(y, 10) === currentYearNum) ? (y + ' (YTD)') : y;
        });
        var yearRowsHtml = yearLabels.map(function(y, i) {
          var d = byYear[y] || { incidents: 0, victims: 0 };
          return '<tr><td style=\"text-align:left;\">' + yearDisplayLabels[i] + '</td>' +
            '<td style=\"text-align:right;\">' + d.incidents + '</td>' +
            '<td style=\"text-align:right;\">' + d.victims + '</td></tr>';
        }).join('');
        var yearTableHtml = '<table class=\"summary-table\">' +
          '<tr><th></th><th>Inc.</th><th>Vic.</th></tr>' + yearRowsHtml + '</table>';

        var agencyLabels = Object.keys(byAgency).sort(function(a, b) {
          return byAgency[b].incidents - byAgency[a].incidents;
        });
        var agencyRowsHtml = agencyLabels.map(function(a) {
          var d = byAgency[a];
          return '<tr><td style=\"text-align:left;\">' + a + '</td>' +
            '<td style=\"text-align:right;\">' + d.incidents + '</td>' +
            '<td style=\"text-align:right;\">' + d.victims + '</td></tr>';
        }).join('');
        var agencyTableHtml = '<table class=\"summary-table\">' +
          '<tr><th></th><th>Inc.</th><th>Vic.</th></tr>' + agencyRowsHtml + '</table>';

        var locationNote = '';
        if (window.searchState.active) {
          locationNote =
            '<div class=\"summary-location-note\">Showing incidents within ' +
            window.searchState.radiusMiles + ' mile(s) of search location.</div>';
        }

        var html =
          locationNote +
          '<div class=\"summary-count\">' + records.length + '</div>' +
          'Homicide Incidents Shown' +
          '<div class=\"summary-subcount\">Total Victims: ' + totalVictims + '</div>' +
          '<div class=\"summary-block-left\">' +
          '<hr style=\"margin:4px 0;\">' +
          '<strong>By Year:</strong>' + (yearLabels.length ? yearTableHtml : '<br/>None selected') +
          '<hr style=\"margin:4px 0;\">' +
          '<strong>By Agency:</strong>' + (agencyLabels.length ? agencyTableHtml : '<br/>None selected') +
          '</div>';

        var el = document.getElementById('map-summary');
        if (el) { el.innerHTML = html; }
      };

      window.enforcePaneOrder = function() {
        if (map.getPane('wardsPane'))     { map.getPane('wardsPane').style.zIndex = 650; }
        if (map.getPane('cityPane'))      { map.getPane('cityPane').style.zIndex = 670; }
        if (map.getPane('ccidPane'))      { map.getPane('ccidPane').style.zIndex = 680; }
        if (map.getPane('incidentsPane')) { map.getPane('incidentsPane').style.zIndex = 690; }
      };

      /* =====================================================================
         TABS: Key Trends / Detailed Breakdowns / Victims
      ===================================================================== */

      window.chartsTabInit = function() {
        document.querySelectorAll('.charts-tab-btn').forEach(function(btn) {
          btn.addEventListener('click', function() {
            var tab = btn.getAttribute('data-tab');

            document.querySelectorAll('.charts-tab-btn').forEach(function(b) {
              b.classList.remove('active');
            });
            btn.classList.add('active');

            document.querySelectorAll('.charts-tab-panel').forEach(function(p) {
              p.classList.remove('active');
            });
            var panel = document.getElementById('tab-' + tab);
            if (panel) { panel.classList.add('active'); }
          });
        });
      };

      /* =====================================================================
         KEY TRENDS (static, filter-independent charts)
      ===================================================================== */

      window.chartColorForYear = function(year, isCurrent) {
        if (isCurrent) { return '#B2182B'; }
        var palette = ['#2166AC', '#4393C3', '#92C5DE', '#67A9CF', '#A6BDDB', '#D2B48C', '#999999'];
        var idx = Math.abs(parseInt(year, 10)) % palette.length;
        return palette[idx];
      };

      window.buildKeyTrendsCharts = function() {
        var cumCanvas = document.getElementById('chart-cumulative-year');
        if (cumCanvas && !window.chartCumulativeYear) {
          var years = Object.keys(data.cumulative_by_year).sort();
          var maxLen = 0;
          years.forEach(function(y) {
            maxLen = Math.max(maxLen, data.cumulative_by_year[y].values.length);
          });
          var labels = [];
          for (var d = 1; d <= maxLen; d++) { labels.push(d); }

          var datasets = years.map(function(y) {
            var series = data.cumulative_by_year[y];
            var vals = series.values.slice();
            while (vals.length < maxLen) { vals.push(null); }
            return {
              label: y + (series.is_current ? ' (YTD): ' + series.final_total : ': ' + series.final_total),
              data: vals,
              borderColor: window.chartColorForYear(y, series.is_current),
              backgroundColor: window.chartColorForYear(y, series.is_current),
              borderWidth: series.is_current ? 3 : 1.5,
              pointRadius: 0,
              tension: 0,
              spanGaps: true
            };
          });

          window.chartCumulativeYear = new Chart(cumCanvas.getContext('2d'), {
            type: 'line',
            data: { labels: labels, datasets: datasets },
            options: {
              responsive: true,
              maintainAspectRatio: false,
              interaction: { mode: 'nearest', intersect: false },
              plugins: { legend: { position: 'bottom', labels: { boxWidth: 12, font: { size: 10 } } } },
              scales: {
                x: { title: { display: true, text: 'Day of Year' }, ticks: { maxTicksLimit: 12 } },
                y: { title: { display: true, text: 'Cumulative Homicides' }, beginAtZero: true }
              }
            }
          });
        }

        var yearCanvas = document.getElementById('chart-homicides-year');
        if (yearCanvas && !window.chartHomicidesYear) {
          var yrs = data.homicides_by_year.map(function(r) { return r.year; });

          window.chartHomicidesYear = new Chart(yearCanvas.getContext('2d'), {
            data: {
              labels: yrs,
              datasets: [
                {
                  type: 'bar', label: 'JPD',
                  data: data.homicides_by_year.map(function(r) { return r.jpd; }),
                  backgroundColor: '#B2182B', stack: 'agency', yAxisID: 'y'
                },
                {
                  type: 'bar', label: 'Capitol Police',
                  data: data.homicides_by_year.map(function(r) { return r.capitol_police; }),
                  backgroundColor: '#1a2b48', stack: 'agency', yAxisID: 'y'
                },
                {
                  type: 'bar', label: 'Other Agencies',
                  data: data.homicides_by_year.map(function(r) { return r.other_agencies; }),
                  backgroundColor: '#92C5DE', stack: 'agency', yAxisID: 'y'
                },
                {
                  type: 'line', label: 'Rate per 100,000 residents',
                  data: data.homicides_by_year.map(function(r) { return r.rate_per_100k; }),
                  borderColor: '#222222', backgroundColor: '#222222',
                  borderWidth: 2, pointRadius: 3, yAxisID: 'y1', tension: 0.2
                }
              ]
            },
            options: {
              responsive: true,
              maintainAspectRatio: false,
              plugins: { legend: { position: 'bottom', labels: { boxWidth: 12, font: { size: 10 } } } },
              scales: {
                y:  { stacked: true, beginAtZero: true, position: 'left', title: { display: true, text: 'Homicide Incidents' } },
                y1: { beginAtZero: true, position: 'right', grid: { drawOnChartArea: false }, title: { display: true, text: 'per 100k' } }
              }
            }
          });
        }
      };

      /* =====================================================================
         DETAILED BREAKDOWNS (filter-responsive charts)
      ===================================================================== */

      window.getFilteredVictimRecords = function() {
        var checkedYears = window.getCheckedYears().map(String);
        var checkedAgencies = window.getCheckedValues('.agency-toggle:checked');
        var checkedCircumstances = window.getCheckedValues('.circumstance-toggle:checked');

        return data.victim_summary.filter(function(r) {
          var passesFilters = checkedYears.indexOf(r.Year) !== -1 &&
            checkedAgencies.indexOf(r.Agency) !== -1 &&
            checkedCircumstances.indexOf(r.Circumstance) !== -1;

          if (!passesFilters) { return false; }

          if (window.searchState.active) {
            if (r.Latitude === null || r.Longitude === null ||
                typeof r.Latitude === 'undefined' || typeof r.Longitude === 'undefined') {
              return false;
            }
            return window.isWithinSearchBuffer(r.Latitude, r.Longitude);
          }

          return true;
        });
      };

      window.countBy = function(records, keyFn) {
        var counts = {};
        records.forEach(function(r) {
          var k = keyFn(r);
          counts[k] = (counts[k] || 0) + 1;
        });
        return counts;
      };

      window.upsertBarChart = function(varName, canvasId, labels, values, colors, indexAxis) {
        var canvas = document.getElementById(canvasId);
        if (!canvas) { return; }

        if (window[varName]) {
          window[varName].data.labels = labels;
          window[varName].data.datasets[0].data = values;
          window[varName].data.datasets[0].backgroundColor = colors;
          window[varName].update();
          return;
        }

        window[varName] = new Chart(canvas.getContext('2d'), {
          type: 'bar',
          data: {
            labels: labels,
            datasets: [{ label: 'Count', data: values, backgroundColor: colors }]
          },
          options: {
            indexAxis: indexAxis || 'x',
            responsive: true,
            maintainAspectRatio: false,
            plugins: { legend: { display: false } },
            scales: {
              x: { beginAtZero: true },
              y: { beginAtZero: true }
            }
          }
        });
      };

      window.upsertPieChart = function(varName, canvasId, labels, values, colors) {
        var canvas = document.getElementById(canvasId);
        if (!canvas) { return; }

        if (window[varName]) {
          window[varName].data.labels = labels;
          window[varName].data.datasets[0].data = values;
          window[varName].data.datasets[0].backgroundColor = colors;
          window[varName].update();
          return;
        }

        window[varName] = new Chart(canvas.getContext('2d'), {
          type: 'pie',
          data: {
            labels: labels,
            datasets: [{ data: values, backgroundColor: colors }]
          },
          options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: { legend: { position: 'right', labels: { boxWidth: 12, font: { size: 10 } } } }
          }
        });
      };

      window.paletteFor = function(n) {
        var base = ['#1a2b48', '#B2182B', '#92C5DE', '#D2B48C', '#2166AC', '#F4A582', '#666666', '#A6BDDB'];
        var out = [];
        for (var i = 0; i < n; i++) { out.push(base[i % base.length]); }
        return out;
      };

      window.dedupeToIncidents = function(records) {
        var seen = {};
        var out = [];
        records.forEach(function(r) {
          if (!seen[r.UUID]) {
            seen[r.UUID] = true;
            out.push(r);
          }
        });
        return out;
      };

      window.updateDetailedBreakdowns = function() {
        var panel = document.getElementById('tab-detailed-breakdowns');
        if (!panel) { return; }

        var victimRecords = window.getFilteredVictimRecords();
        var incidentRecords = window.dedupeToIncidents(victimRecords);

        /* Victim-level attributes: Age and Race describe individual victims,
           so multi-victim incidents should count each victim once here. */
        var ageOrder = ['0-17', '18-29', '30-49', '50+', 'Unknown'];
        var ageCounts = window.countBy(victimRecords, function(r) { return r.AgeGroup; });
        window.upsertBarChart(
          'chartAgeGroup', 'chart-age-group',
          ageOrder, ageOrder.map(function(k) { return ageCounts[k] || 0; }),
          '#1a2b48'
        );

        var raceCounts = window.countBy(victimRecords, function(r) { return r.Race; });
        var raceLabels = Object.keys(raceCounts).sort();
        window.upsertPieChart(
          'chartRace', 'chart-race',
          raceLabels, raceLabels.map(function(k) { return raceCounts[k]; }),
          window.paletteFor(raceLabels.length)
        );

        /* Incident-level attributes: Circumstance/Agency/Ward describe the
           homicide event itself, so a multi-victim incident must count once,
           matching the incident-count convention used everywhere else on
           this dashboard (map dots, ward YoY coloring, Map Summary box). */
        var circCounts = window.countBy(incidentRecords, function(r) { return r.Circumstance; });
        var circLabels = Object.keys(circCounts).sort(function(a, b) { return circCounts[b] - circCounts[a]; });
        window.upsertBarChart(
          'chartCircumstance', 'chart-circumstance',
          circLabels, circLabels.map(function(k) { return circCounts[k]; }),
          '#B2182B'
        );

        var agencyCounts = window.countBy(incidentRecords, function(r) { return r.Agency; });
        var agencyLabels = Object.keys(agencyCounts).sort(function(a, b) { return agencyCounts[b] - agencyCounts[a]; });
        window.upsertPieChart(
          'chartAgency', 'chart-agency',
          agencyLabels, agencyLabels.map(function(k) { return agencyCounts[k]; }),
          window.paletteFor(agencyLabels.length)
        );

        var wardCounts = window.countBy(incidentRecords, function(r) { return r.WardKey || 'Unknown'; });
        var wardLabels = Object.keys(wardCounts).sort();
        window.upsertBarChart(
          'chartWard', 'chart-ward',
          wardLabels, wardLabels.map(function(k) { return wardCounts[k]; }),
          '#B2182B', 'y'
        );
      };

      /* =====================================================================
         VICTIMS TABLE (filter-responsive, plus its own name/address search)
      ===================================================================== */

      window.victimsSearchTerm = '';

      window.escapeHtml = function(str) {
        var div = document.createElement('div');
        div.textContent = str === null || typeof str === 'undefined' ? '' : String(str);
        return div.innerHTML;
      };

      window.updateVictimsTable = function() {
        var tbody = document.getElementById('victims-table-body');
        var countLabel = document.getElementById('victims-count-label');
        if (!tbody) { return; }

        var records = window.getFilteredVictimRecords();

        if (window.victimsSearchTerm) {
          var term = window.victimsSearchTerm.toLowerCase();
          records = records.filter(function(r) {
            var name = (r.Name || '').toLowerCase();
            var addr = (r.Address || '').toLowerCase();
            return name.indexOf(term) !== -1 || addr.indexOf(term) !== -1;
          });
        }

        records = records.slice().sort(function(a, b) {
          var da = new Date(a.Date), db = new Date(b.Date);
          return db - da;
        });

        if (countLabel) { countLabel.textContent = records.length + ' victim' + (records.length === 1 ? '' : 's') + ' shown'; }

        if (records.length === 0) {
          tbody.innerHTML = '<tr class=\"victims-empty-row\"><td colspan=\"6\">No victims match the current filters.</td></tr>';
          return;
        }

        var rowsHtml = records.map(function(r) {
          var statusClass = r.CaseStatus === 'Solved' ? 'victims-status-solved' : 'victims-status-unsolved';
          var coverageHtml = r.CoverageUrl
            ? '<a class=\"victims-coverage-link\" href=\"' + window.escapeHtml(r.CoverageUrl) + '\" target=\"_blank\" rel=\"noopener\">Link</a>'
            : '';
          return '<tr>' +
            '<td>' + window.escapeHtml(r.Name) + '</td>' +
            '<td>' + window.escapeHtml(r.Age) + '</td>' +
            '<td>' + window.escapeHtml(r.Date) + '</td>' +
            '<td>' + window.escapeHtml(r.Address) + '</td>' +
            '<td class=\"' + statusClass + '\">' + window.escapeHtml(r.CaseStatus) + '</td>' +
            '<td>' + coverageHtml + '</td>' +
            '</tr>';
        }).join('');

        tbody.innerHTML = rowsHtml;
      };

      window.searchState = { active: false, lat: null, lng: null, radiusMiles: null };
      window.searchBufferLayer = null;
      window.searchMarker = null;
      window.initialMapView = { center: map.getCenter(), zoom: map.getZoom() };

      window.isWithinBbox = function(lat, lng) {
        var b = data.bbox;
        return lat >= b.south && lat <= b.north && lng >= b.west && lng <= b.east;
      };

      window.showSearchError = function(msg) {
        var el = document.getElementById('search-error');
        if (!el) return;
        if (msg) {
          el.textContent = msg;
          el.style.display = 'block';
        } else {
          el.textContent = '';
          el.style.display = 'none';
        }
      };

      window.showMatchedAddress = function(displayName) {
        var el = document.getElementById('search-matched');
        if (!el) return;
        if (displayName) {
          el.textContent = 'Showing results near: ' + displayName;
          el.style.display = 'block';
        } else {
          el.textContent = '';
          el.style.display = 'none';
        }
      };

      window.milesToMeters = function(miles) {
        return miles * 1609.34;
      };

      window.geocodeAddress = function(address, onSuccess, onError) {
        var b = data.bbox;
        var viewbox = b.west + ',' + b.north + ',' + b.east + ',' + b.south;
        var url = 'https://nominatim.openstreetmap.org/search?format=json&limit=1' +
          '&viewbox=' + viewbox + '&bounded=1' +
          '&q=' + encodeURIComponent(address);

        fetch(url, { headers: { 'Accept': 'application/json' } })
          .then(function(resp) {
            if (!resp.ok) { throw new Error('Network error'); }
            return resp.json();
          })
          .then(function(results) {
            if (!results || results.length === 0) {
              onError('Address not found within the map area. Try a more specific address.');
              return;
            }

            var lat = parseFloat(results[0].lat);
            var lng = parseFloat(results[0].lon);
            var displayName = results[0].display_name || address;

            if (!window.isWithinBbox(lat, lng)) {
              onError('That address is outside the map area covered by this dashboard.');
              return;
            }

            onSuccess(lat, lng, displayName);
          })
          .catch(function(err) {
            onError('Search failed. Please try again.');
          });
      };

      window.drawSearchBuffer = function(lat, lng, radiusMiles) {
        if (window.searchBufferLayer) { map.removeLayer(window.searchBufferLayer); }
        if (window.searchMarker) { map.removeLayer(window.searchMarker); }

        window.searchBufferLayer = L.circle([lat, lng], {
          radius: window.milesToMeters(radiusMiles),
          color: '#2c7be5',
          fillColor: '#2c7be5',
          fillOpacity: 0.15,
          weight: 2
        }).addTo(map);

        window.searchMarker = L.marker([lat, lng]).addTo(map);

        map.fitBounds(window.searchBufferLayer.getBounds());
      };

      window.clearSearchBuffer = function() {
        if (window.searchBufferLayer) { map.removeLayer(window.searchBufferLayer); window.searchBufferLayer = null; }
        if (window.searchMarker) { map.removeLayer(window.searchMarker); window.searchMarker = null; }
        window.searchState = { active: false, lat: null, lng: null, radiusMiles: null };
      };

      window.runSearch = function() {
        var address = document.getElementById('search-address').value.trim();
        var radiusMiles = parseFloat(document.getElementById('search-radius').value);

        window.showSearchError(null);

        if (!address) {
          window.showSearchError('Please enter an address or zip code.');
          return;
        }

        window.showMatchedAddress(null);

        window.geocodeAddress(address, function(lat, lng, displayName) {
          window.searchState = { active: true, lat: lat, lng: lng, radiusMiles: radiusMiles };
          window.drawSearchBuffer(lat, lng, radiusMiles);
          window.showMatchedAddress(displayName);
          window.onAnyFilterChange();
        }, function(errMsg) {
          window.showSearchError(errMsg);
        });
      };

      window.resetSearch = function() {
        document.getElementById('search-address').value = '';
        window.showSearchError(null);
        window.showMatchedAddress(null);
        window.clearSearchBuffer();
        map.setView(window.initialMapView.center, window.initialMapView.zoom);
        window.onAnyFilterChange();
      };

      window.refreshWardComparison = function() {
        var checkedYears = window.getCheckedYears();
        var pairYear = window.findMostRecentConsecutivePair(checkedYears);

        if (pairYear !== null) {
          window.drawWardYear(pairYear);
          var currentYearNum = parseInt(data.current_year, 10);
          var secondYearLabel = (pairYear === currentYearNum) ? (pairYear + ' (Year-to-Date)') : String(pairYear);
          window.updateYoyStatusText('Showing change from ' + (pairYear - 1) + ' to ' + secondYearLabel);
        } else {
          window.drawNoComparisonWards();
          window.updateYoyStatusText('Select two consecutive years to see year-over-year trends.');
        }

        window.enforcePaneOrder();
      };

      window.onAnyFilterChange = function() {
        window.refreshIncidentVisibility();
        window.refreshWardComparison();
        window.updateMapSummary();
        window.enforcePaneOrder();
        window.updateDetailedBreakdowns();
        window.updateVictimsTable();
      };

      window.onYearChange = function() {
        window.refreshFilterOptions();
        window.onAnyFilterChange();
      };

      document.querySelectorAll('.year-toggle').forEach(function(cb) {
        cb.addEventListener('change', window.onYearChange);
      });

      document.getElementById('search-go-btn').addEventListener('click', window.runSearch);
      document.getElementById('search-clear-btn').addEventListener('click', window.resetSearch);
      document.getElementById('search-address').addEventListener('keydown', function(e) {
        if (e.key === 'Enter') { window.runSearch(); }
      });
      document.getElementById('search-radius').addEventListener('change', function() {
        if (window.searchState.active) {
          var radiusMiles = parseFloat(document.getElementById('search-radius').value);
          window.searchState.radiusMiles = radiusMiles;
          window.drawSearchBuffer(window.searchState.lat, window.searchState.lng, radiusMiles);
          window.onAnyFilterChange();
        }
      });

      window.chartsTabInit();
      window.buildKeyTrendsCharts();

      var victimsSearchInput = document.getElementById('victims-search');
      if (victimsSearchInput) {
        victimsSearchInput.addEventListener('input', function() {
          window.victimsSearchTerm = victimsSearchInput.value.trim();
          window.updateVictimsTable();
        });
      }

      window.refreshFilterOptions();
      window.onAnyFilterChange();
    }
    ",
    data = render_payload
  )

sidebar_css <- "
:root { --header-height: 64px; --dashboard-max-width: 1600px; }
* { box-sizing:border-box; }
body { margin:0; padding:0; font-family: Arial, sans-serif; background:#e9e9e9; }
.dashboard-page {
  display:flex; flex-direction:column; min-height:100vh; max-width:var(--dashboard-max-width);
  margin:0 auto; overflow:visible; background:white; box-shadow:0 0 12px rgba(0,0,0,0.15);
}

.dashboard-header {
  background:#ffffff; height:var(--header-height); min-height:var(--header-height);
  display:flex; align-items:center; justify-content:space-between; gap:24px;
  padding:0 16px; border-bottom:1px solid #ddd; box-sizing:border-box; flex-shrink:0;
}
.header-main {
  display:flex; align-items:center; gap:20px; min-width:0; flex-wrap:wrap; flex-shrink:0;
  order:1;
}
.header-title { flex-shrink:0; }
.header-right { display:flex; align-items:center; flex-shrink:0; order:3; margin-left:auto; }
.header-logo { height:40px; width:auto; }
.header-title-main { font-weight:bold; font-size:22px; line-height:1.1; white-space:nowrap; }
.header-title-sub { font-size:12px; color:#666; white-space:nowrap; }
.header-stats {
  display:flex; align-items:center; gap:10px; flex-wrap:wrap;
  justify-content:flex-start; order:2; flex-shrink:0;
}
.header-stat {
  font-size:12px; font-weight:bold; padding:6px 10px; border-radius:4px; white-space:nowrap;
}
.header-stat-count { background:#B2182B; color:white; }
.header-stat-days { background:#1a2b48; color:white; }

.dashboard-layout {
  display:flex; flex-direction:row;
  height:calc(100vh - var(--header-height)); width:100%;
  flex-shrink:0; min-height:0;
}
.dashboard-sidebar {
  width:22%; min-width:230px; max-width:320px; height:100%; overflow-y:auto;
  background:#f7f7f7; border-right:1px solid #ddd; padding:10px; box-sizing:border-box;
  flex-shrink:0;
}
.dashboard-map {
  flex-grow:1; height:100%; min-width:0; position:relative;
}
.dashboard-map .html-widget { height:100% !important; width:100% !important; }

.dashboard-map-yoy-status {
  position:absolute; left:50%; top:6px; transform:translateX(-50%);
  z-index:1000; pointer-events:none;
}
.dashboard-map-yoy-status .yoy-status-box { pointer-events:auto; }
.yoy-status-box {
  background:white; padding:8px 16px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; font-weight:bold; white-space:nowrap; display:inline-block;
}

@media (max-width: 900px) {
  .dashboard-page { height:auto; overflow:auto; max-width:100%; box-shadow:none; }
  .dashboard-header {
    display:flex; flex-wrap:wrap; height:auto; padding:10px 12px; gap:0;
    align-items:flex-start; justify-content:space-between;
  }
  .header-main { flex-wrap:wrap; gap:8px; order:1; flex-basis:70%; }
  .header-title { order:1; }
  .header-title-main { font-size:16px; }
  .header-title-sub { font-size:10px; }
  .header-right { order:2; flex-basis:25%; justify-content:flex-end; margin-left:0; }
  .header-logo { height:28px; }
  .header-stats {
    order:3; flex-basis:100%; width:100%; display:flex; flex-direction:column;
    align-items:stretch; gap:6px; margin-top:6px; justify-content:flex-start;
  }
  .header-stat { width:100%; box-sizing:border-box; text-align:center; margin:0; }
  .dashboard-layout { flex-direction:column; height:auto; }
  .dashboard-sidebar {
    width:100%; max-width:100%; height:auto; max-height:none;
    border-right:none; border-bottom:1px solid #ddd; order:2; padding:14px;
  }
  .sidebar-section { padding:12px 14px; margin-bottom:14px; font-size:14px; }
  .sidebar-scroll, .sidebar-scroll-small { max-height:110px; }
  .dashboard-map { width:100%; height:60vh; min-height:350px; order:1; }
  .custom-legend-box { transform:scale(0.65); transform-origin:top left; }
  .map-summary-box { transform:scale(0.65); transform-origin:top right; }
  .dashboard-map-yoy-status {
    top:auto; bottom:8px; left:50%; transform:translateX(-50%);
  }
  .yoy-status-box { font-size:11px; padding:5px 10px; white-space:normal; max-width:80vw; }
}
.sidebar-section {
  background:white; padding:10px 12px; margin-bottom:10px; border-radius:4px;
  box-shadow:0 1px 3px rgba(0,0,0,0.15); font-size:13px; line-height:1.6;
}
.sidebar-scroll { max-height:70px; overflow-y:auto; }
.sidebar-section-small { padding:8px 10px; }
.sidebar-scroll-small { max-height:70px; overflow-y:auto; }
.sidebar-section input[type=text] {
  width:100%; box-sizing:border-box; padding:5px; margin-bottom:6px;
  font-size:13px; border:1px solid #ccc; border-radius:3px;
}
.sidebar-section select {
  width:100%; box-sizing:border-box; padding:5px; margin-bottom:6px;
  font-size:13px; border:1px solid #ccc; border-radius:3px;
}
.sidebar-section button {
  width:100%; padding:6px; margin-bottom:6px; font-size:13px;
  border:none; border-radius:3px; cursor:pointer;
}
#search-go-btn { background:#2c7be5; color:white; }
#search-clear-btn { background:#6c757d; color:white; }
#search-error { color:#B2182B; font-size:12px; display:none; margin-bottom:6px; }
#search-matched { color:#2c7be5; font-size:12px; display:none; margin-bottom:6px; }
.trends-row {
  display:flex; justify-content:space-between; align-items:baseline;
  padding:4px 0; border-bottom:1px solid #eee; gap:8px;
}
.trends-row:last-child { border-bottom:none; }
.trends-row span:first-child { font-size:12px; color:#333; }
.trends-value { font-weight:bold; font-size:14px; white-space:nowrap; }

.dashboard-charts {
  padding:16px; background:white; border-top:1px solid #ddd;
}
.charts-tabs {
  display:flex; gap:4px; border-bottom:2px solid #eee; margin-bottom:16px; flex-wrap:wrap;
}
.charts-tab-btn {
  background:none; border:none; cursor:pointer; padding:10px 16px; font-size:14px;
  font-weight:bold; color:#555; border-bottom:3px solid transparent; margin-bottom:-2px;
}
.charts-tab-btn.active { color:#B2182B; border-bottom-color:#B2182B; }
.charts-tab-panel { display:none; }
.charts-tab-panel.active { display:block; }
.charts-grid {
  display:grid; grid-template-columns:repeat(3, 1fr); gap:16px;
}
.charts-grid-2col { grid-template-columns:repeat(2, 1fr); }
.chart-card {
  background:white; border:1px solid #eee; border-radius:6px; padding:12px;
  box-shadow:0 1px 3px rgba(0,0,0,0.08);
}
.chart-card-title {
  font-size:14px; font-weight:bold; text-align:center; margin-bottom:8px; color:#222;
}
.chart-card-canvas-wrap { position:relative; height:260px; }
.chart-card-canvas-wrap canvas { max-width:100%; }
.chart-card-toolbar {
  display:flex; justify-content:flex-end; margin-bottom:4px;
}
.chart-toggle-btn {
  font-size:11px; padding:3px 8px; border:1px solid #ccc; border-radius:3px;
  background:#f7f7f7; cursor:pointer;
}

.victims-table-wrap { }
.victims-search-row {
  display:flex; align-items:center; justify-content:space-between; gap:12px; margin-bottom:10px; flex-wrap:wrap;
}
.victims-search-row input[type=text] {
  flex-grow:1; min-width:200px; padding:7px 10px; font-size:13px; border:1px solid #ccc; border-radius:4px;
}
.victims-count-label { font-size:12px; color:#666; white-space:nowrap; }
.victims-table-scroll { max-height:480px; overflow-y:auto; border:1px solid #ddd; border-radius:4px; }
table.victims-table { width:100%; border-collapse:collapse; font-size:13px; }
table.victims-table thead th {
  background:#1a2b48; color:white; text-align:left; padding:8px 10px;
  position:sticky; top:0; font-size:12px; font-weight:bold;
}
table.victims-table tbody td { padding:6px 10px; border-bottom:1px solid #eee; }
table.victims-table tbody tr:hover { background:#f7f7f7; }
.victims-status-solved { color:#1a2b48; font-weight:bold; }
.victims-status-unsolved { color:#B2182B; font-weight:bold; }
.victims-coverage-link { color:#2c7be5; text-decoration:none; }
.victims-empty-row td { text-align:center; color:#888; padding:20px; }

@media (max-width: 900px) {
  .dashboard-charts { padding:12px; }
  .charts-grid, .charts-grid-2col { grid-template-columns:1fr; }
  .charts-tab-btn { padding:8px 10px; font-size:13px; }
  .chart-card-canvas-wrap { height:220px; }
  .victims-table-scroll { max-height:360px; }
}
"

sidebar_html <- htmltools::tags$div(
  class = "dashboard-sidebar",
  htmltools::HTML(search_control_html),
  htmltools::HTML(year_control_html),
  htmltools::HTML(agency_control_html),
  htmltools::HTML(circumstance_control_html),
  htmltools::HTML(trends_control_html)
)

page <- htmltools::tagList(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "UTF-8"),
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    htmltools::tags$title("Jackson Homicide Dashboard"),
    htmltools::tags$style(htmltools::HTML(sidebar_css)),
    htmltools::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.4/chart.umd.min.js")
  ),
  htmltools::tags$div(
    class = "dashboard-page",
    htmltools::HTML(header_html),
    htmltools::tags$div(
      class = "dashboard-layout",
      sidebar_html,
      htmltools::tags$div(
        class = "dashboard-map",
        map,
        htmltools::tags$div(
          class = "dashboard-map-yoy-status",
          htmltools::tags$div(
            id = "yoy-status",
            class = "yoy-status-box",
            "Showing change: loading..."
          )
        )
      )
    ),
    htmltools::HTML(charts_html)
  )
)

dir.create("docs", showWarnings = FALSE)
dir.create("docs/visuals", showWarnings = FALSE)
if (file.exists("visuals/wlbtinv.png")) {
  file.copy("visuals/wlbtinv.png", "docs/visuals/wlbtinv.png", overwrite = TRUE)
}

htmltools::save_html(page, file = "docs/index.html")
