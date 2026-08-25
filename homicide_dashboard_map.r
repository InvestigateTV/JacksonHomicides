library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(stringr)
library(jsonlite)
library(leaflet)
library(readxl)

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

  inc$popup_html <- with(inc, paste0(
    "<strong>", format(Date, "%B %d, %Y"), " | ", Circumstance, "</strong><hr>",
    "Victim", ifelse(victim_count > 1, "s", ""), ": ", victim_details, "<br/>",
    "Agency: ", Investigating.Agency, "<br/>",
    "Arrest made: ", ifelse(Arrest.made == "TRUE", "Yes", "No")
  ))
  inc$agency <- inc$Investigating.Agency
  inc$circumstance <- inc$Circumstance

  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(
    inc[, c("popup_html", "agency", "circumstance")],
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

year_checkbox_rows <- paste0(
  "<label style='display:block; font-weight:normal;'>",
  "<input type='checkbox' class='year-toggle' value='", all_years, "'",
  ifelse(all_years == CURRENT_YEAR, " checked", ""),
  "> ", all_years,
  "</label>",
  collapse = ""
)

year_control_html <- paste0("
<style>
.year-control-box {
  background:white; padding:8px 10px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; line-height:1.5; max-height:220px; overflow-y:auto;
}
.leaflet-control.year-control-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
.yoy-status-box {
  background:white; padding:6px 14px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; font-weight:bold; display:inline-block;
  position:fixed; top:10px; left:50%; transform:translateX(-50%); z-index:1000;
}
.leaflet-control.yoy-status-wrap { background: transparent !important; box-shadow: none !important; border: none !important; position:static !important; }
.map-summary-box {
  background:white; padding:10px 12px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; line-height:1.5; min-width:170px;
}
.summary-count { font-size:26px; font-weight:bold; }
.leaflet-control.map-summary-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
</style>
<div class='year-control-box'>
  <strong>Years Shown</strong><br/>
  ", year_checkbox_rows, "
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
    Longitude = Longitude
  )

incident_summary_records <- lapply(seq_len(nrow(incident_summary_df)), function(i) {
  list(
    Year = incident_summary_df$Year[i],
    Agency = incident_summary_df$Agency[i],
    Circumstance = incident_summary_df$Circumstance[i],
    Latitude = incident_summary_df$Latitude[i],
    Longitude = incident_summary_df$Longitude[i]
  )
})

# =============================================================================
# Agency/Circumstance filter control HTML (populated dynamically by JS)
# =============================================================================

filter_control_html <- "
<style>
.filter-control-box {
  background:white; padding:8px 10px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; line-height:1.5; max-height:220px; overflow-y:auto; margin-top:8px;
}
.leaflet-control.filter-control-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
</style>
<div class='filter-control-box'>
  <strong>Agency</strong><br/>
  <div id='agency-checkboxes'></div>
  <hr style='margin:6px 0;'>
  <strong>Circumstance</strong><br/>
  <div id='circumstance-checkboxes'></div>
</div>
"

# =============================================================================
# Search control HTML (address/zip + radius + go/clear)
# =============================================================================

search_control_html <- "
<style>
.search-control-box {
  background:white; padding:10px 12px; box-shadow:0 1px 4px rgba(0,0,0,0.4);
  font-size:13px; line-height:1.6; width:220px;
}
.search-control-box input[type=text] {
  width:100%; box-sizing:border-box; padding:5px; margin-bottom:6px;
  font-size:13px; border:1px solid #ccc; border-radius:3px;
}
.search-control-box select {
  width:100%; box-sizing:border-box; padding:5px; margin-bottom:6px;
  font-size:13px; border:1px solid #ccc; border-radius:3px;
}
.search-control-box button {
  width:100%; padding:6px; margin-bottom:6px; font-size:13px;
  border:none; border-radius:3px; cursor:pointer;
}
#search-go-btn { background:#2c7be5; color:white; }
#search-clear-btn { background:#6c757d; color:white; }
#search-error { color:#B2182B; font-size:12px; display:none; margin-bottom:6px; }
#search-matched { color:#2c7be5; font-size:12px; display:none; margin-bottom:6px; }
.leaflet-control.search-control-wrap { background: transparent !important; box-shadow: none !important; border: none !important; }
</style>
<div class='search-control-box'>
  <strong>Search Nearby</strong><br/>
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
</style>
<div class='custom-legend-box'>
  <strong>Legend</strong><br/>
  <span style='display:inline-block; width:14px; height:14px; border-radius:50%; background:#B22222; border:1px solid #8B0000; vertical-align:middle; margin-right:6px;'></span>
  Homicide<br/>
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
# =============================================================================

map <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 16, zoomControl = FALSE)) |>
  addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(attribution = "© OpenStreetMap")) |>
  addMapPane("wardsPane",     zIndex = 650) |>
  addMapPane("cityPane",      zIndex = 670) |>
  addMapPane("incidentsPane", zIndex = 690) |>
  addPolygons(
    data = CCID_boundary_sf, color = "#A020F0", fillOpacity = 0, weight = 2,
    options = pathOptions(pane = "cityPane", interactive = FALSE)
  ) |>
  addPolygons(
    data = city_boundary_sf, color = "#000000", fillOpacity = 0, weight = 2,
    options = pathOptions(pane = "cityPane", interactive = FALSE)
  ) |>
  addControl(html = legend_html, position = "topleft", className = "custom-legend-wrap") |>
  addControl(html = year_control_html, position = "bottomleft", className = "year-control-wrap") |>
  addControl(html = filter_control_html, position = "bottomleft", className = "filter-control-wrap") |>
  addControl(html = search_control_html, position = "topleft", className = "search-control-wrap") |>
  addControl(
    html = "<div id='yoy-status' class='yoy-status-box'>Showing YoY change: loading...</div>",
    position = "topleft",
    className = "yoy-status-wrap"
  ) |>
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
            return L.circleMarker(latlng, {
              pane: 'incidentsPane',
              renderer: window.incidentsRenderer,
              radius: 4,
              color: '#8B0000',
              fillColor: '#B22222',
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
        records.forEach(function(r) {
          byYear[r.Year] = (byYear[r.Year] || 0) + 1;
          byAgency[r.Agency] = (byAgency[r.Agency] || 0) + 1;
        });

        var yearRows = checkedYears
          .sort(function(a, b) { return b - a; })
          .map(function(y) { return y + ': ' + (byYear[y] || 0); })
          .join('<br/>');

        var agencyRows = Object.keys(byAgency)
          .sort(function(a, b) { return byAgency[b] - byAgency[a]; })
          .map(function(a) { return a + ': ' + byAgency[a]; })
          .join('<br/>');

        var locationNote = '';
        if (window.searchState.active) {
          locationNote =
            '<div style=\"color:#2c7be5; font-weight:bold; margin-bottom:6px;\">Showing incidents within ' +
            window.searchState.radiusMiles + ' mile(s) of search location.</div>';
        }

        var html =
          locationNote +
          '<div class=\"summary-count\">' + records.length + '</div>' +
          'Homicides Shown<br/><br/>' +
          '<strong>By Year:</strong><br/>' + (yearRows.length ? yearRows : 'None selected') + '<br/><br/>' +
          '<strong>By Agency:</strong><br/>' + (agencyRows.length ? agencyRows : 'None selected');

        var el = document.getElementById('map-summary');
        if (el) { el.innerHTML = html; }
      };

      window.enforcePaneOrder = function() {
        if (map.getPane('wardsPane'))     { map.getPane('wardsPane').style.zIndex = 650; }
        if (map.getPane('cityPane'))      { map.getPane('cityPane').style.zIndex = 670; }
        if (map.getPane('incidentsPane')) { map.getPane('incidentsPane').style.zIndex = 690; }
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
          var periodLabel = (pairYear === currentYearNum) ? ' (Year-to-Date)' : ' (Full Year)';
          window.updateYoyStatusText('Showing YoY change: ' + (pairYear - 1) + ' &rarr; ' + pairYear + periodLabel);
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

      window.refreshFilterOptions();
      window.onAnyFilterChange();
    }
    ",
    data = render_payload
  )

dir.create("docs", showWarnings = FALSE)
htmlwidgets::saveWidget(map, file = "docs/index.html", selfcontained = TRUE)
