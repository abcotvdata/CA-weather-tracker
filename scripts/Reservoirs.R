# libraries

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(htmlwidgets)
library(htmltools)
library(here)
library(readxl)
library(rvest)
library(janitor)
library(fauxnaif)


# reservoirs 

url <- "https://cdec.water.ca.gov/reportapp/javareports?name=RES"

webpage <- read_html(url)

table_html <- html_nodes(webpage, "table")

table_df <- html_table(table_html)[[1]]


url1 <- "https://cdec.water.ca.gov/reportapp/javareports?name=DailyRes"
webpage1 <- read_html(url1)

second_table_html <- html_nodes(webpage1, "table")

second_table_df <- html_table(second_table_html)[[1]]


table_df1 <- table_df %>%
  row_to_names(row_number = 1) %>% 
  mutate(River = `% of Average`) %>% 
  mutate(River = na_if_in(River,~ grepl("[0-9]", .))) %>% 
  mutate(River = na_if(River, "---")) %>% 
  fill(River, .direction = "downup") %>% 
  mutate(`% of Average` = replace(`% of Average`, `% of Average` == "---", 0)) %>% 
   mutate(`% of Average` = na_if_in(`% of Average`,~ grepl("[^0-9]", .))) %>% 
  drop_na(`% of Average`) %>% 
  mutate(`% of Average` = na_if(`% of Average`, "0")) %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(across(where(is.character), ~na_if(., "---")))
 


second_table_df1 <- second_table_df %>% 
    row_to_names(row_number = 1)


table_complete <- left_join(table_df1, second_table_df1, by = c("StaID" = "ID"))


table_complete1 <- table_complete %>% 
  tidyr::separate(`LATITUDE&nbsp&nbsp&nbsp`, into = c("LATITUDE", "&nbsp&nbsp&nbsp"), sep = "&") %>% 
  tidyr::separate(`LONGITUDE&nbsp&nbsp&nbsp`, into = c("LONGITUDE", "&nbsp&nbsp&nbsp1"), sep = "&") %>% 
  dplyr::select(-c(`&nbsp&nbsp&nbsp`, `&nbsp&nbsp&nbsp1`))


write.csv(table_complete1, "reservoir_table.csv", row.names = TRUE)


table_complete1$LATITUDE <- as.numeric(table_complete1$LATITUDE)
table_complete1$LONGITUDE <- as.numeric(table_complete1$LONGITUDE)
table_complete1$`% of Average` <- as.numeric(table_complete1$`% of Average`)
table_complete1$`% of Capacity` <- as.numeric(table_complete1$`% of Capacity`)

table_complete1 <- table_complete1 %>% 
    mutate(desc = case_when(`% of Average` <= 50 ~ 'Under 75%',
                                  `% of Average` <= 99 ~ '75%-100%',
                                  `% of Average` >= 100 ~ 'Over 100%'))
  
  
table_complete1$COUNTY <- str_to_title(table_complete1$COUNTY) 
table_complete1$`Reservoir Name` <- str_to_title(table_complete1$`Reservoir Name`) 
table_complete1$River <- str_to_title(table_complete1$River)

colors_reservoir <- c("#40E0D0", "#0096FF", "#3F00FF", "gray")

labels <- c("Under 50%", "50% - 100%", "Over 100%", "No data")

getColor <- function(table_complete1) {
  sapply(table_complete1$`% of Capacity`, function(`% of Capacity`) {
    if(is.na(`% of Capacity`)) {
      "gray"
    } else if(`% of Capacity` <= 50) {
      "#40E0D0"
    } else if(`% of Capacity` <= 99) {
      "#0096FF"
    } else {
      "#3F00FF" 
    } })
}

popups <- paste(sep = "",
                paste(sep = "","<font size='3'><b><p style='color:#0059F6'>", table_complete$`Reservoir Name`, " <br> ", table_complete1$`COUNTY`, " County </b><br> <font size='2'> <p style='color:black'> Capacity: <b>", table_complete1$`Capacity(AF)`," acre-feet</b>","<br> Storage: <b>", table_complete1$`Storage(AF)`, " acre-feet</b> <br> Percent of Capacity: <b>", table_complete1$`% of Capacity`,"%</b>")) %>% 
  lapply(htmltools::HTML)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    position: fixed !important;
    left: 5px;
    top: 5px;
    width: 30%;
    text-align: left;
    padding: 10px;
    color: white;
  }
  @media only screen and (max-width: 460px) {
    .leaflet-control.map-title {
      font-size: 15px;
    }
  }
"))

title <- tags$div(
  tag.map.title, HTML('<div style="font-weight: bold; font-size: 20px; padding: 10px; background:#0059F6;">Reservoir Tracker</div>')
)

tag.map.footer <- tags$style(HTML("
  .leaflet-control.map-footer {
    position: fixed !important;
    right: 0%;
    bottom: 6px;
    text-align: right;
    padding: 10px;
    background: rgba(255,255,255,0.75);
    font-style: italic;
    font-size: 10px;
  }
  @media only screen and (max-width: 460px) {
    .leaflet-control.map-footer {
      font-size: 8px;
    }
  }
"))

today_UTC <- as.POSIXct(Sys.time())
today_posix <- format(today_UTC, tz="America/Los_Angeles",usetz=TRUE)
today <- as.Date(substr(as.character(today_posix), 1,10))
today_display <- format(today, "%A, %b. %d, %Y")

footer <- tags$div(
  tag.map.footer, HTML("<div> Source: California Department of Water Resources. </div> <div>Last updated",today_display,))

reservoir_map <- leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = FALSE)) %>%
  addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = leafletOptions(pane = "maplabels", zoomControl = FALSE, minZoom = 4, maxZoom = 10, dragging = FALSE), group = "map labels") %>%
  setView(-122.1484334,37.8427456, zoom = 5.5) %>%
addCircleMarkers(lng = table_complete1$LONGITUDE, 
                 lat = table_complete1$LATITUDE, 
                 color = getColor(table_complete1),
                 fillColor = getColor(table_complete1),
                 stroke = FALSE, 
                 fillOpacity = 0.5,
                 radius = 7,
                 label = popups, 
                 labelOptions = labelOptions(
                   direction = "auto")) %>% 
 addLegend(values = table_complete1$`desc`, title = "Percent of Capacity",  position = 'bottomleft',
            #na.label = "No data",
            opacity = 1,
           colors = colors_reservoir,
           labels = labels) %>%
  addControl(footer, position = "bottomright", className="map-footer") %>% 
  addControl(title, position = "topleft", className="map-title") 

saveWidget(reservoir_map, file="reservoir_map.html")

# snowpacks

url2 <- "https://wcc.sc.egov.usda.gov/reports/UpdateReport.html?report=California/Nevada&format=SNOTEL+Snowpack+Update+Report"

webpage1 <- read_html(url2)

snowpack_table <- html_table(html_nodes(webpage1, "table")[[3]], fill = TRUE)

names(snowpack_table) <- snowpack_table[1,]
snowpack_table <- snowpack_table[-1,]
snowpack_table[snowpack_table=="Basin Index (%)"] <- NA

snowpack_table1 <- snowpack_table %>%
  drop_na(`Elev(ft)`) %>%
  mutate(Basin = `Median Peak`) %>%
  mutate(Basin = na_if_in(Basin,~ grepl("[0-9]", .))) %>%
  mutate(Basin = na_if_in(Basin,~ grepl("[*]", .))) %>%
  fill(Basin, .direction = "downup") %>%
  mutate(Basin = na_if(Basin, "NEVADA")) %>%
  drop_na(`Basin`) %>%
  rename(`Percent of Today's Median` = `Today'sMedian`) %>%
  rename(`Percent of Median Peak` = `Median Peak`) %>%
  #rename(`Basin Site Name` = `Basin  Site Name`) %>%
  rename(`Elevation` = `Elev(ft)`) %>%
  rename(`Snow Totals` = `Current(in)`) %>%
  rename(`Daily Median Snow Totals` = `Today'sMedian(in)`)

snowpack_table2 <- snowpack_table1[!grepl("^[A-Z]+(?:[ -][A-Z]+)*", snowpack_table1$`Elevation`), ]

snowpack_table2$`Daily Median Snow Totals` <- gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(snowpack_table2$`Daily Median Snow Totals`))

snowpack_table2$`Median PeakDate` <- gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(snowpack_table2$`Median PeakDate`))

snowpack_table2$`MedianPeak(in)` <- gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(snowpack_table2$`MedianPeak(in)`))


# lake tahoe

snowpack_table2_lake_tahoe <- snowpack_table2 %>%
  filter(Basin == "LAKE TAHOE")

write.csv(snowpack_table2_lake_tahoe, "lake_tahoe.csv", row.names = TRUE)

# truckee river

snowpack_table2_truckee_river <- snowpack_table2 %>%
  filter(Basin == "TRUCKEE RIVER")

write.csv(snowpack_table2_truckee_river, "truckee_river.csv", row.names = TRUE)

# carson river

snowpack_table2_carson_river <- snowpack_table2 %>%
  filter(Basin == "CARSON RIVER")

write.csv(snowpack_table2_carson_river, "carson_river.csv", row.names = TRUE)

# walker river

snowpack_table2_walker_river <- snowpack_table2 %>%
  filter(Basin == "WALKER RIVER")

write.csv(snowpack_table2_walker_river, "walker_river.csv", row.names = TRUE)

# northeast great basin

snowpack_table2_northern_great_basin <- snowpack_table2 %>%
  filter(Basin == "NORTHERN GREAT BASIN")

write.csv(snowpack_table2_northern_great_basin, "northern_great_basin.csv", row.names = TRUE)

# upper humboldt river

snowpack_table2_upper_humboldt_river <- snowpack_table2 %>%
  filter(Basin == "UPPER HUMBOLDT RIVER")

write.csv(snowpack_table2_upper_humboldt_river, "upper_humboldt_river.csv", row.names = TRUE)

# lower humboldt river

snowpack_table2_lower_humboldt_river <- snowpack_table2 %>%
  filter(Basin == "LOWER HUMBOLDT RIVER")

write.csv(snowpack_table2_lower_humboldt_river, "lower_humboldt_river.csv", row.names = TRUE)

# CLOVER VALLEY & FRANKLIN RIVER

snowpack_table2_clover_valley_franklin_river <- snowpack_table2 %>%
  filter(Basin == "CLOVER VALLEY & FRANKLIN RIVER")

write.csv(snowpack_table2_clover_valley_franklin_river, "clover_valley_franklin_river.csv", row.names = TRUE)

# snake river

snowpack_table2_snake_river <- snowpack_table2 %>%
  filter(Basin == "SNAKE RIVER")

write.csv(snowpack_table2_snake_river, "snake_river.csv", row.names = TRUE)

# owyhee river

snowpack_table2_owyhee_river <- snowpack_table2 %>%
  filter(Basin == "OWYHEE RIVER")

write.csv(snowpack_table2_owyhee_river, "owyhee_river.csv", row.names = TRUE)

# eastern nevada

snowpack_table2_eastern_nevada <- snowpack_table2 %>%
  filter(Basin == "EASTERN NEVADA")

write.csv(snowpack_table2_eastern_nevada, "eastern_nevada.csv", row.names = TRUE)

# southern nevada

snowpack_table2_southern_nevada <- snowpack_table2 %>%
  filter(Basin == "SOUTHERN NEVADA - Spring Mountains")

write.csv(snowpack_table2_southern_nevada, "southern_nevada.csv", row.names = TRUE)
