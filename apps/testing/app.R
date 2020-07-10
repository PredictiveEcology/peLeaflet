if (tolower(Sys.info()["sysname"])== "windows") {
  if ((nchar(Sys.getenv("OSGEO4W_ROOT")) == 0) &
      (nchar(Sys.getenv("PYTHONPATH")) == 0) &
      (nchar(Sys.getenv("PYTHONHOME")) == 0 ))
    if (!file.exists(file.path(Sys.getenv("PYTHONHOME"), "Scripts", "gdal2tiles.bat")))
      stop("Must have OSGEO installed and 3 environment variables set to the correct places for example: \n",
           "Sys.setenv('PYTHONPATH' = 'C:\\OSGeo4W64\\apps\\Python37')\n",
           "Sys.setenv('PYTHONHOME' = 'C:\\OSGeo4W64\\apps\\Python37')\n",
           "Sys.setenv('OSGEO4W_ROOT' = 'C:\\OSGeo4W64')\n",
           "... and gdal2tiles.bat located in file.path(Sys.getenv('PYTHONHOME'), 'Scripts')")
}
# gdal2tiles.bat -z 2-10 -r bilinear C:\Eliot\data/LCC2005_V1_4a_BCR6_NWT.tif C:\Eliot\data/newTiles2/
# system("gdal2tiles.bat -z 2-9 C:\\Eliot\\data/LCC2005_V1_4a_BCR6_NWT.tif C:\\Eliot\\data/newTiles3/")
Require::Require(c("shiny", "leaflet", "RColorBrewer", "sf", "reproducible", "raster", "sp", "LandR",'rmapshaper',
                   "leafgl", "map"))
options(reproducible.cachePath = "cache", reproducible.cacheSaveFormat = "qs")
readBCR6 <- function(...) {
  BCR6 <- st_read(...)
  crsOrig <- st_crs(BCR6)
  extSP <- st_as_sf(as(extent(c(xmin = -150, xmax = -80, ymin = 60, ymax = 90)), "SpatialPolygons"))
  st_crs(extSP) <- sp::CRS("+init=epsg:4326")
  BCR6 <- sf::st_transform(BCR6, crs = st_crs("+init=epsg:4326"))
  BCR6_NWT <- st_crop(BCR6, extSP)
  BCR6_NWT <- sf::st_transform(BCR6_NWT, crs = crsOrig)
  BCR6_NWT <- as(BCR6_NWT, "Spatial")
}
st_read_to_sp <- function(...) {
  out <- st_read(...)
  as(out, "Spatial")
}
prepInputsToSp <- function(...) {
  out <- Cache(prepInputs, url = "https://drive.google.com/file/d/1yChZJ1D9W141X0UXx4KMaUACRTA8CROK/view?usp=sharing",
               overwrite = TRUE, fun = "st_read", studyArea = BCR6)
  out <- out[, c("YEAR", "NFIREID", "FIRECAUS", "POLY_HA", "ADJ_HA", "AGENCY", "AFSDATE")]
  out <- Cache(ms_simplify, out)
  out <- sf::st_transform(out, sp::CRS("+init=epsg:4326"))
  out <- sf::st_cast(out, "POLYGON")
}

getFires <- function(..., studyArea) {
  system.time(fires <- Cache(prepInputs, "NFDB_point_20190801.shp", destinationPath = "c:/Eliot/data", fun = "st_read_to_sp",
                             studyArea = studyArea, useCache = TRUE, overwrite = TRUE))
  fires <- spTransform(fires, sp::CRS("+init=epsg:4326"))
  fires <- fires[fires$SIZE_HA > 1 & fires$YEAR > 1950 & 
                   fires$LATITUDE > 60 & fires$LATITUDE <= 70 & fires$LONGITUDE > -138 & fires$LONGITUDE < -90,]
}

BCR6 <- Cache(prepInputs, url = "https://drive.google.com/file/d/1sScLiFW6eaFa1knVyT7ZasBEN9niE7ua/view?usp=sharing",
              fun = "readBCR6", overwrite = TRUE, purge = 7)

LCCfilenameBase <- "LCC2005_V1_4a_NWT"
if (!dir.exists(file.path("www", LCCfilenameBase))) {
  LCC <- Cache(prepInputsLCC, filename2 = paste0(LCCfilenameBase, ".tif"))
  LCC <- Cache(prepInputsLCC, studyArea = BCR6, filename2 = paste0(LCCfilenameBase, ".tif"))
  map::makeTiles(file.path("www",LCCfilenameBase), obj = LCC, overwrite = TRUE)
  #system(paste0("python ", file.path(Sys.getenv("PYTHONHOME"), "Scripts", "gdal2tiles.py"),
  #             " -z 2-8 ", filename(LCC)," www/",LCCfilenameBase,"/"))
}

fires <- Cache(getFires, studyArea = BCR6)

nbac <- Cache(prepInputsToSp, url = "https://drive.google.com/file/d/1yChZJ1D9W141X0UXx4KMaUACRTA8CROK/view?usp=sharing",
              overwrite = TRUE, fun = "st_read", studyArea = BCR6)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 130, right = 10,
                sliderInput("range", "Fire Size", min(fires$SIZE_HA)+1, max(fires$SIZE_HA),
                            value = range(fires$SIZE_HA), step = 100
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    fires[fires$SIZE_HA >= input$range[1] & fires$SIZE_HA <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, fires$YEAR)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(fires) %>% addTiles() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addTiles(urlTemplate = paste0(LCCfilenameBase, "/{z}/{x}/{y}.png"), options = tileOptions(tms = TRUE),
               group = "Land Cover") %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("Land Cover", "Fire Points", "Fire Polygons"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(~min(LONGITUDE), ~min(LATITUDE), ~max(LONGITUDE), ~max(LATITUDE)) %>%
      hideGroup("Fire Points")
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearGroup(group = "Fire Points") %>%
      addCircles(radius = ~10^log10(SIZE_HA/10), weight = 1, color = "#777777",
                 fillColor = ~pal(YEAR), fillOpacity = 0.7, popup = ~paste(SIZE_HA), group = "Fire Points"
      )
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map") %>%
      clearGroup(group = "Fire Polygons") %>%
      addGlPolygons(data = nbac, group = "Fire Polygons", # fill = TRUE, opacity = 0.5,
                    color = ~pal(YEAR), popup = paste0("<strong>Year: </strong>", 
                                                       nbac$YEAR, 
                                                       "<br><strong> Fire Size (ha): </strong>", 
                                                       nbac$POLY_HA,
                                                       "<br><strong> Fire Cause: </strong>", 
                                                       nbac$FIRECAUS)) 
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = fires)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~YEAR,
                          labFormat = labelFormat(big.mark = "")
      )
    }
  })
}

shinyApp(ui, server)

# ui = fluidPage(leafletOutput("tilemap"))
#
#
# server <- function(input, output,session) {
#   #addResourcePath("mytiles", "c:/Eliot/data/Tutorials/Tiles")
#   output$tilemap <- renderLeaflet({
#     leaflet() %>%
#       # addTiles() %>%
#       # flyToBounds() %>%
#       flyTo(lng = -123.194773, lat = 64.431635, zoom = 4) %>%
#       addTiles(urlTemplate = "Tiles/{z}/{x}/{y}.png", options = tileOptions(tms = TRUE),
#                group = "DEM (SRTM) Vietnam") #%>%
#   })
#
# }
#
# shinyApp(ui, server)
