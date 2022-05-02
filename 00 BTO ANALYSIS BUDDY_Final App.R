library(shiny)
library(ggmap)
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(geosphere)
library(tidyverse)
library(gtools)
library(rgdal)
library(shinyWidgets)
library(plotly)
library(scales)
library(xml2)
library(DT)

MRT <- read.csv("station_data_clean.csv")
Malls <- read.csv("malls_data2_clean.csv")
Parks <- read.csv("Parks.csv")
Clinics <- read.csv("chas.csv")
Schools <- read.csv("schools.csv")
bto <- read.csv("bto_cleaned.csv")
hdb_resale <- read.csv("hdb_resale_cleaned.csv")
condo_resale <- read.csv("condo_resale_cleaned.csv")

dat <- read.csv("resaleHDB_pred.csv")
model_dat <- read.csv("model_summary_update.csv")
dat_resale <- read.csv("dat_for_predict2.csv")
rownames(model_dat) <- model_dat$X
shape_base <- readOGR(dsn = "singapore-towns", layer = "Area")

hdb_resale$Postal_Code <- str_pad(hdb_resale$Postal_Code, width = 6, side = 'left', pad = 0)
condo_resale$Postal_Code <- str_pad(condo_resale$Postal_Code, width = 6, side = 'left', pad = 0)

hdb_resale2 <- hdb_resale[order(hdb_resale$Postal_Code, hdb_resale$Address),]
postalcode_resale <- sort(unique(hdb_resale2$Postal_Code))

bto$Age <- 99
bto2 <- bto[order(bto$Property.Name),]
bto_names <- sort(unique(bto2$Property.Name))

#App building
ui <- navbarPage(img(src = 'Logo2.png', height = '32px'), id="nav",
                 
                 tabPanel("Home Page",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%")),
                          absolutePanel(id = "controls", class = "panel panel-default", fluid = TRUE,
                                        draggable = TRUE, top = "65%", left = "40%", right = "auto", bottom = "auto",
                                        width = "auto", height = "auto", h4("Selected Address"), tableOutput("locationid1")),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 100, left = 10, right = "auto", bottom = "auto",
                                        width = 500, height = "auto",
                                        
                                        h3("Filter for housing with nearby amenities"),
                                        
                                        radioButtons(inputId = "housing",
                                                     label = "Housing Type",
                                                     choices = c("BTO", "Resale HDB", "Resale Condo")),
                                        
                                        h6("Move the slider to view slected house type within a distance (in km) from amenities"),
                                        h6(tags$i("If you are uninterested in any amenities, please place slider button at '0'.")),
                                        
                                        sliderInput(inputId = "distance", 
                                                    label = tags$b("MRT :"),
                                                    min = 0, max = 5, value = 2, step = 0.5),
                                        
                                        sliderInput(inputId = "distance1", 
                                                    label = tags$b("Malls :"),
                                                    min = 0, max = 5, value = 2, step = 0.5),
                                        
                                        sliderInput(inputId = "distance2", 
                                                    label = tags$b("Parks :"),
                                                    min = 0, max = 5, value = 2, step = 0.5),
                                        
                                        sliderInput(inputId = "distance3", 
                                                    label = tags$b("Clinics :"),
                                                    min = 0, max = 5, value = 2, step = 0.5),
                                        
                                        sliderInput(inputId = "distance4", 
                                                    label = tags$b("Schools :"),
                                                    min = 0, max = 5, value = 2, step = 0.5)
                                        
                          )),
                 tabPanel("Comparison", 
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              )),
                          fluidPage(headerPanel("Comparison Table"),
                                    sidebarPanel(
                                      h4("Compare Resale HDB Flats"),
                                      selectInput("postalcode", "Select Postal Code:", postalcode_resale),
                                      actionButton("add", "Add Address"),
                                      tags$br(),
                                      h4("Compare BTO Flats"),
                                      selectInput("btoinput", "Select BTO Flat:", bto_names),
                                      actionButton("add2", "Add Address"),
                                      
                                      numericInput("row", "Input Row to Delete:", 1, min=1),
                                      actionButton("del", "Delete Row"),
                                      actionButton("clear", "Clear All"),
                                    ),
                                    mainPanel(dataTableOutput("table")),
                                    setBackgroundImage(src="https://image.freepik.com/free-vector/watercolour-background-with-light-blue-stains_23-2148525000.jpg"),
                                    tags$style(HTML(".navbar {background-image: url(https://wallpaperaccess.com/full/788126.png);}"))
                          )
                 ),
                 tabPanel("Analytics", 
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              )),
                          
                          fluidPage(headerPanel("Analyse House Characteristics"),
                                    sidebarPanel(
                                      
                                      h4("Compare factors against price"),
                                      
                                      h6("Select each factor to view the predicted effect on price:"),
                                      
                                      radioButtons(inputId = "type", label="",
                                                   choices = c("Size of house"=1, "Floor & House type"=2, "Age of house"=3,"Distance from Amenities"=4,"Region Price"=5)),
                                      
                                      conditionalPanel(condition = "input.type==5", radioButtons(inputId = "type2", label="Region Price",
                                                                                                 choices = c("Historical"=6,"Predicted"=7)))),
                                    
                                    mainPanel(conditionalPanel(condition = "input.type!=5",plotlyOutput("Plot", height = "600px")), conditionalPanel(condition = "input.type==5",leafletOutput("Plot2", height="600px")))
                                    
                          )
                 ),
                 tabPanel("Amenities",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Filter for BTO with amenities nearby"),
                                            
                                            radioButtons(inputId = "typeofflat", 
                                                         label = "Select Type of Flats",
                                                         choices = c("BTO" = 1, "HDB Resale" = 2)),
                                            
                                            conditionalPanel(condition = "input.typeofflat == 1", radioButtons(inputId = "location",
                                                         label = "BTO:",
                                                         choices = c("Kim Keat Ripples", "Toa Payoh Ridge", "Canberra Vista", "Dakota One", "Costa Grove", "Tampines GreenCrest", "Tampines GreenGlade", "Tampines GreenOpal", "Keat Hong Verge", "Bishan Towers", "Kebun Baru Edge", "Parc Residences @ Tengah", "Champions Bliss", "UrbanVille @ Woodlands"))),
                                            
                                            conditionalPanel(condition = "input.typeofflat == 2", selectInput("postalcode1", "Select Postal Code:", postalcode_resale, selected = postalcode_resale[1])),
                                            
                                            sliderInput(inputId = "distance5", 
                                                        label = tags$b("Distance :"),
                                                        min = 0, max = 2, value = 1, step = 0.5),
                                            
                                            checkboxInput(inputId = "all_amenities",
                                                          label = tags$b("Show all amenities in map"), value = FALSE)
                              )
                              
                          )
                 )
)




#Define server logic
server = function(input, output, session){
  
  emptydf <- data.frame("property.name"=character(),
                        "category"=character(),
                        "neighbourhood"=character(),
                        "address"=character(),
                        "propertytype"=character(),
                        "postal Code"=character(),
                        "Size"=character(),
                        "bedrooms"=character(),
                        "bathrooms"=character(),
                        "avg Asking Price"=character(),
                        "age" = character(),
                        "predict value"=character(),
                        "predict value 5Y"=character())
  
  mydata = emptydf
  
  bto_clean <- bto[!duplicated(bto$Property.Name), ]
  
  shape <- readOGR(dsn = "singapore-towns", layer = "Area")
  hdb_resale4 <- hdb_resale %>% group_by(HDB.Town) %>% summarise( avg = mean(Asking.Num, na.rm = TRUE) )
  df <- as.data.frame(toupper(hdb_resale4$HDB.Town))
  colnames(df) <- "Town"
  df$Num <- hdb_resale4$avg
  df2 <- df[df$Town != "BUKIT TIMAH", ]
  
  shape <- sp::merge(shape_base,df2,by.x='PLN_AREA_N',by.y='Town',all.x=T)
  
  shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80"))
  
  color_pal <- colorNumeric(palette = "inferno", domain = shapeData$Num, reverse = F)
  
 #For BTO 
  mrt_reactive_dist_bto <- reactive({
   bto2[bto2$min_MRT_dist < input$distance,]
  })
  
  malls_reactive_dist_bto <- reactive({ 
  bto2[bto2$min_malls_dist < input$distance1,]
  })
  
  parks_reactive_dist_bto <- reactive({ 
   bto2[bto2$min_parks_dist < input$distance2,]
  })
  
  clinics_reactive_dist_bto <- reactive({ 
    bto2[bto2$min_clinics_dist < input$distance3,]
  })
  
  schools_reactive_dist_bto <- reactive({ 
   bto2[bto2$min_schools_dist < input$distance4,]
  })
  
  #For hdb resale
  mrt_reactive_dist_hdb <- reactive({
   hdb_resale[hdb_resale$min_MRT_dist  < input$distance,]
  })
  
  malls_reactive_dist_hdb <- reactive({ 
  hdb_resale[hdb_resale$min_malls_dist < input$distance1,]
  })
  
  parks_reactive_dist_hdb <- reactive({ 
   hdb_resale[hdb_resale$min_parks_dist < input$distance2,]
  })
  
  clinics_reactive_dist_hdb <- reactive({ 
   hdb_resale[hdb_resale$min_clinics_dist < input$distance3,]
  })
  
  schools_reactive_dist_hdb <- reactive({ 
   hdb_resale[hdb_resale$min_schools_dist < input$distance4,]
  })
  
  #For condo resale
  mrt_reactive_dist_condo <- reactive({
  condo_resale[condo_resale$min_MRT_dist < input$distance,]
  })
  
  malls_reactive_dist_condo <- reactive({ 
   condo_resale[condo_resale$min_malls_dist < input$distance1,]
  })
  
  parks_reactive_dist_condo <- reactive({ 
    condo_resale[condo_resale$min_parks_dist < input$distance2,]
  })
  
  clinics_reactive_dist_condo <- reactive({ 
   condo_resale[condo_resale$min_clinics_dist < input$distance3,]
  })
  
  schools_reactive_dist_condo <- reactive({ 
   condo_resale[condo_resale$min_schools_dist < input$distance4,]
  })
  
  
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>% 
      addPolygons(data=shapeData, weight = 2, stroke = TRUE, smoothFactor = 0.1, fillOpacity = 0.6, fillColor = ~color_pal(Num), group="View Prices"
      )
    m <- addCircleMarkers(m, lng = bto2$Long, lat = bto2$Lat, layerId = bto2$X, color = 'red', radius = 3, popup = paste('<h4>', "Name:", bto2$Property.Name, '</h4>', "<br>", "Town:", bto2$HDB.Town,  '</h4>', "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random())' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>", "<br>",
                                                                                                                                              "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+10)' id='addlocation' type='button' class='btn btn-default action-button'>Add to Comparison</button>"))
    
    m <- addCircleMarkers(m, lng = hdb_resale$Long, lat = hdb_resale$Lat, layerId = hdb_resale$X, color = 'green', radius = 3, popup = paste('<h4>', "Name:", hdb_resale$Property.Name, '</h4>', "<br>", "Town:", hdb_resale$HDB.Town,  '</h4>', "<br>", "Address:", hdb_resale$Address, "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+2)' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>", "<br>",
                                                                                                                                                                  "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+12)' id='addlocation' type='button' class='btn btn-default action-button'>Add to Comparison</button>"))
    
    m <- m %>% 
      addCircleMarkers(data = condo_resale, lng = ~Long, lat = ~Lat, layerId = ~X ,color = 'blue', radius = 3, popup = paste(condo_resale$Property.Name, "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+4)' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>")) %>% 
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addProviderTiles("Stamen.Watercolor", group = "Watercolor") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
      addLegend(pal = color_pal,
                values  = shapeData$Num,
                position = "bottomright",
                title = "Price Ranges",
                labFormat = labelFormat(digits=1))%>%
      addLayersControl(
        baseGroups = c("Default", "Watercolor", "Toner Lite", "Terrain"),
        overlayGroups = c("View Prices"),
        options = layersControlOptions(collapsed = FALSE))
        
    if(input$housing == "BTO"){
      mrt = mrt_reactive_dist_bto()
      mall = malls_reactive_dist_bto()
      park = parks_reactive_dist_bto()
      clinic = clinics_reactive_dist_bto()
      school = schools_reactive_dist_bto()
      
      all <- data.frame()
      
      if (nrow(mrt) != 0){
        all <- mrt
      }
      
      if (nrow(mall) != 0 & nrow(all) != 0){
        all <- inner_join(all, mall)
      } else if (nrow(mall) != 0){
        all <- mall
      }
      
      if (nrow(park) != 0 & nrow(all) != 0){
        all <- inner_join(all, park)
      } else if (nrow(park) != 0){
        all <- park
      }
      
      if (nrow(clinic) != 0 & nrow(all) != 0){
        all <- inner_join(all, clinic)
      } else if (nrow(clinic) != 0){
        all <- clinic
      }
      
      if (nrow(school) != 0 & nrow(all) != 0){
        all <- inner_join(all, school)
      } else if (nrow(school) != 0){
        all <- school 
      }

      if (nrow(all) == 0){
        all <- bto2
      }
      
      m %>%
        clearMarkers %>%
        addCircleMarkers(data=all, lng = ~Long, lat = ~Lat, layerId = ~X, color = 'red', radius = 3, popup = paste('<h4>', "Name:", all$Property.Name, '</h4>', "<br>", "Town:", all$HDB.Town,  '</h4>', "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random())' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>", "<br>",
                                                                                                                                           "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+10)' id='addlocation' type='button' class='btn btn-default action-button'>Add to Comparison</button>"))
      }
    
    else if(input$housing == "Resale HDB"){
      mrt = mrt_reactive_dist_hdb()
      mall = malls_reactive_dist_hdb()
      park = parks_reactive_dist_hdb()
      clinic = clinics_reactive_dist_hdb()
      school = schools_reactive_dist_hdb()
      
      all <- data.frame()
      
      if (nrow(mrt) != 0){
        all <- mrt
      }
      
      if (nrow(mall) != 0 & nrow(all) != 0){
        all <- inner_join(all, mall)
      } else if (nrow(mall) != 0){
        all <- mall
      }
      
      if (nrow(park) != 0 & nrow(all) != 0){
        all <- inner_join(all, park)
      } else if (nrow(park) != 0){
        all <- park
      }
      
      if (nrow(clinic) != 0 & nrow(all) != 0){
        all <- inner_join(all, clinic)
      } else if (nrow(clinic) != 0){
        all <- clinic
      }
      
      if (nrow(school) != 0 & nrow(all) != 0){
        all <- inner_join(all, school)
      } else if (nrow(school) != 0){
        all <- school 
      }
      
      if (nrow(all) == 0){
        all <- hdb_resale
      }
      
      m %>%
        clearMarkers %>%
        addCircleMarkers(data=all,lng = ~Long, lat = ~Lat, layerId = ~X, color = 'green', radius = 3, popup = paste('<h4>', "Name:", all$Property.Name, '</h4>', "<br>", "Town:", all$HDB.Town,  '</h4>', "<br>", "Address:", all$Address, "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+2)' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>", "<br>",
                                                                                                                    "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+12)' id='addlocation' type='button' class='btn btn-default action-button'>Add to Comparison</button>"))
      }
    
    else {
      mrt = mrt_reactive_dist_condo()
      mall = malls_reactive_dist_condo()
      park = parks_reactive_dist_condo()
      clinic = clinics_reactive_dist_condo()
      school = schools_reactive_dist_condo()

      all <- data.frame()
      
      if (nrow(mrt) != 0){
        all <- mrt
      }
      
      if (nrow(mall) != 0 & nrow(all) != 0){
        all <- inner_join(all, mall)
      } else if (nrow(mall) != 0){
        all <- mall
      }
      
      if (nrow(park) != 0 & nrow(all) != 0){
        all <- inner_join(all, park)
      } else if (nrow(park) != 0){
        all <- park
      }
      
      if (nrow(clinic) != 0 & nrow(all) != 0){
        all <- inner_join(all, clinic)
      } else if (nrow(clinic) != 0){
        all <- clinic
      }
      
      if (nrow(school) != 0 & nrow(all) != 0){
        all <- inner_join(all, school)
      } else if (nrow(school) != 0){
        all <- school 
      }
      
      if (nrow(all) == 0){
        all <- condo_resale
      }
      
      m %>%
        clearMarkers %>%
        addCircleMarkers(data=all, lng = ~Long, lat = ~Lat, layerId = ~X, color = 'blue', radius = 3, popup = paste(all$Property.Name, "<br>", "", "<br>", "<button onclick='Shiny.onInputChange(\"button_click\", Math.random()+4)' id='selectlocation' type='button' class='btn btn-default action-button'>Select Address</button>"))
    }
        
  })

  
  id1 <- eventReactive(input$button_click, {
    if (input$button_click <= 5){
      a <- input$map_marker_click$id
      if (input$button_click <= 1){
        b <- bto[bto$X == a,c("Property.Name", "HDB.Town", "Property.Type", "Asking.Num", "Category", "Size_sqm")]
        c <- bto[bto$Property.Name == rep(b$Property.Name, nrow(bto)) & bto$X != rep(a, nrow(bto)), c("Property.Name", "HDB.Town", "Property.Type", "Asking.Num", "Category","Size_sqm")]
        d <- rbind(b, c)
        colnames(d) <- c("Property Name", "HDB Town", "Property Type", "Asking Price", "Category", "Size (sqm)")
        d
      } else if (input$button_click <= 3){
        b <- hdb_resale[hdb_resale$X == a,c("Property.Name", "HDB.Town", "Property.Type", "Size_sqm", "Category", "Address", "Postal_Code", "Asking.Num", "predict_value", "Bedrooms", "Bathrooms")]
        c <- hdb_resale[hdb_resale$Postal_Code == rep(b$Postal_Code, nrow(hdb_resale)) & hdb_resale$X != rep(a, nrow(hdb_resale)), c("Property.Name", "HDB.Town", "Property.Type", "Size_sqm", "Category", "Address", "Postal_Code", "Asking.Num", "predict_value", "Bedrooms", "Bathrooms")]
        c <- rbind(b, c)
        c <- c %>% group_by(Property.Name, HDB.Town, Property.Type, Asking.Num, Category, Address, Postal_Code, Bedrooms, Bathrooms, Size_sqm) %>% summarise(Units = n())
        colnames(c) <- c("Property Name", "HDB Town", "Property Type", "Asking Price","Category", "Address", "Postal Code", "Bedrooms", "Bathrooms", "Size (sqm)", "Units")
        c
      } else {
        b <- condo_resale[condo_resale$X == a,c("Property.Name", "District", "Property.Type", "Asking.Num", "Category", "Address", "Postal_Code", "Bedrooms", "Bathrooms")]
        c <- condo_resale[condo_resale$Postal_Code == rep(b$Postal_Code, nrow(condo_resale)) & condo_resale$X != rep(a, nrow(condo_resale)), c("Property.Name", "District", "Property.Type", "Asking.Num", "Category", "Address", "Postal_Code", "Bedrooms", "Bathrooms")]
        c <- rbind(b, c)
        c <- c %>% group_by(Property.Name, District, Property.Type, Asking.Num, Category, Address, Postal_Code, Bedrooms, Bathrooms) %>% summarise(Units = n())
        colnames(c) <- c("Property Name", "District", "Property Type", "Asking Price","Category", "Address", "Postal Code", "Bedrooms", "Bathrooms", "Units")
        c
      }
    }})
  
  output$locationid1 <- renderTable({id1()})
  
  observeEvent(input$button_click, {
    if (input$button_click >= 10){
        if (input$button_click <= 11){
          y <- bto
        } else {
          y <- hdb_resale
        }
        a <- input$map_marker_click$id
        b <- y[y$X == a, c("Property.Name", "HDB.Town", "Property.Type", "Bathrooms", "Bedrooms", "Size_sqm", "Category", "Address", "Postal_Code", "Asking.Num", "predict_value", "predict_value5Y", "Age")]
        
        if (b$Category == "BTO"){
          c <- y[y$Property.Name == rep(b$Property.Name, nrow(y)) & y$X != rep(a, nrow(y)), c("Property.Name", "HDB.Town", "Property.Type", "Bathrooms", "Bedrooms", "Size_sqm", "Category", "Address", "Postal_Code", "Asking.Num", "predict_value", "predict_value5Y", "Age")]
          
        } else if (b$Category == "Resale HDB"){
          c <- y[y$Postal_Code == rep(b$Postal_Code, nrow(y)) & y$X != rep(a, nrow(y)), c("Property.Name", "HDB.Town", "Property.Type", "Bathrooms", "Bedrooms", "Size_sqm", "Category", "Address", "Postal_Code", "Asking.Num", "predict_value", "predict_value5Y", "Age")]
        }
        
        d <- rbind(b, c)
        d <- d[!duplicated(d[, c("Asking.Num", "Property.Type", "Bedrooms", "Bathrooms", "Size_sqm", "Age")]),]
        row.names(d) <- NULL
        e <- d %>% select(Property.Name, Category, HDB.Town, Address, Property.Type, Postal_Code, Bedrooms, Bathrooms, Size_sqm, Age, predict_value, predict_value5Y, Asking.Num)
        colnames(e) <- c("Property Name", "Category", "Neighbourhood", "Address", "Property Type", "Postal Code","Bedrooms", "Bathrooms", "Size(sqm)", "Age", "Predicted Price (Now)", "Predicted Price (5 Yrs)", "Current Price")
        
        mydata <<- rbind(mydata, e)
        output$table <- renderDataTable(mydata)
    }})
    
  mrt_reactive_bto <- reactive({ 
    dist <- distm(x = matrix(data = c(MRT$lon, MRT$lat), ncol = 2), 
                  y = c(lon = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location]), 
                  fun = distHaversine)
    dist <- dist/1000
    MRT[dist < input$distance5,]
  })
  
  
  malls_reactive_bto <- reactive({ 
    dist <- distm(x = matrix(data = c(Malls$lon, Malls$lat), ncol = 2), 
                  y = c(lon = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location]), 
                  fun = distHaversine)
    dist <- dist/1000
    Malls[dist < input$distance5,]
  })
  
  parks_reactive_bto <- reactive({ 
    dist <- distm(x = matrix(data = c(Parks$lon, Parks$lat), ncol = 2), 
                  y = c(lon = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location]), 
                  fun = distHaversine)
    dist <- dist/1000
    Parks[dist < input$distance5,]
  })
  
  clinics_reactive_bto <- reactive({ 
    dist <- distm(x = matrix(data = c(Clinics$lon, Clinics$lat), ncol = 2), 
                  y = c(lon = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location]), 
                  fun = distHaversine)
    dist <- dist/1000
    Clinics[dist < input$distance5,]
  })
  
  schools_reactive_bto <- reactive({ 
    dist <- distm(x = matrix(data = c(Schools$lon, Schools$lat), ncol = 2), 
                  y = c(lon = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location]), 
                  fun = distHaversine)
    dist <- dist/1000
    Schools[dist < input$distance5,]
  })

  mrt_reactive_resale <- reactive({ 
    dist <- distm(x = matrix(data = c(MRT$lon, MRT$lat), ncol = 2), 
                  y = c(lon = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1]), 
                  fun = distHaversine)
    dist <- dist/1000
    MRT[dist < input$distance5,]
  })
  
  malls_reactive_resale <- reactive({ 
    dist <- distm(x = matrix(data = c(Malls$lon, Malls$lat), ncol = 2), 
                  y = c(lon = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1]), 
                  fun = distHaversine)
    dist <- dist/1000
    Malls[dist < input$distance5,]
  })
  
  parks_reactive_resale <- reactive({ 
    dist <- distm(x = matrix(data = c(Parks$lon, Parks$lat), ncol = 2), 
                  y = c(lon = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1]), 
                  fun = distHaversine)
    dist <- dist/1000
    Parks[dist < input$distance5,]
  })
  
  clinics_reactive_resale <- reactive({ 
    dist <- distm(x = matrix(data = c(Clinics$lon, Clinics$lat), ncol = 2), 
                  y = c(lon = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1]), 
                  fun = distHaversine)
    dist <- dist/1000
    Clinics[dist < input$distance5,]
  })
  
  schools_reactive_resale <- reactive({ 
    dist <- distm(x = matrix(data = c(Schools$lon, Schools$lat), ncol = 2), 
                  y = c(lon = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1]), 
                  fun = distHaversine)
    dist <- dist/1000
    Schools[dist < input$distance5,]
  })
    
  
  
  
  #browser()
  
  map1 <- reactive({ if (input$all_amenities == TRUE){   
    
    map <- leaflet() %>% addTiles()
    
    if (input$typeofflat == 1){
      map <- setView(map, lng = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location], zoom = 14)
    } else {
      map <- setView(map, lng = hdb_resale$Long[hdb_resale$Postal_Code == input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code == input$postalcode1][1], zoom =14)
    }
    
    MRT_Icon <- makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/4/42/Singapore_MRT_logo.svg",
      iconWidth = 18, iconHeight = 18,
      iconAnchorX = 5, iconAnchorY = 22)
    
    school_Icon <- makeIcon(
      iconUrl = "https://www.pinclipart.com/picdir/big/197-1976429_we-provide-recruitment-solutions-to-kindergarten-schools-educational.png",
      iconWidth = 18, iconHeight = 18,
      iconAnchorX = 5, iconAnchorY = 22)
    
    Park_Icon <- makeIcon(
      iconUrl = "https://icons.iconarchive.com/icons/sonya/swarm/256/Park-icon.png",
      iconWidth = 24, iconHeight = 24,
      iconAnchorX = 5, iconAnchorY = 22)
    
    Mall_Icon <- makeIcon(
      iconUrl = "https://icons-for-free.com/iconfiles/png/512/buy+citycons+mall+shopping+icon-1320136423977087098.png",
      iconWidth = 24, iconHeight = 24,
      iconAnchorX = 5, iconAnchorY = 22)
    
    Clinic_Icon <- makeIcon(
      iconUrl = "https://icon-library.com/images/clinic-icon/clinic-icon-17.jpg",
      iconWidth = 18, iconHeight = 18,
      iconAnchorX = 5, iconAnchorY = 22)
    
    if (input$typeofflat == 1){
      map <- addMarkers(map, lng = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location])
      map <- map %>% addMarkers(data=MRT, lng=~lon, lat=~lat, popup=~mrt_station, icon=MRT_Icon, group = "MRT")
      map <- map %>% addMarkers(data=Schools, lng=~lon, lat=~lat, popup=~Schools.school_name, icon=school_Icon, group = "School")
      map <- map %>% addMarkers(data=Parks, lng=~lon, lat=~lat, popup=~name, icon=Park_Icon, group = "Park")
      map <- map %>% addMarkers(data=Malls, lng=~lon, lat=~lat, popup=~malls, icon=Mall_Icon, group = "Mall")
      map <- map %>% addMarkers(data=Clinics, lng=~lon, lat=~lat, popup=~name, icon=Clinic_Icon, group = "Clinic")
    } else {
      map <- addMarkers(map, lng = hdb_resale$Long[hdb_resale$Postal_Code == input$postalcode1], lat = hdb_resale$Lat[hdb_resale$Postal_Code == input$postalcode1])
      map <- map %>% addMarkers(data=MRT, lng=~lon, lat=~lat, popup=~mrt_station, icon=MRT_Icon, group = "MRT")
      map <- map %>% addMarkers(data=Schools, lng=~lon, lat=~lat, popup=~Schools.school_name, icon=school_Icon, group = "School")
      map <- map %>% addMarkers(data=Parks, lng=~lon, lat=~lat, popup=~name, icon=Park_Icon, group = "Park")
      map <- map %>% addMarkers(data=Malls, lng=~lon, lat=~lat, popup=~malls, icon=Mall_Icon, group = "Mall")
      map <- map %>% addMarkers(data=Clinics, lng=~lon, lat=~lat, popup=~name, icon=Clinic_Icon, group = "Clinic")
    }
    
    map <- map %>% addLayersControl(
      overlayGroups = c("MRT","School","Park","Mall","Clinic"),
      options = layersControlOptions(collapsed = F)
    )
    map
    
  } else if (input$all_amenities == FALSE){
    map <- leaflet() %>% addTiles()
  
    if (input$typeofflat == 1){
    map <- setView(map, lng = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location], zoom = 14)
  } else {
    map <- setView(map, lng = hdb_resale$Long[hdb_resale$Postal_Code == input$postalcode1][1], lat = hdb_resale$Lat[hdb_resale$Postal_Code == input$postalcode1][1], zoom =14)
  }
  
  MRT_Icon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/4/42/Singapore_MRT_logo.svg",
    iconWidth = 18, iconHeight = 18,
    iconAnchorX = 5, iconAnchorY = 22)
  
  school_Icon <- makeIcon(
    iconUrl = "https://www.pinclipart.com/picdir/big/197-1976429_we-provide-recruitment-solutions-to-kindergarten-schools-educational.png",
    iconWidth = 18, iconHeight = 18,
    iconAnchorX = 5, iconAnchorY = 22)
  
  Park_Icon <- makeIcon(
    iconUrl = "https://icons.iconarchive.com/icons/sonya/swarm/256/Park-icon.png",
    iconWidth = 24, iconHeight = 24,
    iconAnchorX = 5, iconAnchorY = 22)
  
  Mall_Icon <- makeIcon(
    iconUrl = "https://icons-for-free.com/iconfiles/png/512/buy+citycons+mall+shopping+icon-1320136423977087098.png",
    iconWidth = 24, iconHeight = 24,
    iconAnchorX = 5, iconAnchorY = 22)
  
  Clinic_Icon <- makeIcon(
    iconUrl = "https://icon-library.com/images/clinic-icon/clinic-icon-17.jpg",
    iconWidth = 18, iconHeight = 18,
    iconAnchorX = 5, iconAnchorY = 22)
  
  if (input$typeofflat == 1){
    map <- addMarkers(map, lng = bto_clean$Long[bto_clean$Property.Name== input$location], lat = bto_clean$Lat[bto_clean$Property.Name==input$location])
    map <- map %>% addMarkers(data=mrt_reactive_bto(), lng=~lon, lat=~lat, popup=~mrt_station, icon=MRT_Icon, group = "MRT")
    map <- map %>% addMarkers(data=schools_reactive_bto(), lng=~lon, lat=~lat, popup=~Schools.school_name, icon=school_Icon, group = "School")
    map <- map %>% addMarkers(data=parks_reactive_bto(), lng=~lon, lat=~lat, popup=~name, icon=Park_Icon, group = "Park")
    map <- map %>% addMarkers(data=malls_reactive_bto(), lng=~lon, lat=~lat, popup=~malls, icon=Mall_Icon, group = "Mall")
    map <- map %>% addMarkers(data=clinics_reactive_bto(), lng=~lon, lat=~lat, popup=~name, icon=Clinic_Icon, group = "Clinic")
  } else if (input$typeofflat == 2){
    map <- addMarkers(map, lng = hdb_resale$Long[hdb_resale$Postal_Code == input$postalcode1], lat = hdb_resale$Lat[hdb_resale$Postal_Code == input$postalcode1])
    map <- map %>% addMarkers(data=mrt_reactive_resale(), lng=~lon, lat=~lat, popup=~mrt_station, icon=MRT_Icon, group = "MRT")
    map <- map %>% addMarkers(data=schools_reactive_resale(), lng=~lon, lat=~lat, popup=~Schools.school_name, icon=school_Icon, group = "School")
    map <- map %>% addMarkers(data=parks_reactive_resale(), lng=~lon, lat=~lat, popup=~name, icon=Park_Icon, group = "Park")
    map <- map %>% addMarkers(data=malls_reactive_resale(), lng=~lon, lat=~lat, popup=~malls, icon=Mall_Icon, group = "Mall")
    map <- map %>% addMarkers(data=clinics_reactive_resale(), lng=~lon, lat=~lat, popup=~name, icon=Clinic_Icon, group = "Clinic")
  }
  
  
  map <- map %>% addLayersControl(
    overlayGroups = c("MRT","School","Park","Mall","Clinic"),
    options = layersControlOptions(collapsed = F)
  )
  map}
  })
  
  output$mymap <- renderLeaflet(map1())
  
  observeEvent(input$location, {
    leafletProxy('mymap', session) %>%
      addMarkers(lng = bto$Long[bto$Property.Name.== input$location],lat = bto$Lat[bto$Property.Name.==input$location])
  })
  
  observeEvent(input$postalcode1, {
      leafletProxy('mymap', session) %>%
        addMarkers(lng = hdb_resale$Long[hdb_resale$Postal_Code== input$postalcode1][1],lat = hdb_resale$Lat[hdb_resale$Postal_Code== input$postalcode1][1])
  })

  #Comparison Tab
  observeEvent(input$add, {
    df <- hdb_resale[hdb_resale$Postal_Code==input$postalcode,]
    df_1 <- df[!duplicated(df[, c("Asking.Num", "Property.Type", "Bedrooms", "Bathrooms", "Size_sqm", "Age")]),]
    df_1 <- df_1 %>% select(Property.Name, Category, HDB.Town, Address, Property.Type, Postal_Code, Bedrooms, Bathrooms, Size_sqm, Age, predict_value, predict_value5Y, Asking.Num)
    names(df_1) <- c("Property Name", "Category", "Neighbourhood", "Address", "Property Type", "Postal Code", "Bedrooms", "Bathrooms", "Size(sqm)", "Age", "Predicted Price (Now)", "Predicted Price (5 Yrs)", "Current Price")
    row.names(df_1) <- NULL
    mydata <<- rbind(mydata, df_1)
    output$table <- renderDataTable(mydata)
  })
  observeEvent(input$clear, {
    mydata <<- emptydf
    output$table <- renderDataTable(mydata)
  })
  observeEvent(input$del,{
    if(input$row <= nrow(mydata) & input$row > 0 & nrow(mydata)>1){
      mydata <<- mydata[-input$row,]
      row.names(mydata) <- NULL
      output$table <- renderDataTable(mydata)
    }
    if(nrow(mydata)==1){
      mydata <<- emptydf
      output$table <- renderDataTable(mydata)
    }
  })
  observeEvent(input$add2,{
    df <- bto[bto$Property.Name==input$btoinput,]
    df <- df %>% select(Property.Name, Category, HDB.Town, Address, Property.Type, Postal_Code, Bedrooms, Bathrooms, Size_sqm, Age, predict_value, predict_value5Y, Asking.Num)
    names(df) <- c("Property Name", "Category", "Neighbourhood", "Address", "Property Type", "Postal Code", "Bedrooms", "Bathrooms", "Size(sqm)", "Age", "Predicted Price (Now)", "Predicted Price (5 Yrs)", "Current Price")
    row.names(df) <- NULL
    mydata <<- rbind(mydata, df)
    output$table <- renderDataTable(mydata)
  })
  
  ## For analysis tab
  
  p <- reactive(
    
    if (input$type == 1){
      avg_size <- mean(dat$Size_sqm)
      
      rownames(model_dat) <- model_dat$X
      coef_size <- round(model_dat['Size_sqm','Estimate'],2)
      
      size <- seq(35,185,10)
      `predicted value` <- size*coef_size
      size_tab <- data.frame(size,`predicted value`)
      
      p <- ggplot() + geom_line(data=size_tab, aes(x = size, y = `predicted value`)) + geom_point(data=size_tab, aes(x = size, y = `predicted value`)) +  scale_y_continuous(breaks=pretty_breaks(n=5),name="Value",labels=comma) + xlab("Size(sqm)") + geom_point(data=dat, aes(x=Size_sqm,y=Asking),color='blue',alpha=0.5,size=0.5) + ggtitle("Pricing trend when housing size increases")
      p <- ggplotly(p)
    } 
    
    else if (input$type == 2){
      mid_floor_pred <- round(model_dat['(Intercept)','Estimate'],2)
      low_floor_pred <- mid_floor_pred + round(model_dat['Low_floor','Estimate'],2)
      high_floor_pred <- mid_floor_pred + round(model_dat['High_floor','Estimate'],2)
      
      variable <- c("Low Floor","Mid Floor","High Floor")  
      values <- c(low_floor_pred,mid_floor_pred,high_floor_pred)
      type <- rep("Prediction",3)
      
      tab <- data.frame(variable,values,type)
      
      low_floor_hist <- dat %>% filter(Low_floor==1) %>% summarise(mean(Asking)) %>% pull()
      mid_floor_hist <- dat %>% filter(Low_floor!=1 & High_floor!=1) %>% summarise(mean(Asking)) %>% pull()
      high_floor_hist <- dat %>% filter(High_floor==1) %>% summarise(mean(Asking)) %>% pull()
      
      values <- c(low_floor_hist,mid_floor_hist,high_floor_hist)
      type <- rep("Historical",3)
      tab2 <- data.frame(variable,values,type)
      
      floor_dat <- rbind(tab,tab2)
      floor_dat$variable <- factor(floor_dat$variable, levels=c("Low Floor", "Mid Floor", "High Floor"))
      
      rooms4_pred <- round(model_dat['(Intercept)','Estimate'],2)
      rooms3_pred <- rooms4_pred + round(model_dat['Property.TypeHDB.3.Rooms','Estimate'],2)
      rooms5_pred <- rooms4_pred + round(model_dat['Property.TypeHDB.5.Rooms','Estimate'],2)
      roomsExec_pred <- rooms4_pred + round(model_dat['Property.TypeHDB.Executive','Estimate'],2)
      roomsJumbo_pred <- rooms4_pred + round(model_dat['Property.TypeHDB.Jumbo','Estimate'],2)
      
      variable <- c("3 Rooms","4 Rooms","5 Rooms","Executive","Jumbo")  
      values <- c(rooms3_pred,rooms4_pred,rooms5_pred,roomsExec_pred,roomsJumbo_pred)
      type <- rep("Prediction",5)
      
      tab <- data.frame(variable,values,type)
      
      hist <- read.csv("pred_set1.csv")
      hist_data <- hist %>% filter(Property.Type!=1 & Property.Type!=2) %>% group_by(Property.Type) %>% summarise(avg=mean(Asking))
      
      values <- hist_data$avg
      type <- rep("Historical",5)
      
      tab2 <- data.frame(variable,values,type)
      house_type_dat <- rbind(tab,tab2)
      
      #combine house and floor
      com_dat <- rbind(floor_dat,house_type_dat)
      
      p2 <- ggplot(data=com_dat,aes(x=type,y=values,fill=variable)) + geom_bar(stat="identity",position="dodge") + scale_y_continuous(breaks=pretty_breaks(n=5),name="Value",labels=comma) + ggtitle("Pricing trend for different house types")
      
      p2 <- ggplotly(p2, tooltip = c("values","variable"))
    }
    else if (input$type == 3){
      age_change <- round(model_dat['Age','Estimate'],2)
      base <- round(model_dat['(Intercept)','Estimate'],2)
      avg_age <- round(mean(dat$Age),0)
      age1 <- base + ((avg_age-1)*-age_change)
      age60 <- base - ((60-avg_age)*-age_change)
      
      line_dat <- data.frame(x=c(1,avg_age,60),y=c(age1,base,age60))
      
      p3 <- ggplot() + geom_boxplot(dat, mapping = aes(x=Age, y=Asking, group=Age, fill=Age)) + scale_y_continuous(breaks=pretty_breaks(n=10),name="Values",labels=comma) + scale_x_continuous(breaks=pretty_breaks(n=10),name="Age") + geom_line(data = line_dat, aes(x=x,y=y,text="prediction"),color='orange') + ggtitle("Age of house against housing price")
      
      p3 <- ggplotly(p3, tooltip = c("text"))
    }
    else if (input$type == 4){
      dist <- seq(0,1500,50)
      Prischool_dist <- round(model_dat['min_PRIschool_dist','Estimate'],2)*dist
      Secschool_dist <- round(model_dat['min_SECschool_dist','Estimate'],2)*dist
      Mall_dist <- round(model_dat['min_malls_dist','Estimate'],2)*dist
      Mrt_dist <- round(model_dat['min_MRT_dist','Estimate'],2)*dist
      
      dist_dat1 <- data.frame(dist,value=Prischool_dist,type="Primary School")
      dist_dat2 <- data.frame(dist,value=Secschool_dist,type="Secondary School")
      dist_dat3 <- data.frame(dist,value=Mall_dist,type="Mall")
      dist_dat4 <- data.frame(dist,value=Mrt_dist,type="MRT/LRT")
      
      dist_dat <- rbind(dist_dat1,dist_dat2,dist_dat3,dist_dat4)
      
      p4 <- ggplot(dist_dat, aes(x=dist,y=value,color=type,group=type)) + geom_line() + geom_point()+scale_y_continuous(breaks=pretty_breaks(n=10),name="Value change",labels=comma) + scale_x_continuous(breaks=pretty_breaks(n=10),name="Distance from house (meters)") + geom_hline(yintercept = 0) + ggtitle("Distance from nearest amenity against housing price")
      
      p4 <- ggplotly(p4,tooltip = c("dist","value"))
    })
  
  p2 <- reactive(
    if (input$type2 == 6){

      
      dat_resale2 <- subset(dat_resale,select = c(14:39))
      dat$Town <- toupper(names(dat_resale2)[max.col(dat_resale2)])
      
      hist_area_price <- dat %>% group_by(Town) %>% summarise(avg=round(mean(Asking),0))
      
      hist_area_price$Town <- str_remove(hist_area_price$Town,"HDB.TOWN")
      hist_area_price$Town <- str_replace_all(hist_area_price$Town,"\\."," ")
      hist_area_price$Town <- str_trim(hist_area_price$Town)
      
      shape <- sp::merge(shape_base,hist_area_price,by.x='PLN_AREA_N',by.y='Town',all.x=T)
      
      shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80"))
      
      pal <- colorNumeric( palette = "Oranges", domain = shapeData$avg)
      
      labs <- lapply(seq(length(shapeData$PLN_AREA_N)), function(i) {
        paste0( '<p>', shapeData$PLN_AREA_N[i], '<p></p>','avg price:', 
                shapeData$avg[i])})
      
      m<-leaflet() %>% addTiles() %>% addPolygons(data=shapeData, weight = 5, stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.8, color = "grey", fillColor =  ~pal(avg), label = lapply(labs, htmltools::HTML))
      m
    } 
    else if (input$type2 == 7) {
      pred_area_amk <- round(model_dat['(Intercept)','Estimate'],2)
      pred_data <- model_dat[6:29,1:2]
      pred_data$value <- pred_data$Estimate + pred_area_amk
      
      pred_data <- subset(pred_data,select=-Estimate)
      rownames(pred_data) <- NULL
      
      pred_data <- rbind(pred_data,c('HDB.TownAng.Mo.Kio',pred_area_amk))
      pred_data$value <- round(parse_number(pred_data$value),0)
      pred_data$Town <- toupper(pred_data$X)
      
      pred_data$Town <- str_remove(pred_data$Town,"HDB.TOWN")
      pred_data$Town <- str_replace_all(pred_data$Town,"\\."," ")
      pred_data$Town <- str_trim(pred_data$Town)
      
      shape <- sp::merge(shape_base,pred_data,by.x='PLN_AREA_N',by.y='Town',all.x=T)
      
      shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80"))
      
      pal <- colorNumeric( palette = "Oranges", domain = shapeData$avg)
      
      labs <- lapply(seq(length(shapeData$PLN_AREA_N)), function(i) {
        paste0( '<p>', shapeData$PLN_AREA_N[i], '<p></p>','avg price:', 
                shapeData$value[i])})
      
      m<-leaflet() %>% addTiles() %>% addPolygons(data=shapeData, weight = 5, stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.8, color = "grey", fillColor =  ~pal(value), label = lapply(labs, htmltools::HTML))
      m  
    })
  
  output$Plot <- renderPlotly(p())
  output$Plot2 <- renderLeaflet(p2())
}
shinyApp(ui=ui, server=server)

