source("R/libraries.R")
source("R/LoadData.R")

## UI module ----
## inputs and outputs (of one tabPanel)

tabPanelUI <- function(id, countryS) {
    tabPanel(
        id, 
        
        column(12, id = "col1",
               fluidRow(
                   style = "position:relative",
                   column(3, id = "col2", style = 'padding:0px;',
                          wellPanel(
                              checkboxGroupInput(NS(id,"kingdomGroup"), 
                                                 h4("Choose kingdom:"), 
                                                 choices = list("Animalia" = "Animalia", 
                                                                "Plantae" = "Plantae", 
                                                                "Fungi" = "Fungi"),
                                                 selected = countryS$kingdom %>% unique() %>% as.character() %>% sort()
                                                 ),
                              
                              radioButtons(NS(id,"radio"), h4("Search  by:"),
                                           choices = list("scientific name" = 1, "vernacular name" = 2), selected = 1),
                              
                              conditionalPanel(
                                  condition="input.radio == 1", ns = NS(id),
                                  selectInput(NS(id,"scientific"), "Scientific name:", 
                                              choices = countryS$scientificName %>% sort()
                                  )
                              ),
                              
                              conditionalPanel(
                                  condition="input.radio==2", ns = NS(id),
                                  selectInput(NS(id,"vernacular"), "Vernacular name:", 
                                              choices = countryS$vernacularName %>% sort()
                                              )
                                  ),
                              
                              actionBttn(
                                  inputId = NS(id,"reset"),
                                  label = "Reset criteria",
                                  color = "success",
                                  style = "jelly",
                                  icon = icon("sync"),
                                  block = FALSE, 
                                  size = "sm"
                                  )
                              )
                          ),
                   
                   column(9,
                          fluidRow(
                              wellPanel(
                                  leafletOutput(NS(id,"Map"))
                                  )
                              )
                          ),
                   
                   column(12, id = "tml",
                          fluidRow(
                              timevisOutput(NS(id,"timeline"),
                                            )
                              )
                          )
                   )
               )
    )
    }

## Server module ----

tabPanelServer <- function(id, countryS, centerLat =  52, centerLon =  19, zoom = 5) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            rv <- reactiveValues(selected = NULL)
            
            observe({
                rv$selected <- input$timeline_selected
                })
            
            observeEvent(input$kingdomGroup, {
                if(length(input$kingdomGroup)==0){
                    
                    shinyalert("Select at least one kingdom", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 3000)
                    
                    updateCheckboxGroupInput(
                        session, 
                        "kingdomGroup",
                        selected = countryS$kingdom %>% unique() %>% as.character() %>% sort())
                }
                
                if(input$radio==1){
                    
                    choice1 <- countryS$scientificName[which(countryS$kingdom  %in% input$kingdomGroup)] %>% 
                        sort() %>% as.vector() %>% unique()
                    
                    if(length(choice1)==0){
                        shinyalert("No scientific name in this kingdom!", "Resetting kingdom criteria", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 3000)
                        
                        updateCheckboxGroupInput(
                            session, 
                            "kingdomGroup",
                            selected = countryS$kingdom %>% unique() %>% as.character() %>% sort())
                    }
                    updateSelectInput(session, "scientific", 
                                      choices = choice1
                    )
                    }else
                        {
                        choice2 <- countryS$vernacularName[which(countryS$kingdom  %in% input$kingdomGroup)] %>% 
                            sort() %>% as.vector() %>% unique()
                        
                        if(length(choice2)==0){
                            shinyalert("No vernacular name in this kingdom!", "Resetting kingdom criteria", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 3000)

                            updateCheckboxGroupInput(
                                session, 
                                "kingdomGroup",
                                selected = countryS$kingdom %>% unique() %>% as.character() %>% sort())
                            }
                        
                        updateSelectInput(session, "vernacular", 
                                      choices =  choice2
                        )
                    }
                
                rv$selected <- NULL
                
            })
            
            observeEvent(input$radio, {

                if(input$radio==1){
                    
                    choice1 <- countryS$scientificName[which(countryS$kingdom  %in% input$kingdomGroup)] %>% 
                        sort() %>% as.vector() %>% unique()
                    
                    
                    if(is.null(choice1)){
                        choice1 = ""
                    }
                    updateSelectInput(session, "scientific", 
                                      choices = choice1)
                }
                
                if(input$radio==2){
                    
                    choice2 <- countryS$vernacularName[which(countryS$kingdom  %in% input$kingdomGroup)] %>% 
                        sort() %>% as.vector() %>% unique()
                    
                            if(length(choice2)==0){
                        
                                shinyalert("No vernacular name in this kingdom!", "Resetting kingdom criteria", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, timer = 3000)
                        
                                updateCheckboxGroupInput(
                                    session, 
                                    "kingdomGroup",
                                    selected = countryS$kingdom %>% unique() %>% as.character() %>% sort())
                                }
                            updateSelectInput(session, "vernacular", 
                                      choices =  choice2)
                }
                rv$selected <- NULL
                })
            
            observeEvent(input$vernacular, {
                rv$selected <- NULL
                })
            observeEvent(input$scientific, {
                rv$selected <- NULL
                })
        
            output$Map <- renderLeaflet({
            
                if(input$radio==1){
                selectData <- countryS[which(countryS$scientificName == input$scientific),]
                }
        
                if(input$radio==2){
                selectData <- countryS[which(countryS$vernacularName == input$vernacular),]
                }
            
                iconTable <- data.frame("kingdom"= c("Animalia", "Plantae", "Fungi"), 
                                        "icon"   = c("paw",      "leaf",    "umbrella"), 
                                        "marker" = c("orange",   "green",   "blue"))
                
                iconTable <- iconTable[which(iconTable$kingdom == selectData$kingdom %>% unique() %>% as.character()),] %>% suppressWarnings()
            
                iconS <- iconTable[which(as.character(iconTable$kingdom) == selectData$kingdom %>% unique() %>% as.character()),]
            
                iconK <- makeAwesomeIcon(
                    icon = iconS$icon,
                    iconColor = "black",
                    markerColor = iconS$marker,
                    library = "fa")
            
                if (is.null(rv$selected)) {
                    
                    
                    leaflet(data = selectData) %>%
                        setView(lat = centerLat, lng = centerLon, zoom = zoom) %>%
                        addTiles() %>%
                        addAwesomeMarkers(~longitudeDecimal, ~latitudeDecimal, layerId = ~id, 
                                          popup = ~paste("<br> <b> Scientific name: </b>", scientificName,
                                                         "<br> <b> Vernacular name: </b>", vernacularName,
                                                         "<br> <b> Family: </b>", family , 
                                                         "<br>", "<a href='",
                                                         selectData$occurrenceID,
                                                         "' target='_blank'>",
                                                         "More info:</a>"), 
                                          label = ~scientificName, icon = iconK)
                    } 
                else {
                    
                    leaflet(data = selectData[selectData$id == rv$selected, ]) %>%
                    setView(lat = centerLat, lng = centerLon, zoom = zoom) %>%
                    addTiles() %>%
                    addAwesomeMarkers(~longitudeDecimal, ~latitudeDecimal, layerId = ~id, 
                                      popup = ~paste("<br> <b> Scientific name: </b>", scientificName,
                                                     "<br> <b> Vernacular name: </b>", vernacularName,
                                                     "<br> <b> Family: </b>", family , 
                                                     "<br>", "<a href='",
                                                     selectData$occurrenceID,
                                                     "' target='_blank'>",
                                                     "More info:</a>"), 
                                      label = ~scientificName, icon = iconK)
                    }
                })
            
            output$timeline <- renderTimevis({
            
                if(input$radio==1){
                selectData <- countryS[which(countryS$scientificName == input$scientific),]
                }
            
                if(input$radio==2){
                selectData <- countryS[which(countryS$vernacularName == input$vernacular),]
                }
            
            
                selectData$start <- paste(selectData$eventDate)
                selectData$content <- selectData$locality
                
                timevis(selectData, options = list(
                    stack = FALSE, 
                    stackSubgroups = TRUE, 
                    orientation = "top",
                    verticalScroll = TRUE,
                    
                    minHeight = 750, 
                    maxHeight = 750
                )) %>% suppressWarnings()
                })
            
            observeEvent(input$reset, {
                
            updateCheckboxGroupInput(
                session, 
                "kingdomGroup",
                selected = countryS$kingdom %>% unique() %>% as.character() %>% sort())
                })
            
            
        })
    }

## App ----
## using modules, I'm passing selected country datasets  

ModularApp <- function() {
    ui <- fillPage(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        column(12, id = "title",  h1(" Biodiversity dashboard")),
        tabsetPanel(
            tabPanel("PL",  tabPanelUI("PL",  countryS = poland)),
            tabPanel("AT",  tabPanelUI("AT",  countryS = austria)),
            tabPanel("KE",  tabPanelUI("KE",  countryS = kenya)),
            tabPanel("NP",  tabPanelUI("NP",  countryS = nepal)),
            )
        )

    server <- function(input, output, session) {
        tabPanelServer("PL", countryS = poland, centerLat =  centerLatPl, 
                       centerLon =  centerLonPl, zoom = 5)
        
        tabPanelServer("AT", countryS = austria, centerLat =  centerLatAt, 
                       centerLon =  centerLonAt, zoom = 6)
        
        
        tabPanelServer("KE", countryS = kenya, centerLat =  centerLatKe, 
                       centerLon =  centerLonKe, zoom = 5)
        
         tabPanelServer("NP", countryS = nepal, centerLat =  centerLatNp, 
                        centerLon =  centerLonNp, zoom = 5)
         
        
        }
    shinyApp(ui, server)
    }

ModularApp()
