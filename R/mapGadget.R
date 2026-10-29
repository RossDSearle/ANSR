



getSitesTable <- function(providerNames){

  catdf <-  apiProviderCatalogue(providerNames = providerNames, includeProperties = F)
  return(catdf)
}



# The Gadget Wrapper
my_map_tool <- function(providers=NULL) {

 sites <-  getSitesTable(providerNames = providers )
 sites$sid <- paste0(sites$Provider, '_', sites$SiteID)


 shiny::addResourcePath(
   prefix = "app_assets",
   directoryPath = system.file("images", package = "ANSR")
 )


 ui <- shiny::fluidPage(
   shiny::tags$head(
     shiny::tags$style(shiny::HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      .app-container {
        height: 100vh;
        display: flex;
        flex-direction: column;
        padding: 15px;
        box-sizing: border-box;
      }
      .header-section {
  display: flex;          /* Places logo and text side-by-side */
  align-items: center;    /* Vertically centers image with text */
  gap: 16px;              /* Prevents overlap by creating a fixed horizontal space */
  padding-bottom: 12px;
  flex: 0 0 auto;
}
     .logo-placeholder {
  flex-shrink: 0;         /* Prevents the image container from collapsing or overlapping */
  display: flex;
  align-items: center;
     }

.logo-placeholder img {
  height: 56px;
  width: auto;
  display: block;
}
.header-text {
  display: flex;
  flex-direction: column;
}

      .header-text h2 {
        margin: 0;
        color: #2c3e50;
        font-weight: 600;
      }
      .header-text p {
        margin: 4px 0 0 0;
        color: #7f8c8d;
        font-size: 14px;
      }
      .content-section {
        flex: 1;
        display: flex;
        flex-direction: column;
        min-height: 0;
        gap: 10px;
      }
      .top-half {
        flex: 1;
        min-height: 0;
      }
      .bottom-half {
        flex: 1;
        overflow-y: auto;
        min-height: 0;
      }
      #map {
        height: 100% !important;
      }

      /* Keeps flextable header fixed at the top during scroll */
.bottom-half table thead th {
  position: sticky;
  top: 0;
  z-index: 10;
  background-color: #F4F6F7 !important; /* Matches header background so scrolled rows don't show through */
}

    "))
   ),

   shiny::div(
     class = "app-container",

     # 1. Header Section with Logo + Text
     shiny::div(
       class = "header-section",

       # Logo Box
       shiny::div(
         class = "logo-placeholder",
         shiny::tags$img(src = "app_assets/logo.png", height = "56px")
       ),

       # Header Text Group (Positioned strictly to the right)
       shiny::div(
         class = "header-text",
         shiny::h2("ANSR Site Viewer"),
         shiny::p("Click on the points in the map to see ANSIS site data.")
       )
     ),

     # 2. Main Content Split (Map + Table)
     shiny::div(
       class = "content-section",

       # Top Half: Leaflet Map
       shiny::div(
         class = "top-half",
         leaflet::leafletOutput("ansr_map", height = "100%")
       ),

       # Bottom Half: Flextable
       shiny::div(
         class = "bottom-half",
         shiny::uiOutput("flextable")
       )
     )
   )
 )
  server <- function(input, output, session) {

    RV <- reactiveValues()
    RV$CurrentSiteDF <- NULL


    output$ansr_map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = sites) |>
      leaflet::addTiles() |>
        leaflet::addMarkers(
            layerId = ~sid,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = ~paste0("<b>Provider:</b> ", Provider, "<br><b>SiteID:</b> ", SiteID),
            clusterOptions = leaflet::markerClusterOptions()
          )
    })

    # Listen for the click event
    shiny::observeEvent(input$ansr_map_marker_click, {
      click <- input$ansr_map_marker_click
      if (is.null(click)) return()


      bits <- stringr::str_split(click$id, '_')
      p <- bits[[1]][1]
      s <- bits[[1]][2]
      RV$CurrentSiteDF <- getSingleSite(providerID=p, siteID=s, )


    })


    output$flextable <- shiny::renderUI({

      req(RV$CurrentSiteDF)

      sdf <- RV$CurrentSiteDF$Data[, c(1, 10, 11, 12:17)]
      codes <- DataSets@Enums
      cdf <- data.frame(codes$const, codes$const_description)

      mdf<- merge(sdf, cdf, by.x = 'Value', by.y = 'codes.const', all.x=T)

      ### Filter out unwanted properties in the output table
      ignorelistProps <- c('relatedProject', 'relatedVisitor')
      idxs <- which(mdf$Property %in% ignorelistProps)
      if(length(idxs)>0){mdf <- mdf[-idxs,]}


      ### Filter out unwanted properties in the output table
      ignorelistSubProps <- c('source', 'colourModel', 'colour.colourModel')
      idxs <- which(mdf$subProperty %in% ignorelistSubProps)
      if(length(idxs)>0){mdf <- mdf[-idxs,]}

      ### do decoding of property values
      idxs <- which(is.na(mdf$codes.const_description))
      if(length(idxs)>0){mdf$codes.const_description[idxs] <- mdf$Value[idxs]}

      mdf <- mdf |> dplyr::arrange(fieldDepth, depthUpper, depthLower,  Property )

      # ## reorder columns
      mdf <- mdf[, c(2, 3, 4, 5, 6, 7, 10, 8, 9)]
      colnames(mdf) <- c('schemaLevel','depthUpper','depthLower','Property','PropertyIndex','subProperty','Value','Unit','UsedProcedure')

      idxs <- which(mdf$Property=='designation')
      if(length(idxs)>0){
        #mdf$Value[idxs] <- stringr::str_remove(mdf$Value[idxs], "^0+")
        mdf$Value[idxs] <- stringr::str_remove_all(mdf$Value[idxs], "0")
      }


       ft <-  flextable::flextable(mdf) |>
      flextable::theme_vanilla() |>
         flextable::color(part = "header", color = "#333333") |>
         flextable::bold(part = "header") |>
         flextable::bg(part = "header", bg = "#F4F6F7") |>
         flextable::align(align = "left", part = "all") |>  # Left-aligns cell values only %>%
         flextable::autofit()

       flextable::htmltools_value(ft)
    })






    # Handle the 'Done' button in the title bar
    shiny::observeEvent(input$done, {
      stopApp("Tool closed.") # You can return data back to the R console here
    })
  }

  # runGadget with paneViewer() forces it into the RStudio Viewer pane
  # runGadget(ui, server, viewer = paneViewer())
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}


