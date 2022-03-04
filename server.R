# Click the small lights next to the publish button (top right of this code window) to navigate by headers

## Define a server for the Shiny app
function(input, output, session) {
  options(shiny.port = 1221) #Used for local testing, unused in shinyapps.
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  ###############################################.
  ## Server - UI Body  ----
  ###############################################.
  output$uibody <- renderUI({
    
    ###############################################.
    ## UI - Header NavBar Page ---- 
    ###############################################.
    navbarPage(title = "SupplementalCalc",
               theme = shinytheme("cerulean"),
               
               ###############################################.
               ## UI - Home ----
               ###############################################.           
               tabPanel("Home",
                        isolate({Header_Details}),
                        p(strong("How to use this calulator"), align="left", style="font-size:30px"),
                        p("1. Start by selecting the 'Add Location' tab where you provide a zip code and electricity rate in $/kWh.", align="left", style="font-size:30px"),
                        p("2. Then you can add a Greenhouse Design. Note: this information is discarded once you exit the calculator.", align="left", style="font-size:30px"),
                        p("3. Next, calculate your Required Supplemental Lighting by selecting a design for your location. ", align="left", style="font-size:30px"),
                        p("4. Voila! You've calculated your lighting costs!", align="left", style="font-size:30px"),
                        isolate({Footer_Details})
               ),#bracket Home tab panel 
               
               ###############################################.
               # UI - Locations - Add New ----
               ###############################################. 
               tabPanel("Add Location",
                        Header_Details,
                        sidebarPanel(titlePanel("Enter Greenhouse Location"), 
                                     div(title="Enter your Location Name Here",
                                         textInput(inputId ="New_Loc_Name",
                                                   label = shiny::HTML("<p>Location Name <br/> <span style='font-weight: 400'>(Enter a unique identifier for your Location)</span></p>"), width = '400px')),
                                     div(title="Enter the Zipcode Here",
                                         textInput(inputId ="New_Loc_Zip",
                                                   label = shiny::HTML("<p>Zip Code <br/> <span style='font-weight: 400'>(5 digits)</span></p>"), width = '125px')),
                                     div(title="Enter an Electricity Rate in $/kWh",
                                         textInput(inputId ="New_Loc_Elec_Rate",
                                                   label = p("Electricity Rate ($/kWh)",
                                                             bsButton("b2", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                         bsPopover(id = "b2", title="", content = "Typically, between $0.05 and $0.30. Only use numbers with a decimal. Do NOT include a dollar sign ($)", placement="right", trigger="hover",options = list(container = "body"))),
                                     actionButton("Location_save", "Save")
                        ), #bracket sidebar Panel
                        p(strong("Enter Greenhouse Location"), align="left", style="font-size:30px"),
                        p("1. Add your location and electricity price ($/kWh).", align="left", style="font-size:30px"),
                        p("2. Do not add the dollar sign ($) in the electricity rate box.", align="left", style="font-size:30px"),
                        p("3. ", em("Not sure what your electricity rate is? Reference your most recent power bill for more information."), align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               # UI - Greenhouse Designs - Add New ----
               ###############################################.
               tabPanel("Add Greenhouse Design",
                        Header_Details,
                        sidebarPanel(
                          titlePanel("Enter Greenhouse Design"),
                          p(strong("Update Gear Icon First"), align="center", style="font-size:20px"),
                          fluidRow(column(7,
                                          div(title="Enter your Design Name Here",
                                              textInput(inputId ="New_GHD_Name",
                                                        label = shiny::HTML("<p>Design Name <span style='font-weight: 400'></span></p>"), width = '300px')),
                                          div(title="Enter Greenhouse Length Here",
                                              numericInput(inputId ="New_GHD_Length",
                                                           label = shiny::HTML("<p>Length <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter Greenhouse Width Here",
                                              numericInput(inputId ="New_GHD_Width",
                                                           label = shiny::HTML("<p>Width <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter a % Transmission Here ",
                                              textInput(inputId ="New_GHD_Trans",
                                                        label = p("Greenhouse Transmission (%) ",
                                                                  bsButton("b3", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:175px")),
                                              bsPopover(id = "b3", title="", content = "Typically, between 40 and 90 percent. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter a Target DLI Here (mol/m2/day)",
                                              textInput(inputId ="New_GHD_TargetDLI",
                                                        label = p("Target DLI (mol/m2/day) ",
                                                                  bsButton("b4", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b4", title="", content = "Typically, between 5 and 30 mol/m2/day. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter Lighting Efficacy Here (umol/J)",
                                              textInput(inputId ="New_GHD_Efficacy",
                                                        label = p("Lighting Efficacy (umol/J) ",
                                                                  bsButton("b5", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b5", title = "", content = "Typically, between 1.2 and 3.5 umol/J. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Percent of days where you reach your target DLI",
                                              textInput(inputId ="New_GHD_Percentile",
                                                        label = p("Percentage of Days Where You Reach Target DLI (%)",
                                                                  bsButton("b6", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b6", title="Between 0 to 100 Use numbers only.", content = "Percent of days where you reach your target DLI.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Photoperiod",
                                              textInput(inputId ="New_GHD_hours",
                                                        label = p("Hours on",
                                                                  bsButton("b7", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b7", title="", content = "How many hours each day will you have your lights on?", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Monthly Demand Charge Price",
                                              textInput(inputId ="New_GHD_demand_charge_price",
                                                        label = p("Monthly Demand Charge Price ($/kW)",
                                                                  bsButton("b8", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b8", title="", content = "This charge may range from $5 to over $15/kW. Use numbers only. Commercial customers typically pay a demand charge as part of the elecricity bill, based on their peak power usage. The price for this charge is typically shown on the power bill as $/kW.", placement="right", trigger="hover",options = list(container = "body")))
                          ),#bracket column 1
                          #Gear icon for customize % of lighting area by month
                          column(5, 
                                 h6("Click gear to customize lighting % of area by month"),
                                 #Percent of the area to light by month
                                 shinyWidgets::dropdownButton(
                                   tags$h3("Percent of Greenhouse Area By Month"),
                                   uiOutput('percent_mat'), 
                                   shiny::actionButton(
                                     inputId = "update_permat", 
                                     label = "Update Percentages"
                                   ),
                                   circle = TRUE, status = "primary", icon = icon("gear"), width = "450",
                                   tooltip = tooltipOptions(title = "Click here to customize lighting area by month!")
                                 ),#bracket drop down Button
                          ),#bracket column 2
                          ),#bracket fluid row
                          actionButton("GHD_save", "Save")
                        ), #bracket sidebar Panel
                        p(strong("Enter Greenhouse Design"), align="left", style="font-size:30px"),
                        p("1. Add a unique greenhouse design to calculate electricity costs for your location.", align="left", style="font-size:30px"),
                        p("2. If desired, select the gear icon to customize lighting % of area by month.", align="left", style="font-size:30px"),
                        p("3. Indicate the square footage by providing the greenhouse length and width in feet. Provide your greenhouse transmission percentage,
                          target daily light integral (DLI) and lighting efficacy.", align="left", style="font-size:30px"),
                        p("4. Indicate the percentile of days that the system should reach the target DLI, the hours per day that the system will be on, and the monthly demand charge price.", align="left", style="font-size:30px"),
                        p("5. Once saved, select the 'Electricity Costs' tab to see your calculated costs for the provided greenhouse design.", align="left", style="font-size:30px"),
                        p("* Note: If running multiple tests, press 'Save' after updating gear icon for accurate results.",  align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               ## UI - Supplemental Costs ----
               ###############################################.                   
               tabPanel("Required Supplemental Lighting",
                        h2("Required Supplemental Lighting"),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('ui_SC_location'),
                            uiOutput('ui_SC_design'),
                            actionButton(inputId = 'SC_Choices', label = 'Update'),
                            actionButton(inputId = 'SC_Adjust_Supp', label = 'Adjust'),
                            actionButton(inputId = 'SC_Reset', label = 'Reset'),
                            downloadButton("SC_downloadData", "Download Results"), 
                            downloadButton("SC_Adjusted_downloadData", "Download Adjusted Results"),
                            width = "2",
                            h4("Press the Update Button to view results"),
                            h4(span("WARNING:", style = "color: red"), " Press the Adjust Button to adjust the supplemental DLI capacity ", span("AFTER", style = "color: red"), " clicking on the chart"),
                            h4("Press the Reset Button to revert changes")
                          ),#bracket sidebar panel
                          mainPanel(
                            fluidRow(
                              column(8,
                                     h4("Monthly Area Lighting %"), 
                                     tableOutput("Percent_table_SC"),
                                     h4("Grower Input"),
                                     tableOutput('SC_table')
                              ),
                              column(4,
                                     h4("Annual Lighting Cost"),
                                     tableOutput("Annual_SC_Display_table"),
                                     h4("Annual Demand Cost"),
                                     tableOutput("DSC_Display_table")
                              )
                            ), fluidRow(column(6,
                                               h4("Sunlight"),
                                               plotOutput({"naturalDLI"})),
                                        column(6,
                                               h4("Supplemental Light"),
                                               plotOutput({"supplementalDLI"}, click = "plot_click_supp"))
                            ), fluidRow(column(3,
                                               h4("Weekly Lighting Cost"),
                                               tableOutput({"SC_df_W_table"}),
                                               DT::dataTableOutput("trace_table"),
                                               style = "height:500px; overflow-y: scroll;"),
                                        column(4),
                                        column(3,
                                               valueBoxOutput("PPFD_tile", width = NULL),
                                               valueBoxOutput("DLI_tile", width = NULL)))
                          )
                        )),
               tabPanel(title="FAQ",
                        FAQ_Details
               ),#Bracket FAQ Page
               tabPanel(title = "Quit",
                        actionButton("close", "Click Here to End Session"))
    )#Bracket UI navbar Page
  }) #bracket render UI
  
  ###############################################.
  ## Server - Quit Application ---- 
  ###############################################.
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  ###############################################.
  # Server - Locations - Add New ----
  ###############################################. 
  Loc_values <- reactiveValues() # empty list to store reactive values for new location try
  XTRA_values <- reactiveValues() # empty list to store various values so it can be used downstream
  
  # Note that we use observeEvent() here, which depends on input$Location_save (the action button), so that the output is only updated when the grower clicks the button
  observeEvent(input$Location_save, { # When action button "Location_save" is clicked run code in this section
    
    elec_test <- FALSE #Electricity rate must be valid: no $. 
    geo_test <- FALSE #Location must be found
    field_test <- FALSE #All fields must be entered
    
    # Check values and get lat and long using zip code
    if(nchar(input$New_Loc_Zip)!=5 & input$New_Loc_Zip!=""){
      shinyalert("Please enter a 5 digit zipcode", type="error")
      updateTextInput(session,"New_Loc_Zip", value="")
    } else{ 
      t <- try(geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio"))
      if ("try-error" %in% class(t)) {
        shinyalert("Location not found please try a different address or zip code", type="error")
        updateTextInput(session,"New_Loc_Street", value="")
      } else{ 
        geoloc <- geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio")
        if(is.logical(geoloc$lat)){ #is.logical() is used if geocascade doesn't find address, it returns N/A
          shinyalert("Location not found please try a different address or zip code", type="error")
          updateTextInput(session,"New_Loc_Street", value="")
        } 
        else{
          geo_test <- TRUE #Set geo_test to true if location is found
          Loc_values$lat <- geoloc$lat[1] #Set lat to value from geo() function
          Loc_values$long <- geoloc$long[1] #Set long to value from geo() function
        }
      }
    }
    
    #Continue checking validity of inputs
    rate_test <- as.numeric(input$New_Loc_Elec_Rate)
    if(is.na(rate_test)){
      shinyalert("Please check Electricity Rate input: No '$' needed", type="error")
      updateTextInput(session,"New_Loc_Elec_Rate", value="")
    } else{
      elec_test <- TRUE #Set to true if electricity rate is a valid number 
    }
    
    if(input$New_Loc_Name=="" | input$New_Loc_Zip==""| input$New_Loc_Elec_Rate==""){
      shinyalert("Please make sure all fields are filled.", type="error")
    } else{
      field_test <- TRUE #Set to true if all inputs have been entered, no blanks
    }
    
    if(elec_test==TRUE & geo_test==TRUE & field_test==TRUE){ #If all tests are true, store the values. 
      Loc_values$Name <- input$New_Loc_Name
      Loc_values$Zip <- input$New_Loc_Zip
      Loc_values$Elec_Rate <- input$New_Loc_Elec_Rate  
      shinyalert("Success! Now add your Greenhouse Design.")
    }
    
    #Store the longitude and latitude to be used in the TMY data pull below
    lon <- as.numeric(Loc_values$long)
    lat <- as.numeric(Loc_values$lat)
    
    # Pull TMY data from NSRDB
    URL <- paste0(URLbase,'&wkt=POINT(', lon, '+', lat,')')#,'&utc=', 0) # utc=0 allows for timezone, but will crash the app fairly frequently, so it's commmented out
    req <- GET(url = URL)
    NSRDB <- content(req)
    radiation <- NSRDB[-(1:2),(1:6)]
    names(radiation)<- c("Y","M","D","H","Min","ghi")
    radiation$Y <- rep(year(Sys.Date()),nrow(radiation))
    radiation$M <- sprintf("%02d", as.numeric(radiation$M))
    radiation$D <- sprintf("%02d", as.numeric(radiation$D))
    radiation$oldTimeStamp <- paste0(radiation$M, "/", radiation$D, "/", radiation$Y, " ", radiation$H,":",radiation$Min)
    radiation$TimeStamp <- as.POSIXct(strptime(radiation$oldTimeStamp,"%m/%d/%Y %H:%M"))
    radiation$Month <- as.character(paste0(radiation$M))
    radiation <- radiation %>% 
      filter(TimeStamp >= FirstMonday) #remove this if you don't want to exclude the days prior to the first monday of the year
    radiation <- radiation[c(9,8,6)]
    
    # Store the radiation table in a reactive list to be used outside of this observe event
    XTRA_values$radiation <- radiation
  })
  
  ###############################################.
  # Server - Greenhouse Designs - Add New ----
  ###############################################.
  ## Add New Greenhouse Design ####
  GHD_values <- reactiveValues() # empty list to store reactive values for new design
  AP_values <- reactiveValues() # empty list to store reactive values for percent of area to light by month
  
  #UI output of matrix values default to 100%
  output$percent_mat <- renderUI({
    matrixInput(
      "per_area_input",#name of matrix input
      value = m_area,#value which is the matrix
      rows = list(names = TRUE,extend = FALSE),
      cols = list(names = TRUE),
      class = "numeric"
    )
  })
  
  # List of 100% area if the area percent matrix is never updated (did not click button)
  AP_values$percent <- m_area
  
  # Observe event to update the percent area matrix if the "update_permat" button is clicked
  observeEvent(input$update_permat, {
    #make a dataframe of the input matrix
    per_m <- data.frame(input$per_area_input)
    
    #conditional outcomes of update_permat button
    #all months must have a % entered and the % must be between 0 and 100
    if(all(!is.na(per_m))){#if all month have a % entered
      if(all(per_m >= 0,per_m <=  100)){ #if all % are between 0 and 100
        #replace defult reactive list with the percent area input
        for (i in 1:length(per_m)){
          AP_values$percent[[i]] <- per_m[i][[1]]
        }
        
        #UI output of matrix - change values to default on input
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = input$per_area_input,#vaule which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        shinyalert("Success!")
      } else {
        #pop up warning message
        shinyalert("Oops!", "Precent must be between 0 and 100.", type = "error")
        
        #UI output of matrix reset to values default to 100%
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = m_area,#vaule which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        #return list to 100% area for all months
        AP_values$percent <- m_area
      }
    } else {
      #pop up warning message
      shinyalert("Oops!", "Each month must have a percent area.", type = "error")
      
      #UI output of matrix reset to values default to 100%
      output$percent_mat <- renderUI({
        matrixInput(
          "per_area_input",#name of matrix input
          value = m_area,#vaule which is the matrix
          rows = list(names = TRUE,extend = FALSE),
          cols = list(names = TRUE),
          class = "numeric"
        )
      })
      #return list to 100% area for all months
      AP_values$percent <- m_area
    }
  })
  
  # Observe event for when the "Design_save" button is clicked
  observeEvent(input$GHD_save, {
    #Test to make sure all fields are filled
    field_test <- FALSE
    
    if(input$New_GHD_Name=="" | input$New_GHD_Length=="" | input$New_GHD_Width==""| input$New_GHD_Trans=="" | input$New_GHD_TargetDLI=="" | input$New_GHD_Efficacy=="" | input$New_GHD_Percentile=="" | input$New_GHD_hours=="" | input$New_GHD_demand_charge_price==""){
      shinyalert("Please make sure all fields are filled.", type="error")
    } else {
      field_test <- TRUE
    }
    
    #Test to make sure only numbers are used for inputs
    trans_test <- as.numeric(input$New_GHD_Trans)
    DLI_test <- as.numeric(input$New_GHD_TargetDLI)
    eff_test <- as.numeric(input$New_GHD_Efficacy)
    perc_test <- as.numeric(input$New_GHD_Percentile)
    hours_test <- as.numeric(input$New_GHD_hours)
    demand_test <- as.numeric(input$New_GHD_demand_charge_price)
    length_test <- as.numeric(input$New_GHD_Length)
    width_test <- as.numeric(input$New_GHD_Width)
    f_trans_test <- FALSE
    f_DLI_test <- FALSE
    f_Eff_test <- FALSE
    f_perc_test <- FALSE
    f_hours_test <- FALSE
    f_demand_test <- FALSE
    lw_test <- FALSE
    
    if(is.na(trans_test) | trans_test <= 1 | trans_test > 100){
      shinyalert("Please check Transmission percentage: no % needed, enter as 90 if 90%", type="error")
      updateTextInput(session,"New_GHD_Trans", value="")
    } else{
      f_trans_test <- TRUE
    }
    
    if(is.na(DLI_test) | DLI_test <= 0){
      shinyalert("Please check Target DLI: only positive numerical values are accepted", type="error")
      updateTextInput(session,"New_GHD_TargetDLI", value="")
    } else{
      f_DLI_test <- TRUE
    }
    
    if(is.na(eff_test) | eff_test <= 0 | eff_test > 100){
      shinyalert("Please check Lighting Efficacy: only positive numerical values between 0 and 100 are accepted", type="error")
      updateTextInput(session,"New_GHD_Efficacy", value="")
    } else{
      f_Eff_test <- TRUE
    }
    
    if(is.na(perc_test) | perc_test <= 1 | perc_test > 100){
      shinyalert("Please check percentile: no % needed, enter as 90 if 90%", type="error")
      updateTextInput(session,"New_GHD_Percentile", value="")
    } else{
      f_perc_test <- TRUE
    }
    
    if(is.na(length_test) | is.na(width_test) | length_test <= 0 | width_test <= 0 | input$New_GHD_Length=="0" | input$New_GHD_Width=="0"){
      shinyalert("Please enter positive non zero values for length and width", type="error")
      updateTextInput(session,"New_GHD_Length", value="1")
      updateTextInput(session,"New_GHD_Width", value="1")
    } else{
      lw_test <- TRUE
    }
    
    if(is.na(hours_test) | hours_test > 24 | hours_test < 0){
      shinyalert("Please check Hours on: only positive numerical values between 0 and 24 are accepted", type="error")
      updateTextInput(session,"New_GHD_hours", value="")
    } else{
      f_hours_test <- TRUE
    }
    
    if(is.na(demand_test) | demand_test < 0 | demand_test > 100){
      shinyalert("Please check Monthly Demand Charge Price: only positive numerical values between 0 and 100 are accepted", type="error")
      updateTextInput(session,"New_GHD_demand_charge_price", value="")
    } else{
      f_demand_test <- TRUE
    }
    
    #store input values in reactive list
    if(field_test==TRUE & f_trans_test==TRUE & f_DLI_test==TRUE & f_Eff_test==TRUE & lw_test==TRUE & f_perc_test==TRUE & f_hours_test==TRUE & f_demand_test==TRUE){
      GHD_values$Name <- input$New_GHD_Name
      GHD_values$Area <- isolate({input$New_GHD_Length}) * isolate({input$New_GHD_Width})
      GHD_values$Trans <- input$New_GHD_Trans
      GHD_values$TargetDLI <- input$New_GHD_TargetDLI
      GHD_values$Efficacy <- input$New_GHD_Efficacy
      GHD_values$Percentile <- input$New_GHD_Percentile
      GHD_values$hours <- input$New_GHD_hours
      GHD_values$demand_charge_price <- input$New_GHD_demand_charge_price
      
      shinyalert("Success! Now go to the Electricity Cost tab to view your results.")
    }
    
    #convert percent to a decimal 
    AP_values$PerOfOne <- data.frame(AP_values$percent)/100
    
    #loop to add percent area by month records to percent_area table 
    percent <- list()
    month <- list()
    for (i in 1:12){ #for each month of the year
      PerOfOne_i <- AP_values$PerOfOne[1,i] #percent of area
      MonthNum_i <- i #month number
      percent[[i]] <- PerOfOne_i
      month[[i]] <- MonthNum_i
    }
    percent1 <- unlist(percent)
    month1 <- unlist(month)
    
    Area <- data.frame(percent1,month1)
    names(Area) <- c("Percent", "Month")
    
    #Make copy of area table to display to user
    Area2 <- Area
    Area2$Percent2 <- label_percent()(Area$Percent) 
    
    #Transpose the area table
    t_area <- t(Area2)
    colnames(t_area) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    
    #Remove extra columns to display only month and area %
    output$Percent_table_SC <- renderTable(t_area[3,, drop=F])
    
    # Create lists to store values used in the for-loop below to manipulate the start date of the year to that of the first Monday of the year
    val <- list()
    weeksum <- list()
    weekavg <- list()
    weekStart <- c(FirstMonday) 
    
    # The outer loop iterate through the weeks while the inner loop adds the days
    for(i in 1:53){
      weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
      for (j in seq(0,7,1)){
        val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
      }
      weeksum[[i]] <- val
      weekavg[i] <- mean(sapply(weeksum[[i]],sum))
    }
    
    #loop to drop the 53rd week if it starts in the next year
    if (year(weekStart[53]) == year(Sys.Date() + years(1))){
      weeksum <- weeksum[-53]
      weekavg <- weekavg[-53]
    }
    length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
    
    #unlist the values
    weekavg1 <- unlist(weekavg)
    week1 <- unlist(weekStart)
    
    #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
    if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]] 
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
      weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
      weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
      weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
      weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
      weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
      weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
      weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
    }
    
    Area_week <- data.frame(weekavg1,week1)
    names(Area_week) <- c("Percent", "Week")
    
    #Make copy of area table to display to user
    Area_week_2 <- Area_week
    Area_week_2$Percent2 <- label_percent()(Area_week$Percent) 
    
    #Transpose the area table
    t_area_week <- t(Area_week_2)
    colnames(t_area_week) = week1
    
    #Remove extra columns to display only month and area %
    output$table_week <- renderTable(t_area_week[3,, drop=F])
  })
  
  ###############################################.
  ## Server - Electricity Costs ----
  ###############################################.
  
  # Location value to display on the results page
  output$ui_SC_location <- renderUI({
    textInput(inputId = 'SC_Select_location',
              label ='Location',
              width = "auto",
              value=Loc_values$Name)
  })
  
  # Design value to display on the results page
  output$ui_SC_design <- renderUI({
    textInput(inputId = 'SC_Select_design',
              label ='Design',
              width = "auto",
              value=GHD_values$Name)
  })
  
  ####################################.
  ## Calculations and Graphs  ####
  ####################################.
  observeEvent(input$SC_Choices, { # When action button "SC_Choices" is clicked run code in this section
    
    ####################################.
    ## Variables and Dataframes ####
    ####################################.
    # Upon clicking to view results, display a success message for user feedback
    shinyalert("Success! Updating your results.")
    
    # Then run this code below
    {
      # Assign variables from user inputs
      Location <- as.character(Loc_values$Name)
      ele_rate <- as.numeric(Loc_values$Elec_Rate)
      Design <- as.character(GHD_values$Name)
      trans <- as.numeric(GHD_values$Trans)
      target <- as.numeric(GHD_values$TargetDLI)
      area <- as.numeric(GHD_values$Area)
      efficacy <- as.numeric(GHD_values$Efficacy)
      percentile <- as.numeric(GHD_values$Percentile)/100
      hours <- as.numeric(GHD_values$hours)
      demand_charge_price <- as.numeric(GHD_values$demand_charge_price)
      
      #Calculate the area for each month based on percent of coverage
      PA_Months <- as.numeric(AP_values$percent)/100
      monthly_areaft2 <- vector(mode="numeric", length=12)
      monthly_aream2 <- vector(mode="numeric", length=12)
      
      for(i in 1:12) {
        monthly_areaft2[i] <- area*(PA_Months[i])
        monthly_aream2[i] <- monthly_areaft2[i]/10.764
      }
      
      # This code below is repetitive but needed for the weekly costs table
      val <- list()
      weeksum <- list()
      weekavg <- list()
      weekStart <- c(FirstMonday)
      
      for(i in 1:53){
        weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
        for (j in seq(0,7,1)){
          val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
        }
        weeksum[[i]] <- val
        weekavg[i] <- mean(sapply(weeksum[[i]],sum))
      }
      
      #loop to drop the 53rd week if it starts in the next year
      if (year(weekStart[53]) == year(Sys.Date() + years(1))){
        weeksum <- weeksum[-53]
        weekavg <- weekavg[-53]
      }
      length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
      
      #unlist the values
      weekavg1 <- unlist(weekavg)
      week1 <- unlist(weekStart)
      
      #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
      if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]] 
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
        weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
        weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
        weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
        weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
        weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
        weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
        weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      }
      
      PA_Weeks <- weekavg1
      
      # Load in the radiation data set from earlier to then use here
      radiation <- XTRA_values$radiation
      
      # Create a weekly version of the radiation data set
      radiationWeek <- radiation
      radiationWeek$Week <- week(date(radiation$TimeStamp) - day(FirstMonday - days(1)))
      radiationWeek$Month <- month(date(radiation$TimeStamp))
      
      # Store the monthly aream2 for later use
      XTRA_values$monthly_aream2 <- monthly_aream2
      
      ####################################.
      ## Calculations for Ele Cost ####
      ####################################.
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiation$ghi <- (as.numeric(radiation$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      SC_df <- radiation %>%
        group_by(date(TimeStamp),Month) %>%                   
        summarize(dli_umol = sum(ghi)) %>%
        na.omit
      colnames(SC_df) <- c("TimeStamp","Month","dli_umol")
      # 3. Convert micromoles to moles
      SC_df$dli_mol = SC_df$dli_umol/(1*10^6) 
      # 4. Calculate Sunlight Present in Greenhouse (SPG):
      SC_df$trans <- trans  # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      SC_df$spg_mol <- SC_df$dli_mol * (trans/100) #transmission as %
      # 5. Calculate Supplemental Light Needed (SL):
      SC_df$dli_target <- target # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      SC_df$SL_mol <- ifelse(SC_df$spg_mol > target, 0, target - SC_df$spg_mol)
      # 6. Calculate the percentile suppl DLI. If the value is higher than the percentile, assign percentile value
      SL_mol_Decreasing <- sort(SC_df$SL_mol,decreasing = TRUE)
      Percentile_SL_mol_DLI <- as.numeric(quantile(SL_mol_Decreasing,percentile)) #Grab the percentile covered
      SC_df$SL_mol <- ifelse(SC_df$SL_mol >= Percentile_SL_mol_DLI, yes = Percentile_SL_mol_DLI, no = SC_df$SL_mol) #We assign the percentile DLI to the value because the user doesn't want to go over the supp DLI, so they shouldn't be charged over the supp DLI.
      # Store this data set for later use
      XTRA_values$SC_df <- SC_df
      # 7. Calculate Total light intake
      SC_df$totalDLImols <- SC_df$spg_mol + SC_df$SL_mol
      # 8. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      SC_df$Elec_rate <- ele_rate # Electricity Cost $/(m^2d)
      SC_df$SC_m2d <- (SC_df$SL_mol/(efficacy*3.6))*ele_rate
      # 9. Convert the grower input of greenhouse size in square feet to square meters
      SC_df$aream2 <- (area/10.764) # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764
      
      # Create a data set with these values grouped by month
      SC_df_M <- SC_df %>%
        group_by(Month) %>%
        summarize(SC_m2m  = sum(SC_m2d)) %>%
        na.omit
      # Create the columns below. PA_Months represents the PercentArea for each month, which is calculated in the percent matrix
      SC_df_M$Month2 <- as.numeric(SC_df_M$Month)
      SC_df_M$SC_acre <- SC_df_M$SC_m2m*4046.86*PA_Months
      SC_df_M$SC_ftsq <- SC_df_M$SC_m2m/10.764*PA_Months
      SC_df_M$SC_design <- SC_df_M$SC_m2m*(monthly_aream2[SC_df_M$Month2])
      # Rename the column headers and round the values
      SC_df_M_Display <- SC_df_M[c("Month2","SC_ftsq","SC_acre","SC_design")]
      colnames(SC_df_M_Display) <- c("Month","$ Per ft2","$ Per acre","$ Total Design")
      SC_df_M_Display$`$ Per ft2` <- prettyNum(round(SC_df_M_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      SC_df_M_Display$`$ Per acre` <- prettyNum(round(SC_df_M_Display$`$ Per acre`, digits = 0), big.mark = ',')
      SC_df_M_Display$`$ Total Design` <- prettyNum(round(SC_df_M_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiationWeek$ghi <- (as.numeric(radiationWeek$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      SC_df_Week <- radiationWeek %>%
        group_by(date(TimeStamp),Week) %>%                    
        summarize(dli_umol = sum(ghi)) %>%
        na.omit
      colnames(SC_df_Week) <- c("TimeStamp","Week","dli_umol")
      # 3. Convert micromoles to moles
      SC_df_Week$dli_mol = SC_df_Week$dli_umol/(1*10^6)
      SC_df_Week$trans <- trans # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      # 4. Calculate Sunlight Present in Greenhouse (SPG):
      SC_df_Week$spg_mol <- SC_df_Week$dli_mol * (trans/100) #transmission as %
      # 5. Calculate Supplemental Light Needed (SL):
      SC_df_Week$dli_target <- target # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      SC_df_Week$SL_mol <- ifelse(SC_df_Week$spg_mol > target, 0, target - SC_df_Week$spg_mol)
      # Store a simple data set to be used for the dynamic functionality
      XTRA_values$SC_df1 <- SC_df_Week[,c('Week', 'SL_mol')]
      # 6. Calculate the percentile suppl DLI. If the value is higher than the percentile, assign percentile value
      SC_df_Week$SL_mol <- ifelse(SC_df_Week$SL_mol >= Percentile_SL_mol_DLI, yes = Percentile_SL_mol_DLI, no = SC_df_Week$SL_mol) #We assign the percentile DLI to the value because the user doesn't want to go over the supp DLI, so they shouldn't be charged over the supp DLI.
      # 7. Calculate Total light intake
      SC_df_Week$totalDLImols <- SC_df_Week$spg_mol + SC_df_Week$SL_mol
      # 8. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      SC_df_Week$Elec_rate <- ele_rate # Electricity Cost $/(m^2d)
      SC_df_Week$SC_m2d <- (SC_df_Week$SL_mol/(efficacy*3.6))*ele_rate
      # 9. Convert the grower input of greenhouse size in square feet to square meters
      SC_df_Week$aream2 <- (area/10.764) # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764
      
      # Create two empty vectors equal to the number of weeks in the year and fill them with the weekly area lit by interacting PA_Weeks with area
      weekly_areaft2 <- vector(mode="numeric", length = length(weekStart))
      weekly_aream2 <- vector(mode="numeric", length = length(weekStart))
      for(i in 1:length(weekStart)) {
        weekly_areaft2[i] <- area*(PA_Weeks[i])
        weekly_aream2[i] <- weekly_areaft2[i]/10.764
      }
      
      # Create a weekly table to summarize costs by week
      SC_df_W <- SC_df_Week %>%
        group_by(Week) %>%
        summarize(SC_m2m  = sum(SC_m2d)) %>%
        na.omit
      
      # Store this value to be used in the interactive section
      XTRA_values$weekly_aream2 <- weekly_aream2
      
      # Create a weekly cost table to display to the user
      SC_df_W$WeekNum <- paste(SC_df_W$Week)  
      SC_df_W$SC_acre <- SC_df_W$SC_m2m*4046.86*PA_Weeks
      SC_df_W$SC_ftsq <- (SC_df_W$SC_m2m/10.764)*PA_Weeks
      SC_df_W$SC_design <- rep(SC_df_W$SC_m2m*(weekly_aream2[as.numeric(SC_df_W$Week)]))
      # Create a list of start dates
      a <- list()
      for (i in 1:52) {
        a[i] = toString(x = weekStart[i])
      }
      for (i in 1:52) {
        a[i] = substr(weekStart[i],nchar(weekStart[i])+1, 10)
      }
      # Rename the columns and round the values
      SC_df_W_Display <- SC_df_W[c("WeekNum","SC_ftsq","SC_acre","SC_design")]
      colnames(SC_df_W_Display) <- c("Week","$ Per ft2","$ Per acre","$ Total Design")
      SC_df_W_Display$`$ Per ft2` <- prettyNum(round(SC_df_W_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      SC_df_W_Display$`$ Per acre` <- prettyNum(round(SC_df_W_Display$`$ Per acre`, digits = 0), big.mark = ',')
      SC_df_W_Display$`$ Total Design` <- prettyNum(round(SC_df_W_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Store this summary table to be used in the interactive section
      XTRA_values$SC_df_W_Display <- SC_df_W_Display
      
      
      # Calculate the estimated annual electricity cost & round the values
      Annual_SC <- data.frame(sum(SC_df_W$SC_ftsq),sum(SC_df_W$SC_acre),sum(SC_df_W$SC_design))
      colnames(Annual_SC) <- c("ASC_df_ft2m","ASC_df_acre","ASC_df_design")
      Annual_SC_Display <- Annual_SC
      colnames(Annual_SC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
      Annual_SC_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      Annual_SC_Display$`$ Per acre` <- prettyNum(round(Annual_SC_Display$`$ Per acre`, digits = 0), big.mark = ',')
      Annual_SC_Display$`$ Total Design` <- prettyNum(round(Annual_SC_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Store this summary table to be used in the interactive section
      XTRA_values$Annual_SC_Display <- Annual_SC_Display
      
      # Grower Input Table
      output$SC_table <- renderTable({
        # Create a dataframe that will be used to display the user's inputs back to them
        SC_table_output <- data.frame(trans,target,efficacy,ele_rate, hours, demand_charge_price, percentile*100, area)
        names(SC_table_output) <- (c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Hours On", "Demand Charge ($/kW/month)",  "Percentile (%)", "Area (ft2)"))
        # Needed to keep the formatting pretty
        SC_table_output
      })
      
      # Create an hourly DLI table
      DLI_Hourly <- radiation %>%
        group_by(date(TimeStamp),hour(TimeStamp)) %>%                   
        summarize(dli_h_umol = sum(ghi)) %>% 
        na.omit
      colnames(DLI_Hourly) <- c("TimeStamp","Month","dli_h_umol")
      
      # Convert micromoles to moles to get DLI (or HLI since this is an hourly value)
      DLI_Hourly$dli_h_mol = DLI_Hourly$dli_h_umol/(1*10^6)
      
      # Create a DLI table
      DLI_table <- DLI_Hourly %>%
        group_by(date(TimeStamp)) %>%
        summarize(dli_mol = sum(dli_h_mol)*(trans/100)) %>% 
        na.omit()
      colnames(DLI_table) <- c("Date", "Natural_DLI", "DLI_Cost", "Aream2") #natural accounts for efficacy
      
      # Create a plot that visualized the natural sunlight that enters the greenhouse after accounting for transmission for each day, and create an intercept representing the target DLI based on the percentile entered
      # A. Calculate the Xth percentile for Sunlight DLI
      DLI_table$Natural_DLI_Reached <- DLI_table$Natural_DLI >= target
      # B. Visualize the data
      output$naturalDLI <- renderPlot({
        ggplot(DLI_table,aes(Date,Natural_DLI)) +
          geom_point(aes(color = Natural_DLI_Reached)) + # Meaning does the natural sunlight entering the greenhouse fall above or below the target DLI
          geom_hline(yintercept = target, color = "red", size = .5) +
          labs(title = paste0("Days at or below a DLI of ", target),
               caption = paste0("Red line indicates the target DLI of ",target, "(mol/m2/d)")) +
          theme(plot.caption = element_text(size = 13)) +
          ylab("Sunlight DLI (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
      })
      
      # Create a plot to visualize the amount of supplemental light needed each day where the intercept represents the DLI capacity needed to cover X percent of days
      # A. calculate the amount of supplemental light needed each day
      DLI_table$Supplemental <- target - DLI_table$Natural_DLI
      # If there is ample light for the day, assign 0 to supplemental light needed
      DLI_table$Supplemental[DLI_table$Supplemental < 0] <- 0
      DLI_Supplemental_Decreasing <- sort(DLI_table$Supplemental,decreasing = TRUE)
      # B. calculate the Xth percentile
      Percentile_Supplemental_DLI <- as.numeric(quantile(DLI_Supplemental_Decreasing,percentile))
      # Create a column for T/F if the target DLI was reached (this is used to color the points in ggplot)
      DLI_table$Target_DLI_Reached <- DLI_table$Supplemental <= Percentile_Supplemental_DLI
      # C. Visualize the data
      output$supplementalDLI <- renderPlot({
        ggplot(DLI_table,aes(Date,Supplemental)) +
          geom_point(aes(color = Target_DLI_Reached)) + # Meaning does the supplemental light needed to reach the target DLI fall above or below the calculated system capacity
          geom_hline(yintercept = Percentile_Supplemental_DLI, color = "red", size = .5) +
          labs(title = paste0("Supplemental light needed to reach a DLI of ",
                              target, "(mol/m2/d) ",
                              percentile*100, "% of the days"),
               caption = paste0("Red line indicates the maximum supplemental DLI needed, which is ",round(Percentile_Supplemental_DLI,2), "(mol/m2/d)")) +
          theme(plot.caption = element_text(size = 13)) +
          ylab("Supplemental DLI needed (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
      })
      
      # Calculate the implicit PPFD Capability of the system
      PPFD_capability <- Percentile_Supplemental_DLI/(hours*0.0036)
      
      # Store the original PPFD_capability for the reset feature. Percentile_Supplemental_DLI doesn't need to be stored because suppint is used in the adjust feature
      XTRA_values$PPFD_capability_1 <- PPFD_capability
      XTRA_values$Percentile_Supplemental_DLI_1 <- Percentile_Supplemental_DLI
      
      # Output the PPFD Capability & Supplemental DLI as a valueBox for users to see
      output$PPFD_tile <- renderValueBox({
        valueBox(value = tags$p("Required Lighting System Capacity", style = "font-size: 80%;"), 
                 subtitle = tags$p(paste0(round(PPFD_capability, digits = 0), " umol/m2/s"), style = "font-size: 200%;"))
      })
      output$DLI_tile <- renderValueBox({
        valueBox(value = tags$p("Max Supplemental DLI Capacity", style = "font-size: 80%;"), 
                 subtitle = tags$p(paste0(round(Percentile_Supplemental_DLI, digits = 2), " mol/m2/d"), style = "font-size: 200%;"))
      })
      
      # Create an empty vector equal to 12 (months) to store the monthly demand charge and interact the percent of the greenhouse lit
      demand_charge <- vector(mode="numeric", length=12)
      for(i in 1:12) {
        demand_charge[i] <- demand_charge_price * (((PPFD_capability/efficacy)*monthly_aream2[i])/1000)
      }
      # Store the monthly demand charge in a monthly table to be used later
      SC_df_M$SC_demand_design <- demand_charge
      
      # Create an estimated annual cost summary table
      Annual_SC_demand <- data.frame(sum(SC_df_M$SC_demand_design)/area,sum(SC_df_M$SC_demand_design)/(area/43560),sum(SC_df_M$SC_demand_design))
      colnames(Annual_SC_demand) <- c("ADSC_df_ft2m","ADSC_df_acre","ADSC_df_design")
      Annual_SC_demand_Display <- Annual_SC_demand
      # Rename and round the values
      colnames(Annual_SC_demand_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
      Annual_SC_demand_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_demand_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      Annual_SC_demand_Display$`$ Per acre` <- prettyNum(round(Annual_SC_demand_Display$`$ Per acre`, digits = 0), big.mark = ',')
      Annual_SC_demand_Display$`$ Total Design` <- prettyNum(round(Annual_SC_demand_Display$`$ Total Design`, digits = 0), big.mark = ',')
      # Store this demand charges summary table to be used in the interactive section
      XTRA_values$Annual_SC_demand_Display <- Annual_SC_demand_Display
      
      # Create an event that recreates the values when a user clicks on the graph (starts the dynamic functionality)
      observeEvent(input$plot_click_supp,{
        # Import values
        XTRA_values$suppInt <- round(input$plot_click_supp$y, 1) # Store the y value of the user click which represents the DLI capacity of the system
        suppInt <- XTRA_values$suppInt
        SC_df1 <- XTRA_values$SC_df1
        SC_df <- XTRA_values$SC_df
        weekly_aream2 <- XTRA_values$weekly_aream2
        
        # Manipulate SCdf1 to hold adjusted values (weekly)
        SC_df1$SL_mol <- ifelse(SC_df1$SL_mol >= suppInt, yes = suppInt, no = SC_df1$SL_mol)
        SC_df1$Elec_rate <- ele_rate
        SC_df1$SC1_m2d <- (SC_df1$SL_mol/(efficacy*3.6))*ele_rate
        SC_df1$aream2 <- (area/10.764)
        
        # Manipulate SC_df to hold adjusted values (monthly)
        SC_df$SL_mol <- ifelse(SC_df$SL_mol >= suppInt, yes = suppInt, no = SC_df$SL_mol)
        SC_df$totalDLImols <- SC_df$spg_mol + SC_df$SL_mol
        SC_df$Elec_rate <- ele_rate
        SC_df$SC_m2d <- (SC_df$SL_mol/(efficacy*3.6))*ele_rate
        SC_df$aream2 <- (area/10.764)
        
        # Store the user input table to be outputted in the excel file
        XTRA_values$SC_df_Adjusted <- SC_df
        
        # Calculate weekly electricity costs
        SC_df_W <- SC_df1 %>%
          group_by(Week) %>%
          summarize(SC_m2m  = sum(SC1_m2d)) %>%
          na.omit
        SC_df_W$WeekNum <- paste(SC_df_W$Week)  
        SC_df_W$SC_acre <- SC_df_W$SC_m2m*4046.86*PA_Weeks
        SC_df_W$SC_ftsq <- SC_df_W$SC_m2m/10.764*PA_Weeks
        SC_df_W$SC_design <- rep(SC_df_W$SC_m2m*(weekly_aream2[as.numeric(SC_df_W$Week)]))
        
        # Cleanup weekly electricity cost and create an output table
        SC_df_W_Display <- SC_df_W[c("WeekNum","SC_ftsq","SC_acre","SC_design")]
        colnames(SC_df_W_Display) <- c("Week","$ Per ft2","$ Per acre","$ Total Design") 
        SC_df_W_Display$`$ Per ft2` <- prettyNum(round(SC_df_W_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        SC_df_W_Display$`$ Per acre` <- prettyNum(round(SC_df_W_Display$`$ Per acre`, digits = 0), big.mark = ',')
        SC_df_W_Display$`$ Total Design` <- prettyNum(round(SC_df_W_Display$`$ Total Design`, digits = 0), big.mark = ',')
        XTRA_values$SC_df_W_Display_Adjusted <- SC_df_W_Display
        
        # Create and clean up annual electricity cost and create an output table
        Annual_SC <- data.frame(sum(SC_df_W$SC_ftsq),sum(SC_df_W$SC_acre),sum(SC_df_W$SC_design))
        colnames(Annual_SC) <- c("ASC_df_ft2m","ASC_df_acre","ASC_df_design")
        Annual_SC_Display <- Annual_SC
        colnames(Annual_SC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
        Annual_SC_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        Annual_SC_Display$`$ Per acre` <- prettyNum(round(Annual_SC_Display$`$ Per acre`, digits = 0), big.mark = ',')
        Annual_SC_Display$`$ Total Design` <- prettyNum(round(Annual_SC_Display$`$ Total Design`, digits = 0), big.mark = ',')
        
        # Create a monthly electricity cost to be displayed in the excel sheet via the adjusted download button
        SC_df_M <- SC_df %>%
          group_by(Month) %>%
          summarize(SC_m2m  = sum(SC_m2d)) %>%
          na.omit
        SC_df_M$Month2 <- as.numeric(SC_df_M$Month)
        SC_df_M$SC_acre <- SC_df_M$SC_m2m*4046.86*PA_Months
        SC_df_M$SC_ftsq <- SC_df_M$SC_m2m/10.764*PA_Months
        SC_df_M$SC_design <- SC_df_M$SC_m2m*(monthly_aream2[SC_df_M$Month2])
        
        # Create and clean up monthly electricity cost and create an output table
        SC_df_M_Display <- SC_df_M[c("Month2","SC_ftsq","SC_acre","SC_design")]
        colnames(SC_df_M_Display) <- c("Month","$ Per ft2","$ Per acre","$ Total Design")
        SC_df_M_Display$`$ Per ft2` <- prettyNum(round(SC_df_M_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        SC_df_M_Display$`$ Per acre` <- prettyNum(round(SC_df_M_Display$`$ Per acre`, digits = 0), big.mark = ',')
        SC_df_M_Display$`$ Total Design` <- prettyNum(round(SC_df_M_Display$`$ Total Design`, digits = 0), big.mark = ',')
        XTRA_values$SC_df_M_Display_Adjusted <- SC_df_M_Display
        
        # Calculate the implicit PPFD Capability of the system
        PPFD_capability <- suppInt/(hours*0.0036)
        
        # Store adjusted values for excel adjusted download
        XTRA_values$PPFD_capability_adjusted <- PPFD_capability
        XTRA_values$suppInt_adjusted <- suppInt
        
        # Output the PPFD Capability & Supplemental DLI as a valueBox for users to see
        output$PPFD_tile <- renderValueBox({
          valueBox(value = tags$p("Required Lighting System Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(PPFD_capability, digits = 0), " umol/m2/s"), style = "font-size: 200%;"))
        })
        output$DLI_tile <- renderValueBox({
          valueBox(value = tags$p("Max Supplemental DLI Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(suppInt, digits = 2), " mol/m2/d"), style = "font-size: 200%;"))
        })
        
        # Create an empty vector equal to 12 (months) to store the monthly demand charge and interact the percent of the greenhouse lit
        demand_charge <- vector(mode="numeric", length=12)
        for(i in 1:12) {
          demand_charge[i] <- demand_charge_price * (((PPFD_capability/efficacy)*monthly_aream2[i])/1000)
        }
        # Store the monthly demand charge in a monthly table to be used later
        SC_df_M$SC_demand_design <- demand_charge
        
        # Create an estimated annual cost summary table
        Annual_SC_demand <- data.frame(sum(SC_df_M$SC_demand_design)/area,sum(SC_df_M$SC_demand_design)/(area/43560),sum(SC_df_M$SC_demand_design))
        colnames(Annual_SC_demand) <- c("ADSC_df_ft2m","ADSC_df_acre","ADSC_df_design")
        Annual_SC_demand_Display <- Annual_SC_demand
        # Rename and round the values
        colnames(Annual_SC_demand_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
        Annual_SC_demand_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_demand_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        Annual_SC_demand_Display$`$ Per acre` <- prettyNum(round(Annual_SC_demand_Display$`$ Per acre`, digits = 0), big.mark = ',')
        Annual_SC_demand_Display$`$ Total Design` <- prettyNum(round(Annual_SC_demand_Display$`$ Total Design`, digits = 0), big.mark = ',')
        
        # Store the adjusted values for excel download
        XTRA_values$Annual_SC_demand_Display_Adjusted <- Annual_SC_demand_Display
        
        # Calculate the new percentile that results when the user interacts with the graph so it can be stored in the adjusted user values dataframe
        DLI_table$Target_DLI_Reached <- DLI_table$Supplemental <= XTRA_values$suppInt
        percentile2 <- round((sum(DLI_table$Target_DLI_Reached == TRUE)/length(DLI_table$Target_DLI_Reached)),3)
        # Create a dataframe with the adjusted user inputs (percentile changes)
        Grower_input_Adjusted <- data.frame(trans,target,efficacy,ele_rate, hours, demand_charge_price, percentile2*100, area) #move to within reactive
        names(Grower_input_Adjusted) <- (c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Hours On", "Demand Charge ($/kW/month)",  "Adjusted Percentile (%)", "Area (ft2)"))
        # Store the user input table to be outputted in the excel file
        XTRA_values$Grower_input_Adjusted <- Grower_input_Adjusted
        
        # Send the tables to the UI
        output$Annual_SC_Display_table<- renderTable({ 
          Annual_SC_Display # Estimated Annual Electricity Summary Costs
        })
        output$DSC_Display_table<- renderTable({
          Annual_SC_demand_Display # Estimated Annual Demand Summary Costs
        })
        output$SC_df_W_table <- renderTable({
          SC_df_W_Display # Estimated Weekly Electricity Costs
        })
      })
      
      # Run the code below when the user clicks around to interact with the graph
      observeEvent(input$SC_Adjust_Supp, {
        # Import values
        SC_df1 <- XTRA_values$SC_df1 # Used for weekly costs
        SC_df <- XTRA_values$SC_df # Used for monthly costs
        suppInt <- XTRA_values$suppInt
        weekly_aream2 <- XTRA_values$weekly_aream2
        
        # Manipulate SCdf1 to hold adjusted values (weekly)
        SC_df1$SL_mol <- ifelse(SC_df1$SL_mol >= suppInt, yes = suppInt, no = SC_df1$SL_mol)
        SC_df1$Elec_rate <- ele_rate
        SC_df1$SC1_m2d <- (SC_df1$SL_mol/(efficacy*3.6))*ele_rate
        SC_df1$aream2 <- (area/10.764)
        
        # Manipulate SC_df to hold adjusted values (monthly)
        SC_df$SL_mol <- ifelse(SC_df$SL_mol >= suppInt, yes = suppInt, no = SC_df$SL_mol)
        SC_df$totalDLImols <- SC_df$spg_mol + SC_df$SL_mol
        SC_df$Elec_rate <- ele_rate
        SC_df$SC_m2d <- (SC_df$SL_mol/(efficacy*3.6))*ele_rate
        SC_df$aream2 <- (area/10.764)
        
        # Store the user input table to be outputted in the excel file
        XTRA_values$SC_df_Adjusted <- SC_df
        
        # Calculate weekly electricity costs
        SC_df_W <- SC_df1 %>%
          group_by(Week) %>%
          summarize(SC_m2m  = sum(SC1_m2d)) %>%
          na.omit
        SC_df_W$WeekNum <- paste(SC_df_W$Week)  
        SC_df_W$SC_acre <- SC_df_W$SC_m2m*4046.86*PA_Weeks
        SC_df_W$SC_ftsq <- SC_df_W$SC_m2m/10.764*PA_Weeks
        SC_df_W$SC_design <- rep(SC_df_W$SC_m2m*(weekly_aream2[as.numeric(SC_df_W$Week)]))
        
        # Cleanup weekly electricity cost and create an output table
        SC_df_W_Display <- SC_df_W[c("WeekNum","SC_ftsq","SC_acre","SC_design")]
        colnames(SC_df_W_Display) <- c("Week","$ Per ft2","$ Per acre","$ Total Design")
        SC_df_W_Display$`$ Per ft2` <- prettyNum(round(SC_df_W_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        SC_df_W_Display$`$ Per acre` <- prettyNum(round(SC_df_W_Display$`$ Per acre`, digits = 0), big.mark = ',')
        SC_df_W_Display$`$ Total Design` <- prettyNum(round(SC_df_W_Display$`$ Total Design`, digits = 0), big.mark = ',')
        XTRA_values$SC_df_W_Display_Adjusted <- SC_df_W_Display
        
        # Create and clean up annual electricity cost and create an output table
        Annual_SC <- data.frame(sum(SC_df_W$SC_ftsq),sum(SC_df_W$SC_acre),sum(SC_df_W$SC_design))
        colnames(Annual_SC) <- c("ASC_df_ft2m","ASC_df_acre","ASC_df_design")
        Annual_SC_Display <- Annual_SC
        colnames(Annual_SC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
        Annual_SC_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        Annual_SC_Display$`$ Per acre` <- prettyNum(round(Annual_SC_Display$`$ Per acre`, digits = 0), big.mark = ',')
        Annual_SC_Display$`$ Total Design` <- prettyNum(round(Annual_SC_Display$`$ Total Design`, digits = 0), big.mark = ',')
        
        # Create a monthly electricity cost to be displayed in the excel sheet via the adjusted download button
        SC_df_M <- SC_df %>%
          group_by(Month) %>%
          summarize(SC_m2m  = sum(SC_m2d)) %>% 
          na.omit
        SC_df_M$Month2 <- as.numeric(SC_df_M$Month)
        SC_df_M$SC_acre <- SC_df_M$SC_m2m*4046.86*PA_Months
        SC_df_M$SC_ftsq <- SC_df_M$SC_m2m/10.764*PA_Months
        SC_df_M$SC_design <- SC_df_M$SC_m2m*(monthly_aream2[SC_df_M$Month2])
        
        # Create and clean up monthly electricity cost and create an output table
        SC_df_M_Display <- SC_df_M[c("Month2","SC_ftsq","SC_acre","SC_design")]
        colnames(SC_df_M_Display) <- c("Month","$ Per ft2","$ Per acre","$ Total Design")
        SC_df_M_Display$`$ Per ft2` <- prettyNum(round(SC_df_M_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        SC_df_M_Display$`$ Per acre` <- prettyNum(round(SC_df_M_Display$`$ Per acre`, digits = 0), big.mark = ',')
        SC_df_M_Display$`$ Total Design` <- prettyNum(round(SC_df_M_Display$`$ Total Design`, digits = 0), big.mark = ',')
        XTRA_values$SC_df_M_Display_Adjusted <- SC_df_M_Display
        
        # Calculate the implicit PPFD Capability of the system
        PPFD_capability <- suppInt/(hours*0.0036)
        
        # Output the PPFD Capability & Supplemental DLI as a valueBox for users to see
        output$PPFD_tile <- renderValueBox({
          valueBox(value = tags$p("Required Lighting System Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(PPFD_capability, digits = 0), " umol/m2/s"), style = "font-size: 200%;"))
        })
        output$DLI_tile <- renderValueBox({
          valueBox(value = tags$p("Max Supplemental DLI Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(suppInt, digits = 2), " mol/m2/d"), style = "font-size: 200%;"))
        })
        
        # Create an empty vector equal to 12 (months) to store the monthly demand charge and interact the percent of the greenhouse lit
        demand_charge <- vector(mode="numeric", length=12)
        for(i in 1:12) {
          demand_charge[i] <- demand_charge_price * (((PPFD_capability/efficacy)*monthly_aream2[i])/1000)
        }
        # Store the monthly demand charge in a monthly table to be used later
        SC_df_M$SC_demand_design <- demand_charge
        
        # Create an estimated annual cost summary table
        Annual_SC_demand <- data.frame(sum(SC_df_M$SC_demand_design)/area,sum(SC_df_M$SC_demand_design)/(area/43560),sum(SC_df_M$SC_demand_design))
        colnames(Annual_SC_demand) <- c("ADSC_df_ft2m","ADSC_df_acre","ADSC_df_design") 
        Annual_SC_demand_Display <- Annual_SC_demand
        # Rename and round the values
        colnames(Annual_SC_demand_Display) <- c("$ Per ft2","$ Per acre","$ Total Design") 
        Annual_SC_demand_Display$`$ Per ft2` <- prettyNum(round(Annual_SC_demand_Display$`$ Per ft2`, digits = 3), big.mark = ',')
        Annual_SC_demand_Display$`$ Per acre` <- prettyNum(round(Annual_SC_demand_Display$`$ Per acre`, digits = 0), big.mark = ',')
        Annual_SC_demand_Display$`$ Total Design` <- prettyNum(round(Annual_SC_demand_Display$`$ Total Design`, digits = 0), big.mark = ',')
        
        # Store the adjusted values for excel download
        XTRA_values$Annual_SC_demand_Display_Adjusted <- Annual_SC_demand_Display
        
        # Send the tables to the UI
        output$Annual_SC_Display_table<- renderTable({
          Annual_SC_Display # Estimated Annual Electricity Summary Costs
        })
        output$DSC_Display_table<- renderTable({
          Annual_SC_demand_Display # Estimated Annual Demand Summary Costs
        })
        output$SC_df_W_table <- renderTable({
          SC_df_W_Display # Estimated Weekly Electricity Costs
        })
        
        # Create a plot showing lighting capacity (supplemental DLI) to reach target DLI X% of days
        output$supplementalDLI <- renderPlot({
          # Add target reached code and percentile change
          DLI_table$Target_DLI_Reached <- DLI_table$Supplemental <= XTRA_values$suppInt
          percentile2 <- round((sum(DLI_table$Target_DLI_Reached == TRUE)/length(DLI_table$Target_DLI_Reached)),3)
          # Visualize the data
          ggplot(DLI_table,aes(Date,Supplemental)) +
            geom_point(aes(color = Target_DLI_Reached)) + # Meaning does the supplemental light needed to reach the target DLI fall above or below the calculated system capacity
            geom_hline(yintercept = XTRA_values$suppInt, color = "red", size = .5) +
            labs(title = paste0("Supplemental light needed to reach a DLI of ",
                                target, "(mol/m2/d) ",
                                percentile2*100, "% of the days"),
                 caption = paste0("Red line indicates the maximum supplemental DLI needed, which is ",XTRA_values$suppInt, "(mol/m2/d)")) +
            theme(plot.caption = element_text(size = 13)) +
            ylab("Supplemental DLI needed (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
        })
      })
      
      # Reset the visuals and tables back to the original user inputs if the user clicks reset
      observeEvent(input$SC_Reset, {
        
        PPFD_capability_1 <- XTRA_values$PPFD_capability_1
        
        # Create a plot that visualized the natural sunlight that enters the greenhouse after accounting for transmission for each day, and create an intercept representing the target DLI based on the percentile entered
        output$naturalDLI <- renderPlot({
          # Visualize the data
          ggplot(DLI_table,aes(Date,Natural_DLI)) +
            geom_point(aes(color = Natural_DLI_Reached)) + # Meaning does the natural sunlight entering the greenhouse fall above or below the target DLI
            geom_hline(yintercept = target, color = "red", size = .5) +
            labs(title = paste0("Days at or below a DLI of ", target),
                 caption = paste0("Red line indicates the target DLI of ",target, "(mol/m2/d)")) + 
            theme(plot.caption = element_text(size = 13)) +
            ylab("Sunlight DLI (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
        })
        
        # Create a plot showing lighting capacity (supplemental DLI) to reach target DLI X% of days
        output$supplementalDLI <- renderPlot({
          # Visualize the data
          ggplot(DLI_table,aes(Date,Supplemental)) +
            geom_point(aes(color = Target_DLI_Reached)) + # Meaning does the supplemental light needed to reach the target DLI fall above or below the calculated system capacity
            geom_hline(yintercept = Percentile_Supplemental_DLI, color = "red", size = .5) +
            labs(title = paste0("Supplemental light needed to reach a DLI of ",
                                target, "(mol/m2/d) ",
                                percentile*100, "% of the days"),
                 caption = paste0("Red line indicates the maximum supplemental DLI needed, which is ",round(Percentile_Supplemental_DLI,2), "(mol/m2/d)")) +
            theme(plot.caption = element_text(size = 13)) +
            ylab("Supplemental DLI needed (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
        })
        
        # Output the PPFD Capability & Supplemental DLI as a valueBox for users to see
        output$PPFD_tile <- renderValueBox({
          valueBox(value = tags$p("Required Lighting System Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(PPFD_capability_1, digits = 0), " umol/m2/s"), style = "font-size: 200%;"))
        })
        output$DLI_tile <- renderValueBox({
          valueBox(value = tags$p("Max Supplemental DLI Capacity", style = "font-size: 80%;"), 
                   subtitle = tags$p(paste0(round(Percentile_Supplemental_DLI, digits = 2), " mol/m2/d"), style = "font-size: 200%;"))
        })
        
        # Send the tables to the UI
        output$Annual_SC_Display_table<- renderTable({
          XTRA_values$Annual_SC_Display # Estimated Annual Electricity Summary Costs
        })
        output$DSC_Display_table<- renderTable({
          XTRA_values$Annual_SC_demand_Display # Estimated Annual Demand Summary Costs
        })
        output$SC_df_W_table <- renderTable({
          XTRA_values$SC_df_W_Display # Estimated Weekly Electricity Costs
        })
      })
      
      #####################################.
      #### Download Results #####
      #####################################.
      # Create a dataframe with the original user inputs
      Grower_input <- data.frame(trans,target,efficacy,ele_rate, hours, demand_charge_price, percentile*100, area)
      names(Grower_input) <- (c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Hours On", "Demand Charge ($/kW/month)",  "Percentile (%)", "Area (ft2)"))
      
      # Load in the detailed values based on the original user inputs
      SC_df2 <- SC_df
      names(SC_df2) <- c("TimeStamp", "Month", "DLI_umol", "DLI_mol", "Transmission (%)", "Sunlight Present in Greenhouse (mol)", "DLI Target",
                         "Supplemental Light Provided (mol)", "Total DLI Mols", "Electricity Rate", "Cost per m2 per day", "Area (m2)")
      SC_df2 <- SC_df2[-c(3)]
      SC_df2 <- SC_df2[-c(11)]
      SC_df2$`Cost per m2 per day` <- round(SC_df2$`Cost per m2 per day`, digits = 2)
      
      # Create an area matrix dataframe
      months <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      Monthly_Area_Percent <- data.frame(months, label_percent()(PA_Months))
      names(Monthly_Area_Percent) <- c("Month", "Percent Area")
      
      # Create a dataframe for the DLI max capability and PPFD capability
      Value <- c(paste0(round(XTRA_values$PPFD_capability_1,0), " umol/m2/s"), paste0(round(XTRA_values$Percentile_Supplemental_DLI_1,2)," mol/m2/d"))
      Name <- c("Required Lighting System Capacity", "Max Supplemental DLI Capacity")
      Summary_Frame <- data.frame(Name, Value)
      
      # Store above tables in a list to write to Excel File based on the original user inputs
      SC_data_list <- reactive({
        list(
          Grower_Input = Grower_input,
          Monthly_Area = Monthly_Area_Percent,
          Monthly_Costs = SC_df_M_Display,
          Weekly_Costs = SC_df_W_Display,
          Demand_Costs = Annual_SC_demand_Display,
          Detailed_Electricity_Data = SC_df2,
          Summary_Values = Summary_Frame
        )
      })
      # Button to Download Results based on the original user inputs
      output$SC_downloadData <- downloadHandler(
        filename = paste0("SuppCalc_", as.character(Sys.Date()), ".xlsx"), #File with current date
        content = function(file) {
          write_xlsx(SC_data_list(), path=file, col_names = TRUE)
        }
      )
      
      # Store adjusted tables in a list to write to Excel File based on the adjusted user inputs
      SC_Adjusted_data_list <- reactive({
        # Import and rename grower adjusted inputs
        Grower_input_Adjusted <- data.frame(XTRA_values$Grower_input_Adjusted)
        names(Grower_input_Adjusted) <- c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Hours On", "Demand Charge ($/kW/month)",  "Adjusted Percentile (%)", "Area (ft2)")
        # Import and rename monthly electricity cost table
        SC_df_M_Display_Adjusted <- data.frame(XTRA_values$SC_df_M_Display_Adjusted)
        names(SC_df_M_Display_Adjusted) <- c("Month","$ Per ft2","$ Per acre","$ Total Design")
        # Import and rename weekly electricity cost table
        SC_df_W_Display_Adjusted <- data.frame(XTRA_values$SC_df_W_Display_Adjusted)
        names(SC_df_W_Display_Adjusted) <- c("Week","$ Per ft2","$ Per acre","$ Total Design")
        # Import and rename demand  cost table
        Annual_SC_demand_Display_Adjusted <- data.frame(XTRA_values$Annual_SC_demand_Display_Adjusted)
        names(Annual_SC_demand_Display_Adjusted) <- c("$ Per ft2","$ Per acre","$ Total Design")
        # Import and rename adjusted detailed electricity cost table
        SC_df_Adjusted <- data.frame(XTRA_values$SC_df_Adjusted)
        names(SC_df_Adjusted) <- c("TimeStamp", "Month", "DLI_umol", "DLI_mol", "Transmission (%)", "Sunlight Present in Greenhouse (mol)", "DLI Target",
                                   "Supplemental Light Provided (mol)", "Total DLI Mols", "Electricity Rate", "Cost per m2 per day", "Area (m2)")
        SC_df2$`Cost per m2 per day` <- round(SC_df2$`Cost per m2 per day`, digits = 2)
        
        # Create a dataframe for the summary values to be downloaded via excel
        Value <- c(paste0(round(XTRA_values$PPFD_capability_adjusted,0), " umol/m2/s"), paste0(round(XTRA_values$suppInt_adjusted,2)," mol/m2/d"))
        Name <- c("Required Lighting System Capacity", "Max Supplemental DLI Capacity")
        Summary_Frame_Adjusted <- data.frame(Name, Value)
        
        # Create a list to represent a workbook where each line is an excel sheet based on adjusted values
        list(
          Grower_Input = Grower_input_Adjusted,
          Monthly_Area = Monthly_Area_Percent,
          Monthly_Costs = SC_df_M_Display_Adjusted,
          Weekly_Costs = SC_df_W_Display_Adjusted,
          Demand_Costs = Annual_SC_demand_Display_Adjusted,
          Detailed_Electricity_Data = SC_df_Adjusted,
          Summary_Values = Summary_Frame_Adjusted
        )
      })
      #Button to Download Adjusted Results
      output$SC_Adjusted_downloadData <- downloadHandler(
        filename = paste0("SuppCalcAdjusted_", as.character(Sys.Date()), ".xlsx"), #File with current date
        content = function(file) {
          write_xlsx(SC_Adjusted_data_list(), path=file, col_names = TRUE)
        }
      )
    }
    
    #################################.
    ### Render Tables and Graph ###
    #################################.
    # Send the tables to the UI
    output$Annual_SC_Display_table<- renderTable({
      Annual_SC_Display # Estimated Annual Electricity Summary Costs
    })
    output$DSC_Display_table<- renderTable({
      Annual_SC_demand_Display # Estimated Annual Demand Summary Costs
    })
    output$SC_df_W_table <- renderTable({
      SC_df_W_Display # Estimated Weekly Electricity Costs
    })
  })
} #bracket server