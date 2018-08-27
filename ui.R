jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
dashboardPage(skin = "blue",
  dashboardHeader(title = "iDX"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Add/Edit Assets", tabName = "add", icon = icon("inventory"),
               menuSubItem("Add New Asset", tabName = "add_asset", icon = icon("plus")),
               menuSubItem("Edit Exisiting Asset", tabName = "edit_asset", icon = icon("edit"))),
      menuItem("View Assets", tabName = "view",icon = icon("search")),
      menuItem("Track Assets",tabName= "track",icon = icon("globe"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName="home",fluidRow(
              valueBoxOutput("machineCount"),
              valueBoxOutput("totalValue"),
              valueBoxOutput("averageValue")),
              fluidRow(column(2, selectInput("selected", "Filter Asset Inventory",
                    choices = list("Brand" = "Brand", "Type" = "Type"), selected = "OEM")),
                    column(width = 2, offset =4, selectInput("Pick", "Filter Asset Map",
                                choices = list("All" = "All", "GE" = "GE", "Philips" = "Philips",
                                               "Siemens" = "Siemens", "Toshiba" = "Toshiba"),
                                selected = "All"))),
              fluidRow(
                box(status = "primary", width = 6, plotOutput("oemplot")),
                box(status = "success", width = 6, plotlyOutput("allAsset"))
              )),
      tabItem(tabName = "edit_asset",
              sidebarPanel(
                selectInput("choose_uid", "Choose Asset to Edit",choices = unique(machine_data$UID), selected = unique(machine_data$UID)[1]),
                h4(htmlOutput("edit_oem")),
                h4(htmlOutput("edit_category")),
                h4(htmlOutput("edit_model")),
                br(),
                helpText("Use this to edit the exisiting machines in the Inventory"),
                actionButton("edit","Edit Features")
              ),
              mainPanel(
                fluidRow(
                  column(6,
                  hidden(uiOutput("edit_purchase_price"))),
                  column(6,
                  hidden(uiOutput("edit_number_of_service")))
                ),
                fluidRow(
                  column(3,
                  hidden(uiOutput("edit_city"))),
                  column(3,
                  hidden(uiOutput("edit_state"))),
                  column(3,
                  hidden(uiOutput("edit_zip")))
                ),
                fluidRow(
                  column(6,
                  hidden(uiOutput("edit_condition"))),
                  column(6,
                  hidden(uiOutput("edit_make_of_asset")))
                ),
                hidden(actionButton("edit_submit","Edit Asset in Inventory"))
              )),
    tabItem(tabName = "add_asset",
            sidebarPanel(
              selectInput("company","Choose OEM",
                          choices = unique(machine_data$OEM),
                          selected = value2),
              selectInput("machine_type","Type of Machine",
                          choices= unique(machine_data$Category),
                          selected = unique(machine_data$Category)[1]),
              uiOutput("machine_name"),
              dateInput("year","Date of Purchase",format = "yyyy-mm-dd"),
              useShinyjs(),
              actionButton("Add","Add Features")
            ),
            mainPanel(
              uiOutput("ui"),
              useShinyjs(),
              fluidRow(
                column(6,
              hidden(textInput("purchase_price","Purchase Price",value = machine_data[which(machine_data$Model==value1),"Purchase_Price"]))),
                column(6,
              hidden(textInput("service","Service record",value = machine_data[which(machine_data$Model==value1),"Number_of_services"])))),
              fluidRow(
                column(6,
              hidden(textInput("city","Location City",value = machine_data[which(machine_data$Model==value1),"City"][length(machine_data[which(machine_data$Model==value1),"City"])]))),
                column(6,
              hidden(selectInput("state","Location State",choices = state.name,
                                 selected = state.name[which(state.name==machine_data[which(machine_data$Model==value1),"State"][length(machine_data[which(machine_data$Model==value1),"State"])])])))),
              fluidRow(
                column(6,
              hidden(selectInput("condition","Condition of Asset",choices = c("New","Used","Refurbished","Unknown"),selected = "New"))),
                column(6,
              hidden(textInput("make_of_asset","Make of Asset",value = machine_data[which(machine_data$Model==value1),"Make"])))),
              extendShinyjs(text = jsResetCode),
              hidden(actionButton("price","Price Analysis")),
             hidden(actionButton("submit","Add to Inventory")),
             br(),
              dataTableOutput("priceTable"),
             br(),
             plotlyOutput("price_plot")
              )),
    tabItem("view",
            fluidRow(box(status = "primary", width = 6,
            dataTableOutput("mytable")),
              box(status = "primary", width = 4,
              h4(htmlOutput("select_text_1")),
              h4(htmlOutput("select_text_2")),
              h4(htmlOutput("type.asset")),
              h4(htmlOutput("select_text")),
              br(),
              h4(htmlOutput("year_mfg")),
              h4(htmlOutput("location_text")),
              br(),
              h4(htmlOutput("p.price")),
              h4(htmlOutput("r.price")),
              h4(htmlOutput("s.price")),
              h4(htmlOutput("b.price")),
              
              
              downloadButton('report', "Generate Asset Report"),
              br(),
            plotOutput("avg_plot", height = "125px", width = "350px")))
            
            ),
    tabItem("track",
            sidebarPanel(
              selectInput("select_asset","Choose Asset to Track",
                          choices = unique(machine_data$Model),
                          selected = unique(machine_data$Model[1]))
            ),
              mainPanel(
                box(title = "Asset Map", width = 12 ,status = "primary", height = "auto",
                    solidHeader = T,plotlyOutput("track_asset_plot")),
             box(title = "Asset History", width = 12 ,status = "primary", height = "auto",
                 solidHeader = T, dataTableOutput("track_asset_table"))))
  )
))


