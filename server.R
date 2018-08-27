
shinyServer(function(input,output,session){
  machine_data <- read.csv("mockdata.csv",stringsAsFactors = F, row.names = NULL)
  metaTable <- machine_data
  metaTable$row.names <- NULL
  colnames(machine_data) <- c( "UID","YearManufactured","City","State","zip","Purchase_Price","Number_of_services","Model","Category",
                               "OEM","Currency","Make","Type", "Date", "Retail_Price","Condition", "Buyback_Price", "Sell_Price")
  
  output$machine_name <- renderUI({
    filter.name.bycompany <- filter(metaTable,OEM==input$company)
    filter.name <- filter(filter.name.bycompany,Category==input$machine_type)
    selectInput("name","Name of Machine",choices= unique(filter.name$Model),selected = value1)
  })
  
  output$edit_purchase_price <- renderUI({
    textInput("edit_pp","New Purchase Price",value = machine_data[which(machine_data$UID==input$choose_uid),"Purchase_Price"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  output$edit_number_of_service <- renderUI({
    textInput("edit_nos","New Service Record",value = machine_data[which(machine_data$UID==input$choose_uid),"Number_of_services"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  output$edit_city <- renderUI({
    textInput("edit_city1","Location City", value = machine_data[which(machine_data$UID==input$choose_uid),"City"][length(which(machine_data$UID==input$choose_uid))] )
  })
  
  output$edit_state <-renderUI({
    textInput("edit_state1","Location State",value = machine_data[which(machine_data$UID==input$choose_uid),"State"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  output$edit_zip <- renderUI({
    textInput("edit_zip1","Location Zip",value =machine_data[which(machine_data$UID==input$choose_uid),"zip"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  output$edit_condition <- renderUI({
    selectInput("edit_condition1","Condition of Asset", choices = c("New","Used","Refurbished","Unknown"),selected = "New")
  })
  
  output$edit_make_of_asset <- renderUI({
    textInput("edit_make_of_asset1","Make of Asset",value =machine_data[which(machine_data$UID==input$choose_uid),"Make"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  observeEvent(input$edit, {
    show("edit_purchase_price")
    show("edit_number_of_service")
    show("edit_city")
    show("edit_state")
    show("edit_zip")
    show("edit_condition")
    show("edit_make_of_asset")
    show("edit_submit")
  })
  
  dataReactive1 <- reactive({
    data.frame(UID = c(input$choose_uid),YearManufactured = c(2018),City = c(input$edit_city1),State = c(input$edit_state1),zip = c(which(zipcode$city==input$edit_city1)[1]),Purchase_Price = c(input$edit_pp),
               Number_of_services = c(input$edit_nos),Model = c(machine_data[which(machine_data$UID==input$choose_uid),"Model"][length(which(machine_data$UID==input$choose_uid))]),
               Category = c(machine_data[which(machine_data$UID==input$choose_uid),"Category"][length(which(machine_data$UID==input$choose_uid))]),
               OEM = c(machine_data[which(machine_data$UID==input$choose_uid),"OEM"][length(which(machine_data$UID==input$choose_uid))]),Currency = c("USD"),Make = c(input$edit_make_of_asset1),
               Type = c("Complete System"), Date = as.character(format(Sys.Date(), format="%m/%d/%Y")),
               Retail_Price = c(round(mean(na.omit(as.numeric(pricing_info[which(pricing_info$Model==(machine_data[which(machine_data$UID==input$choose_uid),"Model"][length(which(machine_data$UID==input$choose_uid))])),"PriceConvertedUSD"]))))),
               Condition = c(input$edit_condition1))
  })
  
  observeEvent(input$edit_submit, {
    edit_asset_row <- rbind(metaTable,dataReactive1())
    write.csv(edit_asset_row,"mockdata.csv",row.names = F,na = "")
    js$reset()
  })
  
  observe({
    updateSelectInput(session, "choose_uid", choices = unique(machine_data$UID), selected =unique(machine_data$UID)[1])
  })
  
  output$mytable <- renderDataTable(
    showTable <- select(metaTable,OEM,Category,Model), rownames = F,server = T, selection = "single")
  
  output$info_table <- renderDataTable({
    s = input$mytable_rows_selected
    car_fax <- unique(machine_data[s,c("UID")])
    machine_data[machine_data$UID %in% car_fax, ]})

  
  observeEvent(input$Add, {
    show("purchase_price")
    show("service")
    show("city")
    show("state")
    show("condition")
    show("make_of_asset")
    show("submit")
    show("price")
  })
  
  
  output$machineCount <- renderValueBox({
    valueBox(max(as.numeric(gsub("Asset","",machine_data$UID,ignore.case=T))), "Total Assets", icon = icon("list"), color = "blue")
  })
  output$totalValue <- renderValueBox({
    valueBox(paste0("$",format(sum(machine_data$Retail_Price, na.rm=TRUE), big.mark = ",")),"Total Value of Assets",icon = icon("dollar"),color = "yellow")
  })
  
  output$averageValue <- renderValueBox({
    valueBox(paste0("$",format(floor(sum(machine_data$Retail_Price, na.rm=TRUE)/length(machine_data$Model)), big.mark = ",")),"Average Value of Assets",icon = icon("dollar"),color = "green")
  })
  
  output$oemplot <- renderPlot({
    if(input$selected == "Brand"){
      plot_company           
    }                                        
    else if(input$selected == "Type"){
      plot_type
    }
  })
  
  output$track_asset_plot <- renderPlotly({
    tracking_asset(input$select_asset)
  })
  
  output$allAsset <- renderPlotly({
    allMachines(input$Pick)
  })
  output$track_asset_table <- renderDataTable({
    asset_history_table <- filter(metaTable,metaTable$Model==input$select_asset) %>% select(UID,OEM,Type,Model,Retail_Price)
    datatable(asset_history_table,options = list(dom = 't'))
  })
  
  dataReactive <- reactive({
    data.frame(UID = c(paste0("Asset",length(unique(metaTable$UID))+1)),YearManufactured = c(2018),City = c(input$city),State = c(input$state),zip = c(91105),Purchase_Price = c(input$purchase_price),
               Number_of_services = c(input$service),Model = c(input$name),Category = c(input$machine_type),
               OEM = c(input$company),Currency = c("USD"),Make = c(input$make_of_asset),
               Type = c("Complete System"), Date = as.character(format(Sys.Date(), format="%m/%d/%Y")),
               Retail_Price = c(round(mean(na.omit(as.numeric(pricing_info[which(pricing_info$Model==input$name),"PriceConvertedUSD"]))))),
               Condition = c(input$condition))
  })
  observeEvent(input$price, {
    tableReactive()
    plotReactive()
  })
  
  observeEvent(input$submit,{
    addRow <- rbind(machine_data,dataReactive())
    write.csv(addRow,"mockdata.csv",row.names = F,na = "")
    machine_data <- addRow
    js$reset()
  })
  
  plotReactive <- reactive({
    output$price_plot <- renderPlotly(price.plot(input$company,input$name))
  })
  
  output$select_text <- renderText({
    paste("<B>Asset Model:</B>",
    machine_data[input$mytable_rows_selected,c("Model")])
  })
  
  output$select_text_1 <- renderText({
    paste("<B>Asset Company:</B>",
    machine_data[input$mytable_rows_selected,c("OEM")])
  })
  
  output$select_text_2 <- renderText({
    paste("<B>Asset Category:</B>",
    machine_data[input$mytable_rows_selected,c("Category")])
  })
  
  output$type.asset <- renderText({
    paste("<B>Asset Type:</B>",
    machine_data[input$mytable_rows_selected,c("Type")])
  })
  
  output$year_mfg <- renderText({
    paste("<B>Year Installed:</B>",
    machine_data[input$mytable_rows_selected,c("YearManufactured")])
  })
  
  output$location_text <- renderText({
    paste0("<B>Location: </B>",
    machine_data[input$mytable_rows_selected,c("City")],", ",machine_data[input$mytable_rows_selected,c("State")])
  })
  
  output$p.price <- renderText({
    paste("<B>Purchase Price in USD($):</B>",
    format(machine_data[input$mytable_rows_selected,c("Purchase_Price")], big.mark = ","))
  })
  
  output$r.price <- renderText({
    paste("<B>Retail Price in USD($):</B>",
    format(machine_data[input$mytable_rows_selected,c("Retail_Price")], big.mark = ","))
  })
  
  output$s.price <- renderText({
    paste("<B>Selling Price in USD($):</B>",
    format(round(machine_data[input$mytable_rows_selected,c("Retail_Price")]*0.823), big.mark = ","))
  })
  
  output$b.price <- renderText({
    paste("<B>Buyback Price in USD($):</B>",
    format(round(machine_data[input$mytable_rows_selected,c("Retail_Price")]*0.673), big.mark = ","))
  })
  
  output$edit_oem <- renderText({
    paste("<B>Asset Company:</B>",
    machine_data[which(machine_data$UID==input$choose_uid),"OEM"][length(which(machine_data$UID==input$choose_uid))])
  })
  output$edit_category <- renderText({
    paste("<B>Asset Category:</B>",
    machine_data[which(machine_data$UID==input$choose_uid),"Category"][length(which(machine_data$UID==input$choose_uid))])
  })
  output$edit_model <- renderText({
    paste("<B>Asset Model:</B>",
    machine_data[which(machine_data$UID==input$choose_uid),"Model"][length(which(machine_data$UID==input$choose_uid))])
  })
  
  tableReactive <- reactive({
    output$priceTable <- renderDataTable({
      Standard = c("Retail Price","Sell Price","Buyback Price")
      Value = c(paste("$",format(round(pricing.for.retail(input$company,input$name)), big.mark = ",")),paste("$",format(round(pricing.for.sell(input$company,input$name)), big.mark = ",")),
                paste("$",format(round(pricing.for.buyback(input$company,input$name)), big.mark = ",")))
      df1 = data.frame(Standard,Value)
      colnames(df1) <- c("Price Standard", "Dollar Value")
      datatable(df1,options = list(dom = 't'))
    })
  })
  output$avg_plot <- renderPlot({
    if(length(input$mytable_rows_selected)){
    price.line.plot(input$mytable_rows_selected)
    }
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "AssetTrackingReport.Rmd")
      file.copy("AssetTrackingReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data=as.data.frame(machine_data[which(machine_data$UID==input$choose_uid), ]))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    })
})