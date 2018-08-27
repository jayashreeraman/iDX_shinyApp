pricing_info <- read.csv("MySQL_CleanData180509_002.csv",stringsAsFactors = F)
# mean(na.omit(as.numeric(pricing_info$PriceConvertedUSD[which(grepl("Lightspeed Plus",pricing_info$Model))])))

pricing.for.retail <- function(company,model){
  filter.byoem <- filter(pricing_info,pricing_info$OEM==company)
  filter.byoem.bymodel <- filter(filter.byoem,filter.byoem$Model==model)
  retail.price <- mean(na.omit(as.numeric(filter.byoem.bymodel$PriceConvertedUSD)))
  return(retail.price)
}

 # for(i in 1:nrow(machine_data)){
 #   machine_data$Sell_Price[i] <-round((machine_data$Retail_Price[i]* 0.823))
 #   machine_data$Buyback_Price[i] <- round((machine_data$Retail_Price[i]* 0.673))
 # }
 # write.csv(machine_data,"mockdata.csv",row.names = F)


pricing.for.sell  <- function(company,model){
  filter.byoem <- filter(pricing_info,pricing_info$OEM==company)
  filter.byoem.bymodel <- filter(filter.byoem,filter.byoem$Model==model)
  retail.price <- mean(na.omit(as.numeric(filter.byoem.bymodel$PriceConvertedUSD)))
  sell.price <- (retail.price * 0.9) - 34000
  return(sell.price)
}

pricing.for.buyback  <- function(company,model){
  filter.byoem <- filter(pricing_info,pricing_info$OEM==company)
  filter.byoem.bymodel <- filter(filter.byoem,filter.byoem$Model==model)
  retail.price <- mean(na.omit(as.numeric(filter.byoem.bymodel$PriceConvertedUSD)))
  buyback <- retail.price * 0.70
  return(buyback)
}

price.plot <- function(company,model){
  a <- list(
    autotick = TRUE,
    tick0 = 0
  )
  return(plot_ly(x = c(pricing.for.sell(company,model),
                   pricing.for.buyback(company,model),pricing.for.retail(company,model)), y = c("Sell","Buyback","Retail"), type = 'bar', orientation = 'h') %>%
           layout(title=paste0("Price range of ",model),xaxis=a,autosize = F, width = 600, height = 150))
}

# p <- plot_ly(x = c(pricing.for.retail("GE","Lightspeed Plus"),pricing.for.average("GE","Lightspeed Plus"),
#                    pricing.for.buyback("GE","Lightspeed Plus")), y = c("retail","average","buyback"), type = 'bar', orientation = 'h')

price.line.plot <- function(input_row){
  machine_data <- read.csv("mockdata.csv")
  r <- machine_data[input_row,c("Retail_Price")]
  b <- round((r * .7))
  s <- round((r * 0.9) - 34000)
  p <- machine_data[input_row,c("Purchase_Price")]
  
  
  point <- format_format(big.mark = ",", decimal.mark = ",", scientific = FALSE)
  list_avg <- c(b, s, r, p)
  list_avg_mean <- mean(list_avg)
  y <- c(.5, .5, .5, .5)
  labels <- c("Buy-back", "Sale", "Retail", "Purchase")
  avg_df <- data.frame(list_avg, y, labels)
  ggplot(avg_df, aes(list_avg, y)) +
    geom_point() +
    geom_smooth() +
    xlim((b - (.05 * list_avg_mean)), (p + (.05 * list_avg_mean))) +
    geom_text_repel(aes(label = paste0(labels,": ", "\n", "$", comma(list_avg)), hjust= 1.2, vjust= -2, check_overlap = TRUE)) +
    scale_x_continuous(labels = point) +
    labs(x = "Price in USD", y = "") +
    theme(axis.text.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    theme(panel.background = element_blank(), axis.line = element_blank())
}
