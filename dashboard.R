#Determine x axis limit for OEM barplot by finding the OEM with the max number of unique assets
oem_plot_data <- select(metaTable, UID, OEM, Category)
oem_plot_data <- unique(oem_plot_data)
oem_plot_data$OEM <- as.character(oem_plot_data$OEM)
oem_plot_data$Company_Sum <- as.numeric(ave(oem_plot_data$OEM, oem_plot_data$OEM, FUN = length))
oem_plot_data$Category <- as.character(oem_plot_data$Category)
oem_plot_data$Type_Sum <- as.numeric(ave(oem_plot_data$Category, oem_plot_data$Category, FUN = length))
x_axis <- max(oem_plot_data$Company_Sum)
x_axis_2 <- max(oem_plot_data$Type_Sum)

#Create 2 df with only unique companies or unique models (both with totals)
oem_plot_data_company <- oem_plot_data %>% group_by(OEM) %>% summarise(top = max(Company_Sum)) %>%
  arrange(top)
oem_plot_data_type <- oem_plot_data %>% group_by(Category) %>% summarise(top_2 = max(Type_Sum)) %>%
  arrange(top_2)

#Format OEM Plot
plot_company <- ggplot(data=oem_plot_data_company, aes(x=oem_plot_data_company$OEM,
                                                       y=top)) +
  geom_bar(stat="identity", fill="#3C8DBC", colour="#307096") +
  coord_flip() +
  ggtitle("Asset Overview") +
  xlab("Brand") +
  guides(fill=F) +
  ylab("Total") +
  geom_text(aes(label=top, vjust= 2, hjust = 2)) +
  theme(plot.title = element_text(hjust = 0.5, size=16), axis.text=element_text(size=12, face = "italic"),
        axis.title=element_text(size=14,face = "italic"))
#Format Category Plot 
plot_type <- ggplot(data=oem_plot_data_type, aes(x=oem_plot_data_type$Category, y=top_2)) +
  geom_bar(stat="identity", fill="#3C8DBC", colour="#307096") +
  coord_flip() +
  ggtitle("Asset Overview") +
  xlab("Category") +
  guides(fill=F) +
  ylab("Total") +
  geom_text(aes(label=top_2, vjust= 1, hjust = 2)) +
  theme(plot.title=element_text(hjust = 0.5, size=16), axis.text=element_text(size=12, face = "italic"),
        axis.title=element_text(size=14,face = "italic"))