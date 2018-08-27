data(zipcode)

zipcode %>% mutate(zip = readr::parse_number(zip))

zipcode$zip <- as.numeric(zipcode$zip)

new <- right_join(zipcode, machine_data, by = "zip")

tracking_asset <- function(filterName){
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray75"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  
  blah <- filter(new,new$Model==filterName)
  
  p <- plot_geo(blah, lat = ~latitude, lon = ~longitude) %>%
    add_markers(
      text = ~paste(OEM,paste(Type,"-",Model),City,State,sep = "<br />")
    ) %>%
    layout(
      title = ~paste0("Location of ",filterName),
      geo = g,
      autosize = T
    )
  return(p)
}

 
#  xyz <- read.csv("mockdata.csv",stringsAsFactors = F)
# xyz$MD5.Hash.Values <- NULL
#  
#  for(i in 1:nrow(xyz)){
#    text <- xyz$UID[i]
#    hash <- digest(text, algo="md5",serialize = F)
#    xyz$MD5.Hash[i] <- hash
#  }
allMachines <- function(company){
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray75"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  choose <- filter(new,new$OEM==company)
  if(company=="All"){
    p <- plot_geo(new, lat = ~latitude, lon = ~longitude) %>%
      add_markers(
        text = ~paste(OEM,paste(Type,"-",Model),City,State,sep = "<br />")
      ) %>%
      layout(
        title = ~paste("Location of", paste(company), "Assets"),
        geo = g,
        size = 16
      )
    return(p)
  } else {
  p <- plot_geo(choose, lat = ~latitude, lon = ~longitude) %>%
    add_markers(
      text = ~paste(OEM,paste(Type,"-",Model),City,State,sep = "<br />")
    ) %>%
    layout(
      title = ~paste("Location of", paste(company), "Assets"),
      geo = g,
      size = 16
    )
  return(p)
  }
}