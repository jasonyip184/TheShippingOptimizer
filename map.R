library(leaflet)

mapFunction <- function(path, colour_of_path) {
  #add station type
  path_result <- as.data.frame(path)
  for(i in 1:nrow(path_result)) {
    path_result$name[i] = node_coords[node_coords$V2 == path_result$path[i]]$V1
    path_result$Lat[i] = node_coords[which(node_coords$V2 == path_result$path[i]),]$Lat
    path_result$Long[i] = node_coords[which(node_coords$V2 == path_result$path[i]),]$Long
    # it adds the station type A is 1, S is 2, T is 3
    path_result$Station_Type[i] = node_coords[which(node_coords$V2 == path_result$path[i]),]$Station_Type
  }
  
  #split into origins and destinations
  row_num = nrow(path_result)
  od <- as.data.frame(cbind(as.character(path_result[1:(row_num-1),2]), as.character(path_result[2:row_num,2]), path_result[1:(row_num-1),4], path_result[1:(row_num-1),3], path_result[2:row_num,4], path_result[2:row_num,3], path_result[1:(row_num-1),5], path_result[2:row_num,5]))
  names(od) <- c("origins", "destinations", "longitude.x", "latitude.x", "longitude.y", "latitude.y", "origin_station_type", "destination_station_type")
  od$longitude.x <- as.numeric(as.character(od$longitude.x))
  od$latitude.x <- as.numeric(as.character(od$latitude.x))
  od$longitude.y <- as.numeric(as.character(od$longitude.y))
  od$latitude.y <- as.numeric(as.character(od$latitude.y))
  
  od$path_type <- colour_of_path
  
  #create curved lines
  library(geosphere)
  flows <- gcIntermediate(od[,3:4], od[,5:6], n=3, sp=TRUE, addStartEnd = TRUE)
  flows$origins <- od$origins
  flows$destinations <- as.factor(od$destinations)
  
  pathColour <- c("red","darkblue","darkgreen","orange")[od$path_type]
  
  #label for path
  hover <- paste0(od$origins," to ",od$destinations)
  
  nodeColour = c()
  nodeIcon = c()
  #label for node
  for(i in 1:nrow(path_result)) {
    if(path_result$Station_Type[i] == "A") {
      nodeColour = c(nodeColour, "red")
      nodeIcon = c(nodeIcon, "plane")
    } else if (path_result$Station_Type[i] == "S") {
      nodeColour = c(nodeColour, "darkblue")
      nodeIcon = c(nodeIcon, "ship")
    } else {
      nodeColour = c(nodeColour, "darkgreen")
      nodeIcon = c(nodeIcon, "train")
    }
  }
  
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolylines(data = flows, label = hover, 
                 group = ~origins, color = pathColour) %>%
    addAwesomeMarkers(
      data = path_result,
      icon = awesomeIcons(icon=nodeIcon,markerColor=nodeColour,library="fa",iconColor="#FFF"),
      label = path_result$name
    ) %>%
    addLegend(
      position = 'bottomright',
      colors = c("darkred","darkblue","darkgreen","orange"),
      labels = c("Air", "Sea", "Train", "Truck"), opacity = 1,
      title = 'Route taken by'
    )
}

