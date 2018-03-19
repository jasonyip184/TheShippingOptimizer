library(data.table)
library(shiny)

optimize <- function(origin, destination, duration_to_deliver, cargo_value, cargo_weight, cargo_volume, order_qty) {
  source("app.R")
  
  # # # these values are passed in by user
  # node_coords <- readRDS("data/node_coords.rds")
  # routes <- readRDS("data/routes.rds")
  # paths <- readRDS("data/paths.rds")
  # ports <- readRDS("data/ports.rds")
  # trucks <- readRDS("data/trucks.rds")

  # origin = "Singapore Port S"
  # destination = "Wuxi Port S"
  # duration_to_deliver = 10^20
  # cargo_value = 20000
  # cargo_weight = 500
  # cargo_volume = 100
  # order_qty = 5

  num_of_40_containers = floor(cargo_volume/67)
  num_of_20_containers = ceiling((cargo_volume %% 67)/33)
  
  #remove routes that has origin and destination 
  #aka A/B/C/D not origin&destination
  routes <- routes[which( (is.na(routes$A) | routes$A != origin) & (is.na(routes$A) | routes$A != destination) & (is.na(routes$B) | routes$B != origin) & (is.na(routes$B) | routes$B != destination) ),]
  
  #initialize
  cost_result = 10^26
  duration_result = 0
  path_result = ""
  
  #loop through Routes
  for(i in 1:nrow(routes)) {
    #merge route found with o and d -> oABd
    route <- unlist(routes[i])
    route <- route[!is.na(route)]
    route <- as.character(route)
    route <- c(origin, route, destination)
    
    #set cost and duration for a route oABd
    total_cost = 0
    total_duration = 0
    mylength = length(route)
    
    #loop through each node in route
    for(j in 1:(mylength-1)) {
      #anytime in the route where duration of route or cost exceeds, break
      if((total_duration > duration_to_deliver) || (total_cost > cost_result)) {
        break
      }
      
      #checking row of Ports where name == next node
      curr = ports[which(ports$name == route[j+1]),]
      
      #handling/custom cost/duration
      if(nrow(curr) == 0) {next} else {
        total_cost = total_cost + curr$handling_cost*cargo_weight + curr$custom_cost*order_qty
        total_duration = total_duration + curr$handling_duration + curr$custom_duration
      }
      
      #where "from" is current node and "to" is next node
      mypath = paths[which((paths$from_name == route[j]) & (paths$to_name == route[j+1])),]
      
      #if there are no paths in Paths, then it must be nodes connected by trucks
      if(nrow(mypath) == 0) {
        #where "from" column is current node and "to" column is next node 
        total_duration = total_duration + trucks[which(trucks$X == route[j]),][,route[j+1]]
        total_cost = total_cost + 6.9463*cargo_volume*trucks[which(trucks$X == route[j]),][,route[j+1]]
        
        #add import taxes if country of currentnode is different from nextnode
        nextnode = ports[which(ports$name == route[j+1]),]
        if(ports[which(ports$name == route[j]),]$country != nextnode$country) {
          total_cost = total_cost + cargo_value*nextnode$tax
        }
      }
      #check between cities aka S to S, A to A, T to T
      else {
        #add import taxes if country of currentnode is different from nextnode
        if(mypath$to_country != mypath$from_country) {
          #get row from Ports where "name" column is current node and retrieve tax from it
          total_cost = total_cost + cargo_value*ports[which(ports$name == mypath$to_name),]$tax
        }
        
        #compare last character of next nodes for checking transport type
        to_last = substr(route[j], nchar(route[j])-1, nchar(route[j]))
        
        #for ship to ship, train to train, air to air
        if(to_last == " S") {
          total_cost = total_cost + mypath$cost_ship_20*33*num_of_20_containers + mypath$cost_ship_40*67*num_of_40_containers
        } else if(to_last == " T") {
            total_cost = total_cost + mypath$cost_train_20*33*num_of_20_containers +  mypath$cost_train_40*67*num_of_40_containers
        } else { #for air to air
            total_cost = total_cost + mypath$cost_air*50*cargo_volume
        }
        total_duration = total_duration + mypath$duration
      }
    }  
    
    #only update if cost is under the current lowest cost
    if((total_duration <= duration_to_deliver) & (total_cost < cost_result)) {
      cost_result = total_cost
      duration_result = total_duration
      path_result = route
    }
  }

  ## coloured path for map
  path_type_order <- numeric()
  
  if(path_result != "") {
    for(i in 1:(length(path_result)-1)) {
      from_last = substr(path_result[i], nchar(path_result[i])-1, nchar(path_result[i]))
      to_last = substr(path_result[i+1], nchar(path_result[i+1])-1, nchar(path_result[i+1]))
      
      temp_path = paths[which(paths$from_name == path_result[i] & paths$to_name == path_result[i+1]),]
      if(nrow(temp_path) == 0) {
        type = 4
      } else {
        if(from_last == " A") {
          type = 1
        } else if(from_last == " S") {
          type = 2
        } else {
          type = 3
        }
      }
      
      path_type_order <- c(path_type_order, type)
    }
  }
  
  return(list(cost_result, duration_result, path_result, path_type_order))
}
