library(jsonlite)
library(data.table)

paths <- stream_in(file("data/paths.json"))
paths <- data.table(cbind(paths$to$country, paths$to$name, paths$from$country, paths$from$name, paths$cost$air, as.numeric(unlist(paths$cost$train$`20'`)), paths$cost$train$`40'`, as.numeric(unlist(paths$cost$ship$`20'`)), as.numeric(unlist(paths$cost$ship$`40'`)), as.numeric(unlist(paths$duration))))
names(paths) <- c("to_country", "to_name", "from_country", "from_name", "cost_air", "cost_train_20", "cost_train_40", "cost_ship_20", "cost_ship_40", "duration")
for(i in 5:ncol(paths)) {
  paths[[i]] <- as.numeric(paths[[i]])
}
saveRDS(paths, "paths.rds")

tp <- stream_in(file("data/truck_paths.json"))
tp <- data.table(cbind(tp$to$name, tp$from$name, tp$duration, tp$cost))
names(tp) <- c("to", "from", "duration", "cost")
for(i in 3:ncol(tp)) {
  tp[[i]] <- as.numeric(tp[[i]])
}
saveRDS(tp, "truck_paths.rds")

ports <- stream_in(file("data/ports.json"))
ports <- data.table(cbind(ports$name, ports$data$tax, ports$data$`handling cost`, ports$data$`handling duration`, ports$data$`custom cost`, ports$data$`custom duration`))
names(ports) <- c("name","tax","handling_cost","handling_duration", "custom_cost", "custom_duration")
for(i in 2:ncol(ports)) {
  ports[[i]] <- as.numeric(ports[[i]])
}
saveRDS(ports, "ports.rds")

routes <- stream_in(file("data/routes.json"))
a <- data.frame(NA, NA, NA, NA, NA)
names(a) <- names(routes)
routes <- rbind(a, routes)
routes <- data.table(cbind(routes$A,routes$B,routes$C,routes$D))
names(routes) <- c("A", "B", "C", "D")
saveRDS(routes, "routes.rds")

node_coords <- data.table(read.csv("data/node_coords.csv", header=TRUE))
node_coords$V1 <- as.character(node_coords$V1)
node_coords$V2 <- as.character(node_coords$V2)
node_coords$Station_Type <- as.character(node_coords$Station_Type)
node_coords$Station_Type[node_coords$Station_Type == "A"] <- 3
saveRDS(node_coords, "node_coords.rds")



trucks <- read.csv("data/trucks.csv", stringsAsFactors = FALSE)
names(trucks) <- c("X", as.character(trucks$stopped.at.Singapore.Port))
trucks[,2:ncol(trucks)] <- lapply(trucks[,2:ncol(trucks)], function(x) as.numeric(x))
saveRDS(trucks, "trucks.rds")

routes <- readRDS("data/routes.rds")
routes1 <- data.table(cbind(routes$A, routes$B))
routes1unique <- subset(unique(routes1))
names(routes1unique) <- c("A", "B")
routes1unique$A
saveRDS(routes1unique, "routes.rds")

ports <- readRDS("data/ports.rds")
country <- c("HK",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"SG",
"SG",
"MY",
"MY",
"MY",
"TH",
"TH",
"CN",
"CN",
"CN",
"CN",
"CN",
"CN",
"India",
"India",
"Vietnam",
"Vietnam",
"Vietnam",
"Vietnam",
"Vietnam")
ports$country <- country
saveRDS(ports, "ports.rds")

ports$name[25] <- as.character("Butterworth railway station T")

node_coords <- readRDS("data/node_coords.rds")
node_coords[which(node_coords$Station_Type == "3"),]$Station_Type = "A"
saveRDS(node_coords, "node_coords.rds")

paths <- readRDS("data/paths.rds")
cleaned = paths[!which((paths$cost_air == 0.000000 | paths$cost_air == 0 | paths$cost_air == 0.0) & 
                        (paths$cost_train_20 == 0 | paths$cost_train_20 == 0.000000 | paths$cost_train_20 == 0.0) & 
                        (paths$cost_train_40 == 0 | paths$cost_train_40 == 0.000000 | paths$cost_train_40 == 0.0) &
                        (paths$cost_ship_20 == 0 | paths$cost_ship_20 == 0.000000 | paths$cost_ship_20 == 0.0) &
                        (paths$cost_ship_40 == 0 | paths$cost_ship_40 == 0.000000 | paths$cost_ship_40 == 0.0) &
                        paths$duration == 0),]
saveRDS(cleaned, "paths.rds")
