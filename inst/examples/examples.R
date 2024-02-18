# forceNetwork 
data(MisLinks)
data(MisNodes)

links <- MisLinks[1:5,]
nodes <- MisNodes[1:5,]
nodes$x <- c(0,30,40,10,200)
nodes$y <- c(1000,30,70,1,20)
nodes$id <- c(0,1,2,3,4)
links$id <- c(0,1,2,3,4)
nodes$rot <- 78
nodes$dx <- -12
nodes$dy <- ".35em"
nodes$text_anchor <- "end"

# Create graph
networkD3::forceNetwork(Links = links, Nodes = nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", X = "x", Y = "y", dx = "dx", dy = "dy", rotate_angle = "rot", opacity = 1, zoom = T, bounded = T,
             text_anchor = "text_anchor")



## owidex

# prepare links dataset for subset based on the pairs of nodes (user selection) above
# NOTE: this could be done before the app (in data-raw) but need to check where alllinks is used
links <- owidex::alllinks |>
    # tibbles do not work with networkD3
    as.data.frame() |>
    # modify name because it makes more sense
    dplyr::rename(links = nodes) |>
    # create source and target node (see networkD3 library)
    dplyr::mutate(source = purrr::map(links, .f = function(x) x[1]),
                  target = purrr::map(links, .f = function(x) x[2])) |>
    # add colour vector for sign (-1 or +1)
    dplyr::mutate(colour_sign = dplyr::if_else(sign == -1, "red", "black")) |>
    # add colour vector for confidence (1, 2 or 3), from unikn::pal_signal (https://pmassicotte.github.io/paletteer_gallery/#qualitative)
    dplyr::mutate(colour_confidence = dplyr::if_else(
        confidence == 1, "#D01556FF", dplyr::if_else(
            confidence == 2, "#EFDC60FF", "#7CCA89FF"
        )
    )) |>
    # effect needs to be > 0 for networkD3 but its sign is available so no worries here
    dplyr::mutate(effect = effect * sign) |>
    # solve issue for plotting (because initially was in list format)
    dplyr::mutate(source = as.character(source), target = as.character(target)) |>
    # make area bigger so that arrows are wider (note: only impacts this variable, "locally")
    dplyr::mutate(area = dplyr::if_else(
        area == 1, 1, dplyr::if_else(
            area == 2, 10, 30
        )
    )) |>
    # add area effect? @ARTHUR?
    dplyr::mutate(areaeffect = area * effect)
# remove NA
#tidyr::drop_na()


# parameter to fix the nodes using networkD3 (manually adjusted..)
min_x <- 150
min_y <- 100
max_x <- 600
max_y <- 600

# create nodes table for networkD3 (parameter for the look of the final plot such as dx, dy and rotation of text)
node_table <- vroom::vroom('/home/flo/WORK/work_rbins/owf-es/golem/data-raw/2023/csv/node_list.csv')
nodes <- node_table |>
    as.data.frame() |>
    dplyr::select(NodeID = node, group = type, section, id) |>
    # add x and y position based on local build of networkD3
    dplyr::mutate(x = NA, y = NA, dx = NA, dy = NA, rotation = NA, domain = NA, text_anchor = NA)

# number of nodes in each group
n_pres <- length(which(nodes$group == 'pressure'))
n_comp <- length(which(nodes$group == 'component'))
n_func <- length(which(nodes$group == 'function'))
n_serv <- length(which(nodes$group == 'service'))

# create nodes table for networkD3
nodes[nodes$group == "pressure",]$x <- seq(from = min_x, to = max_x, length.out = n_pres)
nodes[nodes$group == "pressure",]$y <- 0
nodes[nodes$group == "pressure",]$dx <- 12
nodes[nodes$group == "pressure",]$dy <- ".35em"
nodes[nodes$group == "pressure",]$rotation <- -20
nodes[nodes$group == "pressure",]$text_anchor <- "start"

nodes[nodes$group == "component",]$x <- 0
nodes[nodes$group == "component",]$y <- seq(from = 50, to = max_y+100, length.out = n_comp)
nodes[nodes$group == "component",]$dx <- -12
nodes[nodes$group == "component",]$dy <- ".35em"
nodes[nodes$group == "component",]$rotation <- 0
nodes[nodes$group == "component",]$text_anchor <- "end"

nodes[nodes$group == "function",]$x <- max_x+100
nodes[nodes$group == "function",]$y <- seq(from = 50, to = max_y+100, length.out = n_func)
nodes[nodes$group == "function",]$dx <- 12
nodes[nodes$group == "function",]$dy <- ".35em"
nodes[nodes$group == "function",]$rotation <- 0
nodes[nodes$group == "function",]$text_anchor <- "start"

nodes[nodes$group == "service",]$x <- seq(from = min_x-50, to = max_x, length.out = n_serv)
nodes[nodes$group == "service",]$y <- max_y+150
nodes[nodes$group == "service",]$dx <- 12
nodes[nodes$group == "service",]$dy <- ".35em"
nodes[nodes$group == "service",]$rotation <- 20
nodes[nodes$group == "service",]$text_anchor <- "start"

# colour for domains + when there is no domain -> black
nodes[nodes$section == "turbine",]$domain <- "turbine"
nodes[nodes$section == "scour protection layer",]$domain <- "scour protection layer"
nodes[nodes$section == "soft sediment",]$domain <- "soft sediment"
nodes[nodes$section == "water column",]$domain <- "water column"
nodes[is.na(nodes$domain),]$domain <- "pressures - functions - services"

# filter node/paths based on user selection
nodes <- nodes |>
    dplyr::filter((id %in% unique(links$source)) | (id %in% unique(links$target))) |>
    dplyr::mutate(index = dplyr::row_number() - 1) # indexing at 0 for D3.js (!)

# match nodes index to source/target from the dataset of links
links <- links |>
    dplyr::mutate(source = purrr::map(source, .f = function(x, df = nodes){
        df[df$id == x,]$index
    } )) |>
    dplyr::mutate(target = purrr::map(target, .f = function(x, df = nodes){
        df[df$id == x,]$index
    } )) |>
    dplyr::mutate(source = as.numeric(source), target = as.numeric(target))

networkD3::forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                        NodeID = "NodeID", Group = "domain", X = "x", Y = "y", dx = "dx", dy = "dy", rotate_angle = "rotation", 
                        text_anchor = "text_anchor",
                        zoom = TRUE, opacity = 1, legend = TRUE,
                        arrows = TRUE, linkColour = links$colour_sign, Value = "effect",
                        fontSize = 12, opacityNoHover = 1, linkDistance = 200,
                        fontFamily = "sans serif", bounded = FALSE, height = 800, width = 600,
                        colourScale = htmlwidgets::JS('d3.scaleOrdinal().domain(["turbine", "scour protection layer", "soft sediment", "water column", "pressures - functions - services"]).range(["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "black"])'))
