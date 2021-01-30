

library(tidyverse); library(sf); library(leaflet); library(leaflet.extras2)

# Create function using leaflet and plugins to do side-by-side slider maps
# doesn't seem possible to do real legends b/c no "option" argument in addLegend
side_by_side_polys <- function(sf_object, left_column, right_column, n_bins, left_colors = 'Blues', right_colors = 'Reds',
                               map_title, left_label, right_label, darker_colors_denote){
  
  left_var <- sf_object %>% pull({{left_column}})
  right_var <- sf_object %>% pull({{right_column}})
  
  left_pal <- colorQuantile(left_colors, left_var, n = n_bins)
  right_pal <- colorQuantile(right_colors, right_var, n = n_bins)
  
  darker_colors_label <- paste('Darker colors denote', darker_colors_denote)
  
  
  sf_object %>%
    leaflet() %>%
    addMapPane("left", zIndex = 0) %>%
    addMapPane("right", zIndex = 0) %>%
    addProviderTiles(providers$Stamen.TonerLite, group="base", layerId = "baseid",
                     options = c(pathOptions(pane = "right"), providerTileOptions(opacity = 0.3))) %>%
    addProviderTiles(providers$Stamen.TonerLite, group="carto", layerId = "cartoid",
                     options = c(pathOptions(pane = "left"), providerTileOptions(opacity = 0.3))) %>%
    addPolygons(fillColor = ~left_pal(left_var),
                fillOpacity = 0.8, weight = 1, color = 'white',
                group = 'left_group', options = pathOptions(pane = 'left')) %>%
    addPolygons(fillColor = ~right_pal(right_var),
                fillOpacity = 0.8, weight = 1, color = 'white',
                group = 'right_group', options = pathOptions(pane = 'right')) %>%
    addLabelOnlyMarkers(lng = -75.39, lat = 40.03, label = HTML(left_label),
                        options = pathOptions(pane = 'left'), group = 'left_group',
                        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                    style = list(
                                                      "color" = "#4D7BAF",
                                                      "font-family" = "arial",
                                                      "text-align" = "center",
                                                      "font-weight" = "bold",
                                                      "background" = "rgba(255,255,255,1)",
                                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "20px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    ))) %>%
    addLabelOnlyMarkers(lng = -74.88, lat = 40.03, label = HTML(right_label),
                        options = pathOptions(pane = 'right'), group = 'right_group',
                        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                    style = list(
                                                      "color" = "#C62312",
                                                      "font-family" = "arial",
                                                      "text-align" = "center",
                                                      "font-weight" = "bold",
                                                      "background" = "rgba(255,255,255,1)",
                                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "20px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    ))) %>%
    addLabelOnlyMarkers(lng = -75.137, lat = 39.80, label = darker_colors_label,
                        options = pathOptions(pane = 'right'), group = 'right_group',
                        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                    style = list(
                                                      "color" = "black",
                                                      "font-family" = "arial",
                                                      "text-align" = "center",
                                                      "background" = "rgba(255,255,255,1)",
                                                      "font-size" = "11px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    ))) %>%
    # addControl(title, position = "topleft", className="map-title") %>%
    addSidebyside(layerId = "sidecontrols",
                  rightId = "baseid",
                  leftId = "cartoid")
}
