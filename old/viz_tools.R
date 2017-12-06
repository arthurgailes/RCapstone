source('R files/cleaning.R')
library(magrittr)

graph_data <- eq_clean_data() %>% 
  dplyr::left_join(eq_location_clean(file), by = 'I_D') %>%
  dplyr::filter(COUNTRY == 'China' | COUNTRY == 'Usa')
  

geom_timeline <- ggplot2::ggplot(graph_data, aes(x = Date, y = COUNTRY)) +
  geom_line() +
  geom_point(aes(size = Richter_Magnitude, fill = DEATHS), alpha = 0.1) +
  scale_y_discrete(breaks = c('Usa', 'China'), labels = c('USA', 'China'))+
  scale_fill_gradient(limits = c(0,10), breaks = c(0,5,10)) +
  scale_size_continuous(breaks = c(2,4,6)) +
  labs(fill = '# deaths', size = 'Richter scale value') +
  theme(legend.position = 'bottom', panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())

geom_timeline

geom_timeline_label <- geom_timeline +
  geom_text(aes(label = ifelse(graph_data$Richter_Magnitude >5, 
                               as.character(graph_data$City),'')),
            angle = 45, hjust = 'bottom', nudge_y = 0.1, size = 2) 

geom_timeline_label



eq_create_label <- function() {
  
  paste('<b>Location: </b>', 
        as.character(map_data$City), '<br />', '<b>Magnitude: </b>', 
        as.character(map_data$Magnitude), '<br />', '<b>Total deaths: </b>', 
        as.character(map_data$DEATHS))
}


map_data <- eq_clean_data() %>% 
  dplyr::left_join(eq_location_clean(file), by = 'I_D') %>%
  dplyr::filter(COUNTRY == 'Mexico') %>%
  dplyr::mutate(popup_text = eq_create_label())



eq_map <- function(annot_col = 'Date') {
  leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addCircleMarkers(data = map_data, lng = ~ LONGITUDE, 
                              lat = ~ LATITUDE, opacity = 0.3, color = '#03F',
                              radius = map_data$Magnitude, 
                              fillOpacity = 0.3, weight = 1, 
                              popup = (as.character(map_data[[annot_col]])))
}

eq_map('popup_text')
