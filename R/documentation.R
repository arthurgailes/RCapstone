#' Imports the U.S. National Oceanographic and Atmospheric Administation's (NOAA) 
#' dataset of significant earthquakes for more info, visit: 
#' \hret{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{click here}
#' 
#' The package imports the data, cleans it, and then produces visualizations of earthquake
#'  data in select countries beginning in 2000.
#'
#' \code{eq_clean_data} reads in the data, cleans it, and reformats the date 
#'  variables
#' @param  filename represents the dataset; defaults to its current directory.
#' 
#' @examples 
#' eq_clean_data()
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom lubridate as_date
#' @importFrom dplyr %>%
#'
#'@export
eq_clean_data <- function(filename = 'data/signif.txt') {
  
  table <- suppressMessages(readr::read_tsv(filename, progress = F)) %>%
    dplyr::filter(YEAR >=2000, is.na(LONGITUDE) ==F, is.na(LATITUDE) == F) %>%
    dplyr::select(I_D, YEAR, MONTH, DAY, LONGITUDE, LATITUDE, 
                  Richter_Magnitude = EQ_MAG_ML, DEATHS, Magnitude = EQ_PRIMARY) 
  
  table[is.na(table)] <- 01
  table <- table %>%
    tidyr::unite(col = "Date", from = c(YEAR, MONTH, DAY), sep = "-") 
  
  table$Date <- lubridate::as_date(table$Date)
  #class(table$LATITUDE) <- "numeric"
  #class(table$LONGITUDE) <- "numeric"
  table

}


#' Clean country and city names
#'
#' \code{eq_location_clean} Returns country and city names along with the I_D from
#' the original dataset.
#' 
#' @param  filename represents the dataset; defaults to its current directory.
#' 
#' @seealso \code{\link{eq_clean_data}}
#' 
#' @examples
#' eq_location_clean('data/signif.txt') 
#' 
#' @importFrom tidyr separate
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr %>%
#' 
#' @export
eq_location_clean <- function(filename = 'data/signif.txt') {
  loc <- suppressMessages(readr::read_tsv(filename, progress = F)) %>%
    dplyr::select(I_D, COUNTRY, LOCATION_NAME) %>% 
    tidyr::separate(LOCATION_NAME, into = c('delete', 'City'), sep = ': ', 
                    extra = 'merge', fill = 'right') %>%
    dplyr::select(-delete) %>% 
    dplyr::mutate_all(.funs = stringr::str_to_title) %>% 
    dplyr::mutate(I_D = as.numeric(I_D))
    
}

#' Graph the timeline
#' 
#' \code{geom_timeline} returns a graph of earthquakes from 2000-2017 in the USA 
#' and China. Size corresponds to the Richter Magnitude of the earthquake
#' 
#' @inheritParams eq_clean_data
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @import ggplot2
#' 
#' @examples
#' geom_timeline()
#' 
#' @export
geom_timeline <- function() {
  graph_data <- eq_clean_data() %>% 
    dplyr::left_join(eq_location_clean(), by = 'I_D') %>%
    dplyr::filter(COUNTRY == 'China' | COUNTRY == 'Usa')
  
  ggplot2::ggplot(graph_data, aes(x = Date, y = COUNTRY)) +
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
}

#' Graph the earthquake timeline for the US and China, with high magnitudes noted.
#' 
#' 
#' @import ggplot2
#' 
#' @examples 
#' geom_timeline_label()
#' 
#' @export
geom_timeline_label <- function(){
  geom_timeline +
  geom_text(aes(label = ifelse(graph_data$Richter_Magnitude >5, 
                               as.character(graph_data$City),'')),
            angle = 45, hjust = 'bottom', nudge_y = 0.1, size = 2) 
}

#' Creating the "popup_text" parameter. Not meant for use.
#' 
#' \code{eq_create_label} is used by \code{eq_map} to create the popup_text parameter,
#' which allows \code{eq_map} to display all relevant data, rather than one statistic.
#' 
#' @examples 
#' eq_map('popup_text')
eq_create_label <- function() {
  
  paste('<b>Location: </b>', 
        as.character(map_data$City), '<br />', '<b>Magnitude: </b>', 
        as.character(map_data$Magnitude), '<br />', '<b>Total deaths: </b>', 
        as.character(map_data$DEATHS))
}


#' Mexico earthquake map
#' 
#' @return \code{eq_map} generates a map of earthquakes in Mexico since 2000;
#' the size of the circles reflects the magnitude of the earthquake.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' 
#' @examples 
#' eq_map('Date')
eq_map <- function(annot_col = 'popup_text') {
  map_data <- eq_clean_data() %>% 
    dplyr::left_join(eq_location_clean(file), by = 'I_D') %>%
    dplyr::filter(COUNTRY == 'Mexico') %>%
    dplyr::mutate(popup_text = eq_create_label())
  
  leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addCircleMarkers(data = map_data, lng = ~ LONGITUDE, 
                              lat = ~ LATITUDE, opacity = 0.3, color = '#03F',
                              radius = map_data$Magnitude, 
                              fillOpacity = 0.3, weight = 1, 
                              popup = (as.character(map_data[[annot_col]])))
}

