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
#' @importFrom magrittr "%>%"
#' @import magrittr
#'
#'@export
eq_clean_data <- function(filename = 'data/signif.txt') {
  
  table <- suppressMessages(readr::read_tsv(filename, progress = F)) 
  table <- dplyr::filter(table, YEAR >=2000, is.na(LONGITUDE) ==F, is.na(LATITUDE) == F) 
  table <- dplyr::select(table, I_D, YEAR, MONTH, DAY, LONGITUDE, LATITUDE, 
                  Richter_Magnitude = EQ_MAG_ML, DEATHS, Magnitude = EQ_PRIMARY) 
  
  table[is.na(table)] <- 01
  table <- tidyr::unite(table, col = "Date", from = c(YEAR, MONTH, DAY), sep = "-") 
  
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
#' @importFrom magrittr "%>%"
#' 
#' @export
eq_location_clean <- function(filename = 'data/signif.txt') {
  loc <- suppressMessages(readr::read_tsv(filename, progress = F)) 
  loc <- dplyr::select(loc, I_D, COUNTRY, LOCATION_NAME) 
  loc <- tidyr::separate(loc, LOCATION_NAME, into = c('delete', 'City'), sep = ': ', 
                    extra = 'merge', fill = 'right') 
  loc <- dplyr::select(loc, -delete) 
  loc <- dplyr::mutate_all(loc, .funs = stringr::str_to_title) 
  loc <- dplyr::mutate(loc, I_D = as.numeric(I_D))
  loc
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
#' @importFrom magrittr "%>%" 
#' @import ggplot2
#' 
#' @examples
#' geom_timeline()
#' 
#' @export
geom_timeline <- function() {
  graph_data <- eq_clean_data() 
  graph_data <- dplyr::left_join(graph_data, eq_location_clean(), by = 'I_D') 
  graph_data <- dplyr::filter(graph_data, COUNTRY == 'China' | COUNTRY == 'Usa')
  
  ggplot2::ggplot(graph_data, ggplot2::aes(x = Date, y = COUNTRY)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(size = Richter_Magnitude, fill = DEATHS), 
                        alpha = 0.1) +
    ggplot2::scale_y_discrete(breaks = c('Usa', 'China'), 
                              labels = c('USA', 'China'))+
    ggplot2::scale_fill_gradient(limits = c(0,10), breaks = c(0,5,10)) +
    ggplot2::scale_size_continuous(breaks = c(2,4,6)) +
    ggplot2::labs(fill = '# deaths', size = 'Richter scale value') +
    ggplot2::theme(legend.position = 'bottom', panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour = 'black'),
        axis.title.y=ggplot2::element_blank(),
        axis.ticks.y=ggplot2::element_blank())
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
  # graph_data <- eq_clean_data() 
  # graph_data <- dplyr::left_join(graph_data, eq_location_clean(), by = 'I_D') 
  # graph_data <- dplyr::filter(graph_data, COUNTRY == 'China' | COUNTRY == 'Usa')
  
  geom_timeline() +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(graph_data$Richter_Magnitude >5, 
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
  orig <- eq_clean_data() 
  orig <- dplyr::left_join(orig, eq_location_clean(), by = 'I_D')
  orig <- dplyr::filter(orig, COUNTRY == 'Mexico') 
  paste('<b>Location: </b>', 
        as.character(orig$City), '<br />', '<b>Magnitude: </b>', 
        as.character(orig$Magnitude), '<br />', '<b>Total deaths: </b>', 
        as.character(orig$DEATHS))
}


#' Mexico earthquake map
#' 
#' @return \code{eq_map} generates a map of earthquakes in Mexico since 2000;
#' the size of the circles reflects the magnitude of the earthquake.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%" 
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @examples 
#' eq_map('Date')
eq_map <- function(annot_col = 'popup_text') {
  map_data <- eq_clean_data() 
  map_data <- dplyr::left_join(map_data, eq_location_clean(), by = 'I_D')
  map_data <- dplyr::filter(map_data, COUNTRY == 'Mexico') 
  map_data <- dplyr::mutate(map_data, popup_text = eq_create_label())
  
  map <- leaflet::leaflet() 
  map <- leaflet::addTiles(map) 
  map <- leaflet::addCircleMarkers(map, data = map_data, lng = ~ LONGITUDE, 
                              lat = ~ LATITUDE, opacity = 0.3, color = '#03F',
                              radius = map_data$Magnitude*1.5, 
                              fillOpacity = 0.3, weight = 1, 
                              popup = (as.character(map_data[[annot_col]])))
  map
}


