#' TOP AQIs: WHERE and HOW HIGH
#'
#' @param num
#'
#' @return The current top "num" worst AQIs in the US, where "num" is an integer from 1 to 5
#' @export
#'
#' @examples
#' maxMinPolutedCity(1)
#' maxMinPolutedCity(2)
#' maxMinPolutedCity(3)
#' @import dplyr Quandl rvest

maxMinPollutedCity <- function(num)  {
  url_highestAQI <- "https://www.airnow.gov/index.cfm?action=airnow.main"
  highAQI <- read_html(url_highestAQI) %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(trim = TRUE,fill=TRUE)
  if (num == 5) {
    Current_Top_Cities <- c(highAQI[[1,1]],highAQI[[4,1]],highAQI[[7,1]],highAQI[[10,1]],highAQI[[13,1]])
    AQIs <- c(highAQI[[1,2]],highAQI[[4,2]],highAQI[[7,2]],highAQI[[10,2]],highAQI[[13,2]])
    (topAQIcities <- data.frame(Current_Top_Cities,AQIs) )
  } else if (num == 4) {
    Current_Top_Cities <- c(highAQI[[1,1]],highAQI[[4,1]],highAQI[[7,1]],highAQI[[10,1]])
    AQIs <- c(highAQI[[1,2]],highAQI[[4,2]],highAQI[[7,2]],highAQI[[10,2]])
    (topAQIcities <- data.frame(Current_Top_Cities,AQIs) )
    } else if (num == 3) {
    Current_Top_Cities <- c(highAQI[[1,1]],highAQI[[4,1]],highAQI[[7,1]])
    AQIs <- c(highAQI[[1,2]],highAQI[[4,2]],highAQI[[7,2]])
    (topAQIcities <- data.frame(Current_Top_Cities,AQIs) )
    } else if (num == 2) {
    Current_Top_Cities <- c(highAQI[[1,1]],highAQI[[4,1]])
    AQIs <- c(highAQI[[1,2]],highAQI[[4,2]])
    (topAQIcities <- data.frame(Current_Top_Cities,AQIs) )
    } else {
    Current_Top_Cities <- c(highAQI[[1,1]])
    AQIs <- c(highAQI[[1,2]])
    (topAQIcities <- data.frame(Current_Top_Cities,AQIs) )
    }
}
