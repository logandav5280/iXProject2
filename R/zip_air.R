#' most recent Air Quality in a zip code.
#'
#' @param zip
#'
#' @return The current Air Quality Index of a certain zip code along with any associated health messages.
#' @export
#'
#' @examples
#' zipAir(90007)
#' zipAir(80021)
#' @import dplyr Quandl rvest

zipAir <- function(zip)  {
    url_air_zip <- paste0("https://www.airnow.gov/index.cfm?action=airnow.local_city&zipcode=",zip,"&submit=Go")
    air_tab <- read_html(url_air_zip) %>%
      html_nodes("table") %>%
      .[[12]] %>%
      html_table(trim = TRUE,fill=TRUE)
    paste0(air_tab[[1,1]]," for the zip code ",zip,": ",air_tab[[2,2]]," and ",air_tab[[3,3]]," indicating... ",air_tab[[5,1]])
  }
