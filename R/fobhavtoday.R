#' @name fobhavtoday
#' @aliases fobhavtoday
#'
#' @title Get F&O Bhavcopy for the present day
#'
#' @note The date should be strictly numerical and mentioned in quotation mark.The present days Bhavcopy would usually available in the evening.
#' @return Todays's F&O Bhavcopy.
#' @author Nandan L. Patil \email{tryanother609@@gmail.com}
#' @details Gets todays Futures and Options bhavcopy from NSE.
#'
#' @seealso \code{\link[nser]{bhavpr}}\code{\link[nser]{bhav}}
#'
#' @import stats httr
#' @importFrom utils download.file read.csv unzip
#' @importFrom curl has_internet
#' @export
#' @examples \dontrun{
#' #Todays F&O Bhavcopy
#' library(nser)
#' report = fobhavtoday()
#' }
fobhavtoday = function()
{
  # Check for internet connection
  if (has_internet()){
    message("Downloading Bhavcopy")
  } else {
    message("No internet connection")
  }

  baseurl = "https://archives.nseindia.com/content/historical/DERIVATIVES/"
  end = ".csv.zip"
  month = format(Sys.time(), "%m")
  month = as.integer(month)
  month = month.abb[month]
  month = toupper(month)
  year = format(Sys.time(), "%Y")
  date = format(Sys.time(), "%d")

  bhavurl = paste0(baseurl, year, "/", month, "/fo", date, month, year, "bhav", end)
  name = paste0(date, month, year)
  zipname = paste0("fo", date, month, year, "bhav", ".csv")

  try_GET <- function(x, ...) {
    tryCatch(
      GET(url = x, timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }

  resp <- try_GET(bhavurl)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (http_error(resp)) {
    message_for_status(resp)
    return(invisible(NULL))
  }

  temp <- tempfile()
  download.file(bhavurl, temp)
  file <-  read.csv(unz(temp, filename = zipname))
  unlink(temp)
  return(file)
}


