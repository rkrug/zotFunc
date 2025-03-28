#' Get Zotero User ID from Username
#'
#' This function retrieves a Zotero user ID from a given username.
#' 
#' **NB:The function relies on analysing the profile page. As this page might change, 
#' it is not recommended to use this function programatically, but rather to retrieve 
#' the id and to use it hardcoded.**
#' @md
#'
#' @param username The Zotero username. **Zotero Usernames are case sensitive!**
#'
#' @return The Zotero user ID as a string.
#' 
#' @md
#' 
#' @importFrom httr2 request req_error req_perform resp_status resp_body_string
#'
#' @export
#'
#' @examples
#' \dontrun{
#' id_from_name("ipbes")
#' # "5760254"
#' }
id_from_name <- function(username) {
  # Construct the profile URL
  profile_url <- paste0("https://www.zotero.org/", username)

  response <- httr2::request(profile_url) |>
    httr2::req_error(is_error = function(resp) {
      code <- httr2::resp_status(resp)
      code >= 400 && code != 404  # treat 404 as non-error
    }) |>
    httr2::req_perform()

  # Check if the request was successful
  if (httr2::resp_status(response) != 200) {
    stop("Username is invalid!\nPlease check the username.\nUsernames are case sensitive!")
  }

  # Extract the raw HTML content
  raw_html <- httr2::resp_body_string(response)

  # Define the regular expression pattern to match the profileUserID
  pattern <- '"profileUserID":(\\d+),'

  # Search for the pattern in the raw HTML
  match <- regexpr(pattern, raw_html)

  # Check if a match was found
  if (match[1] == -1) {
    stop("User ID not found. The profile may not exist or is private.")
  }

  # Extract the matched substring
  matched_text <- regmatches(raw_html, match)

  # Extract the numeric user ID from the matched text
  user_id <- sub(pattern, "\\1", matched_text)

  return(user_id)
}
