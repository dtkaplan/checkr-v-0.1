#' Functions for logging checkr results to a Google Spreadsheet
#'
#' @param sheet_name character string name of the Google sheet that will be used for logging.
#' @param run set this to TRUE if you really want to re-authenticate.

## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE


# test_sheet <- "DCF_checkr_log"

#' Run this only once, before uploading your app to the server
#' It sets up the authentication for Google
#' @rdname log_google
#' @export
set_up_google_auth_token <- function(run = FALSE) {
  if (!run) stop("Are you sure you want to run this function to set up Google authentication? You should only need to do this once for each logging sheet. If you're sure, set the argument run = TRUE")

  if (file.exists("checkr_app_token.rds")) {
    stop("There is already a 'checkr_app_token.rds' file in this directory.\n That suggests that you have already set up a
        Google spreadsheet for logging. If you want to change the spreadsheet, delete the 'checkr_app_token.rds' file first.")
  } else {
    checkr_token <- googlesheets::gs_auth() # authenticate w/ your desired Google identity here
    saveRDS(checkr_token, "checkr_app_token.rds")
  }
}

#' @rdname log_google
#' @export
turn_on_google_logging <- function(sheet_name){
  google_sheet <- get_google_sheet(sheet_name)
  f <- function(log_entry) {
    googlesheets::gs_add_row(google_sheet, input = jsonlite::toJSON(log_entry))
  }
  # Set the logging function to write to the specified sheet
  options(checkr.logger = f)

  invisible()
}

get_google_sheet <- function(sheet_name) {
  # check for a .httr-oauth file
  # if not there, give error message instructing user to setup the token

  if (!file.exists("checkr_app_token.rds")) stop("Need to use `set_up_google_auth_token()` before you can log submissions on google.")
  googlesheets::gs_auth(token = "checkr_app_token.rds")
  ss <- googlesheets::gs_title(sheet_name)

  ss
}
