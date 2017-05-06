#' Functions for logging checkr results to a Google Spreadsheet
#'
#' @param submissions_sheet_name character string name of the Google sheet that will be used for logging.
#' @param id_sheet_name character string name of the Google sheet with the user ids/passwords.
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
turn_on_google_logging <- function(submissions_sheet_name, id_sheet_name){
  submissions <- get_google_log_sheet(submissions_sheet_name)
  get_google_user_ids(id_sheet_name)
  f <- function(log_entry) { # for exercises
    googlesheets::gs_add_row(submissions, input = jsonlite::toJSON(log_entry))
  }
  # an event recorder for all actions. This doesn't need to go through the exercise submission process,
  # but is called directly from learnr.
  g <- function(tutorial_id, tutorial_version, user_id,  # for questions
                         event, data) {
    data$output <- NULL # kluge so that toJSON() works. I don't think I need this, anyways.
    event <- list(user = get_user_name(),
                  date = format(as.POSIXlt(Sys.time()), usetz = TRUE),
                  tutorial_id = tutorial_id, tutorial_version = tutorial_version,
                  event = event, data = data)
    event$user_id <- get_user_name()
    saveRDS(event, "~/Downloads/event.rds")
    googlesheets::gs_add_row(submissions, input = jsonlite::toJSON(event))
  }
  # Set the logging function to write to the specified sheet
  options(checkr.logger = f)
  options(learnr.event_recorder = g) # Changed from tutor.event_recorder on May 5, 2017

  invisible()
}

get_google_log_sheet <- function(sheet_name) {
  # check for a .httr-oauth file
  # if not there, give error message instructing user to setup the token

  if (!file.exists("checkr_app_token.rds")) stop("Need to use `set_up_google_auth_token()` before you can log submissions on google.")
  googlesheets::gs_auth(token = "checkr_app_token.rds")
  ss <- googlesheets::gs_title(sheet_name)

  ss
}
get_google_user_ids <- function(sheet_name) {
  # check for a .httr-oauth file
  # if not there, give error message instructing user to setup the token

  if (!file.exists("checkr_app_token.rds")) stop("Need to use `set_up_google_auth_token()` before you can log submissions on google.")
  googlesheets::gs_auth(token = "checkr_app_token.rds")
  ss <- googlesheets::gs_title(sheet_name)

  set_accounts(gs_read(ss))
}


