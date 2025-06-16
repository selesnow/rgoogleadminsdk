## This file is the interface between bigrquery and the
## auth functionality in gargle.

# initialization happens in .onLoad
.auth <- NULL
app <- NULL

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "rgoogleadminsdk",
  YOUR_STUFF  = "your Google Ads Account",
  PRODUCT     = "Google Workspace Admin",
  API         = "Google Admin SDK",
  PREFIX      = "gas"
)

# main oauth function
#' Authorization in Google Ads API
#'
#'
#' @param email Optional. Allows user to target a specific Google identity.
#' @param path Path to JSON file with identifying the service account
#' @param cache Specifies the OAuth token cache.
#' @param use_oob Whether to prefer "out of band" authentication.
#' @param developer_token Your Google Ads Developer Token.
#' @param token A token with class \link[httr:Token-class]{Token2.0} or an object of
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
#' @family auth functions
#'
#' @return \link[httr:Token-class]{Token2.0}
#' @export
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' gas_auth()
#'
#' ## force use of a token associated with a specific email
#' gas_auth(email = "yourname@example.com")
#'
#' ## force a menu where you can choose from existing tokens or
#' ## choose to get a new one
#' gas_auth(email = NA)
#'
#' ## -----------------------
#' ## use own developer token
#' gas_auth(
#'     email = "yourname@example.com"
#' )
#'
#' ## -----------------------
#' ## use own OAuth client app
#' gas_auth_configure(
#'     path = "path/to/your/oauth_client.json"
#' )
#'
#' gas_auth(email = "yourname@example.com")
#' }
gas_auth <- function(
    email   = gargle::gargle_oauth_email(),
    path    = NULL,
    subject = NULL,
    cache   = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token   = NULL) {

  # check default app
  app <- gas_oauth_app() %||% gas_default_ouath_app()

  cred <- gargle::token_fetch(
    scopes  = c(
      "https://www.googleapis.com/auth/admin.directory.user.readonly",
      "https://www.googleapis.com/auth/admin.directory.orgunit.readonly",
      "https://www.googleapis.com/auth/drive.readonly",
      "https://www.googleapis.com/auth/admin.reports.usage.readonly",
      "https://www.googleapis.com/auth/apps.licensing"
    ),
    app     = app,
    email   = email,
    path    = path,
    subject = subject,
    package = "rgoogleadminsdk",
    cache   = cache,
    use_oob = use_oob,
    token   = token
  )

  if (!inherits(cred, "Token2.0")) {
    stop(
      "Can't get Google credentials.\n",
      "Are you running rgoogleads in a non-interactive session? Consider:\n",
      "  * Call `gas_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Suspend authorization
#' @eval gargle:::PREFIX_deauth_description_with_api_key(gargle_lookup_table)
#' @family auth functions
#' @return only suspend authorization
#' @export
gas_deauth <- function() {
  .auth$set_auth_active(FALSE)
  .auth$clear_cred()

  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#' @family low-level API functions
#' @export
gas_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!gas_has_token()) {
    gas_auth()
  }
  httr::config(token = .auth$cred)
}

# has token
#' Is there a token on hand?
#'
#' @eval gargle:::PREFIX_has_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_has_token_return()
#' @family low-level API functions
#' @export
gas_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}


#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_configure_params()
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' # see and store the current user-configured OAuth app (probaby `NULL`)
#' (original_app <- gas_oauth_app())
#'
#' # see and store the current user-configured API key (probaby `NULL`)
#' (original_api_key <- gas_api_key())
#'
#' if (require(httr)) {
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "YOUR_CLIENT_ID_GOES_HERE",
#'     secret = "YOUR_SECRET_GOES_HERE"
#'   )
#'   google_key <- "YOUR_API_KEY"
#'   gas_auth_configure(app = google_app, api_key = google_key)
#'
#'   # confirm the changes
#'   gas_oauth_app()
#'   gas_api_key()
#'
#'   # bring your own app via JSON downloaded from Google Developers Console
#'   # this file has the same structure as the JSON from Google
#'   gas_auth_configure(path = app_path)
#'
#'   # confirm the changes
#'   gas_oauth_app()
#'
#' }
#'
#' # restore original auth config
#' gs4_auth_configure(app = original_app, api_key = original_api_key)
#' }
#' @export
gas_auth_configure <- function(client, path, api_key, app = lifecycle::deprecated()) {

  if (lifecycle::is_present(app)) {
    gas_auth_configure(client = app, path = path, api_key = api_key)
  }

  if (!missing(client) && !missing(path)) {
    gas_abort("Must supply exactly one of {.arg client} or {.arg path}, not both")
  }

  stopifnot(missing(api_key) || is.null(api_key) || is_string(api_key))

  if (!missing(path)) {
    stopifnot(is_string(path))
    client <- gargle::oauth_app_from_json(path)
  }
  stopifnot(missing(client) || is.null(client) || inherits(client, "oauth_app"))

  if (!missing(client) || !missing(path)) {
    .auth$set_app(client)
  }

  if (!missing(api_key)) {
    .auth$set_api_key(api_key)
  }

  invisible(.auth)
}


#' @export
#' @rdname gas_auth_configure
gas_auth_cache_path <- function() {

  if ( gas_has_token() ) {
    .auth$cred$cache_path
  } else {
    cli_alert_warning("You need to log in to google account")
  }

}

#' Open folder with auth cache files
#' @export
#' @rdname gas_auth_configure
gas_open_auth_cache_folder <- function() {

  if ( gas_has_token() ) {
    shell.exec(.auth$cred$cache_path)
  } else {
    cli_alert_warning("You need to log in to google account")
  }

}

# gas abort
gas_abort <- function(message, ..., .envir = parent.frame()) {
  cli::cli_div(theme = gas_theme())
  cli::cli_abort(message = message, ..., .envir = .envir)
}

# is_path is_string
is_path <- function(x) is.character(x) && !inherits(x, "drive_id")
is_string <- function(x) length(x) == 1L && is_path(x)

# theme
gas_theme <- function() {
  list(
    span.field = list(transform = single_quote_if_no_color),
    # I want to style the Drive file names similar to cli's `.file` style,
    # except cyan instead of blue
    span.drivepath = list(
      color = "cyan",
      fmt = utils::getFromNamespace("quote_weird_name", "cli")
    ),
    # since we're using color so much elsewhere, e.g. Drive file names, I think
    # the standard bullet should be "normal" color
    ".memo .memo-item-*" = list(
      "text-exdent" = 2,
      before = function(x) paste0(cli::symbol$bullet, " ")
    )
  )
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

single_quote_if_no_color <- function(x) quote_if_no_color(x, "'")

quote_if_no_color <- function(x, quote = "'") {
  # TODO: if a better way appears in cli, use it
  # @gabor says: "if you want to have before and after for the no-color case
  # only, we can have a selector for that, such as:
  # span.field::no-color
  # (but, at the time I write this, cli does not support this yet)
  if (cli::num_ansi_colors() > 1) {
    x
  } else {
    paste0(quote, x, quote)
  }
}

# getters
#' @export
#' @rdname gas_auth_configure
gas_api_key <- function() .auth$api_key


#' @export
#' @rdname gas_auth_configure
gas_oauth_app <- function() .auth$app

#' Get info on current user
#'
#' @eval gargle:::PREFIX_user_description()
#' @eval gargle:::PREFIX_user_seealso()
#' @eval gargle:::PREFIX_user_return()
#'
#' @export
gas_user <- function() {
  if (gas_has_token()) {
    gargle::token_email(gas_token())
  } else {
    cli_alert_info("Not logged in as any specific Google user")
    invisible()
  }
}


# default app
gas_default_ouath_app <- function() {

  app <- httr::oauth_app(
    'rgoogleadminsdk',
    key = '321452169616-16j0nopccfv9h80sk6mp2audas1vpq55.apps.googleusercontent.com',
    secret = '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'
  )

  return(app)

}

# local deauth
local_deauth <- function(env = parent.frame()) {
  original_cred <- .auth$get_cred()
  original_auth_active <- .auth$auth_active
  gas_bullets(c("i" = "Going into deauthorized state"))
  withr::defer(
    gas_bullets(c("i" = "Restoring previous auth state")),
    envir = env
  )
  withr::defer({
    .auth$set_cred(original_cred)
    .auth$set_auth_active(original_auth_active)
  }, envir = env)
  gas_deauth()
}

gas_bullets <- function(text, .envir = parent.frame()) {
  quiet <- gas_quiet()
  if (quiet) {
    return(invisible())
  }
  cli::cli_div(theme = gas_theme())
  cli::cli_bullets(text = text, .envir = .envir)
}

gas_quiet <- function() {
  getOption("gas_quiet", default = NA)
}
