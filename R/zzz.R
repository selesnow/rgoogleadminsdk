.onLoad <- function(libname, pkgname) {

  # auth object
  .auth <<- gargle::init_AuthState(
    package     = "rgoogleadminsdk",
    auth_active = TRUE
  )

  # where function
  utils::globalVariables("where")

  ## adw
  if ( Sys.getenv("GAS_EMAIL") != "" ) {

    gads_email <- Sys.getenv("GAS_EMAIL")
    cli_alert_info('Set email from environt variables')

  } else {

    gads_email <- NULL

  }

  # options
  op <- options()
  op.gads <- list(gas.api.version          = "v1",
                  gas.base.url             = 'https://googleads.googleapis.com/',
                  gas.column.name.case.fun = snakecase::to_snake_case,
                  gargle_oauth_email       = gads_email)

  toset <- !(names(op.gads) %in% names(op))
  if (any(toset)) options(op.gads[toset])

  invisible()
}
