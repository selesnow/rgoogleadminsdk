#' Helper for `gas_get_user_drive_usage()`
#'
#' @param user_email User primary email or 'all'
#' @param date Report date
#'
#' @returns tibble
get_drive_usage_by_user <- function(user_email, date = Sys.Date()-2) {
  query_params <- list(
    parameters = "accounts:drive_used_quota_in_mb,accounts:gmail_used_quota_in_mb,accounts:gplus_photos_used_quota_in_mb,accounts:total_quota_in_mb,accounts:used_quota_in_mb",
    userKey = user_email,
    maxResults = 1000
  )

  res_list <- list()
  page_token <- NULL

  repeat {
    if (!is.null(page_token)) query_params$pageToken <- page_token

    res <- gas_make_request(
      paste0("admin/reports/v1/usage/users/all/dates/", date),
      params = query_params
    )

    if (!is.null(res$usageReports)) {
      res_list <- append(res_list, res$usageReports)
    }

    page_token <- res$nextPageToken
    if (is.null(page_token)) break
  }

  return(res_list)
}

#' Retrieves a report which is a collection of properties and statistics for a set of users with the account.
#'
#' @details
#' For more information, see the [User Usage Report guide](https://developers.google.com/admin-sdk/reports/v1/guides/manage-usage-users).
#'
#' @param emails A vector of user emails or "all" to retrieve data for all users.
#' @param date The date for which the data should be retrieved (default: two days ago).
#'
#' @return A list containing user drive usage data.
#' @export
#'
#' @examples
#' \dontrun{
#' gas_get_user_drive_usage("all", date = Sys.Date()-2)
#' }
gas_get_user_drive_usage <- function(emails, date = Sys.Date()-3) {
  drive_usage <- list()

  if (all(emails == "all")) {
    usage_data <- get_drive_usage_by_user("all", date)

    for (report in usage_data) {
      email <- report$entity$userEmail
      drive_usage[[email]] <- list(
        drive_used_quota_in_mb = as.numeric(report$parameters[[1]]$intValue),
        gmail_used_quota_in_mb = as.numeric(report$parameters[[2]]$intValue),
        gplus_photos_used_quota_in_mb = as.numeric(report$parameters[[3]]$intValue),
        total_quota_in_mb = as.numeric(report$parameters[[4]]$intValue),
        available_quota_in_mb = as.numeric(report$parameters[[5]]$intValue)
      )
    }
  } else {
    for (email in emails) {
      usage_data <- get_drive_usage_by_user(email, date)
      if (!is.null(usage_data)) {
        drive_usage[[email]] <- usage_data
      }
    }
  }

  return(drive_usage)
}
