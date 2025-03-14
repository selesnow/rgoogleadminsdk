# Функция для получения данных об использовании диска
get_drive_usage_by_user <- function(user_email, date = Sys.Date()-2) {

  query_params <- list(
    parameters = "accounts:drive_used_quota_in_mb,accounts:gmail_used_quota_in_mb,accounts:gplus_photos_used_quota_in_mb,accounts:total_quota_in_mb",
    userKey = user_email
  )

  res <- gas_make_request(
    paste0("admin/reports/v1/usage/users/all/dates/", date),
    params = query_params
  )

}

# Получение данных о дисковом пространстве для всех пользователей
gas_get_user_drive_usage <- function(emails) {
  drive_usage <- list()

  for (email in emails) {
    usage_data <- get_drive_usage_by_user(email)
    if (!is.null(usage_data)) {
      drive_usage[[email]] <- usage_data
    }
  }

  return(drive_usage)
}
