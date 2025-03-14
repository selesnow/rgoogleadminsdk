gas_get_user_licenses <- function(
    product_id = "Google-Apps",
    customerId = "C00le6y6i"
) {

  all_licenses <- list()
  page_token <- NULL

  repeat {

    query_params <- list(
      customerId = customerId,
      pageToken = page_token
    )

    result <- gas_make_request(
      glue::glue("apps/licensing/v1/product/{product_id}/users"),
      params = query_params
    )

    # Добавляем полученные лицензии в список
    if (!is.null(result$items)) {
      all_licenses <- c(all_licenses, result$items)
    }

    # Проверяем наличие следующей страницы
    page_token <- result$nextPageToken
    if (is.null(page_token)) {
      break
    }
  }

  # Конвертируем в датафрейм
  licenses_df <- tibble::tibble(licenses = all_licenses) %>%
    tidyr::unnest_wider(licenses)

  return(licenses_df)

}
