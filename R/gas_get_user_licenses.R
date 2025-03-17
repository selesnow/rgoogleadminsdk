#' List all users assigned licenses for a specific product SKU.
#'
#' @param product_id A product's unique identifier. For more information about products in this version of the API, see [Products and SKUs](https://developers.google.com/admin-sdk/licensing/v1/how-tos/products).
#' @param customerId The customer's unique ID as defined in the Admin console, such as C00000000. If the customer is suspended, the server returns an error.
#'
#' @returns list
#' @export
#'
#' @examples
#' \dontrun{
#' products <- c(
#' 'Google-Apps',
#' '101031',
#' '101037',
#' '101038',
#' 'Google-Vault',
#' '101001',
#' '101005',
#' '101033',
#' '101034',
#' '101047'
#' )
#'
#' license <- map_dfr(.x = products, gas_get_user_licenses)
#' }
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
