#' Retrieves a list of all organizational units for an account.
#'
#' @param customer_id The unique ID for the customer's Google Workspace account. As an account administrator, you can also use the my_customer alias to represent your account's customerId. The customerId is also returned as part of the Users resource.
#'
#' @returns tibble
#' @export
#'
#' @examples
#' \dontrun{
#' org_units <- gas_get_org_units()
#' }
gas_get_org_units <- function(customer_id = "my_customer") {

  org_units <- gas_make_request(
      "admin/directory/v1/customer/my_customer/orgunits",
      list(type = "all")
    )$organizationUnits %>%
    tibble(orgu = .) %>%
    unnest_wider(orgu)

  return(org_units)

}
