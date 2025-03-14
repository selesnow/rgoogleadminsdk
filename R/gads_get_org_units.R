gas_get_org_units <- function(customer_id = "my_customer") {

  org_units <- gas_make_request(
      "admin/directory/v1/customer/my_customer/orgunits",
      list(type = "all")
    )$organizationUnits %>%
    tibble(orgu = .) %>%
    unnest_wider(orgu)

  return(org_units)

}
