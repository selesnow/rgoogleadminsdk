library(purrr)

products <- c(
  'Google-Apps',
  '101031',
  '101037',
  '101038',
  'Google-Vault',
  '101001',
  '101005',
  '101033',
  '101034',
  '101047'
)



# Основная функция
main <- function() {

  # Получаем данные о пользователях
  users_data <- gas_get_users_data()

  # Получаем данные об использовании диска
  drive_usage <- gas_get_user_drive_usage(users_data$primaryEmail)

  # Получаем список лицензий
  license <- map_dfr(.x = products, gas_get_user_licenses) %>%
             summarise(
               skuId = paste(skuId, collapse = ', '),
               skuName = paste(skuName, collapse = ', '),
               productName = paste(unique(productName), collapse = ', '),
               .by =  userId
             )

  # Преобразуем данные в нужный формат
  final_data <- users_data %>%
    unnest_wider(name) %>%
    mutate(
      nickname = ifelse(!is.null(aliases), aliases[1], NA),
      first_name = givenName,
      last_name = familyName,
      primary_email = primaryEmail,
      aliases = map_chr(aliases, ~ paste(.x, collapse = ", ")),
      is_enforced = isEnforcedIn2Sv,
      #license_type = licenses$skuId,
      account_status = suspended
    ) %>%
    select(
      nickname,
      first_name,
      last_name,
      primary_email,
      aliases,
      is_enforced,
      #license_type,
      account_status
    )

  # Добавляем данные об использовании диска
  final_data <- final_data %>%
    rowwise() %>%
    mutate(
      drive_usage = ifelse(
        !is.null(drive_usage[[primary_email]]),
        list(drive_usage[[primary_email]]),
        NA
      )
    )

  # Добавляем данные про лицензии
  final_data <- left_join(final_data, license, join_by(primary_email == userId))

  return(final_data)
}
