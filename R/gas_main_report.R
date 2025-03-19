library(purrr)
library(stringr)
library(dplyr)
library(tidyr)

# Список продуктов, по которым будем запрашивать лицензии
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
  users_data <- gas_get_users_data() %>%
                rowwise() %>%
                mutate(
                  org_unit = orgUnitPath %>% str_remove('^/') %>% str_split('/') %>% pluck(1, 1),
                  isTechMails = str_detect(orgUnitPath, 'Tech Mails')
                ) %>%
                ungroup()

  # Получаем данные об использовании диска
  drive_usage <- gas_get_user_drive_usage(emails = 'all', date = Sys.Date() - 4)

  # Получаем список лицензий
  license <- map_dfr(.x = products, gas_get_user_licenses) %>%
             filter(skuName != 'Cloud Identity Free')

  # Делаем отчёт по лицензиям
  # Отчёт будет загружаться на отдельный лист
  license_report <- license %>%
                    right_join(users_data, join_by(userId == primaryEmail)) %>%
                    select(
                      'userId',
                      'skuName',
                      'org_unit',
                      'isTechMails'
                    ) %>%
                    mutate(
                      skuName = if_else(isTechMails, str_glue('{skuName} [Tech acc]'), str_glue('{skuName} [Users]'))
                    ) %>%
                    arrange(skuName) %>%
                    summarise(
                      licens_num = n_distinct(userId),
                      .by = c('org_unit', 'skuName')
                    ) %>%
                    pivot_wider(id_cols = org_unit, names_from = skuName, values_from = licens_num, values_fill = 0) %>%
                    rename('Org. Unit' = org_unit)

  # Приводим данные о лицензиях к структуре,
  # необходимой для связки с общими данными по пользователям
  license <- license %>%
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
      nickname = NA,
      first_name = givenName,
      last_name = familyName,
      primary_email = primaryEmail,
      aliases = map_chr(aliases, ~ paste(.x, collapse = ", ")),
      is_2fa_enable = isEnforcedIn2Sv
    ) %>%
    select(
      nickname,
      first_name,
      last_name,
      primary_email,
      aliases,
      is_2fa_enable,
      suspended,
      archived
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
    ) %>%
    unnest_wider(drive_usage)

  # Добавляем данные про лицензии
  final_data <- left_join(final_data, license, join_by(primary_email == userId))

  return(final_data)
}
