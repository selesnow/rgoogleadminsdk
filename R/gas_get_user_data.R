gas_get_users_data <- function() {

  # Инициализация пустого списка для всех пользователей
  all_users <- list()
  page_token <- NULL
  un <- 0

  repeat {
    # Формируем параметры запроса
    query_params <- list(
      customer = "my_customer",
      maxResults = 500,  # Увеличиваем количество результатов на страницу
      pageToken = page_token,
      projection = "full",
      orderBy = "email"  # Добавляем сортировку для консистентности
    )

    # Составляем и выполняем запрос используя gargle
    result <- gas_make_request(
      "admin/directory/v1/users",
      params = query_params
    )

    result_df <- result$users %>%
      tibble(users = .) %>%
      unnest_wider(users)

    # Проверяем наличие пользователей в ответе
    if (!is.null(result$users)) {
      all_users <- c(all_users, list(result_df))
      # Выводим информацию о прогрессе
      un <- un + nrow(result_df)
      cat(sprintf("Получено %d пользователей\n", un))
    }

    # Проверяем наличие следующей страницы
    page_token <- result$nextPageToken
    if (is.null(page_token)) {
      cat("Достигнут конец списка пользователей\n")
      break
    }
  }

  # Объединяем все результаты в один датафрейм
  users_df <- bind_rows(all_users)
  cat(sprintf("Всего получено %d пользователей\n", nrow(users_df)))

  return(users_df)
}
