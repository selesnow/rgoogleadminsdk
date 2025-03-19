
# rgoogleadminsdk

<!-- badges: start -->
<!-- badges: end -->

Пакет rgoogleadminsdk является интерфейсом к [Google Workspace Admin SDK API](https://developers.google.com/admin-sdk/reference-overview), но на данный момент он охватыет лишь небольшую часть функционала предоставляющегося API.

На данный момент в rgoogleadminsdk реализованы следующие функции:

* `gas_auth()` - Авторизация
* `gas_get_org_units()` - Запрос всех орг. юнитов
* `gas_get_users_data()` - Запрос данных по пользователям
* `gas_get_user_licenses()` - Запрос данных по использованию лицензий
* `gas_get_user_drive_usage()` - Запрос данных по использованию Google Drive

## Установка

``` r
# install.packages("pak")
pak::pak("selesnow/rgoogleadminsdk")
```

## Пример использования

Примеры добавлю позже
