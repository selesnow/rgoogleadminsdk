# Функция для отправки запроса через gargle
gas_make_request <- function(endpoint, params = list()) {
  req <- gargle::request_build(
    method = "GET",
    path   = endpoint,
    params = params,
    token  = gas_token()
  )

  resp <- gargle::request_make(req)
  content <- gargle::response_process(resp)

  return(content)
}

