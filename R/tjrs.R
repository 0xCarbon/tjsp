#' Buscar jurisprudência no Tribunal de Justiça do RS
#'
#' Esta função permite buscar jurisprudência no Portal de Jurisprudência do Tribunal de Justiça do Estado do Rio Grande do Sul (TJRS) com base no período de julgamento.
#'
#' @param julgamento_inicial Data de julgamento inicial das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data inicial.
#' @param julgamento_final Data de julgamento final das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data final.
#' @param delay Tempo de espera em segundos entre as requisições. O padrão é 5 segundos.
#' @param timeout_seconds Tempo máximo em segundos para esperar por uma resposta da requisição. O padrão é 60 segundos.
#' @param proxy_string String de conexão do proxy opcional (formato: "http://user:pass@host:port" ou "http://host:port"). Se fornecida, será usada para as requisições. O padrão é NULL (sem proxy).
#' @return Uma string JSON contendo um array com as informações sobre as decisões encontradas (originalmente no campo 'docs' da resposta da API).
#'
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom purrr map list_flatten rate_delay slowly
#' @importFrom httr POST content user_agent config timeout use_proxy
#' @importFrom curl curl_escape
#' @importFrom urltools url_parse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Buscar jurisprudência tjrs
#' tjrs <- tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/03/2023")
#' tjrs |> head(5)
#'
#' # Caso ele não encontre nada, mostrará e um aviso e retornará um valor NULL
#' tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/02/2023")
#' }
tjrs_jurisprudencia <- function(julgamento_inicial = "", julgamento_final = "", delay = 5, timeout_seconds = 60, proxy_string = NULL) {
  # Create log pattern
  pattern <- glue::glue("[{julgamento_inicial}-{julgamento_final}] ")

  message(paste0(pattern, "Iniciando busca de jurisprudência no TJRS...")) # Log start

  # --- Initialize Proxy Variables ---
  proxy_hostname <- NULL
  proxy_port     <- NULL
  proxy_username <- NULL
  proxy_password <- NULL
  use_proxy_config <- FALSE

  # --- Check for Proxy String Parameter ---
  if (!is.null(proxy_string) && nzchar(proxy_string)) {
      message(paste0(pattern, "Tentando usar configuração de proxy da string fornecida..."))
      tryCatch({
          # Parse the proxy URL
          parsed_url <- urltools::url_parse(proxy_string)

          proxy_hostname <- parsed_url$domain
          proxy_port     <- as.integer(parsed_url$port)
          proxy_username <- parsed_url$user
          proxy_password <- parsed_url$password

          if (!is.null(proxy_hostname) && nzchar(proxy_hostname) && !is.na(proxy_port) && proxy_port > 0) {
              use_proxy_config <- TRUE
              message(paste0(pattern, "Usando proxy da string: ", proxy_hostname, ":", proxy_port))
          } else {
              warning(paste0(pattern, "String de proxy fornecida ('", proxy_string, "') não pôde ser completamente parseada ou está inválida. Procedendo sem proxy."))
          }
      }, error = function(e) {
          warning(paste0(pattern, "Erro ao parsear a string de proxy '", proxy_string, "': ", e$message, ". Procedendo sem proxy."))
      })
  } else {
      message(paste0(pattern, "Nenhuma string de proxy fornecida. Procedendo sem proxy."))
  }

message(paste0(pattern, "Configuração de proxy:"))
message(paste0(pattern, "  - Hostname: ", proxy_hostname))
message(paste0(pattern, "  - Porta: ", proxy_port))
message(paste0(pattern, "  - Usuário: ", proxy_username))
message(paste0(pattern, "  - Senha: ", proxy_password))

  message(paste0(pattern, "Proxy configurado: ", use_proxy_config))
  # --- End Proxy Configuration ---

  url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

  pagina <- 1
  dt_julgamento_de <- curl::curl_escape(julgamento_inicial)
  dt_julgamento_ate <- curl::curl_escape(julgamento_final)

  parametros <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual=1&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
  )

  message(paste0(pattern, "Realizando consulta inicial para obter o número de páginas..."))

  # --- Create initial config list ---
  initial_config <- list(ssl_verifypeer = FALSE, timeout = httr::timeout(timeout_seconds))
  if (use_proxy_config) {
      initial_config <- c(initial_config, list(
          proxy = httr::use_proxy(
              url = proxy_hostname,
              port = proxy_port,
              username = if (!is.null(proxy_username) && !is.na(proxy_username) && nzchar(proxy_username)) proxy_username else NULL,
              password = if (!is.null(proxy_password) && !is.na(proxy_password) && nzchar(proxy_password)) proxy_password else NULL
          )
      ))
  }
  # --- End initial config list ---

  res <- httr::POST(
    url = url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
    body = parametros,
    httr::config(!!!initial_config) # Apply the config list
  )

  if(res$status_code != 200){
    message(paste0(pattern, "Erro ", res$status_code, " ao acessar o portal de jurisprudencia do TJRS na consulta inicial.")) # Log error with status code
    return(NULL)
  }

  conteudo <- httr::content(res, as = "text") |> jsonlite::fromJSON()

  # Check if the response itself contains an error field
  if (!is.null(conteudo$error)) {
    message(paste0(pattern, "Erro retornado pela API do TJRS na consulta inicial. Detalhes: ", conteudo$error)) # Log API error
    return(NULL)
  }

  # Check for response structure validity (optional but good practice)
  if (is.null(conteudo$response) || is.null(conteudo$response$numFound)) {
     message(paste0(pattern, "Estrutura de resposta inesperada da API do TJRS na consulta inicial."))
     return(NULL)
  }

  total_resultados <- conteudo$response$numFound
  if (is.null(total_resultados) || total_resultados == 0) { # Check explicitly for 0 results too
    message(paste0(pattern, "Nenhuma decisao encontrada para os critérios informados.")) # Log no results
    return(jsonlite::toJSON(list(), auto_unbox = TRUE))
  }

  n_paginas <- ceiling(total_resultados / 10)
  message(paste0(pattern, "Total de resultados encontrados: ", total_resultados))
  message(paste0(pattern, "Número total de páginas a serem baixadas: ", n_paginas)) # Log total pages

  # Use map to get a list of lists (each inner list is the 'docs' from a page)
  lista_docs <- purrr::map(1:n_paginas, purrr::slowly(~ {
    pagina_atual <- .x
    message(paste0(pattern, "Baixando página ", pagina_atual, " de ", n_paginas, "...")) # Log page download progress

    url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

    # --- Create page config list ---
    page_config <- list(ssl_verifypeer = FALSE, timeout = httr::timeout(timeout_seconds))
    if (use_proxy_config) {
        page_config <- c(page_config, list(
            proxy = httr::use_proxy(
                url = proxy_hostname,
                port = proxy_port,
                username = if (!is.null(proxy_username) && !is.na(proxy_username) && nzchar(proxy_username)) proxy_username else NULL,
                password = if (!is.null(proxy_password) && !is.na(proxy_password) && nzchar(proxy_password)) proxy_password else NULL
            )
        ))
    }
    # --- End page config list ---

    parametros <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina_atual}&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )

    res_pagina <- httr::POST( # Renamed variable to avoid conflict
      url = url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
      body = parametros,
      httr::config(!!!page_config) # Apply the config list
    )

    # Optional: Add basic check for page request status
    if(res_pagina$status_code != 200){
       message(paste0(pattern, "Aviso: Falha ao buscar página ", pagina_atual, " (Status: ", res_pagina$status_code, "). Pulando esta página."))
       return(NULL) # Return NULL for this page if it fails
    }

    conteudo_pagina <- httr::content(res_pagina, as = "text") |> jsonlite::fromJSON() # Renamed variable

    # Check if the page response contains an error field
    if (!is.null(conteudo_pagina$error)) {
        message(paste0(pattern, "Aviso: Erro retornado pela API do TJRS ao buscar página ", pagina_atual, ". Detalhes: ", conteudo_pagina$error,". Pulando esta página."))
        return(NULL) # Return NULL for this page if it has an API error
    }

    # Check for expected response structure on the page
    if (is.null(conteudo_pagina$response) || is.null(conteudo_pagina$response$docs)) {
         message(paste0(pattern, "Aviso: Estrutura de resposta inesperada da API do TJRS na página ", pagina_atual, ". Pulando esta página."))
         return(NULL) # Return NULL if structure is unexpected
    }

    return(conteudo_pagina$response$docs)
  }, purrr::rate_delay(delay)))

  # Filter out NULL entries from failed page requests
  lista_docs <- Filter(Negate(is.null), lista_docs)

  if (length(lista_docs) == 0) {
      message(paste0(pattern, "Nenhum documento foi baixado com sucesso após processar as páginas."))
      return(NULL)
  }

  # Flatten the list of lists into a single list
  docs_combinados <- purrr::list_flatten(lista_docs)

  # Convert the combined list to a JSON string
  json_output <- jsonlite::toJSON(docs_combinados, auto_unbox = TRUE)

  message(paste0(pattern, "Busca de jurisprudência no TJRS concluída.")) # Log completion
  return(json_output)
}