#' Buscar jurisprudência no Tribunal de Justiça do RS
#'
#' Esta função permite buscar jurisprudência no Portal de Jurisprudência do Tribunal de Justiça do Estado do Rio Grande do Sul (TJRS) com base no período de julgamento.
#'
#' @param julgamento_inicial Data de julgamento inicial das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data inicial.
#' @param julgamento_final Data de julgamento final das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data final.
#' @param delay Tempo de espera em segundos entre as requisições. O padrão é 5 segundos.
#' @return Uma string JSON contendo um array com as informações sobre as decisões encontradas (originalmente no campo 'docs' da resposta da API).
#'
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr POST content user_agent config
#' @importFrom curl curl_escape
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
tjrs_jurisprudencia <- function(julgamento_inicial = "", julgamento_final = "", delay = 5) {
  # Create log pattern
  pattern <- glue::glue("[{julgamento_inicial}-{julgamento_final}] ")

  message(paste0(pattern, "Iniciando busca de jurisprudência no TJRS...")) # Log start

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
  res <- httr::POST(
    url = url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
    body = parametros,
    httr::config(ssl_verifypeer = FALSE)
  )

  if(res$status_code != 200){
    message(paste0(pattern, "Erro ", res$status_code, " ao acessar o portal de jurisprudencia do TJRS na consulta inicial.")) # Log error with status code
    return(NULL)
  }

  conteudo <- httr::content(res, as = "text") |> jsonlite::fromJSON()

  total_resultados <- conteudo$response$numFound
  if (is.null(total_resultados) || total_resultados == 0) { # Check explicitly for 0 results too
    message(paste0(pattern, "Nenhuma decisao encontrada para os critérios informados.")) # Log no results
    return(NULL)
  }

  n_paginas <- ceiling(total_resultados / 10)
  message(paste0(pattern, "Total de resultados encontrados: ", total_resultados))
  message(paste0(pattern, "Número total de páginas a serem baixadas: ", n_paginas)) # Log total pages

  # Initialize an empty list to store combined documents
  docs_combinados <- list()

  # Loop through each page to fetch and combine documents
  for (pagina_atual in 1:n_paginas) {
    message(paste0(pattern, "Baixando página ", pagina_atual, " de ", n_paginas, "...")) # Log page download progress

    # Define parameters for the current page request
    parametros <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina_atual}&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )

    # Make the POST request for the current page
    res_pagina <- httr::POST(
      url = url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
      body = parametros,
      httr::config(ssl_verifypeer = FALSE)
    )

    # Check page request status
    if(res_pagina$status_code != 200){
       message(paste0(pattern, "Aviso: Falha ao buscar página ", pagina_atual, " (Status: ", res_pagina$status_code, "). Pulando esta página."))
       Sys.sleep(delay) # Wait before next request even if this one failed
       next # Skip to the next iteration
    }

    # Parse content and extract documents
    conteudo_pagina <- httr::content(res_pagina, as = "text") |> jsonlite::fromJSON()
    current_docs <- conteudo_pagina$response$docs

    # Append current page's documents to the combined list if any exist
    if (!is.null(current_docs) && length(current_docs) > 0) {
        # Use c() which is efficient for appending lists in R
        docs_combinados <- c(docs_combinados, current_docs)
    }
    # Optional: Log if a specific page returned no documents (already handled by initial check if total_resultados > 0)

    # Wait for 5 seconds before fetching the next page to avoid overwhelming the server
    Sys.sleep(delay)
  } # End of loop through pages

  # Check if any documents were successfully downloaded after processing all pages
  if (length(docs_combinados) == 0) {
      message(paste0(pattern, "Nenhum documento foi baixado com sucesso após processar todas as páginas."))
      return(NULL)
  }

  # Convert the final combined list to a JSON string
  json_output <- jsonlite::toJSON(docs_combinados, auto_unbox = TRUE)

  message(paste0(pattern, "Busca de jurisprudência no TJRS concluída.")) # Log completion
  return(json_output)
}