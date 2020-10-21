#' Baixa tabelas com metadados das teses e dissertações
#'
#' @param ano Ano no formato aaaa
#' @param diretorio Diretório onde armazenar os htmls
#'
#' @return html
#' @export
#'
tde_baixar_metadados_ano <- function(ano = NULL, diretorio = NULL){



  uri <- "https://teses.usp.br/index.php"

  query <-
    list(
      option = "com_jumi",
      fileid = "19",
      Itemid = "87",
      lang = "pt-br",
      g = "1",
      b6 = ano,
      c6 = "a",
      o6 = "AND"
    )

  paginas <- httr::GET(uri,query=query) %>%
             httr::content() %>%
             xml2::xml_find_first("//div[@class='dadosLinha']") %>%
             xml2::xml_text(trim=T) %>%
             stringr::str_extract("\\d+$") %>%
             as.integer()

  pb <- progress::progress_bar$new(total = length(1:paginas))

  purrr::walk(1:paginas,purrr::possibly(~{

    pb$tick()

   arquivo <- file.path(diretorio,paste0("uspteses_ano_",ano,"_pagina_",.x,".html"))

   query$pagina <- .x

   httr::GET(uri,query= query,httr::write_disk(arquivo,overwrite = TRUE))


  },NULL))


}


