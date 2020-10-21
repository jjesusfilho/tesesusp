#' Baixar dados de teses e dissertações com base no nome
#'
#' @param url Vetor de urls
#' @param diretorio Diretório onde armazenar os dados
#'
#' @return htmls
#' @export
#'
tde_baixar_dados_nome <- function(url = NULL,diretorio ="."){


  pb <- progress::progress_bar$new(total = length(url))


  purrr::walk(url,purrr::possibly(~{

     pb$tick()

    id <- .x %>%
       stringr::str_extract("\\d.+(?=/)") %>%
       stringr::str_replace_all("\\W","_")

    arquivo <- file.path(diretorio,paste0(id,".html"))

    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}
