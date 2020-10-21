#' Ler teses, dissertações e livre docência
#'
#' @param arquivos Arquivos
#' @param diretorio Se não informar arquivos
#'
#' @return tibble
#' @export
#'
tde_ler_dados_nome <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "tde",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))


  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    x <- .x %>%
        xml2::read_html()

    documento <- x %>%
            xml2::xml_find_first("//div[@class='DocumentoTitulo']") %>%
            xml2::xml_text()

    doi <- x %>%
          xml2::xml_find_first("//div[contains(text(),'DOI')]/following-sibling::div") %>%
          xml2::xml_text()

    nome_completo <- x %>%
      xml2::xml_find_first("//div[contains(text(),'Nome completo')]/following-sibling::div") %>%
      xml2::xml_text()


    email <- x %>%
      xml2::xml_find_first("//div[contains(text(),'E-mail')]/following-sibling::div") %>%
      xml2::xml_text()


    unidade<- x %>%
      xml2::xml_find_first("//div[contains(text(),'Unidade da USP')]/following-sibling::div") %>%
      xml2::xml_text()

     area_conhecimento <- x %>%
      xml2::xml_find_first("//div[contains(text(),'\u00c1rea do Conhecimento')]/following-sibling::div") %>%
      xml2::xml_text()


     data_defesa <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Data de Defesa')]/following-sibling::div") %>%
       xml2::xml_text() %>%
       lubridate::ymd()


     imprenta <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Imprenta')]/following-sibling::div") %>%
       xml2::xml_text()

     orientador <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Orientador')]/following-sibling::div") %>%
       xml2::xml_text(trim = TRUE)

     banca_examinadora <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Banca examinadora')]/following-sibling::div") %>%
       xml2::xml_text(trim = TRUE)

     titulo_portugues <- x %>%
       xml2::xml_find_first("//div[contains(text(),'T\u00edtulo em portugu\u00eas')]/following-sibling::div") %>%
       xml2::xml_text()


     palavras_chave_portugues <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Palavras-chave em portugu\u00eas')]/following-sibling::div") %>%
       xml2::xml_text(trim= TRUE)

     resumo_portugues <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Resumo em portugu\u00eas')]/following-sibling::div") %>%
       xml2::xml_text()

     palavras_chave_ingles <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Palavras-chave em ingl\u00eas')]/following-sibling::div") %>%
       xml2::xml_text(trim = TRUE)

     resumo_ingles <- x %>%
       xml2::xml_find_first("//div[contains(text(),'Resumo em ingl\u00eas')]/following-sibling::div") %>%
       xml2::xml_text()

    tibble::tibble(documento,
                   doi,
                   nome_completo,
                   email,
                   unidade,
                   area_conhecimento,
                   data_defesa,
                   imprenta,
                   orientador,
                   banca_examinadora,
                   titulo_portugues,
                   palavras_chave_portugues,
                   resumo_portugues,
                   palavras_chave_ingles,
                   resumo_ingles
                   )


  },NULL))

}

