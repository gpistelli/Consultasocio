#' Get active businesses
#'
#' Counts how many active businesses there is in a business owner data.frame
#'
#' @param vec A vector with businesses statuses, preferably collected from CS_get_bus_df
#' @return An integer
#' @examples
#' CS_count_active_bus(df$Situação.Cadastral)
#'
#' @export
CS_count_active_bus <- function(vec){
  return(length(which(vec == "ATIVA.")))
}
#'
#' Get biggest income from businesses
#'
#' Filter the biggest income from a business owner
#'
#' @param vec A vector with businesses incomes preferably collected from CS_get_bus_df
#' @return An integer, following our dictionary
#' @examples
#' CS_get_big_income(df$Faturamento)
#'
#' @export
CS_get_big_income <- function(vec){
vec <- as.numeric(factor(x = vec,
                         levels = c("Até R$240.000,00.", "De R$240.000,00 a R$2.400.000,00.",
                                    "De R$2.400.000,00 a R$5.000.000,00.", "De R$5.000.000,00 a R$10.000.000,00.",
                                    "De R$10.000.000,00 a R$30.000.000,00.", "De R$30.000.000,00 a R$50.000.000,00.",
                                    "De R$50.000.000,00 a R$100.000.000,00.", "Mais de R$100.000.000,00."
                                    ),
                         labels = c(1:8)
                         )
                  )
return(max(sort(vec)))
}
#'
#' Get biggest income from businesses
#'
#' Filter the biggest income from a business owner
#'
#' @param vec A vector with businesses incomes preferably collected from CS_get_bus_df
#' @return An integer, following our dictionary
#' @examples
#' CS_get_big_n_employees(df$Número.de.funcionários)
#'
#' @export
CS_get_big_n_employees <- function(vec){
vec <- as.numeric(factor(x = vec,
                         levels = c("Ate 19 Funcionarios (Me).", "Entre 10 E 49 Funcionarios (Epp).",
                                    "Entre 20 E 99 Funcionarios (Epp).", "Entre 50 E 199 Funcionarios (Media).",
                                    "Entre 100 E 199 Funcionarios (Media).", "Acima De 200 Funcionarios (Grande)."
                                    ),
                         labels = c(1:6)
                         )
                )
return(max(sort(vec)))
}
#'
#' Get biggest business size from businesses
#'
#' Filter the biggest business from a business owner
#'
#' @param vec A vector with businesses sizes preferably collected from CS_get_bus_df
#' @return An integer, following our dictionary
#' @examples
#' CS_get_big_size(df$Número.de.funcionários)
#'
#' @export
CS_get_big_size <- function(vec){
vec <- as.numeric(factor(x = vec,
                           levels = c("Micro Empresa.", "Pequena Empresa.", "Média Empresa.", "Grande Empresa."),
                           labels = c(1:4)
                           )
                     )
return(max(sort(vec)))
}
#'
#' Convert CNAE code
#'
#' Convert the business CNAE to our dictionary (check `lista_cod_CNAE` object)
#'
#' @param vec A vector with businesses CNAEs, preferably collected from CS_get_bus_df
#' @return A named vector with CNAEs sector and character values indicating `yes` or `no`
#' @examples
#' CS_CNAE_conv(CS_get_CNAE(df$Atividade.econômica))
#'
#' @export
CS_CNAE_conv <- function(vec){
  vec <- as.character(stringr::str_extract(pattern = "^[:digit:][:digit:]?", string = vec))
  vec <- cnae_vec[vec]
  return(vec)
}
#'
#' Get CNAE codes
#'
#' Extract CNAE codes from businesses
#'
#' @param vec A vector with businesses CNAEs, preferably collected from CS_get_bus_df
#' @return A character vector with its businesses CNAEs
#' @examples
#' CS_get_CNAE(df$Atividade.econômica)
#'
#' @export
CS_get_CNAE <- function(vec){
  vec <- str_extract(string = vec, pattern = "\\(([[:digit:]]+)\\)") %>%
    gsub(pattern = "[[:punct:]]", replacement = "", x = .)
  return(vec)
}
#'
#' Convert Monetary values to numeric
#'
#' Convert the business CNAE to our dictionary (check `cnae_vec` object)
#'
#' @param vec A vector with businesses CNAEs, preferably collected from CS_get_bus_df
#' @return A named vector with CNAEs sector and character values indicating `yes` or `no`
#' @examples
#' CS_CNAE_conv(CS_get_CNAE(df$Atividade.econômica))
#'
#' @export
CS_convert_mon_to_num <- function(vec){
  gsub(pattern = "R\\$", replacement = "", x = vec) %>% gsub("\\.|[[:blank:]]", "", .) %>% gsub(",", "\\.", .) %>%
    trimws() %>% as.numeric()
}
#'
#' Collect and sum Share Capital
#'
#' Process values, retire NAs and sum them up
#'
#' @param vec A vector with businesses share capital, preferably collected from CS_get_bus_df
#' @return A numeric element
#' @examples
#' CS_get_total_ksoc(unique(df$Capital.social))
#'
#' @export
CS_get_total_ksoc <- function(vec){
  vec <- CS_convert_mon_to_num(vec)
  vec <- vec[which(!is.na(vec))]
  return(sum(vec))
}
#'
#' Extract businesses state
#'
#' Process values, retire NAs and returns a vector with these states
#'
#' @param vec A vector with businesses address, preferably collected from CS_get_bus_df
#' @return A character vector containing states location of these businesses
#' @examples
#' CS_get_total_ksoc(unique(df$Capital.social))
#'
#' @export
CS_get_bus_states <- function(vec){
vec <- str_extract(pattern = "AC|AL|AM|AP|BA|CE|DF|ES|GO|MA|MG|MS|MT|PA|PB|PE|PI|PR|RJ|RN|RO|RR|RS|SC|SE|SP|TO", string = vec)
vec <- vec[!is.na(vec)]
return(vec)
}
#'
#' Convert states into regions
#'
#' Changes states into regions through our package internal data
#'
#' @param vec A vector with businesses states, preferably collected from CS_get_bus_states
#' @return A character vector containing these businesses region
#' @examples
#' CS_get_total_ksoc(unique(df$Capital.social))
#'
#' @export
CS_convert_state_to_region <- function(vec){
  vec <- estado_regioes[vec]
  return(vec)
}
#'
#' Extract CEP from address
#'
#' Extract CEP from an address displayed
#'
#' @param vec A vector with businesses adresses, preferably collected from CS_get_bus_states
#' @return A character vector containing these businesses CEPs
#' @examples
#' CS_get_bus_CEP(df$Endereço)
#'
CS_get_bus_CEP <- function(vec){
  vec <- unlist(str_extract_all(vec, "([[:digit:]]+?)-([[:digit:]]+)"))
  vec <- gsub(pattern = "[[:punct:]]", replacement = "", x = vec)
  vec <- vec[which(str_length(vec) > 7)]
  return(vec)
}
#'
#' Switch CEP into city and state
#'
#' Convert a CEP into a vector with city and state through a check in cepdarua.net
#'
#' @param vec A vector CEPs extracted from
#' @return A named vector with city and state
#' @examples
#' CS_convert_CEP_into_city_state(CS_get_bus_CEP(df$Endereço))
#'
CS_convert_CEP_into_city_state <- function(cep_string){
  vec <- rvest::read_html(paste0("https://cepdarua.net/cep/", cep_string)) %>% html_nodes(".u-text-left") %>%
    html_text()
  vec <- vec[grep(pattern = " / ", x = vec)] %>% gsub("\n|  ", "", .) %>% str_split(string = ., pattern = " / ") %>% unlist()
  names(vec) <- c("Cidade", "Estado")
  return(vec)
}
