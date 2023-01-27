# Função para renderizar tabelas
"%>%" <- magrittr::"%>%"

knit_kable <- function(df, caption = "", font_size = 15, position = "center"){
  kableExtra::kbl(df, align = "c",
                  caption = paste0("<center><strong>", caption, "</strong></center>"),
                  escape = FALSE,
                  format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = "striped",
                              font_size = font_size,
                              position = position) %>%
    kableExtra::kable_paper("hover")
}


# percent_table
percent_table <- function(dados, var1, var2, condition_var2, kable = F){
  table <- dados %>%
    dplyr::count({{var1}}, {{var2}}) %>%
    dplyr::group_by({{var1}}) %>%
    dplyr::mutate("percent" = round(n/sum(n), 4) * 100) %>%
    dplyr::filter({{var2}} == condition_var2)

  if(kable == T){
    table %>%
      knit_kable()
  } else {
    return(table)
  }
}
