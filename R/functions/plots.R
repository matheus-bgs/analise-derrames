# Funções para gráficos
plot_hist <- function(dados, x, fill, binwidth,
                      title = NULL, xlab = NULL, ylab = NULL){
  ggplot(dados, aes(x = {{x}}, fill = {{fill}})) +
    geom_histogram(color = "black", binwidth = binwidth,
                   position = "stack") +
    scale_fill_manual(values = c("#003366", "#336600")) +
    labs(title = title,
         x = xlab, y = ylab)
}

plot_box <- function(dados, y, title = NULL, xlab = NULL, ylab = NULL){
  ggplot(dados, aes(x = stroke, y = {{y}})) +
    geom_boxplot(fill = c("#003366", "#336600")) +
    labs(title = title, x = xlab, y = ylab)
}

plot_bar <- function(dados, x, fill, title = NULL, xlab = NULL){
  ggplot(dados, aes(x = {{x}}, fill = {{fill}})) +
    geom_bar(color = "black", position = "dodge") +
    scale_fill_manual(values = c("#003366", "#336600")) +
    labs(title = title, x = xlab, y = NULL) +
    theme(legend.position = "bottom") +
    coord_flip()
}

# Temas para gráficos
theme_custom <- function(){
  ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text = ggplot2::element_text(color = "#3b3c36",
                                                     size = 10))
}

ggplot2::theme_set(theme_custom())

