# Define as predições com base no melhor threshold
# retorna uma lista contendo o df com as preds e um gráfico da curva ROC
library(ranger)
library(workflows)

pred <- function(df_test, model){
  pred_df <- df_test %>%
    dplyr::mutate(stroke_pred = predict(model,
                                        new_data = df_test,
                                        type = "prob")$.pred_1)

  roc <- pROC::roc(response = pred_df$stroke,
                   predictor = pred_df$stroke_pred)

  best_threshold <- pROC::coords(roc, "best", "threshold")

  p <- pROC::ggroc(roc, size = 1) +
    geom_vline(xintercept = best_threshold$specificity,
               linetype = 2, color = "red")

  p <- p +
    geom_point(data = p$data %>%
                 dplyr::filter(threshold == best_threshold$threshold,
                               specificity == best_threshold$specificity),
               aes(x = specificity, y = sensitivity),
               color = "red", size = 3) +
    geom_label(data = p$data %>%
                 dplyr::filter(threshold == best_threshold$threshold,
                               specificity == best_threshold$specificity),
               aes(x = specificity, y = sensitivity, label = round(threshold, 4)),
               vjust = -0.5)


  pred_df <- pred_df %>%
    dplyr::mutate(stroke_pred = ifelse(stroke_pred >= best_threshold$threshold, 1, 0),
                  stroke_pred = factor(stroke_pred, levels = c(0, 1)))

  lista_return <- list(df = pred_df, plot = p)

  return(lista_return)
}
