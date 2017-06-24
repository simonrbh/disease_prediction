
# Define server logic required to give diabetes prediction
shinyServer(function(input, output) {
    output$diab_plot <- renderPlot({
    case_input <- data.frame(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE,
      AGE = input$AGE, 
      HYPERTENSION_DURATION_DEG_ACTIVE = input$HYPERTENSION_DURATION_DEG_ACTIVE, 
      AMT_CLAIMED = input$AMT_CLAIMED_k * 1000, 
      HYPERTENSION_DRUGS = input$HYPERTENSION_DRUGS_k * 1000,
      HYPERTENSION_DURATION = max(-1, input$HYPERTENSION_DURATION_y * 365.25), 
      TOTAL_PAID_HCC = input$TOTAL_PAID_HCC_k * 1000,
      CHRONIC_INDICATOR_KEY = input$CHRONIC_INDICATOR_KEY, 
      HYPERLIPIDAEMIA_DURATION = max(-1, input$HYPERLIPIDAEMIA_DURATION_y * 365.25),                  
      GP_VISITS = input$GP_VISITS, 
      VEM_SCORE = input$VEM_SCORE_k * 1000, 
      HYPERLIPIDAEMIA_DRUGS = input$HYPERLIPIDAEMIA_DRUGS_k * 1000,
      HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, 
      BMI = input$BMI)
    case_input$CHRONIC_INDICATOR_KEY <- factor(case_input$CHRONIC_INDICATOR_KEY, levels = c("N", "Y"))
    case_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- factor(case_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE, levels = c("-1", "1"))
    case_input$HYPERTENSION_DURATION_DEG_ACTIVE <- factor(case_input$HYPERTENSION_DURATION_DEG_ACTIVE, levels = c("-1", "1"))
    case_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE <- factor(case_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, levels = c("-1", "1"))
    case_input$BMI <- factor(case_input$BMI, levels = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"))
    
    case_pred <- predict(fit_rf, case_input, type = "prob", predict.all = TRUE)
    pred <- as.matrix(case_pred$aggregate, ncol = 2)
    class(pred) <- "numeric"
    
    ## Get a confidence interval - note assumption of normality of underlying distributions...
    case_all_trees <- case_pred$individual
    case_all_trees <- as.matrix(case_pred$individual, nrow = dim(case_pred$aggregate)[1])
    class(case_all_trees) <- "numeric"
 #   apply(case_all_trees, 1, mean)
    sds <- apply(case_all_trees, 1, sd)
    case_pred_ci <- as.data.frame(cbind(pmax(0, pred[, 2] - 2*sds), 
                                        pmax(0, pred[, 2] - sds),
                                        pred[, 2],
                                        pmin(1, pred[, 2] + sds),
                                        pmin(1, pred[, 2] + 2*sds)))
    names(case_pred_ci) <- c("lower95",  "lower66", "mean", "upper66", "upper95")

    ggplot(case_pred_ci, aes(x = row.names(case_pred_ci))) +
      # geom_boxplot(aes(ymin = lower95, lower = lower66, middle = mean, upper = upper66, ymax = upper95),
      #              stat = "identity") + 
      geom_segment(data = case_pred_ci, aes(x = 0.41, xend = 0.41, 
                                            y = lower95, yend = upper95), colour="#D1C0C0", size = 1, linetype = 2) +
      geom_segment(data = case_pred_ci, aes(x = 0.4, xend = 1.6, 
                                 y = mean, yend = mean), colour="#8A1923", size = 3) +
      geom_segment(data = case_pred_ci, aes(x = 0.4, xend = 0.9, 
                                            y = lower66, yend = lower66), colour="#AA5E61", size = 2) +
      geom_segment(data = case_pred_ci, aes(x = 0.4, xend = 0.9, 
                                            y = upper66, yend = upper66), colour="#AA5E61", size = 2) +
      geom_segment(data = case_pred_ci, aes(x = 0.4, xend = 0.6, 
                                            y = lower95, yend = lower95), colour="#D1C0C0", size = 1) +
      geom_segment(data = case_pred_ci, aes(x = 0.4, xend = 0.6, 
                                            y = upper95, yend = upper95), colour="#D1C0C0", size = 1) +
      geom_segment(data = case_pred_ci, aes(x = 0.6, xend = 0.9,
                                            y = lower95, yend = lower66), colour="#D1C0C0", size = 1, linetype = 2) +
      geom_segment(data = case_pred_ci, aes(x = 0.9, xend = 1.6,
                                            y = lower66, yend = mean), colour="#D1C0C0", size = 1, linetype = 2) +
      geom_segment(data = case_pred_ci, aes(x = 1.6, xend = 0.9,
                                            y = mean, yend = upper66), colour="#D1C0C0", size = 1, linetype = 2) +
      geom_segment(data = case_pred_ci, aes(x = 0.9, xend = 0.6,
                                            y = upper66, yend = upper95), colour="#D1C0C0", size = 1, linetype = 2) +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.1)), labels = scales::percent) +
      # geom_text(data = crossval2, aes(x = year, y = q3, label = percent(round(q3, 3))), size = 3, vjust = -0.5, hjust = 1) +
      # geom_text(data = crossval2, aes(x = year, y = q1, label = percent(round(q1, 3))), size = 3, vjust = 1, hjust = 1) +
      theme(axis.text.x = element_text(size = 16)) +
      scale_x_discrete(labels = NULL, breaks = NULL, expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme(legend.position = "none") +
      coord_fixed(ratio = 1) +
      coord_flip()
    })
    
    # output$prob_t <- renderText({
    #   paste(percent(round(case_pred_ci()$lower95, digits = 2)), 
    #         percent(round(case_pred_ci()$lower66, digits = 2)), 
    #         percent(round(case_pred_ci()$mean, digits = 2)), 
    #         percent(round(case_pred_ci()$upper66, digits = 2)), 
    #         percent(round(case_pred_ci()$upper95, digits = 2)), 
    #         sep = " - ")
    # })
    # output$diab_plot <- renderPlot({
    # 
    # })
    
    }
    )

