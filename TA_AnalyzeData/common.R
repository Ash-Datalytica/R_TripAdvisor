wwwPrefix <- "http://www.tripadvisor.ru"

## Нормализовать числовую переменную в диапазон (-1,1)
## mode="median" Тогда после нормализации Медиана новоро распределения будет равна targetValue
## mode="mean" Тогда после нормализации Среднее новоро распределения будет равно targetValue
normalizeTanh <- function (x, mode="median", targetValue=0.5) {
    if (mode == "median") {
        alpha <- median (x)/atanh(targetValue)
    } else if (mode == "mean") {
        alpha <- mean (x)/atanh(targetValue)
    } else {
        stop ("не известный режим mode  в normalizeTanh")
    }
    tanh(x/alpha)
}


## Нарисовать график с ROC кривой, используя ggplot2
## predicted -вектор предсказанных значений
## expected - вектор ожидаемых значенийъ
myPlotROC <- function (predicted, expected, title=NULL) {
    #ROC curve - сделать сводный по моделям
    pred <- prediction (predicted, expected)
    pe <- performance(pred, "tpr", "fpr")
    au <- performance(pred, "auc")@y.values[[1]]
    pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
    p <- ggplot(pd, aes(x=fpr, y=tpr))
    p <- p + geom_line(colour="red")
    p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
    
    p <- p + theme(plot.title=element_text(size=10))
    p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
    p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                      label=paste("AUC =", round(au, 2))) 
    if (!is.null(title)) {
        p <- p + ggtitle(title)    
    }
    
    p
}
