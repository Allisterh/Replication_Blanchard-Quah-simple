library(ggplot2)
library(zoo)
library(xts)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

1colnames(Infl) <- c('Data', 'Total', 'Upper', 'Lower')
Infl$Data
is.numeric(Infl$Data)

Infl <- Infl[-c(335, 336, 337, 338, 339, 340, 341, 342),]

Infl$Data <- as.Date(as.yearmon(as.character(Infl$Data), "%Y%m"))

tsinfl <- xts(Infl, order.by = Infl$Data)


a <- ggplot(Infl[1:82,], aes(x=Data, y=Total)) + geom_line() + xlab('') + ylab("Inflação") + scale_x_date(date_labels = '%Y-%m') + geom_hline(yintercept = 3, linetype = 'dashed', color = 'blue')
a<- a + geom_hline(yintercept = 4, linetype = "dashed", color = 'red')
a<- a + geom_hline(yintercept = 2, linetype = "dashed", color = 'red')
a <- a + ggtitle('Inflação doméstica') + theme(plot.title = element_text(size = 20, face = 'bold', vjust = 1))
a + theme_grey()


install.packages('tibble')
