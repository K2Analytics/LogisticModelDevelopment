m <-data.frame()
x <- sapply(sapply(ls(), get), is.data.frame)
m <- data.frame(names(x)[(x == TRUE)])
LogisticModelDevelopment(m)
