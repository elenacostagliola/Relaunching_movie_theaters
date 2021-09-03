predict.mnl <- function(model, data) {
        # Function for predicting preference shares from a MNL model 
        # model: mlogit object returned by mlogit()
        # data: a data frame containing the set of designs for which you want to 
        # predict shares.  Same format at the data used to estimate model. 
        data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
        logitUtility <- data.model%*%model$coef
        share <- exp(logitUtility)/sum(exp(logitUtility))
        cbind(share, data)
}

sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
        # Function for creating data for a preference share-sensitivity chart
        # model: mlogit object returned by mlogit() function
        # attrib: list of vectors with attribute levels to be used in sensitivity
        # base.data: data frame containing baseline design of target product
        # competitor.data: data frame contining design of competitive set
        a <- rbind(base.data, competitor.data)
        base.share <- predict.mnl(model, data)[1,1]
        share <- NULL
        for (a in seq_along(attrib)) {
                for (i in attrib[[a]]) {
                        data[1,] <- base.data
                        data[1,a] <- i
                        share <- c(share, predict.mnl(model, data)[1,1])
                }
        }
        data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}

predict.mixed.mnl <- function(model, data, nresp=1000) {
        # Function for predicting shares from a mixed MNL model 
        # model: mlogit object returned by mlogit()
        # data: a data frame containing the set of designs for which you want to 
        #       predict shares. Same format at the data used to estimate model. 
        # Note that this code assumes all model parameters are random
        data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
        coef.Sigma <- cov.mlogit(model)
        coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
        draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
        shares <- matrix(NA,nrow=nresp, ncol=nrow(data))
        for (i in 1:nresp) {
                utility <- data.model%*%draws[i,]
                share = exp(utility)/sum(exp(utility))
                shares[i,] <- share
        }
        cbind(colMeans(shares), data)
}