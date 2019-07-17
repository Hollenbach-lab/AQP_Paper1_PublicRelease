

find.Cluster <- function(haplotype, gene.cluster.input) {
  res <- gene.cluster.input[gene.cluster.input$unique.haps == haplotype, "cluster"]
  return(res)
}


mln.function <- function(surveyData.element, surveyData.input = surveyData, grpGene.List.input = grpGene.List, haps.input = haps, unique.haps.input = unique.haps) {
  mln.data <- data.frame(haps = haps.input, surveyData.input[[surveyData.element]] %>% select(-studyid), stringsAsFactors = F)
  
  # Replace haplotypes 171 levels by 18 clusters
  cluster <- as.numeric(unlist(strsplit(as.character(grpGene.List.input[[surveyData.element]]$cluster),' ')))
  gene.cluster <- data.frame(unique.haps = unique.haps.input, cluster, stringsAsFactors = F)
  
  mln.data$code <- sapply(mln.data$haps, gene.cluster.input = gene.cluster, FUN = find.Cluster)
  mln.data$code <- as.factor(mln.data$code)
  mln.data$code2 <- relevel(mln.data$code, ref = 1)
  # Model building on Training dataset
  formula.mln <- as.formula(paste("code2", "~",
                                  paste(colnames(surveyData.input[[surveyData.element]])[colnames(surveyData.input[[surveyData.element]]) != "studyid"], collapse = "+"),
                                  sep = ""))
  mln.data.training <- mln.data[1:floor(nrow(mln.data)*0.9),]
  # mln.data.training <- mln.data[1:floor(nrow(mln.data)*0.66),]
  
  mlogistic <- nnet::multinom(formula.mln, maxit = 1000, data = mln.data.training)
  
  
  # Need to source directly the 2 below functions from pscl package as the pscl::pR2 function is a Primitive function.
  pR2Work <- function (llh, llhNull, n) {
    McFadden <- 1 - llh/llhNull
    G2 <- -2 * (llhNull - llh)
    r2ML <- 1 - exp(-G2/n)
    r2ML.max <- 1 - exp(llhNull * 2/n)
    r2CU <- r2ML/r2ML.max
    out <- c(llh = llh, llhNull = llhNull, G2 = G2, McFadden = McFadden, 
             r2ML = r2ML, r2CU = r2CU)
    out
  }
  
  pR2.multinom <- function (object, ...) {
    llh <- logLik(object)
    cat("fitting null model for pseudo-r2\n")
    objectNull <- update(object, ~1)
    llhNull <- logLik(objectNull)
    n <- dim(object$fitted.values)[1]
    pR2Work(llh, llhNull, n)
  }
  
  summaryModel <- pR2.multinom(mlogistic)
  
  R2MF <- summaryModel["McFadden"]
  R2ML <- summaryModel["r2ML"]
  R2CU <- summaryModel["r2CU"]
  AIC <- extractAIC(mlogistic)[2]
  BIC <- stats4::BIC(mlogistic)
  
  # Prediction
  # Training
  predicted_class.training <- predict(mlogistic, mln.data.training)
  table(predicted_class.training, mln.data.training$code)
  mis.error.training <- mean(as.character(predicted_class.training) != as.character(mln.data.training$code))
  # Testing
  mln.data.testing <- mln.data[(floor(nrow(mln.data)*0.9)+1):nrow(mln.data),]
  # mln.data.testing <- mln.data[(floor(nrow(mln.data)*0.66)+1):nrow(mln.data),]
  
  predicted_class.testing <- predict(mlogistic, mln.data.testing)
  table(predicted_class.testing, mln.data.testing$code, useNA = "ifany")
  mis.error.testing <- mean(as.character(predicted_class.testing) != as.character(mln.data.testing$code))
  
  # Create the output
  out <- data.frame(Analysis = surveyData.element,
                    r2ML = R2ML, r2CU = R2CU, r2MF = R2MF, aic = AIC, bic = BIC,
                    Misclassication.training = mis.error.training, Misclassication.testing = mis.error.testing,
                    stringsAsFactors = F, row.names = surveyData.element)
  # out.List <- list(mlogistic, out)
  # names(out.List) <- c(paste(surveyData.element, "model", sep = "_"), paste(surveyData.element, "out", sep = "_"))
  return(out)
}