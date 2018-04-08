require(e1071)
require(randomForest)

gaussFilter <- function(stack, sigma) {
  gauss.filter <- focalWeight(stack[[1]], sigma, "Gauss")
  layer.ls <- vector("list", dim(stack)[3])
  for (i in 1:dim(stack)[3]){
    layer.ls[[i]] <- focal(stack[[i]], w=gauss.filter, na.rm=TRUE, pad=TRUE, padValue=0)
  }
  output <- stack(layer.ls)
  return(output)
}

labImg <- function(stack) {
  stack.df <- data.frame(getValues(stack))
  lab <- convertColor(as.matrix(stack.df), from="sRGB", to="Lab")
  stack <- setValues(stack, lab)
  return(stack)
}


prepareTrainingData <- function(lab, sigma, training.road.orig.img, training.road.class.img, training.noroad.img,
                                training.road.orig.img.lab, training.noroad.img.lab) {
  if (lab) {
    tr.ro.img <- training.road.orig.img.lab
    tr.nr.img <- training.noroad.img.lab
  } else {
    tr.ro.img <- training.road.orig.img
    tr.nr.img <- training.noroad.img
  }
  tr.rc.img <- training.road.class.img[[2]]
  tr.rc.img.red <- training.road.class.img[[1]]
  tr.rc.img <- setValues(tr.rc.img, ifelse(getValues(tr.rc.img)==255 & getValues(tr.rc.img.red)<50,1,0))
  
  # Gaussian Filter
  ro.gauss <- gaussFilter(tr.ro.img, sigma)
  nr.gauss <- gaussFilter(tr.nr.img, sigma)
  
  # Prepare data frame with road pixels
  roadclass.df <- data.frame(getValues(tr.rc.img))
  names(roadclass.df) <- c("roadpix")
  roadorig.df <- data.frame(getValues(tr.ro.img))
  names(roadorig.df) <- c("b1", "b2", "b3")
  roadgauss.df <- data.frame(getValues(ro.gauss))
  names(roadgauss.df) <- c("b1g", "b2g", "b3g")
  valid <- !(roadorig.df$b1 == 0 & roadorig.df$b2 == 0 & roadorig.df$b3 == 0)
  roadclass.df <- roadclass.df[valid,,drop=FALSE]
  roadorig.df <- roadorig.df[valid,,drop=FALSE]
  roadgauss.df <- roadgauss.df[valid,]
  road.training.df <- cbind(roadorig.df, roadgauss.df, roadclass.df)
  
  # Prepare data frame with noroad pixels
  noroad.df <- data.frame(getValues(tr.nr.img))
  names(noroad.df) <- c("b1", "b2", "b3")
  noroadgauss.df <- data.frame(getValues(nr.gauss))
  names(noroadgauss.df) <- c("b1g", "b2g", "b3g")
  valid <- !(noroad.df$b1 == 0 & noroad.df$b2 == 0 & noroad.df$b3 == 0)
  noroad.df <- noroad.df[valid,]
  noroadgauss.df <- noroadgauss.df[valid,]
  roadpix <- FALSE
  noroad.training.df <- cbind(noroad.df, noroadgauss.df, roadpix)
  
  # Combine data sets and define response
  all.df <- rbind(noroad.training.df, road.training.df)
  all.df$road <- as.factor(all.df$roadpix)
  
  return(all.df)
}


learnClassifier <- function(training.noroad.img, training.road.class.img, training.road.orig.img, evaluate.best=FALSE) {
  
  # Only keep first three color bands in classified images (discard alpha-channel from PNG images)
  if (nlayers(training.road.orig.img) > 3) {
    training.road.orig.img <- dropLayer(training.road.orig.img, 4) 
  }
  if (nlayers(training.road.class.img) > 3) {
    training.road.class.img <- dropLayer(training.road.class.img, 4) 
  }
  if (nlayers(training.noroad.img) > 3) {
    training.noroad.img <- dropLayer(training.noroad.img, 4) 
  }
  
  # Convert images to LAB colors
  training.road.orig.img.lab <- labImg(training.road.orig.img)
  training.noroad.img.lab <- labImg(training.noroad.img)
  
  if (evaluate.best) {
    # Iterate through specifications and estimate average error via k-fold cross validation
    # Test different gaussian kernels and color codings (lab vs. RGB)
    specs.svm <- cbind(expand.grid(model="svm", sigma=7, lab=c(TRUE), param1=0.1, param2=10), NA, NA, NA)
    specs.rf <- cbind(expand.grid(model="RF", sigma=c(7), lab=c(TRUE), param1=c(2.5), param2=c(50,75,100)), NA, NA, NA)
    specs <- rbind(specs.svm, specs.rf)
    
    # Prepare data for all combinations of lab and sigma
    labsig <- unique(specs[,2:3])
    data.ls <- vector("list", nrow(labsig))
    for (i in 1:nrow(labsig)) {
      lab <- labsig[i,2]
      sigma <- labsig[i,1]
      all.df <- prepareTrainingData(lab, sigma, training.road.orig.img, training.road.class.img, training.noroad.img,
                                    training.road.orig.img.lab, training.noroad.img.lab)
      data.ls[[i]] <- all.df
    }
    
    # Run through specs and compute test errors
    for (i in 1:nrow(specs)) {
      
      # Get specs
      model <- specs[i,1]
      lab <- specs[i,3]
      sigma <- specs[i,2]
      param1 <- specs[i,4]
      param2 <- specs[i,5]
      
      # Get data
      labsig.idx <- which(labsig[,1]==sigma & labsig[,2]== lab)
      all.df <- data.ls[[labsig.idx]]
      
      # Validate on random sample K times
      training.frac <- 0.1
      testing.frac <- 0.5
      K <- 3
      set.seed(0)
      cv.errors <- vector("numeric", K)
      fit.times <- vector("numeric", K)
      pred.times <- vector("numeric", K)
      for (k in 1:K) {
        
        test.size <- floor(testing.frac*nrow(all.df))
        test.id <- sample(nrow(all.df), size=test.size)
        testset.df <- all.df[test.id,]
        train.cand.id <- c(1:nrow(all.df))[-test.id]
        train.size <- floor(training.frac*nrow(all.df))
        train.id <- sample(train.cand.id, size=train.size)
        trainset.df <- all.df[train.id,]
        
        if (model == "svm") {
          fit.time <- system.time(svm.fit <- svm(road ~ b1 + b2 + b3 + b1g + b2g + b3g, data = trainset.df, gamma = param1, cost = param2)) 
          pred.time <- system.time(svm.pred <- predict(svm.fit, testset.df))
          svm.tab <- table(pred = svm.pred, true = testset.df$roadpix)
          svm.error <- (svm.tab[1,2]+svm.tab[2,1])*100/sum(svm.tab)
          cv.errors[k] <- svm.error
          fit.times[k] <- fit.time[3]
          pred.times[k] <- pred.time[3]
        } else {
          fit.time <- system.time(rf.fit <- randomForest(road ~ b1 + b2 + b3 + b1g + b2g + b3g, data = trainset.df, mtry=param1, ntree=param2))
          pred.time <- system.time(rf.pred <- predict(rf.fit, testset.df))
          rf.tab <- table(pred = rf.pred, true = testset.df$roadpix)
          rf.error <- (rf.tab[1,2]+rf.tab[2,1])*100/sum(rf.tab)
          cv.errors[k] <- rf.error
          fit.times[k] <- fit.time[3]
          pred.times[k] <- pred.time[3]
        }
      }
      print(mean(cv.errors))
      flush.console()
      specs[i,ncol(specs)] <- mean(cv.errors)
      specs[i,ncol(specs)-2] <- mean(fit.times)
      specs[i,ncol(specs)-1] <- mean(pred.times)
    }
    
    best.specs <- specs[specs[,ncol(specs)] == min(specs[,ncol(specs)], na.rm=TRUE),]
  } else {
    best.specs <- cbind(expand.grid(model="RF", sigma=c(7), lab=c(TRUE), param1=c(2.5), param2=100))
  }
  
  # Fit a model with the best specs to all the training data
  all.df <- prepareTrainingData(lab=best.specs[,3], sigma=best.specs[,2], training.road.orig.img, training.road.class.img, training.noroad.img,
                                training.road.orig.img.lab, training.noroad.img.lab)
  if (best.specs[,1] == "svm") {
    best.fit <- svm(road ~ b1 + b2 + b3 + b1g + b2g + b3g, data = all.df, gamma = best.specs[,4], cost = best.specs[,5])
  } else {
    best.fit <- randomForest(road ~ b1 + b2 + b3 + b1g + b2g + b3g, data = all.df, mtry=best.specs[,4], ntree=best.specs[,5])
  }
  
  # Return fitted model with best specs
  ret.val <- list(best.fit, best.specs)
  
  # Clean up to save memory
  rm(best.fit)
  rm(all.df)
  rm(list=c("training.road.orig.img", "training.road.class.img", "training.noroad.img", "training.road.orig.img.lab", "training.noroad.img.lab"))
  
  return(ret.val)
}



classifyPixels <- function(partition.map, gauss.ml.fit) {
  
  svm.fit <- gauss.ml.fit[[1]]
  sigma <- (gauss.ml.fit[[2]][,2])*res(partition.map)[1]
  lab <- gauss.ml.fit[[2]][,3]
  
  class.img <- partition.map
  
  # Convert RGB to Lab values if so specified by fitted model
  if (lab) {
    class.img <- labImg(class.img)
  } 
  
  # Apply gaussian filter
  gauss.img <- gaussFilter(class.img, sigma)
  
  # Prepare dat
  pixels.df <- data.frame(getValues(class.img))
  names(pixels.df) <- c("b1", "b2", "b3")
  pixelsgauss.df <- data.frame(getValues(gauss.img))
  names(pixelsgauss.df) <- c("b1g", "b2g", "b3g")
  classify.df <- cbind(pixels.df, pixelsgauss.df)
  
  # Classify
  classified <- predict(svm.fit, classify.df)
  classified <- ifelse(classified == "1", 1, 0)
  
  # Prepare output image
  output.img <- class.img[[1]]
  output.img <- setValues(output.img, classified)
  
  return(output.img)
}


