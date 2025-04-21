


 # Height percentiles

  h99 <-function(x){ # 0 < p < 1
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.99][1]
 }
 
 h95 <-function(x){ # 0 < p < 1
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.95][1]
 }
 
 h90 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.9][1]
 }
 
 h80 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.8][1]
 }
 
 h70 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.7][1]
 }
 
 h60 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.6][1]
 }
 
 h50 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.5][1]
 }
 
 h40 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.4][1]
 }
 
 h30 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.3][1]
 }
 
 h20 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x);  x[cs>.2][1]
 }
 
 h10 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); x[cs>.1][1]
 }
 
 h5 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); x[cs>.05][1]
 }
 
 
 # Density percentiles
 
 p99 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.99])+1)/length(x)
 }
 
 p95 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.95])+1)/length(x)
 }
 
 p90 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.9])+1)/length(x)
 }
 
 p80 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.8])+1)/length(x)
 }
 
 p70 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.7])+1)/length(x)
 }
 
 p60 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.6])+1)/length(x)
 }
 
 p50 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.5])+1)/length(x)
 }
 
 p40 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.4])+1)/length(x)
 }
 
 p30 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.3])+1)/length(x)
 }
 
 p20 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.2])+1)/length(x)
 }
 
 p10 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.1])+1)/length(x)
 }
 
 p5 <-function(x){ 
   x<-sort(x);  cs<-cumsum(x)/sum(x); (length(x[cs<=.05])+1)/length(x)
 }


rgb_metrics <- function(z, R, G, B) {
   
  # Spectral normalization
  s <- R + G + B
  R<-R/s
  G<-G/s
  B<-B/s
  hmax <- max(z)
  
   list(
     
     # Relative height percentiles, normalized by hmax
     rel_hmean = mean(z)/hmax, 
     rel_hstd = sd(z)/hmax,     
     rel_h95 = h95(z)/hmax,
     rel_h90 = h90(z)/hmax,
     rel_h80 = h80(z)/hmax,
     rel_h70 = h70(z)/hmax,
     rel_h60 = h60(z)/hmax,
     rel_h50 = h50(z)/hmax,
     rel_h40 = h40(z)/hmax,
     rel_h30 = h30(z)/hmax,
     rel_h20 = h20(z)/hmax,
     rel_h10 = h10(z)/hmax,
     rel_h5  = h5(z)/hmax,
     
     # Red percentiles
     RedMean = mean(R), 
     RedStd = sd(R),     
     Red95 = h95(R),
     Red90 = h90(R),
     Red80 = h80(R),
     Red70 = h70(R),
     Red60 = h60(R),
     Red50 = h50(R),
     Red40 = h40(R),
     Red30 = h30(R),
     Red20 = h20(R),
     Red10 = h10(R),
     Red5  = h5(R),
     
     # Green percentiles
     GreenMean = mean(G), 
     GreenStd = sd(G),     
     Green95 = h95(G),
     Green90 = h90(G),
     Green80 = h80(G),
     Green70 = h70(G),
     Green60 = h60(G),
     Green50 = h50(G),
     Green40 = h40(G),
     Green30 = h30(G),
     Green20 = h20(G),
     Green10 = h10(G),
     Green5  = h5(G),
     
     # Blue percentiles
     BlueMean = mean(B), 
     BlueStd = sd(B),     
     Blue95 = h95(B),
     Blue90 = h90(B),
     Blue80 = h80(B),
     Blue70 = h70(B),
     Blue60 = h60(B),
     Blue50 = h50(B),
     Blue40 = h40(B),
     Blue30 = h30(B),
     Blue20 = h20(B),
     Blue10 = h10(B),
     Blue5  = h5(B)
     )
 }

# A function for finding the largest observation in a vector
is.largest <- function(x) as.integer(seq_along(x) == which.max(x)) 

# Kappa coefficient: 1 = perfect classification, 0 = random guess, <0 = worse than random guess
kappak<-function(t){
  if(ncol(t)==nrow(t)){
    diagsum<-sum(diag(nrow(t))*t)
    (pra<-diagsum/sum(t))     #oikeinluokitusprosentti
    peet<-(colSums(t)/sum(t))*(rowSums(t)/sum(t)) #Kappa
    pre<-sum(peet)
    kappa<-(pra-pre)/(1-pre)
  } else {kappa<-NA}
  kappa
}

oa <- function(mat) sum(diag(mat))/sum(mat)

PickNewSolution<-function(s,kk,n_iterr,prednum,colmin,colmax){
  
  # s = current solution, k=number of iterations
  # Initially change 2/3 of the variable
  # When k/n >=.8, change only one variable
  
  s<-sort(s)
  keepmax<-prednum-1 # Maximum number of predictors to keep = prednum-1
  keepmin<-round(1/3*prednum) # Minimum number of predictors to keep = 1/3*length
  
  kperc<-kk/n_iterr # Iteration progress
  
  # Current number of predictors to keep
  keepcount<-min(length(s)-1,round((keepmax-keepmin)/.8*kperc+keepmin)); keepcount
  
  idxx<-colmin:colmax # List of variables to select from
  keep<-sort(sample(s,keepcount,replace=F)) # List of untouchable variables
  ssel<-idxx[! idxx %in% s] # List of selectable variables - current sample not allowed
  newvars<-sample(ssel,prednum-keepcount,replace=F) # Sampling from selectables
  sol<-sort(c(newvars,keep)) # Merge
  sol # Return new variable list
}

# yaisel-function fits a KNN model and returns the weighted mean RMSE




ldasel<-function(x,yvar,d,opt="kappa",pri=NA){ #Function that is minimized; x=list of decimals related to column id's
  
  x<-unique(x) #remove possible duplicates
  x<-x[!is.na(x)]
  
  xsel<-as.data.frame(d[,x]) # List X-variables
  
  foo<-paste(yvar,"~")
  for(j in 1:length(xsel)) foo<-paste(foo,"+",names(xsel)[j]) ###################################### N A M E S ###################!!!!!!!!!!!!!!!!
  form<-as.formula(foo)
  
  
  if(!is.na(pri[1])){
    model<-tryCatch(lda(form,data = d,CV=F,prior=pri),  ########### UPDATE DATA FRAME NAME AND MODEL TYPE
                    error=function(e) {print(paste(e$message,": rows",paste(x,collapse=" "),
                                                   ", formula",paste(form,collapse="")))})
    
  } else{
    model<-tryCatch(lda(form,data = d,CV=F),  ########### UPDATE DATA FRAME NAME AND MODEL TYPE
                    error=function(e) {print(paste(e$message,": rows",paste(x,collapse=" "),
                                                   ", formula",paste(form,collapse="")))})
  }
  
  
  
  if(class(model)=="list"){ #No error from calculation ###################UPDATE MODEL TYPE...
    #CHANGES IF CV=T
    
    pred.lda <- model$class;
    errmat.lda <- table(d[,yvar], pred.lda) #################### Data frame name
    k<-kappak(errmat.lda)
  }  else if (class(model)=="lda"){ 
    
    #Get predicted values and save them into the data frame
    pred<-predict(model,d) 
    #pred<-model$class #If CV=T, use pred<-model$class
    
    errmat.lda <- table(d[,yvar], pred$class) #################### Data frame name
    k<-kappak(errmat.lda)
    
  }  else { #Some error occurred
    k<--99999 
  }  
  
  
  if(opt=="kappa"){
    out<-1-k
  }else if (opt=="OA"){
    out<- 1 - (sum(diag(errmat.lda))/sum(errmat.lda))
  }else {
    uacc <- errmat.lda[opt,opt] / sum(errmat.lda[,opt]) # User's accuracy
    pacc <- errmat.lda[opt,opt] / sum(errmat.lda[opt,]) # Producer's accuracy
    # c(uacc,pacc)                                    
    out<- -(uacc+pacc)
  }
  return(out)
}



###############################################
### Training an lda model with linked trees ###
###############################################


trainLDA<-function(d,prednum=5,colmin=17,colmax=87,equalPriors=TRUE, yvar="Species",
                   opt="kappa"){
  
  #  d=treesm40$linked;colmin=17;colmax=87;equalPriors=1;prednum=5;yvar<-"Species"
  #   opt="kappa";loo=F
  
  require(MASS)
  
  #names(d)
  #table(d[,yvar])
  
  d<-as.data.frame(d)
  
  t_ini<-.2 # Initial temperature at simulated annealing
  n_iter<-10000 # How many iterations in optimization; larger is better but takes longer
  
  #Extract response variables into their own data frame "ytrain" 
  
  ytrain<-as.data.frame(d[,yvar]) 
  names(ytrain)<-yvar # Rename columns
  
  
  # Convert response to factor if it is not a factor yet
  
  d[,yvar]<-as.factor(d[,yvar])
  
  
  # Set the vector of prior probabilities to be used in lda, or NA for the default proportional priors
  
  resnum<-length(levels(d[,yvar]))  
  
  if(equalPriors==TRUE){
    pri<-rep(1,resnum)/resnum  
  }else{
    pri<-NA
  }
  
  # Normalization of predictors to standard unit interval
  
  for(i in colmin:colmax) d[,i]<-(d[,i]-mean(d[,i]))/sd(d[,i])
  
  #=============================================================
  # Variable selection by simulated annealing
  #=============================================================
  
  t<-t_ini # Initial temperature
  s<-sample(colmin:colmax,prednum,replace=F) # Initial variables
  
  e<-ldasel(s,yvar,d,opt=opt,pri=pri) # Run lda  for initial variables
  
  ebest<-e # Save initial mean rmse
  sbest<-s # Save initial variable combination
  
  k<-0     # Initialize iteration counter
  
  while(k<n_iter){ 
    
    # sdot  = new experimental solution
    # s     = current solution to be improved,
    # sbest = best solution ever found
    
    sdot<-PickNewSolution(s,k,n_iter,prednum,colmin,colmax) # New candidate variables
    edot<-ldasel(sdot,yvar,d,opt=opt,pri=pri) # KNN result for the new candidate variables
    
    # Implement the simulated annealing algorithm
    
    if(exp((-(edot-e))/t) > runif(1)){
      e<-edot
      s<-sdot
    }
    if(edot<ebest){
      ebest<-edot
      sbest<-sdot
    }
    t<-max(0.0000001,-.2/.8*k/n_iter+.2) #Cool temperature
    k<-k+1
    
  }
  
  
  
  (xsel<-names(d)[sbest]) #List of selected predictor variable names
  1-ebest           # Kappa coefficient with the
  
  
  #Generate lda function call formula automatically
  
  foo<-paste(yvar,"~")
  for(j in 1:length(xsel)) foo<-paste(foo,"+",xsel[j])
  (form<-as.formula(foo))
  
  
  if(!is.na(pri[1])){
    modeloo<-lda(form,data=d,prior=pri,CV=T) 
    model<-lda(form,data=d,prior=pri) 
    
  } else{
    modeloo<-lda(form,data=d,CV=T) 
    model<-lda(form,data=d) 
  }
  
  # coef(model) #Model coefficients that are used to calculate discriminant scores
  
  #Get predicted values and save them into the data frame
  
  pred<-predict(model,d)  
  d$pred_species<-pred$class
  
  # Accuracy assessment  
  
  (errmat<-table(d[,yvar], d$pred_species)) #Calculate error matrix
  (kappa<-kappak(errmat)) #Calculate kappa coefficient
  
  
  # Loo prediction and accuracy assessment
  
  looClass<-modeloo$class #If CV=T, use pred<-model$class  
  (errmat.loo<-table(d[,yvar], looClass)) #Calculate error matrix
  (kappa.loo <-kappak(errmat.loo)) #Calculate kappa coefficient
  
  
  out<-list(lda=model,errmat=errmat,kappa=kappa,pred=pred,xsel=xsel,
            errmat.loo=errmat.loo,kappa.loo=kappa.loo,looClass=looClass)
  
  return(out)
}



discrPlot<-function(pred, legpos="bottomleft"){ # Pred = "pred" object from the trainLDA function
  
  
  # Most of stuff below is just setting graphical parameters for plotting
  par(mai=c(1.2,1.1,.5,.5)) 
  
  lev<-levels(pred$class)
  
  colors<-c("black","red","blue","green","orange","cyan")
  colors<-colors[1:length(lev)]
  
  #### Discriminant plot ###
  
  if(ncol(pred$x)>1){ # For 2-class problem there will be only one discriminant
    
    scores<-as.data.frame(pred$x[,1:2]) #Discriminant score,s for the first two dimensions
    tail(scores) #These scores are plotted below in the discriminant plot; LD1 and LD2 are the axes
    
    plot(scores$LD1,scores$LD2,pch=20,cex=.8, col="black",xlab="Discriminant 1",ylab="Discriminant 2",
         xlim=c(min(scores$LD1),max(scores$LD1)),ylim=c(min(scores$LD2),max(scores$LD2)),
         las=1,cex.lab=1.4,cex.axis=1.2)
    
    for(m in 1:length(lev)){
      
      #Color points
      points(scores$LD1[pred$class==lev[m]],scores$LD2[pred$class==lev[m]],pch=20,cex=.8,col=colors[m])  
      
      #Add group means
      points(mean(scores$LD1[pred$class==lev[m]]),mean(scores$LD2[pred$class==lev[m]]),col=colors[m],pch=18,cex=1.8)
    }
    
    legend(legpos,lev,pch=rep(20,length(lev)),col=colors,cex=1.05)
    
  }else{ # 2 classes
    
    scores<-as.data.frame(pred$x)
    
    plot(jitter(rep(-1,sum(pred$class==lev[1]))),scores$LD1[pred$class==lev[1]],
         pch=20,cex=.8,col=colors[1],xlim=c(-1.5,1.5), xlab="",ylim=c(min(scores$LD1), max(scores$LD1)),ylab="LD1")
    
    # Color points
    points(jitter(rep(1,sum(pred$class==lev[2]))),scores$LD1[pred$class==lev[2]],
           pch=20,cex=.8,col=colors[2])  
    
    legend("topright",lev,pch=rep(20,length(lev)),col=colors,cex=1.05)
    
  }
  
}

discrPdf<-function(pred,pdfName="discrplot.pdf"){
  
  # Most of stuff below is just setting graphical parameters for plotting
  par(mai=c(1.2,1.1,.5,.5)) 
  
  lev<-levels(pred$class)
  
  colors<-c("black","red","blue","green","orange","cyan")
  colors<-colors[1:length(lev)]
  
  pdf(pdfName)
  
  #### Discriminant plot ###
  
  if(ncol(pred$x)>1){ # For 2-class problem there will be only one discriminant
    
    scores<-as.data.frame(pred$x[,1:2]) #Discriminant score,s for the first two dimensions
    tail(scores) #These scores are plotted below in the discriminant plot; LD1 and LD2 are the axes
    
    plot(scores$LD1,scores$LD2,pch=20,cex=.8, col="black",xlab="Discriminant 1",ylab="Discriminant 2",
         xlim=c(min(scores$LD1),max(scores$LD1)),ylim=c(min(scores$LD2),max(scores$LD2)),
         las=1,cex.lab=1.4,cex.axis=1.2)
    
    for(m in 1:length(lev)){
      
      #Color points
      points(scores$LD1[pred$class==lev[m]],scores$LD2[pred$class==lev[m]],pch=20,cex=.8,col=colors[m])  
      
      #Add group means
      points(mean(scores$LD1[pred$class==lev[m]]),mean(scores$LD2[pred$class==lev[m]]),col=colors[m],pch=18,cex=1.8)
    }
    
    legend("topright",lev,pch=rep(20,length(lev)),col=colors,cex=1.05)
    
  }else{ # 2 classes
    
    scores<-as.data.frame(pred$x)
    
    plot(jitter(rep(-1,sum(pred$class==lev[1]))),scores$LD1[pred$class==lev[1]],
         pch=20,cex=.8,col=colors[1],xlim=c(-1.5,1.5), xlab="",ylim=c(min(scores$LD1), max(scores$LD1)),ylab="LD1")
    
    # Color points
    points(jitter(rep(1,sum(pred$class==lev[2]))),scores$LD1[pred$class==lev[2]],
           pch=20,cex=.8,col=colors[2])  
    
    legend("topright",lev,pch=rep(20,length(lev)),col=colors,cex=1.05)
    
  }
  
  dev.off()
}



predictLDA<-function(nd, ot, model, xsel, yvar=NA, ...){
  
  
  # nd: new data for which you predict
  # ot: original training data, required for normalization
  # model: lda model to predict with 
  # xsel: list of lda model predictors, required for normalization
  # yvar: known response value for accuracy assessment, if available
  
  require(MASS)
  
  # Convert from spatial to normal data frame
  
  nd<-as.data.frame(nd)
  ot<-as.data.frame(ot)
  
  # Normalization of predictors to standard unit interval using the original training data
  
  for(i in 1:length(xsel)) nd[,xsel[i]]<-(nd[,xsel[i]]-mean(ot[,xsel[i]]))/sd(ot[,xsel[i]])
  
  # Prediction
  
  pred<-predict(model,nd) 
  nd$predSpecies<-pred$class
  
  
  if(is.na(yvar)){
    
    # No field trees: yvar not defined, no accuracy assessment
    
    errmat.lda<-NA
    kappa<-NA
    
    
  }else{
    
    # Predict for linked trees and assess accuracy
    
    #Conversion of response to factor
    
    nd[,yvar]<-as.factor(nd[,yvar])
    
    spec<-as.data.frame(nd)[,yvar]
    
    (errmat.lda<-table(spec, nd$predSpecies)) #Calculate error matrix
    (kappa<-kappak(errmat.lda)) #Calculate kappa coefficient
    
  }
  
  out<-list(pred=nd$predSpecies, errmat=errmat.lda,kappa=kappa)
  
  return(out)
}

 
 