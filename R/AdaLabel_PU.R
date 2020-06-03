#train.X = stats::model.matrix(~.*.-1, as.data.frame(X)) # add 2-way interaction
#train_data = cbind(train.X, X^2)
#use: pred_AdaLabel=AdaLabel_PU(train_data=train_data,Label.obs=Label.obs,valid.id=valid.id,Sample_use_time=30,l.rate=1,qq=0.1)

AdaLabel_PU=function(train_data=train_data,Label.obs=Label.obs,valid.id=valid.id,Sample_use_time=30,l.rate=1,qq=0.1){

  N=dim(train_data)[1]
  Label=Label.obs

  train.X=train_data
  
  pred.y0 = Label.obs
  delta.p = array(Inf, N)
  PP = NULL
  invalid.id=seq(N)[-valid.id]
  prob_choosen=rep(1,length(invalid.id))
  names(prob_choosen)=invalid.id
  change_propor=c(0,0,0,0,0)
  
  
  for ( i in 1:10000 ) {
    # Randomly select the same amount of subjects to train with valid samples
    sample.id = sample(invalid.id, length(valid.id), replace = T ,prob = prob_choosen)
    sample.id = unique(sample.id)
    #Used_time controls how many time each sample are selected
    prob_choosen[as.character(sample.id)]=prob_choosen[as.character(sample.id)]-(1/Sample_use_time)
    
    # train
    fit.pi = cv.glmnet(train.X[c(valid.id, sample.id),], Label.obs[c(valid.id, sample.id)], family = "binomial")
    pred.y = predict(fit.pi, newx = train.X, s = "lambda.min", type = 'response')
    #valid id in Label is true
      #qq = ifelse(unique(Label[valid.id]) == 0, 0.9, 0.1)
    cutoff = quantile(pred.y[valid.id], qq)
    
    # rescale
    map.pred.y = pred.y - cutoff
    map.pred.y[map.pred.y>0] = map.pred.y[map.pred.y>0]/max(map.pred.y[map.pred.y>0])
    map.pred.y[map.pred.y<0] = map.pred.y[map.pred.y<0]/abs(min(map.pred.y[map.pred.y<0]))
    pred.y = 1/(1+exp(-10*(map.pred.y)))
    
    # record something
    delta.p[sample.id] = pred.y[sample.id] - pred.y0[sample.id]
    # if there need learning rate
    #l.rate controls chage rate from old to new
    pred.y0[sample.id] = pred.y[sample.id]*l.rate + (1-l.rate)*pred.y0[sample.id]
    # shuffle
    Label.sample.id.old=Label.obs[sample.id]
    Label.obs[sample.id] = rbinom(length(sample.id), 1, pred.y0[sample.id])
    Label.obs[valid.id] = 1
    Label.sample.id.new=Label.obs[sample.id]
    
    #print(sum(Label.sample.id.old==Label.sample.id.new)/length(sample.id))
    
    #prob_choosen>=0
    prob_choosen[which(prob_choosen<=0)]=0
    #if 90% of prob_choosen<=0.01 stop
    if(sum(prob_choosen<=0.01)>(0.9*length(prob_choosen))){
      break
    }
    change_propor=c(change_propor,sum(Label.sample.id.old==Label.sample.id.new)/length(sample.id))
    #converge
    if(mean(change_propor[(i+1):(i+5)])>0.9)
    {
      break
    }
  }
  
  # overall model, use all data which is given accurate labels from previous steps
  fit.pi = cv.glmnet(train.X, Label.obs, family = "binomial")
  pred.y = predict(fit.pi, newx = train.X, s = "lambda.min", type = 'response')
  
  # if (length(unique(Label[valid.id])) > 1) {
  #   valid.roc = roc(Label[valid.id], pred.y[valid.id], direction = "<", levels = c("0", "1"))
  #   cutoff = valid.roc$thresholds[which.min((1-valid.roc$sensitivities)^2+(1-valid.roc$specificities)^2)]
  # } else {
  #   qq = ifelse(unique(Label[valid.id]) == 0, 0.9, 0.1)
      cutoff = quantile(pred.y[valid.id], qq)
  # }
  
  #pred.coef1=predict(fit.pi, newx = train.X,s = "lambda.min", type = "coefficients")
  pred.coef1=coef(fit.pi, s = "lambda.min")
  

  #cutoff=cutoff
  
  return(list(pred.y=pred.y,cutoff=cutoff,pred.coef1=pred.coef1))

}



