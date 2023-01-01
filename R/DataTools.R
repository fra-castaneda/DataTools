## Functions


#### DataSet Tools ####

## Ravi Varadhan - https://stat.ethz.ch/pipermail/r-help/2008-February/153708
Posdef <- function (n, ev = runif(n, 0, 10)) 
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}


Dataset_Info <- function(Model, Dataset,
                         Facet = "Default", Fill = "Default",
                         Legend = "Default", Style = "Default",
                         Theme = "Default", Colour = "Default"){

  app.data <- list()
  app.data$Model <- Model
  facet = c()
  legend =c()
  theme = ggthemes::theme_clean()
  colour = ggthemes::scale_fill_few(palette = "Medium")


  if(Style == "Stata"){
    theme = ggthemes::theme_stata() 
    colour = ggthemes::scale_fill_stata()
  }
  
  if(Style == "Excel"){
    theme = ggthemes::theme_excel_new() 
    colour = ggthemes::scale_fill_excel_new()
  }
  
  if(Style == "Calc"){
    theme = ggthemes::theme_calc() 
    colour = ggthemes::scale_fill_calc()
  }
  
  
  if (length(Model[[3]]) == 1){
    Data <- Dataset %>% dplyr::relocate(Model[[2]], Model[[3]]) 
    m <- Data[[1]]~Data[[2]]
    if (Colour == "Default"){
      fill <-colnames(Data)[2]
    } 
    if (Colour == "One"){
      fill = "#60BD68"
      
    }

  }
  

  if (length(Model[[3]]) == 3){
    if (length(Model[[3]][[2]]) == 3){
      Data <- Dataset %>% dplyr::relocate(Model[[2]]) 
      print(Data)
      m <- Data[[1]]~interaction(Data[[2]],Data[[3]],Data[[4]])
      facet <- ggplot2::facet_wrap(Data[[3]]~Data[[4]])
      fill = colnames(Data)[2]
      
    }
    Data <- Dataset %>% dplyr::relocate(Model[[2]],Model[[3]][[2]],Model[[3]][[3]]) 
    m <- Data[[1]]~interaction(Data[[2]],Data[[3]])
    facet <- ggplot2::facet_wrap(Data[[3]]~.)
    if (Colour == "Default"){
      fill <-colnames(Data)[2]
    } 
    if (Colour == "One"){
      fill = "#60BD68"
      
    }

  }

  
  if (length(Facet)>1){
    facet = Facet
  }
  
  if(length(Colour)>1){
    colour = Colour
  }
  
  if (length(Theme)>1){
    theme = Theme
  }
  
  if (length(Legend)>1){
    legend = Legend
  }

  Data[[2]] <- as.factor(Data[[2]])
  model <- aov(Model, data= Data)

  Shapiro <- shapiro.test( Data[[1]])
  Levene <- car::leveneTest(model)
  Bartlett <- bartlett.test(m)
  Stats <- Data %>% dplyr::summarise(ID = c("Global"),
                                              Mean = mean(dplyr::cur_data()[[1]]),
                                              SD = sd(dplyr::cur_data()[[1]])) %>%
    dplyr::mutate(Shapiro_p = Shapiro$p.value,
           Shapiro_W = Shapiro$statistic, 
           Bartlett_p = Bartlett$p.value,
           Bartlett_K = Bartlett$statistic,
           Levene_p = Levene[[3]][1])
  
  Shapiro_ <- Data %>%   dplyr::group_by(.[[2]]) %>%
    dplyr::do(Shapiro = shapiro.test(.[[1]]))
  Shapiro_$Shapiro <- purrr::map(Shapiro_$Shapiro,function(x) data.frame(Shapiro_p = x$p.value,
                                                                         Shapiro_W = x$statistic)) 
  Shapiro_ <- tidyr::unnest(Shapiro_, cols = c(Shapiro)) %>% dplyr::rename(ID =1)
  app.data$Shapiro_ <- Shapiro_
  Levene_ <- Data %>% dplyr::group_by(.[[2]]) %>%
    dplyr::do(Levene = car::leveneTest(model))
  Levene_$Levene <- purrr::map(Levene_$Levene,function(x) data.frame(Levene_p = x[[3]])) 
  Levene_ <- tidyr::unnest(Levene_, cols = c(Levene)) %>% dplyr::rename(ID =1) %>% tidyr::drop_na()
  
  app.data$Levene_ <- Levene_
  Bartlett_ <- Data %>% dplyr::group_by(.[[2]]) %>%
    dplyr::do(Bartlett= bartlett.test(m))
  Bartlett_$Bartlett <- purrr::map(Bartlett_$Bartlett,function(x) data.frame(Bartlett_p = x$p.value,
                                                                             Bartlett_K = x$statistic)) 
  Bartlett_ <- tidyr::unnest(Bartlett_, cols = c(Bartlett)) %>% dplyr::rename(ID =1)
  app.data$Bartlett_ <- Bartlett_
  Group_Stats <- Data  %>% dplyr::group_by(.[[2]]) %>% dplyr::summarise(Mean = mean(dplyr::cur_data()[[1]]),
                                                                        SD = sd(dplyr::cur_data()[[1]])) %>% dplyr::rename(ID =1)

  Group_Stats <- merge(Group_Stats, Shapiro_) 
  Group_Stats <- merge(Group_Stats, Bartlett_) 
  Group_Stats <- merge(Group_Stats, Levene_)
  app.data$Stats <- rbind(Stats, Group_Stats) %>% 
    dplyr::mutate(Distribution = ifelse(Shapiro_p < 0.05, "Non-Normal", "Normal"), 
           Variance = ifelse(Bartlett_p > 0.05, "Homoscedastic", "Heteroscedastic"))
  
  
   par(mfrow=c(1,2))
   hist(Data[[1]], prob=T, main="Density Plot", xlab=Model[[2]])
   curve(dnorm(x, mean=app.data$Stats$Mean[1], sd=app.data$Stats$SD[1]), col="darkblue", lwd=2, add=TRUE)
   qqnorm(Data[[1]])
   qqline(Data[[1]])
   app.data$Flextable <- flextable::flextable(app.data$Stats %>% dplyr::select(-Shapiro_W,-Bartlett_K, -Levene_p) %>% 
                                     dplyr::mutate(Mean = round(Mean, 1),
                                            SD = round(SD, 1))) %>% flextable::bold(.,1) %>%
     flextable::set_formatter(Shapiro_p = function(x) {
       formatC(x, format = "g", digits = 1)
     },
     Bartlett_p = function(x) {
       formatC(x, format = "g", digits = 1)
     })
   print(app.data$Flextable )
    app.data$BoxPlot <- ggpubr::ggboxplot(Data, x= colnames(Data)[2], 
                                  y = colnames(Data)[1],
                                  fill = fill)+
      facet + theme + colour + legend +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))

   print(app.data$BoxPlot)
  
  return(app.data)
}



Create_Dataset <- function(Tra ,Treatments,
                           Value, Values, 
                           VarA, VariablesA,
                           VarB, VariablesB,
                           Replicates = 3, Seed = 1200,
                           Skewness = -1, Kurtosis = 3, 
                           Normality = F, Sigma = NULL,
                           Report = F){


  set.seed(Seed)
  app.data <- list()
  N_tra <- length(Treatments)
  
  if (is.null(Seed)==T){
    set.seed(seed = NULL)
  }

  
  if ( is.null(Sigma)== T){
    pdmat <- Posdef(n=N_tra, ev=1:N_tra)  
    Sigma <- clusterGeneration::genPositiveDefMat(
      dim = N_tra,
      covMethod = "eigen", 
      eigenvalue = eigen(pdmat)$val)
    Sigma <- Sigma$Sigma
  }
  
  
  if(Normality == T){
    Value_list <- Values
    if (length(Values)==2){
      Value_list <- stats::rnorm(N_tra, mean = Values[1], sd = Values[2])
    }

      Matrix <- MASS::mvrnorm(Replicates, Value_list, Sigma)  

  }
  
  if (Normality == F ){
    Value_list = Values
    if(length(Values)==1){
      Value_list <- mvtnorm::rmvnorm(length(Treatments), mean = Values[1])
    }
    
    Matrix <- semTools::mvrnonnorm(Replicates, Value_list, Sigma, Skewness,Kurtosis)  

  }
  
  colnames(Matrix) <-Treatments  
  app.data$Data <- as.data.frame(Matrix)
  app.data$Data <- app.data$Data %>% tidyr::gather({{Tra}}, {{Value}}) %>% 
    dplyr::mutate({{VarA}} := VariablesA, {{VarB}} := VariablesB) %>% 
    dplyr::select({{Tra}},{{Value}} ,{{VarA}}, {{VarB}})
  app.data$Data[[1]] <- factor(app.data$Data[[1]], levels=Treatments)
  
  if (Report == T){
    model <- as.formula(paste0(colnames(app.data$Data)[2],"~",colnames(app.data$Data)[1]))
    app.data$Stats <- Dataset_Info(model, Dataset =app.data$Data)
    
  }


  return(app.data)
  
}


#
#### Power Analysis ####
aov_info <- function(model) {
  app.data<-list()
  app.data$model <- as.formula(model)
  app.data$groups <- length(unique(model$model[[2]]))
  model.ss <- summary(model)
  if (length(model$terms[[3]]) > 1) {
    app.data$effect <- effectsize::eta_squared(model, partial =F)$Eta2
    
  }
  if (length(model$terms[[3]]) == 1) {
    app.data$effect <- effectsize::eta_squared(model, partial =F)$Eta2
    # app.data$effect<- data.frame(model.ss[[1]])[, 2, drop=FALSE]$Sum.Sq[1] / 
    #   (data.frame(model.ss[[1]])[, 2, drop=FALSE]$Sum.Sq[1] + 
    #      data.frame(model.ss[[1]])[, 2, drop=FALSE]$Sum.Sq[2] ) 
    app.data$n_group <- round(pwr::pwr.anova.test(k =app.data$groups , f =app.data$effect , 
                                             sig.level=0.05 , power =0.80 )$n)
    app.data$n_total <- round(pwr::pwr.anova.test(k =app.data$groups , f =app.data$effect , 
                                             sig.level=0.05 , power =0.80 )$n)*app.data$groups
    
  }
  return(app.data)
  
}

#



