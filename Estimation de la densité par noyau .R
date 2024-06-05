########################################### EXERCICE 1 ###########################################

# QUESTION 1 ----
n = 50

    # a/ GENERATION D'UN ECHANTILLON DE TAILLE 50 CONSTITUE DE REALISATIONS D'UNE GAUSSIENNE N(-1,5;2) ----
    
    Echant1 = rnorm(n,mean = -1.5,sd = sqrt(2))
    Echant1
    
    # b/ GENERATION D'UN ECHANTILLON DE TAILLE 50 CONSTITUE DE REALISATIONS D'UNE LOI UNIFORME ----
    
    Echant2 = runif(n,min = 0,max = 1)
    Echant2
    
    # c/ CONCATENATION DES DEUX VECTEURS ---- 
    
    X = c(Echant1,Echant2)
    X


# QUESTION 2 ----
   
    #install.packages("ggplot2")
    #install.packages("KernSmooth")
    library('ggplot2')
    library('KernSmooth')

    # a/ CONSTRUCTION DE L'HISTOGRAMME ----
    
    hist_plot <- ggplot(data = data.frame(X), aes(x = X)) +
      
      geom_histogram(binwidth = 0.2, fill = "deepskyblue4", color = "black", alpha = 0.7) + 
      
      theme(plot.title = element_text(hjust = 0.5)) +
      
      labs(title = "Histogramme représentant les données du vecteur X", x = "Valeurs de X", y = "Fréquence")
    
    print(hist_plot) # AFFICHAGE DE L'HISTOGRAMME

    # b/ CONSTRUCTION DE L'ESTIMATEUR A NOYAU ----

    # NOUS UTILISONS LE NOYAU GAUSSIEN ET LE NOYAU RECTANGULAIRE POUR ESTIMER LA DENSITE
    
    # install.packages('np')
    library(np)
    
    # LA FONCTION `npudensbw` PERMETS D'ESTIMER LE h OPTIMAL EN UTILISANT LA METHODE DE VALIDATION CROISEE
    bw_optimal <- npudensbw(~X)
    bw_optimal  # on obtient h = 0.1634322
    
    densite_gaussien_optimale <- density(X, bw = 0.1634322 , kernel = "gaussian")        # noyau gaussien
    densite_rectangulaire_optimale <- density(X, bw = 0.1634322, kernel = "rectangular") # noyau rectangulaire
    
    plot(densite_gaussien_optimale, col = "red", main = "Estimation de la densité de X")
    
    lines(densite_rectangulaire_optimale, col = "blue")

    
    # c/ VALIDATION CROISEE ----
    
# 1IERE METHODE
    
    # bw = "ucv" REGLE DE LA VALIDATION CROISEE
    
    densite_gaussien <- density(X, bw = "ucv" , kernel = "gaussian")
    densite_rectangulaire <- density(X, bw = "ucv", kernel = "rectangular")
    
    plot(densite_gaussien, col = "red", main = "Estimation de la densité de X")
    
    lines(densite_rectangulaire, col = "blue")
    
# 2IEME METHODE
    
    # NOYAU GAUSSIEN
    
    gaussien_kernel <- function(u) {
      
      (1/sqrt(2*pi)) * exp(-u^2/2)
      
    }
    
    
    # ESTIMATEUR DE DENSITE A NOYAU GAUSSIEN 
    
    estimateur_densite_gaussien <- function(x, X, h) {
      
      n <- length(X)
      
      densite_estimee <- sapply(x, function(x_val) {
        
        sum(gaussien_kernel((x_val - X) / h)) / (n * h)
        
      })
      
      return(densite_estimee)
      
    }
    
    
    # GENERATION DES DONNEES
    
    set.seed(123)
    
    X <- c(rnorm(50, mean=-1.5, sd=2), runif(50, min=0, max=1))
    
    
    # METHODE DE LA VALIDATION CROISEE AVEC INTEGRATION
    
    optimal_h_integrale <- function(X, h_valeurs) {
      
      x_min <- min(X)
      
      x_max <- max(X)
      
      h_optimal <- h_valeurs[1]
      
      min_cv <- Inf
      
      
      
      for (h in h_valeurs) {
        
        
        
        # 1ER TERME : INTEGRAL DE L'ESTIMATION DE DENSITE AU CARRE
        
        densite_carree <- function(x) {
          
          densite_estimee <- estimateur_densite_gaussien(x, X, h)
          
          return(densite_estimee^2)
          
        }
        
        
        
        integrale_term_1 <- integrate(densite_carree, lower = x_min, upper = x_max)$value
        
        
        
        # 2E TERME : MOYENNE DES DENSITES ESTIMEES EN OMETTANT UN POINT
        
        integrale_term_2 <- mean(sapply(1:length(X), function(i) {
          
          densite_omitted <- estimateur_densite_gaussien(X[i], X[-i], h)
          
          return(densite_omitted)
          
        }))
        
        
        
        # CALCUL DE CV
        
        cv <- integrale_term_1 - 2 * integrale_term_2
        
        
        
        # MISE A JOUR DE h_optimal SI BESOIN
        
        if (cv < min_cv) {
          
          h_optimal <- h
          
          min_cv <- cv
          
        }
        
      }
      
      return(h_optimal)
      
    }
    
    
    h_valeurs <- seq(0.1, 2, by=0.1) 
    
    h_opt <- optimal_h_integrale(X, h_valeurs)
    
    
    # AFICHAGE DE h_optimal 
    
    cat("Le h optimal trouvé est :", h_opt, "\n")
    
    
    
########################################### EXERCICE 2 ###########################################
    
    
# install.packages("writexl")
# install.packages("moments")
library(dplyr)
library(nycflights13)
library(writexl)
library(openxlsx)
library(moments)   
 
    
# STATISTIQUES DESCRIPTIVES ---- 
    
    # CHARGEMENT DES BIBLIOTHEQUES NECESSAIRES POUR L'EXECUTION DE CERTAINES FONCTIONS 
    
    library(nycflights13)
    library(ggplot2)
    
    # a/ DONNEES ----
    
    Vol = data("flights")
    
    write_xlsx(flights, "vol.xlsx") # Lecture du fichier contenant le jeu de données Flights 
    
    arr_delay_na_omit <- na.omit(flights$arr_delay) # On enlève les données manquantes pour la variable arr_delay 
    
    mean_delay <- mean(arr_delay_na_omit) 
    
    median_delay <- median(arr_delay_na_omit)
    
    sd_delay <- sd(arr_delay_na_omit)
    
    skewness_delay <- moments::skewness(arr_delay_na_omit)
    
    kurtosis_delay <- moments::kurtosis(arr_delay_na_omit)
    
    
    cat("Mean Delay:", mean_delay, "\n",
        
        "Median Delay:", median_delay, "\n",
        
        "Standard Deviation of Delay:", sd_delay, "\n",
        
        "Skewness of Delay:", skewness_delay, "\n",
        
        "Kurtosis of Delay:", kurtosis_delay, "\n")
    
    # b/ GRAPHIQUES ----
    
     # HISTOGRAMME DES DELAIS D'ARRIVE 
    
    ggplot(flights, aes(x = arr_delay)) +
      
      geom_histogram(bins = 50, fill="skyblue", color="black") +
      
      labs(title = "Histogramme des délais d'arrivée en minutes", 
           
           x = "Délai d'arrivée", 
           
           y = "Fréquence")
    
     # REPRESENTATION GRAPHIQUE DE LA DENSITE DE NOTRE VARIABLE ETUDIEE 
    
    ggplot(flights, aes(x=arr_delay)) + 
      
      geom_histogram(aes(y = ..density..), binwidth=5, fill="skyblue", alpha=0.5) + 
      
      geom_density(color="blue") +
      
      labs(title="Histogramme de la densité des délais d'arrivée",
           
           x="Délai d'arrivée (minutes)", y="Densité") + 
      
      theme_minimal()
    
    # QQ-plot
    
    qqnorm(arr_delay_na_omit, main="QQ-plot of Arrival Delays")
    
    qqline(arr_delay_na_omit, col="blue")  
    
# ESTIMATION DE LA DENSITE DE NOTRE VARIABLE AVEC LE NOYAU GAUSSIEN ----
    
    density_estimation <- density(arr_delay_na_omit, kernel="gaussian", bw="nrd0") 
    
    plot(density_estimation, main="Représentation de la densité estimée des délais d'arrivée (NOYAU GAUSSIEN)", 
         
         xlab="Délai d'arrivée (minutes)", ylab="Densité", col="blue", lwd=2)
    
# ESTIMATION DE LA DENSITE DE NOTRE VARIABLE AVEC LE NOYAU RECTANGULAIRE ----

    density_estimation <- density(arr_delay_na_omit, kernel="rectangular", bw="nrd0") 
    
    plot(density_estimation, main="Représentation de la densité estimée des délais d'arrivée (NOYAU RECTANGULAIRE)", 
         
         xlab="Délai d'arrivée (minutes)", ylab="Densité", col="blue", lwd=2)

# ESTIMATION DE LA LARGEUR DE LA FENETRE AVEC VALIDATION CROISEE ----

bw_cv <- bw.nrd0(arr_delay_na_omit)
bw_cv

density_estimation_cv <- density(arr_delay_na_omit, kernel="gaussian", bw=bw_cv) 

plot(density_estimation_cv, main="Représentation de la densité estimée des délais d'arrivée (VALIDATION CROISEE)", 
     
     xlab="Délai d'arrivée (minutes)", ylab="Densité", col="green", lwd=2)