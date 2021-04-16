# Process and analyze DATA collected for the paper : 
# Sources of light density microplastic related to two agricultural practices: the use of compost and plastic mulch
# PLEASE CHANGE THE WORKING DIRECTORY (WD) BEFORE RUNNING THE SCRIPT.
# For any issue or question, please contact Nicolas Beriot at nicolas.beriot@wur.nl

# Packages ####
  if(!require(readxl)){install.packages("readxl")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(tidyverse)){install.packages("tidyverse")} 
  if(!require(ggpubr)){install.packages("ggpubr")} 
  if(!require(plyr)){install.packages("plyr")}   
  if(!require(multcompView)){install.packages("multcompView")} 
  if(!require(pals)){install.packages("pals")}
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}

#Loading data ####
  rm(list=ls()) # cleaning console
  graphics.off() # cleaning plots
  
  wd="C:/Users/berio001/OneDrive - WageningenUR/PhD/Field Assessment/Benjamin/Submission" #C:/Users/Directory# Write the directory wich contains SAS_results.xlsx and Raw_data_Beriot.xlsx
  setwd(wd)

 MPs <- read.csv("MPs_results.csv", sep=",")
 
# Selecting data####
  #Remove all particles < 4px
    MPs=subset(subset(MPs,Area>=4))

  # New factor catagory
    MPs$Treat_Layer=paste(MPs$Treat, MPs$Layer, sep = "_")
    
# Setting scale####
  # Convert the results from ImageJ in px into mm with the scale : 115.3 px / mm
    MPs$Area=(MPs$Area/(115.13^2))  # mm2
    MPs$Major=(MPs$Major/115.13)    # mm
    MPs$Minor=(MPs$Minor/115.13)    # mm

# Remove all particles > 4 mm2
  # 3 big particules were found in the compost samples and treated as outliers
  MPs=subset(MPs, Area<4)

# Length estimation ####
  # One dimension estimation
  MPs$LD=sqrt (MPs$Area)

#Length Catagories####
  lenght_fraction=c(0.1, 0.2, 0.3, 0.5)
  lenght_fraction=data.frame( cat= c("M_n_Cat1", "M_n_Cat2", "M_n_Cat3" ,"M_n_Cat4", "M_n_Cat5"),
                              cat_label=factor(c("< 100 µm", "100-200 µm", "200-300 µm", "300-500 µm", "> 500 µm")),
                              fraction=c(0.1, 0.2, 0.3, 0.5, 1) )

  # Check the number of MPs per category : 
    c(nrow(subset(MPs, MPs$LD<lenght_fraction$fraction[1])),
      nrow(subset(MPs, MPs$LD>lenght_fraction$fraction[1] &LD<lenght_fraction$fraction[2])),
      nrow(subset(MPs, MPs$LD>lenght_fraction$fraction[2] &LD<lenght_fraction$fraction[3])),
      nrow(subset(MPs, MPs$LD>lenght_fraction$fraction[3] &LD<lenght_fraction$fraction[4])),
      nrow(subset(MPs, MPs$LD>lenght_fraction$fraction[4])))
  
  # Assign name catogory
    MPs$Areacatagory[MPs$LD< lenght_fraction$fraction[1]]="< 100 µm"
    MPs$Areacatagory[MPs$LD< lenght_fraction$fraction[2]& MPs$LD> lenght_fraction$fraction[1]]="100-200 µm"
    MPs$Areacatagory[MPs$LD< lenght_fraction$fraction[3]& MPs$LD> lenght_fraction$fraction[2]]="200-300 µm"
    MPs$Areacatagory[MPs$LD< lenght_fraction$fraction[4]& MPs$LD> lenght_fraction$fraction[3]]="300-500 µm"
    MPs$Areacatagory[MPs$LD> lenght_fraction$fraction[4]]="> 500 µm"


# MPs nass estimation ####
  # Three equations are used to estiamte the mass of the MPs
    
  # Average plastic density assumed  
  Density=0.920 #[mg/mm3] 

  # *** Ellipsoid approximation ####
    # Estimation desvribed by Simon et al., 2018:
    # The mean ratio between minor and major axes of fit ellipses was calculated. 
    # It was assumed that the ratio of the thickness and the minor dimension of the particle was the same
    #and was used to calculate the volume of the particle
    
    # minor / major axes mean ratio : 
      MPs$D_ratio=MPs$Minor/MPs$Major
      mean(MPs$D_ratio)
  
    # Mass calculation
      MPs$Mass_ellips = MPs$Area *MPs$Minor *mean(MPs$D_ratio) *Density # [mg]

  # *** Sphere ####
    # Zhang et al (2018) proposes to use the particles area to estimate the volume of the plastic particles as 
    # the ninth of sphere of radius √(Area). ). However Zhang et al (2018) measured the area of the particles after heating
    #  assuming that the melting of the MPs will give it the spherical shape 1/9 of a sphere
      MPs$Mass_sphere = 4* Density * sqrt( (MPs$Area)^3/ pi) /27# [mg]

  # *** Cubical####
    # Because we measured the MPs area before heating, we assume MPs have a more angular shape than the ninth of sphere so
    # we use the volume approximation of the ninth of a cube.
      MPs$Mass_cube= Density * (sqrt (MPs$Area) )^3 /9# [mg]


# Summary per Picture ####
# Sum all the particules in one picture
  Picture_summary_R=MPs %>% 
    dplyr::group_by(Treat, Layer, Replicat, Spot, Sample, Treat_Layer, Sample_type ) %>%
    dplyr::summarise(Tot_Area=sum(Area), n_particules=n(), #n_100px=sum((Area<100)), 
                     Minor=sum(Minor), Major=sum(Major), Mass_ellips=sum(Mass_ellips),
                     Mass_sphere=sum(Mass_sphere),Mass_cube=sum(Mass_cube),
                     Mean_particle_Area=mean(Area),
                     n_Cat1= sum(LD< lenght_fraction$fraction[1]) ,
                     n_Cat2= sum( LD>= lenght_fraction$fraction[1] & LD < lenght_fraction$fraction[2]) ,
                     n_Cat3= sum( LD>= lenght_fraction$fraction[2] & LD< lenght_fraction$fraction[3]),
                     n_Cat4= sum( LD>= lenght_fraction$fraction[3] & LD< lenght_fraction$fraction[4]), 
                     n_Cat5= sum( LD>= lenght_fraction$fraction[4]) )
                   
# Summary R per Sample ####
# Average the replicats
  Sample_summary_R=Picture_summary_R %>% 
    dplyr::group_by(Sample, Treat, Spot , Layer, Treat_Layer, Sample_type  ) %>%
    dplyr::summarise(Tot_Area=mean(Tot_Area), n_particules=mean(n_particules),
                     Minor=mean(Minor),       Major=mean(Major), 
                     Mass_ellips=mean(Mass_ellips), Mass_sphere=mean(Mass_sphere),
                     Mass_cube=mean(Mass_cube), 
                     Mean_particle_Area=mean(Mean_particle_Area), 
                     n_Cat1= mean(n_Cat1), n_Cat2= mean(n_Cat2) ,  n_Cat3= mean(n_Cat3),
                     n_Cat4= mean(n_Cat4), n_Cat5= mean(n_Cat5) )         

# MPs per kg sample ####
  # Convert the absolute MPs counts per sample into a MPs content per kg sampl (soil or compost)
      
    # Soil
    Sample_summary_R[Sample_summary_R$Treat %in% c("Sp", "NL1", "NL2" ), c("Tot_Area",  "n_particules", "Mass_ellips","Mass_sphere","Mass_cube",
                                                                            "n_Cat1", "n_Cat2", "n_Cat3", "n_Cat4", "n_Cat5") ]=
      Sample_summary_R[Sample_summary_R$Treat %in% c("Sp", "NL1", "NL2" ), c("Tot_Area",  "n_particules", "Mass_ellips","Mass_sphere","Mass_cube",
                                                                              "n_Cat1", "n_Cat2", "n_Cat3", "n_Cat4", "n_Cat5") ]*100
    # Compost
    Sample_summary_R[Sample_summary_R$Treat %in% c("Cm", "Cg" ), c("Tot_Area",  "n_particules", "Mass_ellips","Mass_sphere","Mass_cube",
                                                                     "n_Cat1", "n_Cat2", "n_Cat3", "n_Cat4", "n_Cat5") ]=
      Sample_summary_R[Sample_summary_R$Treat %in% c("Cm", "Cg"), c("Tot_Area",  "n_particules", "Mass_ellips","Mass_sphere","Mass_cube",
                                                                        "n_Cat1", "n_Cat2", "n_Cat3", "n_Cat4", "n_Cat5") ]*200

# Summary Treat/layer ####
# Average per treat/Layer
  Treat_layer_summary_R=Sample_summary_R %>% 
    dplyr::group_by(Treat, Layer, Treat_Layer, Sample_type  ) %>%
    dplyr::summarise( n_samples=n(),M_n_particules=mean(n_particules),
                      M_Tot_Area=mean(Tot_Area), 
                      M_Mass_ellips=mean(Mass_ellips), M_Mass_sphere=mean(Mass_sphere),
                      M_Mass_cube=mean(Mass_cube),
                      M_Minor=mean(Minor), M_Major=mean(Major), 
                      M_Mean_particle_Area=mean(Mean_particle_Area), 
                      M_n_Cat1= mean(n_Cat1), M_n_Cat2= mean(n_Cat2) ,  M_n_Cat3= mean(n_Cat3),
                      M_n_Cat4= mean(n_Cat4), M_n_Cat5= mean(n_Cat5),
                      SD_n_particules=sd(n_particules), SD_Tot_Area=sd(Tot_Area), 
                      SD_Mass_ellips=sd(Mass_ellips),  SD_Mass_sphere=sd(Mass_sphere),
                      SD_Mass_cube=sd(Mass_cube),
                      SD_n_Cat5= sd(n_Cat5),
                      Q1_n=quantile(n_particules)[4],
                      Q1_a=quantile(Mean_particle_Area)[4]  )

# Summary Sample type ####
# Average per Sample_type 
  Type_summary_R=Sample_summary_R %>% 
    dplyr::group_by( Sample_type  ) %>%
    dplyr::summarise( n_samples=n(),M_n_particules=mean(n_particules),M_Tot_Area=mean(Tot_Area), 
                      M_Mass_ellips=mean(Mass_ellips), M_Mass_sphere=mean(Mass_sphere), M_Mass_cube=mean(Mass_cube),
                      M_Minor=mean(Minor), M_Major=mean(Major), M_Mean_particle_Area=mean(Mean_particle_Area), 
                      M_n_Cat1= mean(n_Cat1), M_n_Cat2= mean(n_Cat2) ,  M_n_Cat3= mean(n_Cat3),
                      M_n_Cat4= mean(n_Cat4), M_n_Cat5= mean(n_Cat5),
                      SD_n_particules=sd(n_particules), SD_Tot_Area=sd(Tot_Area), 
                      SD_Mass_ellips=sd(Mass_ellips),  SD_Mass_sphere=sd(Mass_sphere),
                      SD_Mass_cube=sd(Mass_cube), 
                      SD_n_Cat5= sd(n_Cat5))  
# Summary treat ####   
    # Average per treat/Layer
    Treat_summary_R=Sample_summary_R %>% 
      dplyr::group_by(Treat, Sample_type  ) %>%
      dplyr::summarise( n_samples=n(),M_n_particules=mean(n_particules),
                        M_Tot_Area=mean(Tot_Area), 
                        M_Mass_ellips=mean(Mass_ellips), M_Mass_sphere=mean(Mass_sphere),
                        M_Mass_cube=mean(Mass_cube),
                        M_Minor=mean(Minor), M_Major=mean(Major), 
                        M_Mean_particle_Area=mean(Mean_particle_Area), 
                        Q2_Mean_particle_Area=median(Mean_particle_Area), 
                        M_n_Cat1= mean(n_Cat1), M_n_Cat2= mean(n_Cat2) ,  M_n_Cat3= mean(n_Cat3),
                        M_n_Cat4= mean(n_Cat4), M_n_Cat5= mean(n_Cat5),
                        SD_n_particules=sd(n_particules), SD_Tot_Area=sd(Tot_Area), 
                        SD_Mass_ellips=sd(Mass_ellips),  SD_Mass_sphere=sd(Mass_sphere),
                        SD_Mass_cube=sd(Mass_cube),
                        SD_n_Cat5= sd(n_Cat5),
                        Q1_n=quantile(n_particules)[4],
                        Q1_a=quantile(Mean_particle_Area)[4]  )

# Statistical analysis #### 

  # Normal distribution
    # Kolmogorov-smirnov test :
      ks.test(MPs$Area, pnorm, alternative=c("two.sided"), exact=NULL)
      # p<0.05, the data does not follow a normal distribution
  
    # Shapiro-Wilk test : data follow a normal distribution
      shapiro.test(MPs$Area)

    # not normal -> Kruskal-Wallis test

  #*** Total Area ####
    kruskal.test(Tot_Area ~ Treat_Layer, data =Sample_summary_R) #Kruskal-Wallis chi-squared = 103.44, df = 11, p-value < 2.2e-16
  
    # Compare means with Wilcoxon test   
      cm_TA=compare_means(Tot_Area ~ Treat_Layer, data =Sample_summary_R, method = "wilcox.test",  # or method = "t.test"
                          paired = FALSE, group.by = NULL, ref.group = NULL)
      pval_TA=cm_TA$p # Create a p-value vector ...
      names(pval_TA)<- paste(cm_TA$group1,cm_TA$group2,sep = "-") #...with hyphenated names to fit multcompLetters
    
    # Assign different letters to significant differences
      mcl_TA=multcompLetters(pval_TA,
                             compare="<=",
                             threshold=0.05,
                             Letters=letters,
                             reversed = FALSE)

  #*** Mean Particle Area /picture ####
    kruskal.test(Mean_particle_Area ~ Treat_Layer, data =Sample_summary_R) #Kruskal-Wallis chi-squared = 103.44, df = 11, p-value < 2.2e-16
    
    # Compare means with Wilcoxon test  
      cm_MPA=compare_means(Mean_particle_Area ~ Treat_Layer, data =Sample_summary_R, method = "wilcox.test",  # or method = "t.test"
                           paired = FALSE, group.by = NULL, ref.group = NULL)
      
      pval_MPA=cm_MPA$p #create a p-value vector ...
      names(pval_MPA)<- paste(cm_MPA$group1,cm_MPA$group2,sep = "-") #...with hyphenated names to fit multcompLetters
    
    # Assign different letters to significant differences
      mcl_MPA=multcompLetters(pval_MPA,
                              compare="<=",
                              threshold=0.05,
                              Letters=letters,
                              reversed = FALSE)

  #*** Total Mass ####
    kruskal.test(Mass_ellips ~ Treat_Layer, data = Sample_summary_R) #Kruskal-Wallis chi-squared = 3363.1, df = 11, p-value < 2.2e-16
    
    # Compare means with Wilcoxon test 
      cm_TM=compare_means(Mass_ellips ~ Treat_Layer, data = Sample_summary_R, method = "wilcox.test", # or method = "t.test"
                          paired = FALSE, group.by = NULL, ref.group = NULL)
      
      pval_TM=cm_TM$p #create a p-value vector ...
      names(pval_TM)<- paste(cm_TM$group1,cm_TM$group2,sep = "-") #...with hyphenated names to fit multcompLetters
     
    # Assign different letters to significant differences 
      mcl_TM=multcompLetters(pval_TM,
                             compare="<=",
                             threshold=0.05,
                             Letters=letters,
                             reversed = FALSE)

  # *** mm2 Size distribution ####
    # Kolmogorov-Smirnov test, for loop
        # Matrix of p-values : p_KS
          p_KS=matrix(0,nrow(Treat_layer_summary_R),nrow(Treat_layer_summary_R))
          for (i in 1:nrow(Treat_layer_summary_R)){
            for (j in 1:nrow(Treat_layer_summary_R)){
              p_KS[i,j]= ks.test(MPs$Area[MPs$Treat_Layer == Treat_layer_summary_R$Treat_Layer[i]], 
                                 MPs$Area[MPs$Treat_Layer == Treat_layer_summary_R$Treat_Layer[j]])[[2]]
            }
          }
          colnames(p_KS)=Treat_layer_summary_R$Treat_Layer
          row.names(p_KS)=Treat_layer_summary_R$Treat_Layer
        
        # Assign different letters to significant differences  
          mcl_MPAd=multcompLetters(p_KS,
                                   compare="<=",
                                   threshold=0.05,
                                   Letters=letters,
                                   reversed = FALSE)
          
    # *** um Size distribution ####
          # Kolmogorov-Smirnov test, for loop
          # Matrix of p-values : p_KS
          p_KS_LD=matrix(0,nrow(Treat_layer_summary_R),nrow(Treat_layer_summary_R))
          for (i in 1:nrow(Treat_layer_summary_R)){
            for (j in 1:nrow(Treat_layer_summary_R)){
              p_KS_LD[i,j]= ks.test(MPs$LD[MPs$Treat_Layer == Treat_layer_summary_R$Treat_Layer[i]], 
                                 MPs$LD[MPs$Treat_Layer == Treat_layer_summary_R$Treat_Layer[j]])[[2]]
            }
          }
          colnames(p_KS_LD)=Treat_layer_summary_R$Treat_Layer
          row.names(p_KS_LD)=Treat_layer_summary_R$Treat_Layer
          
          # Assign different letters to significant differences  
          mcl_MPLDd=multcompLetters(p_KS_LD,
                                   compare="<=",
                                   threshold=0.05,
                                   Letters=letters,
                                   reversed = FALSE)
#*** Mean Area ####
  kruskal.test(Area ~ Treat_Layer, data = MPs) #Kruskal-Wallis chi-squared = 3363.1, df = 11, p-value < 2.2e-16
    
  # Compare means with Wilcoxon test 
    cm_MA=compare_means(Area ~ Treat_Layer, data = MPs, method = "wilcox.test", # or method = "t.test" 
                      paired = FALSE, group.by = NULL, ref.group = NULL)
  
    pval_MA=cm_MA$p #create a p-value vector ...
    names(pval_MA)<- paste(cm_MA$group1,cm_MA$group2,sep = "-") #...with hyphenated names to fit multcompLetters
    
  # Assign different letters to significant differences
    mcl_MA=multcompLetters(pval_MA,
                           compare="<=",
                           threshold=0.05,
                           Letters=letters,
                           reversed = FALSE)

# Plots ####

  #*** Graphical Abstract Size distribution, MPs####
    MPs$Areacatagory=factor(MPs$Areacatagory, levels=c("< 100 µm", "100-200 µm", "200-300 µm", "300-500 µm", "> 500 µm"))
    
    # data.frame PLOT1 : P_distrib
      P_distrib=rbind(Type_summary_R, Type_summary_R,Type_summary_R,Type_summary_R,Type_summary_R)
      P_distrib=subset(P_distrib, select = -c(M_n_Cat1, M_n_Cat2, M_n_Cat3, M_n_Cat4, M_n_Cat5))
      
      P_distrib$length_cat="o"
      P_distrib$n_cat=0
      
      for (i in 1:nrow(lenght_fraction)){
        P_distrib$length_cat[ ((i-1)*nrow(Type_summary_R)+1): (i*nrow(Type_summary_R)) ] = as.factor(lenght_fraction$cat[i])
        P_distrib$n_cat[ ((i-1)*nrow(Type_summary_R)+1): (i*nrow(Type_summary_R)) ]= t(as.vector(Type_summary_R[,lenght_fraction$cat[i]]))
      }
      
      P_distrib$length_cat=revalue(factor(P_distrib$length_cat), c("5"="> 500 µm", "4"="300-500 µm",  "3"="200-300 µm", "2"="100-200 µm", "1"="< 100 µm"   ))
      P_distrib$length_cat=revalue(factor(P_distrib$length_cat), c( "1"="< 100 µm" , "2"="100-200 µm",  "3"="200-300 µm","4"="300-500 µm", "5"="> 500 µm"  ))
      
      P_distrib$length_cat=factor(P_distrib$length_cat, levels=rev(c("< 100 µm", "100-200 µm", "200-300 µm", "300-500 µm", "> 500 µm") ))
      P_distrib$Sample_type=factor(P_distrib$Sample_type,c("Soil_Sp","Soil_NL" ,"Compost_NL"))
    
    # color patette PLOT1
      my_palette = c(brewer.pal(5, "BrBG")[c(1,2,3,4,5)]) # my_palette = c(brewer.pal(9, "BrBG")[c(1,2,3,5,6,7,8,9)])
    
    # factor to add the error bars :
      Type_summary_R$Sample_type=factor(Type_summary_R$Sample_type,c("Soil_Sp","Soil_NL" ,"Compost_NL"))

    PLOT1=
      ggplot(P_distrib) + 
      geom_bar(aes(fill=length_cat, y=n_cat, x=Sample_type), position="stack", stat="identity")+ #
      scale_fill_manual(values = my_palette )+
      guides(colour = guide_legend(reverse=TRUE))+
      geom_errorbar(data = Type_summary_R, aes(x=Sample_type,
                                               ymin=(M_n_particules-SD_n_particules/2),
                                               ymax=(M_n_particules+SD_n_particules/2) ),
                    width=0.1, colour="blue", alpha=1, size=1.3  ) +
      geom_label(data = Type_summary_R, aes(x=Sample_type, y=M_n_particules, label=round(M_n_particules,0)),
                 position= position_dodge(width=0.9), vjust=-.05, color="black",
                 fontface = "bold", size=6, alpha=0.8, label.size = NA  ) +
      scale_x_discrete(name= NULL, breaks=c("Soil_sp","Soil_nl" ,"Compost_nl"),
                       labels=c("Soil\nSpain", "Soil\nNetherlands", "Compost\nNetherlands")  )+
      ylab(  expression("Particles per Kg"))+
      guides(fill=guide_legend(title="Microplastics size categories"))+
      theme(
        plot.title = element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        axis.title.x = element_blank(), #remove the x label 
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text( size=16),
        axis.text.x  = element_text( size=16), #angle=90, vjust=0.5, hjust=1, hjust : align the labels, angle: rotate the labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
    # export 600 * 350

  #*** Fig.2 Microplastic content  ####
    # data.frame PLOT2 : Sample_summary_R
    
    # Letters significant differences for PLOT3
    Sample_summary_R$Treat_Layer=factor(Sample_summary_R$Treat_Layer)
    Sample_summary_R= Sample_summary_R[order( Sample_summary_R$Treat_Layer),]
    
      kruskal.test(n_particules ~ Treat_Layer, data =Sample_summary_R) #Kruskal-Wallis chi-squared = 103.44, df = 11, p-value < 2.2e-16
      cm_MP=compare_means(n_particules ~ Treat_Layer, data =Sample_summary_R, method = "wilcox.test",  # or method = "t.test"
                          paired = FALSE, group.by = NULL, ref.group = NULL)
      pval_MP=cm_MP$p # Create a p-value vector ...
      names(pval_MP)<- paste(cm_MP$group1,cm_MP$group2,sep = "-") #...with hyphenated names to fit multcompLetters
      mcl_MP=multcompLetters(pval_MP,
                             compare="<=",
                             threshold=0.05,
                             Letters=letters,
                             reversed = FALSE)
      
      Treat_layer_summary_R=Treat_layer_summary_R[order(Treat_layer_summary_R$Treat_Layer),]
      Treat_layer_summary_R$Sign.letter=mcl_MP[[1]]
      
      
      Sample_summary_R$Treat_Layer=revalue(factor(Sample_summary_R$Treat_Layer), c( "Sp_0_10"="Sp (0-10)", "Sp_10_30"="Sp (10-30)",
                                                                              "NL1_0_10"  ="NL1 (0-10)", "NL1_10_30"="NL1 (10-30)",
                                                                              "NL2_0_10"="NL2 (0-10)", "NL2_10_30"="NL2 (10-30)",
                                                                              "Cg_0"="Cg","Cm_0"="Cm") )

      Treat_layer_summary_R$Treat_Layer=revalue(factor(Treat_layer_summary_R$Treat_Layer), c( "Sp_0_10"="Sp (0-10)", "Sp_10_30"="Sp (10-30)",
                                                                                        "NL1_0_10"  ="NL1 (0-10)", "NL1_10_30"="NL1 (10-30)",
                                                                                        "NL2_0_10"="NL2 (0-10)", "NL2_10_30"="NL2 (10-30)",
                                                                                        "Cg_0"="Cg","Cm_0"="Cm") )
      
      Treat_layer_summary_R$Treat_Layer=factor(Treat_layer_summary_R$Treat_Layer,c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"))
      
      Sample_summary_R$Treat_Layer=factor(Sample_summary_R$Treat_Layer,c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"))
      
    # color patette PLOT2
    my_palette = c(brewer.pal(8, "Paired")[c(5,6,3,4,1,2,8,7)])
    PLOT2= ggplot()+
      geom_boxplot(data =Sample_summary_R, aes(x=Treat_Layer, y=n_particules, fill=Treat_Layer), outlier.shape = -1)+
      geom_jitter(data =Sample_summary_R, aes(x=Treat_Layer, y=n_particules, fill=Treat_Layer), color="black", width=0.11, size=1, alpha=0.9) +
      scale_fill_manual(values = my_palette )+
      stat_summary(data =Sample_summary_R, aes(x=Treat_Layer, y=n_particules, fill=Treat_Layer),
                   fun=mean, colour="black", geom="point", 
                   shape="+", size=7,  show.legend = FALSE)+ #pch = 24, cex=5, lwd=50,
      geom_text(data = Treat_layer_summary_R ,aes(x=Treat_Layer, y= Q1_n+150, label=Sign.letter),
                position=position_nudge(x=-0.3, y=0), size=5, colour="black")+
      scale_x_discrete(name= NULL, breaks=c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"),
                       labels=c("Sp\n(0-10)","Sp\n(10-30)" ,"NL1\n(0-10)","NL1\n(10-30)","NL2\n(0-10)", "NL2\n(10-30)", "Cm", "Cg")  )+
      ylab("Particles per Kg")+
      theme(
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=16,hjust = 0.5),
        axis.text.x  = element_text(size=14), #hjust : align the labels 
        axis.text.y  = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none" ) 
    # Export 900*350

    #*** Fig.3 Microplastic mean Area ####
    # data.frame PLOT2 : Sample_summary_R
    
    # Letters significant differences for PLOT3
    MPs$Treat_Layer=factor(MPs$Treat_Layer, c(  "Sp_0_10","Sp_10_30","NL1_0_10","NL1_10_30","NL2_0_10","NL2_10_30","Cm_0","Cg_0" ))

    
    kruskal.test(Area~ Treat_Layer, data =MPs) #Kruskal-Wallis chi-squared = 103.44, df = 11, p-value < 2.2e-16
    cm_MPpa=compare_means(Area~ Treat_Layer, data =MPs, method = "wilcox.test",  # or method = "t.test"
                        paired = FALSE, group.by = NULL, ref.group = NULL)
    pval_MPpa=cm_MPpa$p # Create a p-value vector ...
    names(pval_MPpa)<- paste(cm_MPpa$group1,cm_MPpa$group2,sep = "-") #...with hyphenated names to fit multcompLetters
    mcl_MPpa=multcompLetters(pval_MPpa,
                           compare="<=",
                           threshold=0.05,
                           Letters=letters,
                           reversed = FALSE)
    
     Treat_layer_summary_R$Treat_Layer= factor(Treat_layer_summary_R$Treat_Layer,c("Sp (0-10)","Sp (10-30)","NL1 (0-10)","NL1 (10-30)","NL2 (0-10)","NL2 (10-30)","Cm","Cg"))
     Treat_layer_summary_R=Treat_layer_summary_R[order(Treat_layer_summary_R$Treat_Layer),]
     Treat_layer_summary_R$Sign.letter=mcl_MPpa[[1]]
     
     Treat_layer_summary_R$Sign.letter=mcl_MPAd[[1]] #Kolomogorov S
    
    
   MPs$Treat_Layer=revalue(factor(MPs$Treat_Layer), c( "Sp_0_10"="Sp (0-10)", "Sp_10_30"="Sp (10-30)",
                                                                                  "NL1_0_10"  ="NL1 (0-10)", "NL1_10_30"="NL1 (10-30)",
                                                                                  "NL2_0_10"="NL2 (0-10)", "NL2_10_30"="NL2 (10-30)",
                                                                                  "Cg_0"="Cg","Cm_0"="Cm") )
  
    Treat_layer_summary_R$Treat_Layer=revalue(factor(Treat_layer_summary_R$Treat_Layer), c( "Sp_0_10"="Sp (0-10)", "Sp_10_30"="Sp (10-30)",
                                                                                           "NL1_0_10"  ="NL1 (0-10)", "NL1_10_30"="NL1 (10-30)",
                                                                                           "NL2_0_10"="NL2 (0-10)", "NL2_10_30"="NL2 (10-30)",
                                                                                           "Cg_0"="Cg","Cm_0"="Cm") )
   
   
   MPs$Treat_Layer=factor(MPs$Treat_Layer,c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"))

   
    # color patette PLOT
    my_palette = c(brewer.pal(8, "Paired")[c(5,6,3,4,1,2,8,7)])
    PLOT3= ggplot()+
      geom_violin(data =MPs[MPs$Area<0.5,], aes(x=Treat_Layer, y=Area, fill=Treat_Layer), outlier.shape = -1)+
      scale_fill_manual(values = my_palette )+
      stat_summary(data = MPs[MPs$Area<0.5,], aes(x=Treat_Layer, y=Area, fill=Treat_Layer),
                   fun=mean, colour="black", geom="point", 
                   shape="+", size=7,  show.legend = FALSE)+ #pch = 24, cex=5, lwd=50,
      geom_text(data = Treat_layer_summary_R ,aes(x=Treat_Layer, y= Q1_a+0.1, label=Sign.letter),
                position=position_nudge(x=-0.3, y=0), size=5, colour="black")+
      scale_x_discrete(name= NULL, breaks=c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"),
                       labels=c("Sp\n(0-10)","Sp\n(10-30)" ,"NL1\n(0-10)","NL1\n(10-30)","NL2\n(0-10)", "NL2\n(10-30)", "Cm", "Cg")  )+
      ylab(expression("MPs area [mm"^2 *"]"))+
      theme(
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=16,hjust = 0.5),
        axis.text.x  = element_text(size=14), #hjust : align the labels 
        axis.text.y  = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none" ) 
    # Export 900*350
    
    
# *** Density size distribution ####
  
  # data.frame Fig.S1 : P_density
    # Crop particles > 0.6 mm to zoom in the graphs
    P_density=MPs[MPs$Area<0.6,]
    P_density=MPs[MPs$LD<1,]
    
    P_density$Treat_Layer=revalue(factor(P_density$Treat_Layer), c( "Sp_0_10"="Sp (0-10)", "Sp_10_30"="Sp (10-30)",
                                                                    "NL1_0_10"  ="NL1 (0-10)", "NL1_10_30"="NL1 (10-30)",
                                                                    "NL2_0_10"="NL2 (0-10)", "NL2_10_30"="NL2 (10-30)",
                                                                    "Cg_0"="Cg","Cm_0"="Cm") )
    
    P_density$Treat_Layer=factor( P_density$Treat_Layer,c("Sp (0-10)","Sp (10-30)" ,"NL1 (0-10)","NL1 (10-30)","NL2 (0-10)", "NL2 (10-30)", "Cm", "Cg"))
    
    P_density$Treat=factor(P_density$Treat, c( "Sp", "NL1", "NL2", "Cm", "Cg") ) 
     
    my_palette =c( "#E31A1C", "black","#33A02C", "black", "#1F78B4",  "black", "#FF7F00","#FDBF6F")
    
    PLOT4=ggplot( P_density, aes(x=LD*1000, fill=Treat_Layer)) + 
      geom_density(alpha=.3)+
      facet_wrap(~Treat) +
      scale_fill_manual(values = my_palette )+
      xlab(expression("Particles area [µm]"))+
      ylab("Density [%]")+
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16,hjust = 0.5),
        axis.text.x  = element_text(size=14), #hjust : align the labels 
        axis.text.y  = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = c(0.8, 0.2) # c(0,0) bottom left, c(1,1) top-right.
         ) 
    # Export as 800*600
