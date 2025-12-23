# set-up ----
rm(list=ls())
library(pacman)

dataws=getwd()



p_load(writexl, readxl, janitor, openxlsx,
       tidyverse, dplyr, scales, 
       likert, labelled,ggpubr, grid,
       compareGroups, labelled,
       
       sandwich,lmtest,
       
       lme4, lqmm,quantreg,
       
       sjstats,
       
       gee,geepack, broom,
       
       car,
       
       Amelia, mice,
       
       jtools
)



ds_pbl=readxl::read_excel(file.path(dataws,paste0("datasets/1. df_pbl_main.xlsx")))
ds_pfu=readxl::read_excel(file.path(dataws,paste0("datasets/2. ds_pfu_main.xlsx")))
df_lcomp=readxl::read_excel(file.path(dataws,paste0("datasets/3. ds_pfu_lcomp_main.xlsx")))
ds_cfu=readxl::read_excel(file.path(dataws,paste0("datasets/4. ds_cfu_main.xlsx")))


source(file.path(dataws,"functions/multi_table.R"))
source(file.path(dataws,"functions/is_dup.R"))
source(file.path(dataws,"functions/toNA.R"))
source(file.path(dataws,"functions/toDate.R"))


propmiss = function(dataframe) {
  m = sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)),
      n=length(x),
      propmiss=sum(is.na(x))/length(x))
  })
  d = data.frame(t(m))
  d = sapply(d, unlist)
  d = as.data.frame (d)
  d$variable = row.names (d)
  row.names(d) = NULL
  d = cbind(d[ncol(d)], d[-ncol(d)])
  return(d[order(d$propmiss),])
}


# analyses ----



## Table 1 - demographics and sexual behaviors ----
### Index peers - self reported ----

labels_df_total=c(#p1_ptid="Participant's ID",
  p1_s0_arm="Study arm",
  p1_s1_q1="Age*",
  p1_s1_q2="Years of school*",
  p1_s1_q3="Currently in school",
  p1_s2_q7="No. of sexual partners*",
  
  p1_s1_q6="Relationship status",
  p1_s1_q7="Monthly income*",
  # p1_s2_q6="No. of sexual acts (2 weeks)*",
  p1_s2_q6a_1="Any condomless sex (past 2 weeks)",
  p1_s2_q10="Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  p1_s2_q2="HIV status",
  p1_s5_q1_1="Prior PrEP use",
  p1_s1_q5_2="Currently using PrEP for prevention",
  p1_s1_q5_1="PrEP champion",
  p1_s2_q12="Emergency contraception (6 months)",
  p1_s5_q9="Has disclosed PrEP use to others",
  prep_first="Time since first PrEP use(months)*",
  
  p1_s1_q9="Alcohol consumption",
  p1_s1_q10_dico="Ilegal drug use",
  age_gap="Age gap between provider/client and last sexual partner (among singles)*",
  p1_s2_q9d_fact="Known partner living with HIV",
  p1_s2_q11="STI (past 6 months)",
  p1_s2_q12="Prior emergency contraception use (past 6 months)",
  p1_s2_q13="Emergency contraception twice (past 6 months)",
  p1_s2_q14="PEP use more than twice (past 6 months)",
  p1_s2_q15="Sex under influence of drugs (past 6 months)",
  p1_s2_q16="Sex exchange (past 6 months)",
  p1_s5_q1_dico="Prior PrEP use",
  p1_s2_q18="Ever being pregnant",
  p1_s2_q18a="No. of living children*",
  incons_condom="Inconsistent condom use",
  new_sexpart_dico="New sexual partner (past 3 months)",
  p1_s2_q8="No. of sexual partners with known HIV status*",
  p1_s1_q4="Marital status",
  p1_s2_q17="Age of first sex",
  
  p1_s2_q9c_rec="Any HIV testing with primary partner",
  p1_s2_q1_rec="Last HIV test, past 3 months"
  
  
  
)



vars=c(#"p1_ptid",
  "p1_s0_arm","p1_s1_q1", "p1_s1_q2","p1_s1_q3", "p1_s2_q7",
  
  
  "p1_s1_q6",
  "p1_s1_q7",
  # "p1_s2_q6",
  "p1_s2_q6a_1","p1_s2_q10",
  "p1_s2_q2",
  "p1_s5_q1_1",
  "p1_s1_q5_2",
  "p1_s1_q5_1","p1_s2_q12","p1_s5_q9","prep_first",
  
  "p1_s1_q9",
  "p1_s1_q10_dico",
  "age_gap",
  "p1_s2_q9d_fact",
  "p1_s2_q11",
  "p1_s2_q12",
  "p1_s2_q13",
  "p1_s2_q14",
  "p1_s2_q15",
  "p1_s2_q16",
  "p1_s5_q1_dico",
  "p1_s2_q18",
  "p1_s2_q18a",
  "incons_condom",
  "new_sexpart_dico",
  "p1_s2_q8",
  "p1_s1_q4",
  "p1_s2_q17",
  
  "p1_s2_q9c_rec",
  "p1_s2_q1_rec"
  
  
)


vars_c=paste(vars,collapse = "+")
vars_f=as.formula(paste("p1_s0_arm~",vars_c))

df_table_total_in_f=ds_pbl[,vars]

#ds_pbl %>% dplyr::select(all_of(vars))
df_table_total_in=set_variable_labels(df_table_total_in_f, 
                                      .labels = labels_df_total)
#label(df_table_total_in)

#compareGroups(year ~ age + triglyc, data=regicor, method = c(triglyc=2), Q1=0, Q3=1)

df_table_total_out=compareGroups(vars_f, data = df_table_total_in,
                                 method=c(p1_s1_q1=2,p1_s1_q2=2,p1_s2_q7=2,p1_s1_q7=2,p1_s2_q6=2,prep_first=2,
                                          age_gap=2,p1_s2_q18a=2,p1_s2_q8=2,p1_s2_q17=2), simplify = T) %>% 
  createTable(digits = 0,
              # show.all = TRUE,
              show.n=T)


summary(ds_pbl[ds_pbl$p1_s0_arm=="Intervention",]$p1_s2_q8)
summary(ds_pbl[ds_pbl$p1_s0_arm=="Control",]$p1_s2_q8)

# Long acting inyectable PrEP - Int vs Control

df_contracept_int=as.data.frame(table(multi_table(ds_pbl[ds_pbl$p1_s0_arm=="Intervention",c("p1_s2_q19")])))
df_contracept_int$Var1=ifelse(df_contracept_int$Var1==0,"None",
                              ifelse(df_contracept_int$Var1==1 | df_contracept_int$Var1==8 | df_contracept_int$Var1==888,"Others (oral, condoms, etc)",
                                     ifelse(df_contracept_int$Var1==2 | df_contracept_int$Var1==3 | df_contracept_int$Var1==6,"Long acting reversible contraception",NA)))
df_contracept_int=df_contracept_int %>% dplyr::group_by(Var1) %>% summarise(N=sum(Freq))
# df_contracept_int=df_contracept_int[!is.na(df_contracept_int$Var1),]
df_contracept_int[which(is.na(df_contracept_int$Var1)),c("Var1")]=c("None")
df_contracept_int$Perc=round(df_contracept_int$N * 100 / sum(df_contracept_int$N, na.rm=T),1)
df_contracept_int=df_contracept_int[order(df_contracept_int$N, decreasing = TRUE),]

df_contracept_ctl=as.data.frame(table(multi_table(ds_pbl[ds_pbl$p1_s0_arm=="Control",c("p1_s2_q19")])))
df_contracept_ctl$Var1=ifelse(df_contracept_ctl$Var1==0,"None",
                              ifelse(df_contracept_ctl$Var1==1 | df_contracept_ctl$Var1==8 | df_contracept_ctl$Var1==888,"Others (oral, condoms, etc)",
                                     ifelse(df_contracept_ctl$Var1==2 | df_contracept_ctl$Var1==3 | df_contracept_ctl$Var1==6,"Long acting reversible contraception",NA)))

df_contracept_ctl=df_contracept_ctl %>% dplyr::group_by(Var1) %>% summarise(N=sum(Freq))
# df_contracept_ctl=df_contracept_ctl[!is.na(df_contracept_ctl$Var1),]
df_contracept_int[which(is.na(df_contracept_int$Var1)),c("Var1")]=c("None")
df_contracept_ctl$Perc=round(df_contracept_ctl$N * 100 / sum(df_contracept_ctl$N, na.rm=T),1)
df_contracept_ctl=df_contracept_ctl[order(df_contracept_ctl$N, decreasing = TRUE),]



# df_contracept_m1=merge(df_contracept_overall,df_contracept_int,by="Var1",all.x=T)
df_contracept_m2=merge(df_contracept_int,df_contracept_ctl,by="Var1",all.x=T)

df_contracept_m2=df_contracept_m2[order(df_contracept_m2$N.x, decreasing = TRUE),]



### Referred peers - reported by index peers ----



### Referred peers - self-reported ----
