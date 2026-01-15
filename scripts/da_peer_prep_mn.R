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

labels_df_lcomp=c(
  #p2_ptid="Participant's ID",
  p2_s0_arm="Study arm", #Added,
  p2_s6_q1_2 ="Peer's age*", #Added
  p2_s6_q1_5 ="Relationship status", #Added
  p2_s6_q1_12a ="Clients had previously used PrEP",
  p2_s6_q1_6="Sex exchange (past 6 months)",
  p2_s6_q1_1 ="Days: provider training to client referral*",
  p2_s6_q1_13 ="Provider disclosed PrEP use to client",
  p2_s6_q1_7 ="Materials clients received from providers",
  # p2_s6_q1_12aa ="PrEP use at deliverying intervention",
  
  
  p2_s6_q0 ="Number of clients referred*", #Added
  p2_s6_q1_17 ="Visited clinic for HIV services", # Formerly PrEP referral
  p2_s6_q1_14="Any HIV testing",
  p2_s6_q1_14b ="HIV results",
  p2_s6_q01a = "HIVST",
  p2_s6_q1_17c ="Provider escorted client to HIV services",
  p2_s6_q1_17d ="PrEP initiation",
  p2_s6_q07 ="PrEP retention/continuation"
  
)



vars_lcomp=names(labels_df_lcomp[1:7])
vars_c_lcomp=paste(vars_lcomp,collapse = "+")
vars_c_f_lcomp=as.formula(paste("~",vars_c_lcomp))
vars_f_lcomp=as.formula(paste("p2_s0_arm~",vars_c_lcomp))


df_table_total_in_fu_lcomp=df_lcomp %>% dplyr::select(all_of(vars_lcomp))
df_table_total_in_fu_lcomp=set_variable_labels(df_table_total_in_fu_lcomp,
                                               .labels = labels_df_lcomp[1:7])


df_table_total_pfu_out_lcomp=compareGroups(vars_f_lcomp, data = df_table_total_in_fu_lcomp,
                                           method=c(p2_s6_q1_2=2,p2_s6_q1_1 =2), simplify=TRUE) %>% createTable(#show.all = T,
                                             show.n=T)




### Referred peers - self-reported ----


labels_df_total_cfu=c(
  c2_s0_arm= "Study arm",
  # c2_ptid="Client's ID",
  # c2_pp_ptid="Peer Provider PTID",
  c2_s1_q1 = "Age*",
  c2_s1_q5 = "Relationship status",
  c2_s2_q2 = "Knowledge of own HIV status",
  c2_s2_q6 = "No. of sexual partners*",
  c2_s6_q1_1 = "Prior PrEP use",
  c2_s6_q1_1_rec = "Prior PrEP use",
  
  c2_s1_q2 = "Years in school*",
  c2_s1_q3 = "Currently in school",
  c2_s1_q4 = "Marital status",
  age_gap = "Age gap between client and last sexual partner*",
  c2_s1_q7_1 = "Enrolled in another HIV study",
  c2_s1_q7 = "Alcohol consumption",
  
  c2_s1_q6 = "Monthly income",
  c2_s1_q8_dico = "Ilegal drug use",
  c2_s2_q18 = "Ever being pregnant",
  c2_s2_q18a = "No. of living children",
  c2_s2_q17 = "Age of first sex*",
  c2_s2_q8_1b = "Known partner living with HIV",
  c2_s2_q6_1 = "Any condomless sex (past 6 months)",
  c2_s2_q10 = "Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  c2_s2_q11 = "STI (past 6 months)",
  c2_s2_q12 = "Prior emergency contraception use (past 6 months)",
  c2_s2_q13 = "Emergency contraception twice (past 6 months)",
  c2_s2_q14 = "PEP use more than twice (past 6 months)",
  c2_s2_q15 = "Sex under influence of drugs (past 6 months)",
  c2_s2_q16 = "Sex exchange (past 6 months)",
  c2_s6_q1_1_1 = "Currently using PrEP for HIV prevention",
  c2_s6_q01e = "Has disclosed PrEP use to others",
  prep_first = "Time since first PrEP use (months)*",
  incons_condom = "Inconsisten condom use",
  new_sexpart_dico = "New sexual partner (past 3 months)",
  c2_s2_q7 = "No. of sexual partners with known HIV status*",
  c2_s6_q01j = "Knowledge of own HIV status",
  c2_s2_q8_1a_rec = "Any HIV testing with primary partner",
  c2_s2_q1_rec = "Last HIV testing, past 3 months"
  
)


vars=c("c2_s0_arm","c2_s1_q1","c2_s1_q5","c2_s2_q2","c2_s2_q6",
       "c2_s6_q1_1","c2_s6_q1_1_rec",
       
       "c2_s1_q2", "c2_s1_q3","c2_s1_q4","age_gap","c2_s1_q7_1","c2_s1_q7",
       
       "c2_s1_q6", "c2_s1_q8_dico", "c2_s2_q18", "c2_s2_q18a","c2_s2_q17",
       "c2_s2_q8_1b","c2_s2_q10","c2_s2_q6_1","c2_s2_q11","c2_s2_q12",
       "c2_s2_q13","c2_s2_q14","c2_s2_q15","c2_s2_q16","c2_s6_q1_1_1",
       "c2_s6_q01e","prep_first","incons_condom","new_sexpart_dico","c2_s2_q7","c2_s6_q01j",
       
       "c2_s2_q8_1a_rec","c2_s2_q1_rec"
       
       
)

vars_c=paste(vars,collapse = "+")
vars_c_f=as.formula(paste("~",vars_c))
vars_f=as.formula(paste("c2_s0_arm~",vars_c))


df_table_total_in_cfu=ds_cfu %>% dplyr::select(all_of(vars))
df_table_total_in_cfu=set_variable_labels(df_table_total_in_cfu, 
                                          .labels = labels_df_total_cfu)

#label(df_table_total_in)

df_table_total_cfu_out=compareGroups(vars_f, data = df_table_total_in_cfu,
                                     method = c(c2_s1_q1=2, c2_s2_q6=2, c2_s1_q2=2, age_gap=2,c2_s1_q6=2,
                                                c2_s2_q18a=2,c2_s2_q17=2,prep_first=2,c2_s2_q7=2),simplify=T) %>% createTable(show.n = T)



# Table 2 ----
## Among referred peers, reported by index peers ----


summary(df_lcomp[df_lcomp$p2_s0_arm=="Intervention","p2_s6_q0"])
summary(df_lcomp[df_lcomp$p2_s0_arm=="Control","p2_s6_q0"])


qreg_nclient_m=lqmm(fixed = p2_s6_q0 ~ p2_s0_arm, random= ~1, group = p2_ptid, data=df_lcomp, tau= 0.5, 
                    # nK=3, 
                    type = "robust")
qreg_nclient=summary(qreg_nclient_m)$tTable
df_client_rprovid_nclient=cbind(cbind(qreg_nclient[2,1],
                                      t(qreg_nclient[2,3:5])))
df_client_rprovid_nclient=as.data.frame(df_client_rprovid_nclient)
rownames(df_client_rprovid_nclient)=c("Number of clients referred*")
colnames(df_client_rprovid_nclient)=c("Estimate","Lower bound","Upper bound","P-value")




## Among referred peers, self-reported ----
addmargins(table(ds_cfu$c2_s6_q01f,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01f,ds_cfu$c2_s0_arm),margin=2)*100,0)

addmargins(table(ds_cfu$c2_s6_q01i,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01i,ds_cfu$c2_s0_arm),margin=2)*100,0)

ds_cfu$c2_s6_q01t=ifelse(ds_cfu$c2_s6_q01t=="Didn't seek HIV services","No",ds_cfu$c2_s6_q01t)
addmargins(table(ds_cfu$c2_s6_q01t,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01t,ds_cfu$c2_s0_arm),margin=2)*100,0)

ds_cfu$c2_s6_q01w=ifelse(ds_cfu$c2_s6_q01w=="Didn't seek HIV services","No",ds_cfu$c2_s6_q01w)
addmargins(table(ds_cfu$c2_s6_q01w,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01w,ds_cfu$c2_s0_arm),margin=2)*100,0)

summary(ds_cfu[ds_cfu$c2_s0_arm=="Intervention","prep_adh_score"])
summary(ds_cfu[ds_cfu$c2_s0_arm=="Control","prep_adh_score"])



ds_cfu$c2_s0_arm_num=as.numeric(as.factor(ds_cfu$c2_s0_arm))-1


ds_cfu$c2_s6_q01f=relevel(as.factor(ds_cfu$c2_s6_q01f), ref='No')
ds_cfu$c2_s6_q01f_num=as.numeric(ds_cfu$c2_s6_q01f)-1

ds_cfu$c2_s6_q01i_num=as.numeric(ds_cfu$c2_s6_q01i)


ds_cfu$c2_s6_q01t=relevel(as.factor(ds_cfu$c2_s6_q01t), ref="No")
ds_cfu$c2_s6_q01t_num=as.numeric(ds_cfu$c2_s6_q01t)
ds_cfu$c2_s6_q01t_num=ifelse(is.na(ds_cfu$c2_s6_q01t_num),1,
                             ifelse(ds_cfu$c2_s6_q01t_num==4,1,ds_cfu$c2_s6_q01t_num))
ds_cfu$c2_s6_q01t_num=ds_cfu$c2_s6_q01t_num-1

ds_cfu$c2_s6_q01w_num=relevel(as.factor(ds_cfu$c2_s6_q01w), ref='No')
ds_cfu$c2_s6_q01w_num=ifelse(ds_cfu$c2_s6_q01w_num == 2, 0, ds_cfu$c2_s6_q01w_num)-1




glmm_anyhivt_pc_m=glmer(c2_s6_q01f_num ~ c2_s0_arm + (1|c2_pp_ptid), 
                        family = gaussian(link="identity"), data=ds_cfu)
df_pc_anyhivt=cbind(summary(glmm_anyhivt_pc_m)$coef[2,1],confint(glmm_anyhivt_pc_m)[4,1],confint(glmm_anyhivt_pc_m)[4,2])
df_pc_anyhivt=as.data.frame(df_pc_anyhivt)
rownames(df_pc_anyhivt)=c("Any recent HIV testing since referral")



glmm_ptest_pc_m=glmer(c2_s6_q01i_num ~ c2_s0_arm + (1|c2_pp_ptid), 
                      family = gaussian(link="identity"), data=ds_cfu)
df_pc_ptest=cbind(summary(glmm_ptest_pc_m)$coef[2,1],confint(glmm_ptest_pc_m)[4,1],confint(glmm_ptest_pc_m)[4,2])
df_pc_ptest=as.data.frame(df_pc_ptest)
rownames(df_pc_ptest)=c("Partner HIV testing")



glmm_pinit_pc_m=glmer(c2_s6_q01t_num ~ c2_s0_arm_num + (1|c2_pp_ptid), 
                      family = gaussian(link="identity"), data=ds_cfu)
df_pc_pinit=cbind(summary(glmm_pinit_pc_m)$coef[2,1],confint(glmm_pinit_pc_m)[4,1],confint(glmm_pinit_pc_m)[4,2])
df_pc_pinit=as.data.frame(df_pc_pinit)
rownames(df_pc_pinit)=c("PrEP initiation")



glmm_pcont_pc_m=glmer(c2_s6_q01w_num ~ c2_s0_arm + (1|c2_pp_ptid), 
                      family = gaussian(link="identity"), data=ds_cfu)
df_pc_pcont=cbind(coef(glmm_padhsc_pc_m)[2],confint(glmm_pcont_pc_m)[4,1],confint(glmm_pcont_pc_m)[4,2])
df_pc_pcont=as.data.frame(df_pc_pcont)
rownames(df_pc_pcont)=c("PrEP continuation")




glmm_padhsc_pc_m=lqmm(fixed = prep_adh_score ~ c2_s0_arm, random= ~1, group = c2_pp_ptid, data=ds_cfu, tau= 0.5, type = "robust", na.action="na.omit")
qreg_padhsc=summary(glmm_padhsc_pc_m)$tTable
df_pc_padhsc=cbind(cbind(qreg_padhsc[2,1],
                         t(qreg_padhsc[2,3:4])))
df_pc_padhsc=as.data.frame(df_pc_padhsc)
rownames(df_pc_padhsc)=c("PrEp adherence score")
colnames(df_pc_padhsc)=names(df_pc_pinit)



df_pc=rbind(
  
  df_pc_anyhivt, df_pc_ptest, df_pc_pinit, df_pc_padhsc
  
)
df_pc_out=data.frame(Var=rownames(df_pc),
                     Estimate=round(df_pc[,1],2),
                     ll=round(df_pc[,2],2),
                     ul=round(df_pc[,3],2),
                     # pval=round(df_pc[,4],3),
                     CI=paste0("(",round(df_pc[,2],2),", ",round(df_pc[,3],2),")"))

df_pc
summary(glmm_pcont_pc_m)$coef
confint(glmm_pcont_pc_m)



## Among index peers, self-reported ----

addmargins(table(ds_pfu$p2_s5_q1_1,ds_pfu$p2_s0_arm))
round(prop.table(table(ds_pfu$p2_s5_q1_1,ds_pfu$p2_s0_arm),margin=2)*100,0)

ds_pfu$p2_s0_arm_num=as.numeric(as.factor(ds_pfu$p2_s0_arm))-1
ds_pfu$p2_s5_q1_1_num=as.numeric(as.factor(ds_pfu$p2_s5_q1_1))-1



glmm_prepcurrent_glmm_m=glmer(p2_s5_q1_1_num ~ p2_s0_arm_num + (1|p2_ptid), 
                              family = gaussian(link="identity"), data=ds_pfu,
                              control=lmerControl(check.nobs.vs.nlev = "ignore",
                                                  check.nobs.vs.rankZ = "ignore",
                                                  check.nobs.vs.nRE="ignore"))
glmm_prepcurrent_glmm=summary(glmm_prepcurrent_glmm_m)$coef
df_client_rprovid_prepcurrent_glmm=cbind(cbind(glmm_prepcurrent_glmm[2,1],
                                               t(confint(glmm_prepcurrent_glmm_m)[4,])),
                                         Anova(glmm_prepcurrent_glmm_m)[3])
df_client_rprovid_prepcurrent_glmm=as.data.frame(df_client_rprovid_prepcurrent_glmm)
rownames(df_client_rprovid_prepcurrent_glmm)=c("Currently using PrEP for prevention")

df_client_rprovid_prepcurrent_glmm


# Table 3 ----
## Process outcomes ----

### Among referred peers, reported by index peers ----


# timestamp enrollment to visit 

ds_pbl[ds_pbl$p1_s5_q1=="01/01/2000",]=NA
ds_pbl$p1_s5_q1=as.Date(ds_pbl$p1_s5_q1)
ds_pbl$p1_s0_visit_date=as.Date(ds_pbl$p1_s0_visit_date)


df_bline=ds_pbl[,c("p1_ptid","p1_s0_arm","p1_s0_visit_date")]
df_bline$p1_s0_arm_num=as.numeric(as.factor(df_bline$p1_s0_arm))-1
# df_bline$p1_s0_arm=ifelse(df_bline$p1_s0_arm==2,0,1)
df_bline=df_bline[,c(1,4,3)]
names(df_bline)=c("ptid","arm","date_enroll")

df_fup=ds_pfu[,c("p2_ptid","p2_s0_arm","p2_s0_visit_date")]
df_fup$p2_s0_arm_num=as.numeric(as.factor(df_fup$p2_s0_arm))-1
df_fup$p2_s0_visit_date=as.Date(df_fup$p2_s0_visit_date)
df_fup=df_fup[,c(1,4,3)]
names(df_fup)=c("ptid","arm","date_fu")


df_fullblfu=merge(df_bline,df_fup,by=c("ptid","arm"), all=T)


df_fullblfu$days_enroll_fu=as.numeric(difftime(df_fullblfu$date_fu,df_fullblfu$date_enroll, units="days"))

summary(df_fullblfu[df_fullblfu$arm==0 & df_fullblfu$days_enroll_fu>1,c("days_enroll_fu")])
summary(df_fullblfu[df_fullblfu$arm==1 & df_fullblfu$days_enroll_fu>1,c("days_enroll_fu")])

df_fullblfu[df_fullblfu$days_enroll_fu==1,]






addmargins(table(df_lcomp$p2_s6_q1_13,df_lcomp$p2_s0_arm))
round(prop.table(table(df_lcomp$p2_s6_q1_13,df_lcomp$p2_s0_arm),margin=2)*100,0)


addmargins(table(df_lcomp$p2_s6_q1_17c,df_lcomp$p2_s0_arm))
round(prop.table(table(df_lcomp$p2_s6_q1_17c,df_lcomp$p2_s0_arm),margin=2)*100,0)



### Among referred peers, self-reported ----

addmargins(table(ds_cfu$c2_s6_q01n,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01n,ds_cfu$c2_s0_arm),margin=2)*100,0)


addmargins(table(ds_cfu$c2_s6_q01o,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01o,ds_cfu$c2_s0_arm),margin=2)*100,0)


addmargins(table(ds_cfu$c2_s6_q01s,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$c2_s6_q01s,ds_cfu$c2_s0_arm),margin=2)*100,0)



## Implementation outcomes ----


### Among index peers, reported by index peers  ----

### Among referred peers, self-reported ----
