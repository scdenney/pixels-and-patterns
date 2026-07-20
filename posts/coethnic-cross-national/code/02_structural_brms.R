## Faithful structural recovery: hierarchical-Bayes mixed logit (random-utility model
## with random coefficients on all attributes, regularized) per country. Recovers the
## DISTRIBUTION of the co-ethnic ancestry preference -- the quantity AMCE averages away.
## This is the proper version of the "structural method"; the NN-indexed estimator of
## Acharya, Hainmueller & Xu (2026) generalizes it. Source data read-only.

suppressMessages({library(readr); library(dplyr); library(tidyr); library(brms); library(posterior)})
set.seed(1)
IFES<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/IFES2023")
NIDX<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/Nat-id-experiments")
PROJ<-file.path(path.expand("~"),"Documents/github/research/substack/drafts/coethnic-cross-national")
common<-c("Origin","Ancestry","Residence","Language","Citizenship","Tradition","Political","Feeling")
rd<-function(p){d<-suppressMessages(read_csv(p,locale=locale(encoding="UTF-8"),show_col_types=FALSE,name_repair="minimal"));nm<-names(d);d[,!duplicated(nm)&!is.na(nm)&nm!=""]}

## attention frames (pass)
de<-rd(file.path(IFES,"Germans/Data/choice_text_IFES+2023+-+German_October+24,+2023_13.32.csv")); de$id<-seq_len(nrow(de))
derf<-de%>%transmute(id, pass=grepl("berhaupt nicht zu",Q114))
krraw<-rd(file.path(NIDX,"data/ROK23/AKS 2023 - ROK_February 13, 2024_12.47.csv"))
krrf<-krraw%>%transmute(ResponseId, pass=suppressWarnings(as.integer(Finished))==1)  # survey completers
twrf<-rd(file.path(NIDX,"data/ROC24/ROC Experiments 2023_March 13, 2024_17.35.csv"))%>%transmute(ResponseId, pass=TRUE)

cfg<-list(
  Germany=list(f=file.path(IFES,"Germans/Data/merged_german.natid.csv"), id="id", co="Hat deutsche Vorfahren", rf=derf, attn=TRUE),
  `South Korea`=list(f=file.path(NIDX,"data/ROK23/merged_df.sk.natidcbc1.csv"), id="ResponseId", co="한국계", rf=krrf, attn=TRUE),  # completers only (Finished==1) -> n=2,000, matches SI native-born ~2,006
  Taiwan=list(f=file.path(NIDX,"data/ROC24/merged_df.tw.natidcbc1.csv"), id="ResponseId", co="漢人", rf=twrf, attn=FALSE))

mkdiff<-function(P){
  d<-rd(P$f); names(d)[names(d)==P$id]<-"uid"; names(P$rf)[1]<-"uid"
  d<-d%>%left_join(P$rf,by="uid")%>%filter(!is.na(person_choice))%>%filter(if_all(all_of(common),~!is.na(.x)))
  if(P$attn) d<-d%>%filter(pass%in%TRUE)
  oth<-setdiff(unique(d$Ancestry),P$co); d$Ancestry<-factor(d$Ancestry,levels=c(oth[1],P$co))
  for(mk in setdiff(common,"Ancestry")) d[[mk]]<-factor(d[[mk]])
  mc<-tapply(d$person_choice,d$Feeling,mean,na.rm=TRUE)  # orient Feeling only (focal); other attrs kept natural + oriented for DISPLAY in 03 (so the focal fit is unperturbed)
  if(length(mc)==2 && !is.na(mc[2]) && mc[2]<mc[1]) d$Feeling<-factor(d$Feeling,levels=rev(levels(d$Feeling)))
  hi<-function(x) as.integer(x==levels(x)[2])
  num<-d%>%mutate(across(all_of(common),hi))%>%separate(question_profile,c("q","p"),sep="\\.",convert=TRUE,remove=FALSE)
  p1<-num%>%filter(p==1)%>%select(uid,q,y=person_choice,all_of(common)); p2<-num%>%filter(p==2)%>%select(uid,q,all_of(common))
  names(p1)[names(p1)%in%common]<-paste0(common,"_1"); names(p2)[names(p2)%in%common]<-paste0(common,"_2")
  m<-inner_join(p1,p2,by=c("uid","q"))%>%filter(!is.na(y)); for(mk in common) m[[paste0("d_",mk)]]<-m[[paste0(mk,"_1")]]-m[[paste0(mk,"_2")]]
  m
}
dv<-paste0("d_",common)
form<-bf(as.formula(paste("y ~ 0 +",paste(dv,collapse=" + "),
        "+ (0 +",paste(dv,collapse=" + "),"|| uid)")), family=bernoulli("logit"))
pri<-c(prior(normal(0,2.5),class=b), prior(normal(0,1),class=sd))

res<-list()
for(nm in names(cfg)){
  m<-mkdiff(cfg[[nm]])
  cat("\n==== fitting", nm, "(", length(unique(m$uid)), "resp,", nrow(m), "tasks ) ====\n")
  fit<-brm(form, data=m, prior=pri, chains=2, iter=1500, warmup=750, cores=2,
           control=list(adapt_delta=0.9, max_treedepth=11), seed=1, refresh=0, silent=2)
  dr<-as_draws_df(fit); b<-dr$b_d_Ancestry; s<-dr[["sd_uid__d_Ancestry"]]
  bf<-dr$b_d_Feeling; sf<-dr[["sd_uid__d_Feeling"]]                 # voluntarist counterpart
  sh<-function(t) 1-pnorm((t-b)/s)            # per-draw share with individual weight >= t
  shf<-function(t) 1-pnorm((t-bf)/sf)
  pull<-function(x) c(est=mean(x), lo=quantile(x,.025,names=FALSE), hi=quantile(x,.975,names=FALSE))
  allw<-do.call(rbind,lapply(common,function(a){bx<-dr[[paste0("b_d_",a)]]   # full 8-attribute hierarchy (oriented to demand)
    data.frame(attr=a, est=mean(bx), lo=quantile(bx,.025,names=FALSE), hi=quantile(bx,.975,names=FALSE))}))
  res[[nm]]<-list(name=nm, n=length(unique(m$uid)),
    mean=pull(b), sd=pull(s),
    prefer=pull(sh(0)), reject=pull(pnorm((0-b)/s)),
    moderate=pull(sh(0.5)), strong=pull(sh(1)),
    draws=data.frame(b=b, s=s), all=allw,   # full posterior draws + the whole attribute hierarchy
    feeling=list(mean=pull(bf), sd=pull(sf), prefer=pull(shf(0)), reject=pull(pnorm((0-bf)/sf)),
                 moderate=pull(shf(0.5)), strong=pull(shf(1)), draws=data.frame(b=bf,s=sf)))
  cat(sprintf("  ANCESTRY %.2f | FEELING %.2f | top: %s %.2f | bottom: %s %.2f\n",
      mean(b), mean(bf), allw$attr[which.max(allw$est)], max(allw$est), allw$attr[which.min(allw$est)], min(allw$est)))
}
saveRDS(res, file.path(PROJ,"analysis","results_brms.rds"))
cat("\n==== BRMS STRUCTURAL FITS DONE ====\n")
