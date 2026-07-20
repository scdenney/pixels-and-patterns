## Presentation layer: tables + figures from cached results.
## Loads results_brms.rds (the slow structural fits -- NOT re-run here) and recomputes
## the fast design-based quantities (AMCE, battery) inline. Drop party. Battery supplementary.

suppressMessages({library(readr); library(dplyr); library(tidyr); library(cregg); library(ggplot2)})
IFES<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/IFES2023")
NIDX<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/Nat-id-experiments")
PROJ<-file.path(path.expand("~"),"Documents/github/research/substack/drafts/coethnic-cross-national")
FIG<-file.path(PROJ,"figures"); dir.create(FIG,showWarnings=FALSE)
terra<-"#C25B39"; turq<-"#2E9FA0"; cobalt<-"#1F4E9B"; gold<-"#E0A526"; ink<-"#17171A"; cream<-"#F3EFE3"; grayln<-"#6E6E73"
pal<-c(`South Korea`=terra, Germany=cobalt, Taiwan=gold)
ord<-c("South Korea","Germany","Taiwan")
theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  plot.background=element_rect(fill=cream,color=NA), panel.background=element_rect(fill=cream,color=NA),
  plot.title=element_text(face="bold",color=ink), plot.subtitle=element_text(color=grayln,size=10.5),
  legend.position="top", strip.text=element_text(face="bold")))
common<-c("Origin","Ancestry","Residence","Language","Citizenship","Tradition","Political","Feeling")
rd<-function(p){d<-suppressMessages(read_csv(p,locale=locale(encoding="UTF-8"),show_col_types=FALSE,name_repair="minimal"));nm<-names(d);d[,!duplicated(nm)&!is.na(nm)&nm!=""]}

## ---- fast design-based quantities (AMCE + battery), attention-filtered ----
de<-rd(file.path(IFES,"Germans/Data/choice_text_IFES+2023+-+German_October+24,+2023_13.32.csv")); de$id<-seq_len(nrow(de))
deimp<-c("Überhaupt nicht wichtig"=1,"Nicht sehr wichtig"=2,"Eher wichtig"=3,"Sehr wichtig"=4)
derf<-de%>%transmute(id,pass=grepl("berhaupt nicht zu",Q114),imp=unname(deimp[as.character(Q63_1)]))
krraw<-rd(file.path(NIDX,"data/ROK23/AKS 2023 - ROK_February 13, 2024_12.47.csv"))
krrf<-krraw%>%transmute(ResponseId,pass=suppressWarnings(as.integer(Finished))==1,imp=suppressWarnings(as.integer(Q72_1)))  # survey completers
twraw<-rd(file.path(NIDX,"data/ROC24/ROC Experiments 2023_March 13, 2024_17.35.csv"))
twimp<-c("非常不重要"=1,"不太重要"=2,"相當重要"=3,"非常重要"=4)
twrf<-twraw%>%transmute(ResponseId,pass=TRUE,imp=unname(twimp[as.character(Q15_1)]))
cfg<-list(
  `South Korea`=list(f=file.path(NIDX,"data/ROK23/merged_df.sk.natidcbc1.csv"),id="ResponseId",co="한국계",rf=krrf,attn=TRUE),  # completers only (Finished==1) -> n=2,000
  Germany=list(f=file.path(IFES,"Germans/Data/merged_german.natid.csv"),id="id",co="Hat deutsche Vorfahren",rf=derf,attn=TRUE),
  Taiwan=list(f=file.path(NIDX,"data/ROC24/merged_df.tw.natidcbc1.csv"),id="ResponseId",co="漢人",rf=twrf,attn=FALSE))
amce<-list(); batt<-list(); valid<-list()
for(nm in names(cfg)){ P<-cfg[[nm]]
  d<-rd(P$f); names(d)[names(d)==P$id]<-"uid"; names(P$rf)[1]<-"uid"
  d<-d%>%left_join(P$rf,by="uid")%>%filter(!is.na(person_choice))%>%filter(if_all(all_of(common),~!is.na(.x)))
  if(P$attn) d<-d%>%filter(pass%in%TRUE)
  oth<-setdiff(unique(d$Ancestry),P$co); d$Ancestry<-factor(d$Ancestry,levels=c(oth[1],P$co))
  for(mk in setdiff(common,"Ancestry")) d[[mk]]<-factor(d[[mk]])
  a<-cj(as.data.frame(d),as.formula(paste("person_choice ~",paste(common,collapse=" + "))),id=~uid,estimate="amce")%>%
     filter(feature=="Ancestry",estimate!=0)
  amce[[nm]]<-c(est=a$estimate[1],se=a$std.error[1])
  bd<-d%>%distinct(uid,imp)%>%filter(!is.na(imp))
  batt[[nm]]<-data.frame(Country=nm,imp=1:4,share=100*as.numeric(table(factor(bd$imp,levels=1:4)))/nrow(bd))
  d$imp_f<-factor(d$imp,levels=1:4,labels=c("Not at all","Not very","Somewhat","Very"))
  mm<-cj(as.data.frame(d%>%filter(!is.na(imp_f))),person_choice~Ancestry,id=~uid,estimate="mm",by=~imp_f)%>%
      filter(level==P$co)%>%transmute(Country=nm,imp_f,mm=estimate,se=std.error)
  valid[[nm]]<-mm
}

## ---- structural results (cached brms) ----
br<-readRDS(file.path(PROJ,"analysis","results_brms.rds"))

## ---- summary table ----
tab<-do.call(rbind,lapply(ord,function(nm){x<-br[[nm]]; data.frame(
  Country=nm, n=x$n, AMCE=round(amce[[nm]]["est"],3), AMCE_se=round(amce[[nm]]["se"],3),
  weight=round(x$mean["est"],2), w_lo=round(x$mean["lo"],2), w_hi=round(x$mean["hi"],2),
  prefer=round(100*x$prefer["est"]), reject=round(100*x$reject["est"]),
  moderate=round(100*x$moderate["est"]), mod_lo=round(100*x$moderate["lo"]), mod_hi=round(100*x$moderate["hi"]),
  strong=round(100*x$strong["est"]), strong_lo=round(100*x$strong["lo"]), strong_hi=round(100*x$strong["hi"]))}))
write_csv(tab,file.path(PROJ,"analysis","summary_structural.csv"))
cat("===== SUMMARY =====\n"); print(tab,row.names=FALSE)

## ---- Fig 1: AMCE vs structural weight (the methodological point) ----
amdf<-do.call(rbind,lapply(ord,function(nm) data.frame(Country=nm,est=amce[[nm]]["est"],se=amce[[nm]]["se"],panel="AMCE\n(change in Pr chosen)")))
stdf<-do.call(rbind,lapply(ord,function(nm){x<-br[[nm]];data.frame(Country=nm,est=x$mean["est"],lo=x$mean["lo"],hi=x$mean["hi"],panel="Structural weight\n(random-utility, logit)")}))
amdf$lo<-amdf$est-1.96*amdf$se; amdf$hi<-amdf$est+1.96*amdf$se; amdf$se<-NULL
both<-rbind(amdf,stdf); both$Country<-factor(both$Country,levels=ord)
g1<-ggplot(both,aes(Country,est,fill=Country))+geom_col(width=.62)+
  geom_errorbar(aes(ymin=lo,ymax=hi),width=.14,color=ink)+
  facet_wrap(~panel,scales="free_y")+scale_fill_manual(values=pal,guide="none")+
  geom_hline(yintercept=0,color=grayln)+
  labs(title="Two estimands, two different exceptions",
       subtitle="Co-ethnic ancestry. On the average, Taiwan is the low outlier and Korea resembles Germany.\nOn the structural weight, Korea is the outlier and Germany resembles Taiwan.",
       x=NULL,y=NULL)
ggsave(file.path(FIG,"fig_amce_vs_structural.png"),g1,width=8.8,height=4.3,dpi=150,bg=cream)

## ---- Fig 2: structural preference distribution by country ----
xs<-seq(-2,3,length.out=400)
dens<-do.call(rbind,lapply(ord,function(nm){x<-br[[nm]];data.frame(Country=nm,x=xs,d=dnorm(xs,x$mean["est"],x$sd["est"]))}))
dens$Country<-factor(dens$Country,levels=ord)
g2<-ggplot(dens,aes(x,d,color=Country))+
  geom_vline(xintercept=0,color=ink,linewidth=.5)+
  geom_vline(xintercept=c(0.5,1),color=grayln,linetype=2,linewidth=.4)+
  geom_line(linewidth=1.15)+scale_color_manual(values=pal)+
  annotate("text",x=-0.04,y=max(dens$d)*0.99,label="reject ←",hjust=1,size=2.9,color=grayln)+
  annotate("text",x=0.04,y=max(dens$d)*0.99,label="→ prefer",hjust=0,size=2.9,color=grayln)+
  annotate("text",x=0.53,y=max(dens$d)*0.42,label="moderate\n(>0.5)",hjust=0,size=2.7,color=grayln,lineheight=.85)+
  annotate("text",x=1.03,y=max(dens$d)*0.60,label="strong\n(>1)",hjust=0,size=2.7,color=grayln,lineheight=.85)+
  labs(title="What the average hides",
       subtitle="Distribution of the individual utility weight on co-ethnic ancestry, by country, with the prefer (0), moderate (0.5)\nand strong (1) thresholds. Korea's mass sits farthest right, Germany and Taiwan cluster together to the left.",
       x="Individual utility weight on co-ethnic ancestry",y="density",color=NULL)
ggsave(file.path(FIG,"fig_structural_dist.png"),g2,width=8.6,height=4.4,dpi=150,bg=cream)

## (No standalone shares figure: lead with the structural mean and the prefer/reject
##  split in the table; the strong-preference share goes to a footnote in the writeup
##  because the upper tail is weakly identified with 4-6 tasks.)

## ---- supplementary: battery validation (one panel) ----
vdf<-do.call(rbind,valid); vdf$Country<-factor(vdf$Country,levels=ord); vdf<-vdf%>%filter(!is.na(imp_f))
g4<-ggplot(vdf,aes(imp_f,mm,color=Country,group=Country))+geom_hline(yintercept=.5,linetype=2,color=grayln)+
  geom_line(linewidth=.9)+geom_point(size=2)+scale_color_manual(values=pal)+
  labs(title="The conjoint tracks the direct item",
       subtitle="Marginal mean for a co-ethnic profile, by stated importance of co-ethnic ancestry, in each country",
       x="Stated importance of co-ethnic ancestry",y="MM for co-ethnic profile",color=NULL)
ggsave(file.path(FIG,"fig_battery_validation.png"),g4,width=8.2,height=4,dpi=150,bg=cream)

## ---- Substack teaser: ascriptive (ancestry) vs voluntarist (feeling), brms scale ----
if(!is.null(br[[ord[1]]]$feeling)){
  av<-do.call(rbind,lapply(ord,function(nm){x<-br[[nm]]; rbind(
    data.frame(Country=nm,criterion="Ancestry (ascriptive)",est=x$mean["est"],lo=x$mean["lo"],hi=x$mean["hi"]),
    data.frame(Country=nm,criterion="Feeling (voluntarist)",est=x$feeling$mean["est"],lo=x$feeling$mean["lo"],hi=x$feeling$mean["hi"]))}))
  av$Country<-factor(av$Country,levels=ord); av$criterion<-factor(av$criterion,levels=c("Feeling (voluntarist)","Ancestry (ascriptive)"))
  g5<-ggplot(av,aes(Country,est,color=criterion))+
    geom_hline(yintercept=0,color=grayln,linewidth=.4)+
    geom_point(size=3.2,position=position_dodge(.45))+geom_errorbar(aes(ymin=lo,ymax=hi),width=.14,linewidth=.8,position=position_dodge(.45))+
    scale_color_manual(values=c(`Ancestry (ascriptive)`=terra,`Feeling (voluntarist)`=turq))+
    labs(title="Two criteria, opposite directions: ancestry divides, feeling unites",
         subtitle="Structural utility weight (hierarchical-Bayes mixed logit). Feeling is demanded most where ancestry is demanded least.",
         x=NULL,y="Structural utility weight",color=NULL)
  ggsave(file.path(FIG,"fig_ascriptive_voluntarist.png"),g5,width=8.6,height=4.1,dpi=150,bg=cream)
  cat(sprintf("\nFEELING vs ANCESTRY (structural mean):\n"))
  for(nm in ord) cat(sprintf("  %-11s ancestry %.2f [%.2f,%.2f]  feeling %.2f [%.2f,%.2f]\n",nm,
    br[[nm]]$mean["est"],br[[nm]]$mean["lo"],br[[nm]]$mean["hi"],
    br[[nm]]$feeling$mean["est"],br[[nm]]$feeling$mean["lo"],br[[nm]]$feeling$mean["hi"]))
}

## ---- research note: full 8-attribute structural hierarchy (brms, oriented to demand) ----
if(!is.null(br[[ord[1]]]$all)){
  hi8<-do.call(rbind,lapply(ord,function(nm){x<-br[[nm]]$all; x$Country<-nm; x}))
  fl<-hi8$est<0   # orient each attribute to its demanded direction for display (magnitude of demand)
  hi8$.lo<-ifelse(fl,-hi8$hi,hi8$lo); hi8$.hi<-ifelse(fl,-hi8$lo,hi8$hi)
  hi8$est<-abs(hi8$est); hi8$lo<-hi8$.lo; hi8$hi<-hi8$.hi; hi8$.lo<-NULL; hi8$.hi<-NULL
  o<-hi8%>%group_by(attr)%>%summarise(m=mean(est),.groups="drop")%>%arrange(m)%>%pull(attr)
  hi8$attr<-factor(hi8$attr,levels=o); hi8$Country<-factor(hi8$Country,levels=ord)
  hi8$focal<-ifelse(hi8$attr=="Ancestry","Ancestry (ascriptive)",ifelse(hi8$attr=="Feeling","Feeling (voluntarist)","other"))
  g6<-ggplot(hi8,aes(est,attr,color=focal))+
    geom_errorbarh(aes(xmin=lo,xmax=hi),height=.25,linewidth=.55)+geom_point(size=2.4)+
    facet_wrap(~Country)+scale_color_manual(values=c(`Ancestry (ascriptive)`=terra,`Feeling (voluntarist)`=turq,other=grayln),name=NULL)+
    theme(legend.position="top")+
    labs(title="The architecture of nationhood: where ancestry and feeling sit among all criteria",
         subtitle="Structural utility weight per attribute (HB mixed logit), demanded direction. Ancestry ranks low everywhere; feeling ranks high.",
         x="Structural utility weight",y=NULL)
  ggsave(file.path(FIG,"fig_hierarchy.png"),g6,width=9.8,height=4.6,dpi=150,bg=cream)
  write_csv(hi8,file.path(PROJ,"analysis","hierarchy.csv"))
}

bt<-do.call(rbind,batt); write_csv(bt,file.path(PROJ,"analysis","battery_dist.csv"))
saveRDS(list(tab=tab,amce=amce,brms=br,valid=valid,battery=bt),file.path(PROJ,"analysis","results_present.rds"))
cat("\nfigures written:\n"); cat(list.files(FIG,pattern="png"),sep="\n")
cat("\n===== DONE =====\n")
