## Observational evidence: % rating each criterion "important" (top-2), ANCESTRY (ascriptive)
## vs FEELING (voluntarist). Two sources only: this study's own battery, and ISSP 2023 (latest).
## Double duty: validates the conjoint's ancestry ordering (KR>DE>TW) and motivates it
## (feeling near-ceiling; the battery cannot recover the forced trade-off the conjoint does).
## Source data read-only. Saves observational.csv + figures/fig_observational.png.
suppressMessages({library(readr);library(dplyr);library(tidyr);library(ggplot2)})
NIDX<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/Nat-id-experiments")
IFES<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/IFES2023")
NB<-file.path(path.expand("~"),"Documents/github/research/projects/natid_book/chapters/03-comparative-cross-national/survey-data/ISSP (1995-2023)")
PROJ<-file.path(path.expand("~"),"Documents/github/research/substack/drafts/coethnic-cross-national"); FIG<-file.path(PROJ,"figures")
terra<-"#C25B39"; turq<-"#2E9FA0"; ink<-"#241F18"; cream<-"#FBF3E6"; grayln<-"#6E6049"
rd<-function(p){d<-suppressMessages(read_csv(p,locale=locale(encoding="UTF-8"),show_col_types=FALSE,name_repair="minimal"));nm<-names(d);d[,!duplicated(nm)&!is.na(nm)&nm!=""]}
t2<-function(x,hi){x<-x[!is.na(x)&x!=""]; if(!length(x))return(c(NA,0)); c(round(100*mean(x%in%hi)),length(x))}

rows<-list()
add<-function(src,ctry,anc,feel){rows[[length(rows)+1]]<<-data.frame(Source=src,Country=ctry,
  Ancestry=anc[1],n_anc=anc[2],Feeling=feel[1],n_feel=feel[2])}

## ---- this study's battery (top-2 = important) ----
kr<-rd(file.path(NIDX,"data/ROK23/AKS 2023 - ROK_February 13, 2024_12.47.csv"))[-c(1,2),]   # numeric 1..4 ascending
add("This study","South Korea",
    t2(suppressWarnings(as.integer(kr$Q72_1)),c(3,4)), t2(suppressWarnings(as.integer(kr$Q72_5)),c(3,4)))
tw<-rd(file.path(NIDX,"data/ROC24/ROC Experiments 2023_March 13, 2024_17.35.csv"))[-c(1,2),] # text labels
add("This study","Taiwan",
    t2(tw$Q15_1,c("非常重要","相當重要")), t2(tw$Q15_6,c("非常重要","相當重要")))
de<-rd(file.path(IFES,"Germans/Data/choice_text_IFES+2023+-+German_October+24,+2023_13.32.csv"))[-c(1,2),]
add("This study","Germany",
    t2(de$Q63_1,c("Sehr wichtig","Eher wichtig")), t2(de$Q63_5,c("Sehr wichtig","Eher wichtig")))

## ---- ISSP 2023 (v6 ancestry, v5 feel; important = top-2 {1,2}) ----
if(suppressWarnings(suppressMessages(require(haven,quietly=TRUE)))){
 tryCatch({
  d<-haven::read_dta(file.path(NB,"ISSP 2023/National Identity & Citizenship - ISSP 2023.dta"))
  cc<-trimws(as.character(d$c_alphan)); a<-as.numeric(d$v6); f<-as.numeric(d$v5); a[a<0]<-NA; f[f<0]<-NA
  map<-c(DE="Germany",KR="South Korea",TW="Taiwan")
  for(k in names(map)){i<-which(cc==k); add("ISSP 2023",map[[k]],t2(a[i],c(1,2)),t2(f[i],c(1,2)))}
 },error=function(e)cat("ISSP read error:",conditionMessage(e),"\n"))
} else cat("haven not available; ISSP skipped\n")

ob<-do.call(rbind,rows); ob$Country<-factor(ob$Country,levels=c("South Korea","Germany","Taiwan"))
ob$Source<-factor(ob$Source,levels=c("This study","ISSP 2023"))
write_csv(ob,file.path(PROJ,"analysis","observational.csv"))
cat("===== % RATING IMPORTANT (top-2) =====\n"); print(as.data.frame(ob%>%mutate(gap=Feeling-Ancestry)),row.names=FALSE)

## ---- dumbbell: ancestry (ascriptive) -> feeling (voluntarist) per country, by source ----
long<-ob%>%select(Source,Country,Ancestry,Feeling)
g<-ggplot(long,aes(y=Country))+
  geom_segment(aes(x=Ancestry,xend=Feeling,yend=Country),color=grayln,linewidth=1)+
  geom_point(aes(x=Ancestry),color=terra,size=3.4)+
  geom_point(aes(x=Feeling),color=turq,size=3.4)+
  geom_text(aes(x=Ancestry,label=Ancestry),color=terra,vjust=-1.1,size=2.7)+
  geom_text(aes(x=Feeling,label=Feeling),color=turq,vjust=-1.1,size=2.7)+
  facet_wrap(~Source)+scale_x_continuous(limits=c(20,100))+
  theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
    plot.background=element_rect(fill=cream,color=NA),panel.background=element_rect(fill=cream,color=NA),
    plot.title=element_text(face="bold",color=ink),plot.subtitle=element_text(color=grayln,size=10.5),strip.text=element_text(face="bold"))+
  labs(title="What the battery shows: feeling is near-universal, ancestry is the variable one",
       subtitle="% rating each criterion important for being truly national (top-2). Terracotta = ancestry (ascriptive), teal = feeling (voluntarist).",
       x="% important",y=NULL)
ggsave(file.path(FIG,"fig_observational.png"),g,width=9,height=3.6,dpi=150,bg=cream)
cat("\nwrote figures/fig_observational.png\n")
