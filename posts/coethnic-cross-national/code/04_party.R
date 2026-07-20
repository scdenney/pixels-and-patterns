## Supplementary: co-ethnic preference by party group. THREE parallel groups per country --
## a left/progressive pole, a centrist-or-unaffiliated middle, and a right/co-ethnic pole --
## so the cross-national comparison is like-for-like. Co-ethnic utility WEIGHT (differenced
## conditional logit, same scale as Table 1) per group; the MM scale is kept only for the note.
## Pairwise significance tests between groups (disjoint subsets -> independent SEs).
## Source data read-only. Saves party_weight.csv, party_tests.csv, party_n.csv, fig_party.png.
suppressMessages({library(readr);library(dplyr);library(tidyr);library(cregg);library(ggplot2)})
IFES<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/IFES2023")
NIDX<-file.path(path.expand("~"),"Documents/github/research/projects/_archive/Nat-id-experiments")
PROJ<-file.path(path.expand("~"),"Documents/github/research/substack/drafts/coethnic-cross-national")
FIG<-file.path(PROJ,"figures")
terra<-"#C25B39"; turq<-"#2E9FA0"; gold<-"#E4B33E"; ink<-"#241F18"; cream<-"#FBF3E6"; grayln<-"#6E6049"
common<-c("Origin","Ancestry","Residence","Language","Citizenship","Tradition","Political","Feeling")
rd<-function(p){d<-suppressMessages(read_csv(p,locale=locale(encoding="UTF-8"),show_col_types=FALSE,name_repair="minimal"));nm<-names(d);d[,!duplicated(nm)&!is.na(nm)&nm!=""]}
theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  plot.background=element_rect(fill=cream,color=NA), panel.background=element_rect(fill=cream,color=NA),
  plot.title=element_text(face="bold",color=ink), plot.subtitle=element_text(color=grayln,size=10.5),
  legend.position="top", strip.text=element_text(face="bold")))

## completion/attention + 3-way party-group frames (key = the conjoint's respondent id column)
kr<-rd(file.path(NIDX,"data/ROK23/AKS 2023 - ROK_February 13, 2024_12.47.csv"))
krf<-kr%>%transmute(key=ResponseId, pass=suppressWarnings(as.integer(Finished))==1,
  bloc=case_when(suppressWarnings(as.integer(Q8))%in%c(1,7)~"Conservative",
                 suppressWarnings(as.integer(Q8))%in%c(2,3,4,5,6)~"Progressive",
                 suppressWarnings(as.integer(Q8))==8~"Independent", TRUE~NA_character_))
tw<-rd(file.path(NIDX,"data/ROC24/ROC Experiments 2023_March 13, 2024_17.35.csv"))
twf<-tw%>%transmute(key=ResponseId, pass=TRUE,
  bloc=case_when(Q5=="民進黨"~"DPP", Q5=="台灣民眾黨"~"TPP", Q5=="國民黨"~"KMT", TRUE~NA_character_))
de<-rd(file.path(IFES,"Germans/Data/choice_text_IFES+2023+-+German_October+24,+2023_13.32.csv")); de$key<-seq_len(nrow(de))
afd<-grepl("(AfD)",de$Q11,fixed=TRUE)
cen<-grepl("Christlich Demokratische",de$Q11)|grepl("Freie Demokratische",de$Q11)
lef<-grepl("Sozialdemokratische",de$Q11)|grepl("90/Die",de$Q11)|grepl("LINKE",de$Q11)
def<-de%>%transmute(key, pass=grepl("berhaupt nicht zu",Q114),
  bloc=ifelse(afd,"AfD",ifelse(cen,"Center",ifelse(lef,"Left",NA_character_))))

## lev = x-axis order (left pole -> middle -> right/co-ethnic pole); pole = c(left,right) for the gap test
cfg<-list(
 `South Korea`=list(f=file.path(NIDX,"data/ROK23/merged_df.sk.natidcbc1.csv"),id="ResponseId",co="한국계",fr=krf,attn=TRUE,
    lev=c("Progressive","Independent","Conservative"),pole=c("Progressive","Conservative")),
 Germany=list(f=file.path(IFES,"Germans/Data/merged_german.natid.csv"),id="id",co="Hat deutsche Vorfahren",fr=def,attn=TRUE,
    lev=c("Left","Center","AfD"),pole=c("Left","AfD")),
 Taiwan=list(f=file.path(NIDX,"data/ROC24/merged_df.tw.natidcbc1.csv"),id="ResponseId",co="漢人",fr=twf,attn=FALSE,
    lev=c("DPP","TPP","KMT"),pole=c("DPP","KMT")))
ord<-c("South Korea","Germany","Taiwan")
dv<-paste0("d_",common); form<-as.formula(paste("y ~ 0 +",paste(dv,collapse=" + ")))

wres<-list(); mres<-list(); ns<-list(); tests<-list()
for(nm in names(cfg)){P<-cfg[[nm]]
  d<-rd(P$f); names(d)[names(d)==P$id]<-"key"
  d<-d%>%left_join(P$fr,by="key")%>%filter(!is.na(person_choice))%>%filter(if_all(all_of(common),~!is.na(.x)))
  if(P$attn) d<-d%>%filter(pass%in%TRUE)
  d<-d%>%filter(!is.na(bloc))
  oth<-setdiff(unique(d$Ancestry),P$co); d$Ancestry<-factor(d$Ancestry,levels=c(oth[1],P$co))
  for(mk in setdiff(common,"Ancestry")) d[[mk]]<-factor(d[[mk]])
  d$bloc<-factor(d$bloc,levels=P$lev)
  ## reference MM (compressed scale) per group, kept for the note
  mres[[nm]]<-cj(as.data.frame(d),person_choice~Ancestry,id=~key,estimate="mm",by=~bloc)%>%
      filter(level==P$co)%>%transmute(Country=nm,bloc=as.character(bloc),mm=estimate)
  ## structural weight by group: differenced conditional logit, coef on d_Ancestry
  hi<-function(x) as.integer(x==levels(x)[2])
  num<-d%>%mutate(across(all_of(common),hi))%>%separate(question_profile,c("q","p"),sep="\\.",convert=TRUE,remove=FALSE)
  p1<-num%>%filter(p==1)%>%select(key,q,bloc,y=person_choice,all_of(common))
  p2<-num%>%filter(p==2)%>%select(key,q,all_of(common))
  names(p1)[names(p1)%in%common]<-paste0(common,"_1"); names(p2)[names(p2)%in%common]<-paste0(common,"_2")
  m<-inner_join(p1,p2,by=c("key","q"))%>%filter(!is.na(y))
  for(mk in common) m[[paste0("d_",mk)]]<-m[[paste0(mk,"_1")]]-m[[paste0(mk,"_2")]]
  est<-sapply(P$lev,function(b){s<-summary(glm(form,binomial,data=subset(m,bloc==b)))$coefficients["d_Ancestry",];c(w=unname(s[1]),se=unname(s[2]))})
  wr<-data.frame(Country=nm,bloc=P$lev,w=est["w",],se=est["se",],lo=est["w",]-1.96*est["se",],hi=est["w",]+1.96*est["se",])
  wres[[nm]]<-wr
  ## pairwise tests (disjoint groups -> independent): diff, se, z, p
  pr<-combn(P$lev,2,simplify=FALSE)
  tt<-do.call(rbind,lapply(pr,function(pp){a<-wr[wr$bloc==pp[2],]; b<-wr[wr$bloc==pp[1],]
     diff<-a$w-b$w; se<-sqrt(a$se^2+b$se^2); z<-diff/se
     data.frame(Country=nm,comparison=paste(pp[2],"-",pp[1]),diff=diff,se=se,z=z,p=2*pnorm(-abs(z)))}))
  tt$pole<-tt$comparison==paste(P$pole[2],"-",P$pole[1])
  tests[[nm]]<-tt
  ns[[nm]]<-d%>%distinct(key,bloc)%>%count(bloc)%>%mutate(Country=nm)
}
pw<-do.call(rbind,wres); pm<-do.call(rbind,mres); nn<-do.call(rbind,ns); tb<-do.call(rbind,tests)
star<-function(p) ifelse(p<.01,"**",ifelse(p<.05,"*",ifelse(p<.1,"+","")))
tb$sig<-star(tb$p)
cat("===== CO-ETHNIC STRUCTURAL WEIGHT BY GROUP =====\n"); print(as.data.frame(pw),row.names=FALSE,digits=3)
cat("\n===== PAIRWISE TESTS (disjoint groups) =====\n"); print(as.data.frame(tb),row.names=FALSE,digits=3)
cat("\n===== POLE GAPS (right - left) =====\n"); print(tb[tb$pole,c("Country","comparison","diff","p","sig")],row.names=FALSE,digits=3)
cat("\n===== MM by group (reference) =====\n"); print(as.data.frame(pm),row.names=FALSE,digits=3)
cat("\n===== respondents per group =====\n"); print(as.data.frame(nn),row.names=FALSE)
write_csv(pw,file.path(PROJ,"analysis","party_weight.csv")); write_csv(tb,file.path(PROJ,"analysis","party_tests.csv"))
write_csv(nn,file.path(PROJ,"analysis","party_n.csv")); write_csv(pm,file.path(PROJ,"analysis","party_mm.csv"))

## figure: structural weight by group, facet by country (free x), 0 = indifference
pw$Country<-factor(pw$Country,levels=ord)
pw$bloc<-factor(pw$bloc,levels=c("Progressive","Independent","Conservative","Left","Center","AfD","DPP","TPP","KMT"))
mid<-c(`South Korea`="Independent",Germany="Center",Taiwan="TPP")
lab<-tb[tb$pole,]%>%transmute(Country=factor(Country,levels=ord),
  bloc=factor(unname(mid[as.character(Country)]),levels=levels(pw$bloc)), w=max(pw$hi)*1.08,
  label=sprintf("pole gap %+.2f%s",diff,ifelse(p<.1,sprintf(", p=%.2f",p),", n.s.")))
g<-ggplot(pw,aes(bloc,w,color=Country))+
  geom_hline(yintercept=0,color=ink,linewidth=.4)+
  geom_errorbar(aes(ymin=lo,ymax=hi),width=.16,linewidth=.7)+geom_point(size=2.9)+
  geom_text(data=lab,aes(label=label),color=grayln,size=2.8,vjust=1,show.legend=FALSE)+
  facet_wrap(~Country,scales="free_x")+
  scale_color_manual(values=c(`South Korea`=terra,Germany=gold,Taiwan=turq),guide="none")+
  labs(title="Co-ethnic preference by party: a cleavage in Taiwan, a consensus in Korea",
       subtitle="Co-ethnic utility weight (differenced logit, same scale as Table 1) by party group. 0 = indifference.\nAnnotation: gap between the most and least co-ethnic party (pole gap), with its p-value.",
       x="Party group",y="Co-ethnic utility weight")
ggsave(file.path(FIG,"fig_party.png"),g,width=9.4,height=4.2,dpi=150,bg=cream)
cat("\nwrote figures/fig_party.png\n")
