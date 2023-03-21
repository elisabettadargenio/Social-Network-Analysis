load('F:/Social Media/Testo e dati prova pratica-20200707/Political_Network_ITA.Rdata')
str(Y)
head(Y)
head(X)

##CONVERTIAMO IN RETE
library(igraph)

net<-graph_from_adjacency_matrix(Y, mode= 'undirected')
summary(net)
V(net)
E(net)

#assegnamo degli attributi(covariate) alla nostra rete
#le abbiamo nella matrice X, vogliamo assegnarle alla rete

for (j in 1:NCOL(X)){
  net= net %>% set_vertex_attr(name=colnames(X)[j],
                               value = X[,j]
  )}

list.vertex.attributes(net)

##STATISTICHE DESCRITTIVE
#grado 
D_net<-degree(net) #ogni elemento è il grado del nodo specifico
#quali sono i nodi con un grado più alto?
sort(D_net, decreasing = T)[1:10]  #i primi 5 nodi col grado maggiore

which(D_net<1)
#nessun nodo è sconnesso completamente

hist(D_net, nclass = 100, 
     main = 'distribuzione del grado')
#la distribuzione del grado non ha un decadimento esponenziale, il grado dei nodi si concentra
#principalmente fino al valore 50 per poi scendere più lentamente fino a 120 circa.
#pochissimi nodi hanno più di 150 connessioni, il valore massimo è 344.

D_net_norm<-degree(net, normalized = T)
hist(D_net_norm, nclass = 100, main='distribuzione del grado', col = '#6699FF')

#cambia solo la scala, la distribuzione è la stessa
#ora possiamo dire che la maggior parte dei nodi presenta
#connessioni che è inferiore al 10% delle connessioni che 
#avrebbero potuto formare
#quelli che ne hanno molte, ora vediamo che i valori più alti
#si collocano su distribuzioni fino ad oltre il 50%. Si connettono in media con 1 nodo su 2
#Diciamo quindi che è una rete molto collegata


#Statistiche di centralità
C_net=closeness(graph=net)
#non dà warnings-> nessun nodo è disconnesso dalla rete
B_net<-betweenness(graph = net)
hist(C_net, nclass = 100, main = 'closeness')
#la closeness ha una distribuzione normale
hist(B_net, nclass = 100, main = 'betweennes')
#un numero elevato di nodi con betweennes bassa, quindi sono periferici/isolati.
#ovvero che quando un info si muove nella rete questi nodi non vengono toccati
#la betweenness decade in modo più rapido di quanto succedesse col grado

C_net_norm = closeness(graph = net,normalized = T)
B_net_norm = betweenness(graph = net,normalized = T)

par(mfrow=c(1,2))
hist(C_net_norm, nclass = 50, main = 'closeness',  col = '#6699FF')
hist(B_net_norm, nclass = 100, main = 'betweennes',  col = '#6699FF')
par(mfrow=c(1,1))
sum(B_net_norm > .05)
#solo tre nodi ha la betweennes superiore a 0.05

sort(D_net_norm,decreasing = T)[1:5]
#i nodi con grado più alto sono ANGELA ROSARIA NISSOLI, IVAN SCALFAROTTO, ROBERTO GIACCHETTI,
#LORENZO BASSO, DANIELA SBROLLINI. Sono i nodi che creano più connessioni nella rete


sort(C_net_norm,decreasing = T)[1:5]
#i nodi con closeness più alta sono ANGELA ROSARIA NISSOLI, LORENZO BASSO, ROBERTO GIACCHETTI,
#IVAN SCALFAROTTO, DANIELA SBROLLINI. Sono i nodi per cui è più facile raggiungere qualsiasi altro 
#nodo della rete. La comunicazione per loro è più veloce e a soggetti diversi.
sort(B_net_norm,decreasing = T)[1:5]
#i nodi con betweenness più alta sono ANGELA ROSARIA NISSOLI, ROBERTO GIACCHETTI, IVAN SCALFAROTTO,
#LAURA GARAVINI, SERENA PELLEGRINO. Questi sono i nodi con un ruolo decisivo nella rete




DBC = data.frame(den=D_net_norm, clos = C_net_norm, bet = B_net_norm, party=aType,
                 sex=vertex_attr(net)$sex, age=vertex_attr(net)$age)



hist(nclass = 10, xlim=c(30,90), as.numeric(age), col = '#6699FF', main = 'Distribuzione età', xlab = 'Età', ylab = 'Frequenza')
hist(table(dat$party))
barplot(prop.table(table(dat$party)), cex.names = 0.4, horiz = T )
coord_flip(barplot(prop.table(table(dat$party)), cex.names = 0.4))

pie(prop.table(table(dat$party)),col = terrain.colors(10))
##########FARE ISTOGRAMMI#######

summary(as.numeric(dat$age))
#valutiamo la correlazione empirica, queste sono tutte quantità continue,
#quindi possiamo vedere se all'aumentare del grado variano closeness e betweennes
cor(cbind(D_net,C_net,B_net)) 

#Ci dice che sono tutte positive e non crescono in magnitudine uguale
#tra grado e closeness, c'è correlazione pari a 0.74,
#mentre tra grado e betweennes c'è correlazione pari a 0.56
#i nodi che formano tante connessioni (grado maggiore) sono anche centrali in media 

require(GGally)
ggpairs((data.frame(B_net,D_net,C_net))) + theme_bw()


#numero di connessioni che abbiamo osservato??
edge_density(net)
#rispetto a tutte le connessioni che abbiamo osservato siamo intorno 
#al 7,6 per cento. è una rete molto densa rispetto alle reti reali che spesso sono molto sparse


##Misure basate sulle distanze

shortest_paths(net, from='ANGELA ROSARIA NISSOLI', to= 'MARINA SERENI')

#prendiamo la lunghezza del cammino più lungo=diametro
diameter(net)
#lo shortest path maggiore, ovvero il massimo numero di step che bisogna fare per passare da un 
#nodo qualsiasi della rete a un altro nodo qualsiasi è di 5 step.
#i due nodi più distanti all'interno della rete, distano 5 archi tra di loro
#risultato prevedibile dal fatto che la rete è molto densa.
#piccolo mondo-> appartengono tutti a cerchie molto vicine tra loro



#prendiamo la media dei cammini nella rete
average.path.length(net)
#in media ci aspettiamo di poter raggiungere un altro nodo con un cammino pari a 2
#la rete è molto ben collegata





############OMOFILIA RISPETTO AL PARTITO POLITICO#########


aType<- vertex_attr(net, name = 'party') #in input la rete e il nome dell'attributo

sort(table(aType))

require(tidyverse)
df_pl = data.frame(den=D_net_norm, clos = C_net_norm, bet = B_net_norm, Partito=aType)
#come varia la distribuzione del grado e centralità tra i 3 gruppi maggiori?

df_pl %>% filter(Partito %in% c("Forza Italia (Il Popolo della Liberta')",
                          "Movimento 5 Stelle","Partito Democratico")) %>%
  gather(.,"den","clos","bet",key="stat",value = "value") %>%
  ggplot(aes(value,after_stat(density),color=Partito)) + geom_freqpoly(lwd=1)+
  facet_wrap(~stat,scales="free")+ theme_bw()



#non ci sono differenze a livello di betweenness, mentre i grafici di closeness e grado 
#mostrano che il partito democratico ha generalemnte closeness e grado più alto degli altri 
#due partiti. quindi crea più legami e sono 'più vicini' tra loro rispetto agli altri.
#Il partito di forza Italia crea molti meno legami rispetto agli altri due partiti e tra di 
#loro sono meno vicini rispetto agli altri.


#Una domanda naturale, a questo punto, è se la divisione in gruppi sia 'buona' in termini di
#coesione.
#calcoliamo MODULARITà e ASSORTATIVITà

modularity(net,factor(aType)) #input (rete, come sono divisi in gruppi)
#factor serve per specificare al comando che è vero che sono stringhe ma 
#rappresentano categorie
assortativity(net,factor(aType))

#non risulta un livello di assortatività elevato, i nodi della rete non tendono troppo a legarsi
#a nodi dello stesso partito politico


#V(net)$label<-V(net)$name
#V(net)$label[which(D_net< quantile(D_net, .99))]= NA 
#V(net)$label.color = "black"
#V(net)$label.size= 0.4
V(net)$label = rep(NA, vcount(net))
set.seed(1)
llFR<-layout_with_fr(net, niter=5000) 
V(net)$size<- log(betweenness(net)+1)  #ci sono tanti con betweennes o bassa o 1, il +1 evita il log(1)
V(net)$color = as.numeric(factor(aType))
color<-as.numeric(factor(aType))
edge.col<-V(net)$color
plot(net, layout=llFR)
legend(1,1,legend=unique(factor(aType)), pt.bg = edge.col, col =unique( edge.col))
legend(1,1, legend=levels(as.factor(aType)), cex =0.8,
       pt.cex = 3, pch=20, col =unique( color))
#llMDS = layout_with_mds(net)
#plot(net, layout=llMDS)
#llKK = layout_with_kk(net,dim = 2,maxiter = 100*vcount(net))
#plot(net,layout= llKK)
#set.seed(1)

#llFR = layout_with_fr(net,niter = 5000)
#plot(net,layout= llFR)
ggnet2(net, color = color )



require(ggraph)
require(tidygraph)
df = as_tbl_graph(net)
pl = df %>% mutate(btw= 10*log(centrality_betweenness() + 1)) %>%
  ggraph(layout="fr") + geom_edge_link(show.legend = FALSE,color="gray")+
  geom_node_point(aes(colour = aType,size= btw),alpha = 1,show.legend=T) + 
  geom_node_label(aes(label=label))+
  scale_size(guide="none") + theme_graph(base_family='sans') +
  theme(legend.position= "bottom")
pl












###########OMOFILIA RISPETTO AL GENERE#############
sex<-vertex_attr(net, name = 'sex')
df_sex<-data.frame(den=D_net_norm, clos = C_net_norm, bet = B_net_norm, x=sex)

df_sex%>% 
  gather(.,"den","clos","bet",key="stat",value = "value") %>%
  ggplot(aes(value,after_stat(density),color=x)) + geom_freqpoly(lwd=1)+
  facet_wrap(~stat,scales="free")+ theme_bw()

#le tre statistiche descrittive si distribuiscono quasi in modo identico, quindi non 
#si evincono differenze tra i nodi nei due generi

#calcoliamo MODULARITà e ASSORTATIVITà

modularity(net,factor(sex)) #input (rete, come sono divisi in gruppi)
#factor serve per specificare al comando che è vero che sono stringhe ma 
#rappresentano categorie
assortativity(net,factor(sex))
#l'assortatività è poco più del 5%. La divisione in gruppi secondo il sesso non ha senso



V(net)$label<-V(net)$name
V(net)$label[which(D_net< quantile(D_net, .99))]= NA 
V(net)$label.color = "black"
set.seed(1)
llFR<-layout_with_fr(net, niter=5000) 
V(net)$size<- log(betweenness(net)+1)  #ci sono tanti con betweennes o bassa o 1, il +1 evita il log(1)
V(net)$color = as.numeric(factor(sex))

plot(net, layout=llFR)

#non c'è differenza tra i due generi, si distribuiscono nella rete indipendentemente che 
#siano uomini o donne.




############VALUTAZIONI TRASVERSALI#################

gr<-cluster_louvain(net)
str(gr)
#in questo oggetto abbiamo la partizione migliore
membership(gr) #ci dice ogni nodo a quale gruppo appartiene
table(membership(gr))
#per ogni gruppo quanti nodi ci sono


#Perchè usare il raggruppamento di prima o quello di ora?
#compariamo ad esempio assortatività o interpretazione
modularity(net, membership(gr))
assortativity(net, membership(gr))
#è più alta di prima--> è un raggruppamento migliore
#prima era 0.547 ora è 0.822


#facciamo una tabella di contingenza per capire se l'info
#che ne esce dai due raggruppamenti è la stessa

tab=table(aType, membership(gr))
knitr::kable(tab)

V(net)$color<- membership(gr)
plot(net, layout=llFR)







