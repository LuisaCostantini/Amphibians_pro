#Biometria e Statistica Modulo 2 
#Progetto di Luisa Costantini, Eleonora Pancaldi e Riccardo Biagini 

#Amphibians Data Set: è una raccolta di informazioni sulla dimensione della popolazione di anfibi
#da inventari naturalistici fatti appositamente per la redazione di VIA (Valutazione Impatto Ambientale)
#per due progetti stradali pianificati in Polonia (strada A e strada B) con 189 siti di occorenza. 
#Abbiamo scelto questo dataset in quanto ci interesserebbe capire come varia la presenza di fattori biotici (le specie di anfibi) rispetto ad alcuni fattori abiotici (strade e serbatori).

#libraries
library(GGally)
library(tidyverse)
library(readxl)

#1 DATASET----
#Il dataset è stato preso da Kaggle : https://www.kaggle.com/datasets/ishandutta/amphibians-data-set
#In questo dataset abbiamo 190 righe (osservazioni) e 23 colonne (variabili)

dataset_amph <- read_xlsx("data/amphibians.xlsx", skip = 1)


#Ispezioniamo il nostro dataset 
view(dataset_amph)

dim(dataset_amph)

head(dataset_amph)

tail(dataset_amph)

str(dataset_amph)

names(dataset_amph)

glimpse(dataset_amph)

#Di seguito sono elencate le categorie per ogni cariabile:
#Nella prima colonna ci sono le osservazioni, 
#la seconda colonna è il tipo di strada/autostrada (fondamentale per la nostra analisi e che presenta due categorie, A1 ed S52),
#la terza colonna rappresenta la superficie dei serbatoi in m2, ci sarà molto utile per la nostra analisi,
#la quarta colonna è il numero di serbatoi/habitat, e maggiore è il numero di serbatoi, 
#più è probabile che ce ne siano di adatti alla riproduzione di anfibi all'interno di un determinato habitat.
#Nella quinta colonna sono presenti i tipi di serbatoi (serbatoi naturali, fossi, 
#prati umidi/paludi, valli fluviali,piccoli corsi d'acuqa, invasi di recente formazione non sottoposti a naturalizzazione,
#serbatoi d'acuqa tecnologica, giardini/serbatoi d'acqua in orti),
#Per quanto riguarda la sesta colonna possiamo vedere i diversi utilizzi delle riserve idriche (inutilizzata da umani,
#ricreativa e scenica, uutilizzata economicamente, tecnologica),
#nella settima colonna abbiamo la distanza serbatoio-strade espressa in mentri tramite range numerici,
#utile per il nostro studio in quanto ci permette di vedere come le specie siano più o meno influenzate dalla vicinanza con le strade 
#Nell'ottava invece, abbiamo la distanza serbatoio-edifici, come per la colonna 7 i dati sono espressi in metri tramite range numerici.
#Stato serbatoio nella nona colonna riguarda lo stato di manutenzione dell'invaso (ordinato, lievemente disordinato,
#pesantemente disordinato),
#Nella decima abbiamo la tipologia di riva (se naturale o antropica),
#Nelle ultime colonne (dalla 11esima alla 17esima) abbiamo in ordine le specie e in ogni colonna
#sono presenti due valori (variabili dicotomiche) : 0 o 1 che indicano la presenza (1) o l'assenza (0) di quella determinata specie.
#di seguito riportate le specie in ordine di apparizione nelle colonne dalla 11esmia alla 17esima
#Green frogs = Rana verde, 
#Brown frogs = Rana marrone,
#Common toad = Rospo comune
#Fire-bellied toad = Ululone dal ventre rosso,
#Tree frog = Raganella, 
#Common newt = Tritone comune, 
#Great crested newt = Tritone crestato maggiore.


#2 DATA CLEANING----
#Abbiamo elimianto le colonne non utili al nostro tipo di studio e ne abbiamo rinominate altre.
#Vedendo poi che la riga "ID" era uguale ai nomi delle varie colonne abbiamo deciso di eliminarla.
new_dataset_amph <- dataset_amph[,c(-6, -7, -8, -9,-10, -11, -12, -14, -15, -16, -4)]
view(new_dataset_amph)

colnames(new_dataset_amph) <- c("ID",
                                "motorway",
                                "estensione_serb_mq",
                                "tipo_serbatoio",
                                "dist_serb_strade",
                                "green_frogs",
                                "brown_frogs",
                                "common_toad",
                                "fire-bellied_toad",
                                "tree_frog",
                                "common_newt",
                                "great_crested_newt")


head(new_dataset_amph)

glimpse(new_dataset_amph)

view(new_dataset_amph)


#creiamo una nuova colonna con la somma delle specie osservate in ogni riga

new_dataset_amph$tot_species <- rowSums(new_dataset_amph[,6:ncol(new_dataset_amph)])

glimpse(new_dataset_amph)

view(new_dataset_amph)

#osserviamo come cambia il numero di specie tra le 2 
autostradedataset_motorwayA1 <- new_dataset_amph[new_dataset_amph$motorway == "A1",]

table(autostradedataset_motorwayA1$tot_species)

dataset_motorwayS52 <- new_dataset_amph[new_dataset_amph$motorway == "S52",]


table(dataset_motorwayS52$tot_species)

#dai risultati sembra che ci siano più specie nei pressi dell'autostrada S52, ci siamo chiesti se questa differenza fosse statisticamente significativa


#3 DATA ANALYSIS----

#test t test
#poniamo come ipotesi nulla H0 che il tipo di autostrada non influenzi il numero di specie presenti e come ipotesi alternativa H1 che il numero di specie osservate sia dipendente da quale autostrada osserviamo
ttest_motorway_totspecies <- t.test(tot_species ~ motorway, data = new_dataset_amph)
ttest_motorway_totspecies
#p-value = 8.656e-06, rifiutiamo l'ipotesi nulla che non ci sia relazione tra l'autostrada osservata e il numero di specie per una alfa = 1


#Ora vedremo qual è la specie con occorenza maggiore e quella con occorenza minore, cioè quale specie compare di più e quale meno 
table(new_dataset_amph$`green_frogs`)
table(new_dataset_amph$`brown_frogs`)
table(new_dataset_amph$`common_toad`)
table(new_dataset_amph$`fire-bellied_toad`)
table(new_dataset_amph$`tree_frog`)
table(new_dataset_amph$`common_newt`)
table(new_dataset_amph$`great_crested_newt`)


# Creazione del database con tutte le osservazioni in cui Brown frogs sono presenti
bfd <- new_dataset_amph %>%
  filter(brown_frogs == '1')
glimpse(bfd)
unique(bfd$brown_frogs)
unique(is.na(bfd$estensione_serb_mq))


# Creazione del database con tutte le osservazioni in cui Great crested newt sono presenti
gcnd <- new_dataset_amph %>%
  filter(great_crested_newt == '1')
glimpse(gcnd)
unique(gcnd$great_crested_newt)


#Durante lo studio verranno esaminati gli stessi parametri sia per la specie
#"Brown frogs" che per quella "Great crested newt". Per praticità verranno
#prima svolte tutte le analisi per la prima specie, e solo successivamente per la seconda.


#statistiche di base di bfd
bfd %>%
  summarise(mean = mean(estensione_serb_mq),
            sd = sd(estensione_serb_mq),
            min = min(estensione_serb_mq),
            max = max(estensione_serb_mq))

#statistiche di base per gcnd
gcnd %>%
  summarise(mean = mean(estensione_serb_mq),
            sd = sd(estensione_serb_mq),
            min = min(estensione_serb_mq),
            max = max(estensione_serb_mq))



#4 DATA VISUALIZATION----
#Vogliamo vedere graficamente le occorrenze delle specie perciò creiamo un dataframe dedicato in cui riportiamo le specie e le relative occorrenze 

valori <- c(108, 148, 124, 58, 71, 58, 21)
nomi <- c("green_frogs",
          "brown_frogs",
          "common_toad",
          "fire-bellied_toad",
          "tree_frog",
          "common_newt",
          "great_crested_newt")
df_secondario <- data.frame(occorrenza=valori, specie=nomi)

specie_occorrenza <- ggplot(df_secondario, aes(x=specie, y=occorrenza)) + 
  geom_bar(stat = "identity")

specie_occorrenza

#Salviamo il barplot 
pdf("specie_occorrenza.pdf")
plot(specie_occorrenza)
dev.off()

#Vediamo con un boxplot com'è la relazione tra il numero delle specie e il tipo di autostrada 
n_specie_autostrada <- ggplot() + geom_boxplot(aes(x= new_dataset_amph$motorway, y= new_dataset_amph$tot_species)) +
  labs(x = "Motorway",
       y = "Tot Species")
n_specie_autostrada
#Come possiamo vedere dal graficamente dal boxplot il totale delle specie è maggiormente distribuito nella strada S52. 
#Possiamo inoltre ossrvare che:  
#nella distribuzione dei dati relativa a A1 : 
#il primo quantile è 1,
#la mediana è 2, 
#il terzo quantile è 3,
#la distribuzione massima (scala) è 6. 
#invece nella distribuzione dei dati relativa a S52 : 
#il primo quantile è 2,
#la mediana è 3 e 
#il terzo quantile è 5,
#la distribuzione massima (scala) è 7 e possiamo notare che non abbiamo outliers. 

#Ora salviamo il grafico 
pdf("n_specie_autostrada.pdf")
plot(n_specie_autostrada)
dev.off()


#Facciamo uno scatterplot per vedere la distribuzione del tot species in funzione dell'estensione serbatoio espresso in mq
scatter_plot <- plot(new_dataset_amph$tot_species, new_dataset_amph$estensione_serb_mq, 
                     xlab = "Estensione serbatoio (mq)", ylab = "Totale specie")

#Il grafico mostra che ad estensioni di serbatoio basse corrispondono numeri di specie bassi, e a
#estensioni di serbatoio alte corrispondono totali di specie alti. Per valori intermedi di estensione
#il grafico tende, invece, ad associare ai valori più bassi di estensione totali di specie più alti, e a
#estensioni alte totali di specie bassi; questo fenomeno probabilmente è imputabile alla presenza di altri fattori
#che influenzano il numero di specie presenti in un serbatoio idrico oltre all'estensione di questo,
#i quali per dimensioni inermedie dei serbatoi acquistano un peso maggiore dell'estensione nell'influenzare il totale di specie.

#Salvo lo scatterplot
pdf("scatterplot.pdf")
plot(scatter_plot)
dev.off()


#test di correlazione
#Indaghiamo la possibile correlazione tra la grandezza dei serbatoi e il numero di specie presenti utilizzando il test di correlazione di pearson
#Ipotesi nulla: Ho -> non c'è correlazione lineare tra le variabili
#Ipotesi alternativa H1 : c'è correlazione lineare tra le variabili
cor_pearson <- cor.test(new_dataset_amph$estensione_serb_mq, new_dataset_amph$tot_species, method = "pearson")
cor_pearson
#p-value = 0.002263, dobbiamo rifiutare l'ipotesi nulla, esiste una relazione lineare tra le variabili tot species e la grandezza del serbatoio

#Ora facciamo un grafico per vedere : specie e tipo di serbatoio
ggplot(new_dataset_amph, aes(x = tipo_serbatoio, y = tot_species)) +
  geom_col(position = "dodge", fill = "steelblue") +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3) +
  labs(x = "Tipo di serbatoio", y = "Media del numero di specie") +
  ggtitle("Numero di specie per tipo di serbatoio")

#Dal grafico sembra che in diversi tipi di serbatoi siano presenti numeri diversi di specie. 
#In particolare: 
#1 = serbatoio naturale, con media 3~ 
#11 = fossi con media 2,6~
#12 = prati umidi (paludi) con media 1,8~
#14 = valli fluviali con media 2,6~
#15 = piccoli corsi d'acqua con media 1,3~
#2 = invasi di recente formazione, non sottoposti a naturalizzazione con media 1,6~ 
#5 = serbatoi d'acqua tecnologica con media 1,9~
#7 = giardini/serbatoi d'acqua negli orti con media 6.
#A giudicare dai risultati assumiamo che le specie preferiscano serbatoio che si trovano in giardini/orti 
# o in serbatoi naturali.

#Studiamo questa relazione con un test di kruskal wallis
kruskal.test(tot_species ~ tipo_serbatoio, data = new_dataset_amph)

#p-value = 1.709e-06 le differenze per tipo di serbatoio sono significative il che significa che a seconda del tipo di serbatoio abbiamo numeri di specie diverse
#H0 ipotesi nulla = non c'è relazione tra tipo di serbatoio e il numero di specie
#H1 ipotesi alternativa = c'è relazione tra tipo di serbatoio e il numero di specie 

#Concludendo da quanto osservato nelle nostre analisi; possiamo affermare che è effettivamente presente 
# una relazione tra le componenti biotiche (specie anfibi) e abiotiche (tipologia strada, tipo di serbatoio, distanza serbatoio-strade, estensione del serbatoio).

