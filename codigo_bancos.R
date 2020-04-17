library(tidylog)
library(tidyverse)
library(geobr)
library(stringr)

bancos<-read.csv("202001AGENCIAS.csv",header = TRUE,sep=";")
head(bancos)

bancos_ce<- bancos %>% filter(UF=="CE")
bancos_ce$MUNICíPIO<-str_trim(bancos_ce$MUNICíPIO)

View(bancos_ce %>% count(NOME.INSTITUIÇÃO) %>% arrange(desc(n)))

bancos_fort<- bancos_ce %>% filter(MUNICíPIO=="FORTALEZA")

rmf<- c("AQUIRAZ","CASCAVEL","CAUCAIA","CHOROZINHO",
        "EUSEBIO","FORTALEZA","GUAIUBA","HORIZONTE",
        "ITAITINGA","MARACANAU","MARANGUAPE","PACAJUS",
        "PACATUBA","PINDORETAMA","SAO GONCALO DO AMARANTE",
        "SAO LUIS DO CURU","PARAIPABA","PARACURU","TRAIRI")

bancos_rmf<- bancos_ce %>% filter(MUNICíPIO %in% rmf)


Cidades<-bancos_ce %>% count(MUNICíPIO) %>% arrange(desc(n))

Ceara<- read_municipality(code_muni="CE")
Teste<- read_meso_region(code_meso = "CE",year = "2018")
plot(Teste)
Ceara$name_muni<-abjutils::rm_accent(Ceara$name_muni)
Ceara$name_muni<-str_to_upper(Ceara$name_muni)

populacoes<-read.csv("populacao_municipios.csv",header=TRUE,sep=";")
noroeste<-c("ACARAU","BARROQUINHA","BELA CRUZ","CAMOCIM","CHAVAL",
            "CRUZ","GRANJA","ITAREMA","JIJOCA DE JERICOACOARA","MARCO",
            "MARTINOPOLE","MORRINHOS","CARNAUBAL","CROATA","GUARACIABA DO NORTE","IBIAPINA",
            "SAO BENEDITO","TIANGUA","UBAJARA","VICOSA DO CEARA",
            "COREAU","FRECHEIRINHA","MORAUJO","URUOCA","ALCANTARAS","MERUOCA",
            "CARIRE","FORQUILHA","GRACA","GROAIRAS","IRAUCUBA","MASSAPE","MIRAIMA","MUCAMBO",
            "PACUJA","SANTANA DO ACARAU","SENADOR SA","SOBRAL","IPU","IPUEIRAS","PIRES FERREIRA",
            "PORANGA", "RERIUTABA","VARJOTA","CATUNDA","HIDROLANDIA","SANTA QUITERIA")

norte<-c("AMONTADA","ITAPIPOCA","TRAIRI","PARACURU","PARAIPABA","SAO GONCALO DO AMARANTE","ITAPAGE",
         "TURURU","UMIRIM","URUBURETAMA","APUIARES","GENERAL SAMPAIO","PENTECOSTE","SAO LUIS DO CURU",
         "TEJUCUOCA","CANINDE","CARIDADE","ITATIRA","PARAMOTI","ACARAPE","ARACOIABA","ARATUBA","BATURITE",
         "CAPISTRANO","GUARAMIRANGA","ITAPIUNA","MULUNGU","PACOTI","PALMACIA","REDENCAO","BARREIRA",
         "CHOROZINHO","OCARA","BEBERIBE","CASCAVEL","PINDORETAMA")

metropolitana<-c("AQUIRAZ","CAUCAIA","EUSEBIO","FORTALEZA","GUAIUBA","ITAITINGA","MARACANAU","MARANGUAPE",
                 "PACATUBA","HORIZONTE","PACAJUS")

sertoes<-c("ARARENDA","CRATEUS","INDEPENDENCIA","IPAPORANGA","MONSENHOR TABOSA",
           "NOVA RUSSAS","NOVO ORIENTE","QUITERIANOPOLIS","TAMBORIL",
           "BANABUIU","BOA VIAGEM","CHORO","IBARETAMA","MADALENA","QUIXADA","QUIXERAMOBIM",
           "AIUABA","ARNEIROZ","CATARINA","PARAMBU","SABOEIRO","TAUA","ACOPIARA","DEPUTADO IRAPUAN PINHEIRO",
           "MILHA","MOMBACA","PEDRA BRANCA","PIQUET CARNEIRO","SENADOR POMPEU","SOLONOPOLE")

jaguaribe<-c("ARACATI","FORTIM","ICAPUI","ITAICABA","ALTO SANTO","IBICUITINGA","JAGUARUANA",
             "LIMOEIRO DO NORTE","MORADA NOVA","PALHANO","QUIXERE","RUSSAS","SAO JOAO DO JAGUARIBE",
             "TABULEIRO DO NORTE","JAGUARETAMA","JAGUARIBARA","JAGUARIBE","ERERE","IRACEMA","PEREIRO","POTIRETAMA")

centro_sul<-c("CEDRO","ICO","IGUATU","OROS","QUIXELO","ANTONINA DO NORTE","CARIUS",
              "JUCAS","TARRAFAS","VARZEA ALEGRE","BAIXIO","IPAUMIRIM","LAVRAS DA MANGABEIRA",
              "UMARI")

sul<-c("ARARIPE","ASSARE","CAMPOS SALES","POTENGI","SALITRE","ALTANEIRA",
       "CARIRIACU","FARIAS BRITO","GRANJEIRO","AURORA","BARRO","MAURITI",
       "BARBALHA","CRATO","JARDIM","JUAZEIRO DO NORTE","MISSAO VELHA","NOVA OLINDA",
       "PORTEIRAS","SANTANA DO CARIRI","ABAIARA","BREJO SANTO","JATI","MILAGRES","PENAFORTE")




meso<- populacoes %>% mutate(name_meso= ifelse(name_muni %in% noroeste,"Noroeste Cearense",
                                        ifelse(name_muni %in% norte,"Norte Cearense",
                                        ifelse(name_muni %in% metropolitana,"Metropolitana De Fortaleza",
                                        ifelse(name_muni %in% sertoes,"Sertões Cearenses",
                                        ifelse(name_muni %in% jaguaribe,"Jaguaribe",
                                        ifelse(name_muni %in% centro_sul,"Centro-Sul Cearense",
                                        ifelse(name_muni %in% sul,"Sul Cearense","ERRO"))))))))

mesorregioes<- meso %>% group_by(name_meso) %>% summarise(populacao_est = sum(populacao_est_2019))


Meso_Regiao<- left_join(Teste,mesorregioes,by = "name_meso")

colnames(Cidades)<-c("name_muni","N_Bancos")



Dados_Mapa<-left_join(Ceara,Cidades,by="name_muni")
Dados_Mapa[is.na(Dados_Mapa)] <- 0

names(Dados_Mapa)
Dados_Mapa<-Dados_Mapa[,c(2,5)]

Municipios<-Dados_Mapa %>% mutate(name_meso= ifelse(name_muni %in% noroeste,"Noroeste Cearense",
                                                            ifelse(name_muni %in% norte,"Norte Cearense",
                                                            ifelse(name_muni %in% metropolitana,"Metropolitana De Fortaleza",
                                                            ifelse(name_muni %in% sertoes,"Sertões Cearenses",
                                                            ifelse(name_muni %in% jaguaribe,"Jaguaribe",
                                                            ifelse(name_muni %in% centro_sul,"Centro-Sul Cearense",
                                                            ifelse(name_muni %in% sul,"Sul Cearense","ERRO"))))))))


Meso_Bancos<- Municipios %>% select(N_Bancos,name_meso) %>% group_by(name_meso) %>% summarise(Total_Bancos=sum(N_Bancos))
Meso_Bancos<-Meso_Bancos[,c(1,2)]


Mesob<-c(Meso_Bancos$name_meso)
n_banc<-c(Meso_Bancos$Total_Bancos)

mesobancos<-data.frame(Mesob,n_banc)
names(mesobancos)<-c("name_meso","N_Bancos")





Meso_Comp<-left_join(Meso_Regiao,mesobancos,by="name_meso")
Meso_Comp<-Meso_Comp %>% mutate(Taxa_Banco_100mil=(N_Bancos/populacao_est)*100000)



Todas<-Dados_Mapa %>% select(name_muni,N_Bancos)
View(Todas %>% filter(N_Bancos==0))



Bancos_NE<- bancos %>% filter(UF %in% c("CE","MA","BA","SE","PE","RN","PB","PI","AL"))
View(Bancos_NE %>% count(UF) %>% arrange(desc(n)))

View(bancos %>% count(UF) %>% arrange(desc(n)))




Mapa_Carol<-Meso_Comp %>% 
  ggplot() +
  geom_sf(aes(fill = Taxa_Banco_100mil)) + 
  scale_fill_continuous(name = "Nº Agências por 100 mil hab.", low = 'white', high = 'darkblue',
                        na.value = 'white', breaks = seq(0,7,1))
Mapa_Carol
