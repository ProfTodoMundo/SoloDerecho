library(readr)
library(dplyr)
setwd("~/Desktop/MiGithub/SoloDerecho")
BDD1 <- read_csv("BDD/BDD1.csv")
BDD2 <- read_csv("BDD/BDD2.csv")
View(BDD1)
View(BDD2)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
data1 <- BDD1
data2 <- BDD2
MiBDD <- rbind(data1,data2)
summary(MiBDD)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
MiBDD$Nombre_Carrera <- factor(MiBDD$Nombre_Carrera,
                               levels = c('Ingeniería en Sistemas Electrónicos y de Telecomunicaciones',
                                          'Arte y Patrimonio Cultural','Ciencia Política y Administración Urbana',
                                          'Ciencias Ambientales y Cambio Climático','Ciencias Genómicas',
                                          'Ciencias Sociales','Comunicación y Cultura',
                                          'Creación Literaria','Derecho','Filosofía e Historia de las Ideas',
                                          'Historia y Sociedad Contemporánea','Ingeniería de Software',
                                          'Ingeniería en Sistemas de Transporte Urbano',
                                          'Ingeniería en Sistemas Electrónicos Industriales',
                                          'Ingeniería en Sistemas Energéticos',
                                          'Modelación Matemática','Nutrición y Salud',
                                          'Promoción de la Salud','Protección Civil y Gestión de Riesgos'),
                               labels = c('ISET','AyPC','CPyAU','CAyCC','Genomicas','CiSOC','ComyCult',
                                          'CreLit','Derecho','FeHdI','HistySocCont','Software','ISTU',
                                          'ISEI','ISENER','Modelacion','Nutricion','Promocion','ProtCivil'))
summary(MiBDD$Nombre_Carrera)

BDD_Derecho <- MiBDD%>% filter(MiBDD$Nombre_Carrera=="Derecho")
summary(BDD_Derecho)
View(BDD_Derecho)
misdatos <- BDD_Derecho
save.image("Wkspaces/Checkpoint1.RData")
write.csv(BDD_Derecho,"BDD/BDD_Derecho.csv")
summary(BDD_Derecho)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
misdatos$GEN <- factor(misdatos$GEN,
                       levels = c('01','02','03','04','05','06','07','08','09','10','11','12',
                                  '13','14','15','16','17','18','19','20','21'),
                       labels = c('01','02','03','04','05','06','07','08','09','10','11','12',
                                  '13','14','15','16','17','18','19','20','21'))
misdatos$Turno <- factor(misdatos$Turno,levels = c('M','V','MIXTO'),labels = c('M','V','MIXTO'))
summary(misdatos)
misdatos$Situacion <- factor(misdatos$Situacion,
                             levels = c('SUSPENSION TEMPORAL POR ADEUDO O PRESTAMO DE DOCUMENTOS',
                                        'TITULADO','ACTIVO','EGRESADO','BAJA DEFINITIVA','BAJA TEMPORAL'),
                             labels = c('SUSPENSION_TEMPORAL','TITULADO','ACTIVO','EGRESADO','BAJA_DEFINITIVA',
                                        'BAJA_TEMPORAL'))
summary(misdatos)
misdatos$Nombre_Plantel <- factor(misdatos$Nombre_Plantel,
                                  levels = c('Centro Histórico','Cuautepec','Del Valle','Iztapalapa','San Lorenzo Tezonco',
                                             'Reclusorio Preventivo Varonil Norte',
                                             'Centro Escolar Dr. Pedro López Penitenciaría del Distrito Federal',
                                             'Centro Escolar Francisco I Madero Centro Femenil de Readaptación Social de Tepepan',
                                             'Centro Escolar Francisco I Madero Ceresova Centro de Readaptación Social Varonil Oriente',
                                             'Centro Escolar José Vasconcelos Ceresova Centro de Readaptación Social Varonil Sur',
                                             'Centro Escolar Rosario Ibarra de Piedra Cefereso Centro Femenil de Readaptación Social Santa Martha Acatitla',
                                             'Centro Escolar Santiago Ramírez',
                                             'Centro Escolar Valentín Campa Salazar Ceresova Centro de Readaptación Social Varonil Santa Martha Acatitla'),
                                  labels = c('CH','GAM','DV','IZT','SLT','PESCER','PESCER','PESCER','PESCER',
                                             'PESCER','PESCER','PESCER','PESCER'))
summary(misdatos)
misdatos$Calificacion <- factor(misdatos$Calificacion,levels = c('7','8','9','10','NC','NP'), labels = c('C','C','C','C','NC','NC'))
summary(misdatos)
fechas <- misdatos$Periodo_Certificacion; head(fechas); summary(fechas)
n <- length(fechas); v <- as.character(fechas); w <- as.Date(v); misdatos$Periodo_Certificacion <- w
summary(misdatos)
summary(misdatos$GEN)
summary(misdatos$Nombre_Carrera)
summary(misdatos$Nombre_Plantel)
summary(misdatos$Turno)
summary(misdatos$Calificacion)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
misdatos <- as.data.frame(misdatos); View(misdatos)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
MisMaterias <- misdatos$Materia; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Materia <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias; head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Materia <- factor(misdatos$Materia, levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(misdatos$Materia)); View(tempo)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
TemasSelectos <- misdatos$Tema_Especifico; n<- length(TemasSelectos)
TemasTemporales <- TemasSelectos; MateriasCorrected <- c()
for(i in 1:n){
  MateriasCorrected <- toupper(TemasSelectos[i])
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TemasSelectos[i] <- MateriasCorrected
}
for(i in 1:n){temas <- TemasSelectos[i];if(is.na(temas)==TRUE){TemasSelectos[i] <- LasMaterias[i]}}
misdatos$Tema_Especifico <- TemasSelectos; todasmismaterias <- sort(TemasSelectos)
TemasSelectos <- todasmismaterias;materia <- TemasSelectos[1]; n <- length(TemasSelectos); k <- 2;
ListaTemasSelectos <- c(); ListaTemasSelectos[1] <- materia; Materia2 <- c()
for(i in 2:n){
  Materia2 <- TemasSelectos[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaTemasSelectos[k]<- Materia2;materia <- Materia2; k <- k+1}
  }
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
typeof(ListaTemasSelectos); length(ListaTemasSelectos)
temp <- factor(ListaTemasSelectos); niveles <- levels(temp)
misdatos$Tema_Especifico <- factor(misdatos$Tema_Especifico, levels = niveles,labels = ListaTemasSelectos)
summary(misdatos$Tema_Especifico); View(as.data.frame(table(misdatos$Tema_Especifico)))
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
save.image("Wkspaces/Checkpoint2.RData")
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
write.csv(misdatos,"BDD/BDD_Derecho.csv")
# misdatos es la base de datos original
MyDataBase <- misdatos; 
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
temp <- misdatos$Periodo_Certificacion; n <- length(temp)
anho <- c(); mess <- c(); LocMenos  <- unlist(gregexpr("-", temp[1]));
for(i in 1:n){anho[i] <- substring(temp[i], LocMenos[2]+1, 10); mess[i] <- substring(temp[i], LocMenos[1]+1, 4);}
head(anho,15);print(anho)
head(mess,15);print(mess)
head(temp,15)
Checking <- cbind(as.character(temp),mess,anho); #Checking <- as.data.frame(Checking)
View(Checking)
periodo <- c()
for(i in 1:n){
  tempo1 <- as.numeric(mess[i]); tempo2 <- as.numeric(anho[i])
  if(is.na(tempo1)==TRUE){tempo1 <- 7;}
  if(is.na(tempo2)==TRUE){anho[i] <- 1;}
  if(tempo1<=6){periodo[i] <- 'Primer';}else{periodo[i] <- 'Segundo';}}
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
print(periodo)
Checking <- cbind(Checking,periodo); View(Checking)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
save.image("Wkspaces/Checkpoint3.RData")
write.csv(misdatos,"BDD/BDD_Derecho.csv")
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
SubBDD <- c(); SubBDD <- cbind(misdatos,anho,periodo); summary(SubBDD); View(SubBDD)
summary(SubBDD); colnames(SubBDD)
MiBDD <- SubBDD[,c('Matrícula','Nombre_Plantel','Turno','Situacion','Tema_Especifico',
                   'Calificacion','anho','periodo','GEN','Nombre_Carrera','Periodo_Certificacion')]
misdatos <- MiBDD; View(SubBDD); summary(MiBDD)
MiBDD$periodo <- factor(MiBDD$periodo,levels = c('Primer','Segundo'),labels = c('Primer','Segundo'))
summary(MiBDD)
MiBDD$anho <- factor(MiBDD$anho,
                     levels = c('1','02','03','04','05','06','07','08','09','10',
                                '11','12','13','14','15','16','17','18','19',
                                '20','21','22'),
                     labels =  c('01','02','03','04','05','06','07','08','09','10',
                                 '11','12','13','14','15','16','17','18','19',
                                 '20','21','22'))
summary(MiBDD)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
colnames(MiBDD) <- c('Matricula','Plantel','Turno','Estatus','Materias','Cal','Anho',
                     'PeriodoCert','Gen','Lic','Periodo')
summary(MiBDD)
NumMaterias <- length(MiBDD$Materias); MateriaSelecc <- sort(MiBDD$Materias)
head(MateriaSelecc,15)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
save.image("Wkspaces/Checkpoint4.RData")
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
print(MateriaSelecc)
print(MiBDD$Materias)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
write.csv(MiBDD,"BDD/SubBDD_Derecho.csv")
library(dplyr)
NumMaterias    <- length(MiBDD$Materias); MateriaSelecc  <- sort(MiBDD$Materias)
#---------------------------------------------------------
milista <- c(1,6,8,9,12,17,18,20,23,27,28,30,31,38,39,
             49,50,75,80,81,83,84,85,115,116,122,123,
             124,125,126,127,131,142,143,153,157,166,
             168,170,171,174,175,177,178,181,182,185,186,
             187,188,189,190,191,192,193,194,195,
             196,198,199,200,202,207,211,212,213,214,215,217,
             218,219,220,221,222,223,224,226,227,228,233,235,237,
             238,239,240,241,242,243,244,
             248,249,250,251,252,253,255,258,261,263,264,265,270,
             271,272,273,274,276,277,279,281,282,283,284,285,286,
             287,288,289,290,291,292,293,297,299,300,301,302,306,
             307,308,309,310,
             311,312,313,314,315,316,317,318,321,322,323,324,325,
             326,327,328,329,334,
             337,338,341,342,343,344,346,347,348,349,350,
             352,353,353,355,356,358,359,361,365,366,367,378,
             380,381,382,383,384,385,393,394,
             401,403,406,408,409,410,412,413,414,415,416,417,
             418,419,420,421,422,424,425,426,427,428,429,430,435,437,
             442,448,449,452,465,466)
m <- length(milista); print(milista);
N <- length(ListaTemasSelectos); print(N);
K <- N-m; print(K);ProbaMatrix <- matrix(0,K,9)
colnames(ProbaMatrix) <- c('Materia','ProbCert1a',
                           'NumMaxIntentosNuncaCert','IntentosPromNuncaCert',
                           'MedianaIntentosNuncaCert','ProbNoCertNunca',
                           'NumMaxIntentosSiCert','IntentosPromedioSiCert','MedianaIntentosSiCert')
lista <- seq(1,N); print(lista); listareducida <-lista[-milista]; print(listareducida)
#lista <- seq(1,N); print(lista); listareducida <-lista
#---------------------------------------------------------------------
NumMaterias    <- length(MiBDD$Materias); MateriaSelecc  <- sort(MiBDD$Materias)
#---------------------------------------------------------
#<==> <==> <==> <==> > <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
contador <-1;NumIter <- listareducida[contador];MaxIter <- length(listareducida)
NumIter <- 2
contador <- 84
contador <- NumIter+1
contador <- 467
contador <- 183
while (contador <= MaxIter) {
  #---------------------------------------------------------
  # INICIA LA ITERACION
  #---------------------------------------------------------
  NumIter <- listareducida[contador]
  MateriaToStudy <- as.character(ListaTemasSelectos[NumIter]); print(MateriaToStudy)
  datos1   <- MiBDD %>% filter(MiBDD$Materias==MateriaToStudy);head(datos1,30)
  datostowork <- datos1[c('Matricula','Plantel','Turno','Estatus','Cal','PeriodoCert','Anho','Gen','Lic','Periodo')]
  solodos <- datos1[c('Matricula','Cal')]; head(solodos,30)
  NoCertificaron <- solodos %>% filter(Cal == 'NC'); head(NoCertificaron,30)
  SiCertificaron <- solodos %>% filter(Cal == 'C');  head(SiCertificaron,30)
  Principal <- NoCertificaron; n  <-  length(Principal$Matricula)
  contando <- c(); repetidas <- matrix(0,n,2); v <- c(); w <- c();  i <-1;  j <- 0;  l <- 1; k <- 1;
  while (i<n) {
    j <- i+1; 
    while (j<=n) {
      v <- NoCertificaron[i,1]; w <- NoCertificaron[j,1]; 
      if(v==w){
        k <- k+1;repetidas[i,1] <- NoCertificaron[i,1];repetidas[i,2] <- k; j <- j+1; 
      }else{
        j <- j+1;
      }
    }
    i <- i+k; k <- 1
  }
  repetidas <- as.data.frame(repetidas); row_sub = apply(repetidas, 1, function(row) all(row !=0 ));
  repetidasNC <- repetidas[row_sub,]; head(repetidasNC)
  Principal <- SiCertificaron; n  <-  length(Principal$Matricula)
  contando <- c(); repetidas <- matrix(0,n,2); v <- c(); w <- c();  i <-1;  j <- 0;  l <- 1; k <- 1;
  while (i<n) {
    j <- i+1; 
    while (j<=n) {
      v <- SiCertificaron[i,1]; w <- SiCertificaron[j,1]; 
      if(v==w){
        k <- k+1;repetidas[i,1] <- SiCertificaron[i,1];repetidas[i,2]  <- k; j <- j+1;
      }else{
        j <- j+1;
      }
    }
    i <- i+k; k <- 1
  }
  repetidas <- as.data.frame(repetidas); row_sub = apply(repetidas, 1, function(row) all(row !=0 ))
  repetidasSiC <- repetidas[row_sub,]; 
  matriculas <- solodos$Matricula; todasmatriculas <- sort(matriculas)
  Matricula <- todasmatriculas[1]; n <- length(todasmatriculas);k <- 2;
  ListaMatriculas <- c(); ListaMatriculas[1] <- Matricula; i <- 2
  for(i in 2:n){
    Matricula2 <- todasmatriculas[i]
    if(Matricula==Matricula2){
      Matricula <- Matricula2
    }else{
      ListaMatriculas[k]<- Matricula2; Matricula <- Matricula2; k <- k+1
    }
  }
  ListaMatriculas <- as.data.frame(ListaMatriculas); ListaReferencia <- ListaMatriculas;
  n <- length(ListaReferencia[,1]); m <- length(repetidasNC[,1])
  k <- length(repetidasSiC[,1]);    l <- length(SiCertificaron[,1])
  ll <- length(NoCertificaron[,1]); MateriasResultados <- matrix(0,n,3); i<- 1;
  while (i<=n) {
    matricula <- ListaReferencia[i,1];  MateriasResultados[i,1]<- matricula; j <- 1;
    while(j<=ll){
      matricula4 <- NoCertificaron[j,1]; 
      if(matricula == matricula4){
        MateriasResultados[i,2] <- 1}
      j <- j+1
    }
    j <- 1
    while(j<=m){
      matricula2 <- repetidasNC[j,1]; 
      if(matricula == matricula2){MateriasResultados[i,2] <- repetidasNC[j,2]}
      j <- j+1
    }
    j <- 1
    while (j<= l) {
      matricula3 <- SiCertificaron[j,1];
      if(matricula== matricula3){MateriasResultados[i,3] <- SiCertificaron[j,2]}
      j <- j+1
    }
    i <- i+1; 
  }
  Resultados <- as.data.frame(MateriasResultados)
  colnames(Resultados) <- c('Matricula','NC','C'); head(Resultados,30)
  nombre <- paste(MateriaToStudy,'.csv'); write.csv(Resultados,nombre)
  #<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
  #<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
  ProbaMatrix[contador,1] <- MateriaToStudy;
  CertPrimerIntento <- Resultados %>% filter(Resultados$NC==0);head(CertPrimerIntento,30)
  NuncaCertificaron <- Resultados %>% filter(Resultados$C==0);head(NuncaCertificaron,30)
  IntentaronCertificar <- Resultados%>% filter(Resultados$NC!=0 & Resultados$C!=0); head(IntentaronCertificar)
  n <- length(Resultados$Matricula); print(n); head(Resultados)
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - -
  n1 <- length(CertPrimerIntento$Matricula); print(n1);head(CertPrimerIntento)
  P_Cert_Intento1 <- n1/n; print(P_Cert_Intento1); #ProbaMatrix[NumIter,1] <- P_Cert_Intento1;
  ProbaMatrix[contador,2] <- P_Cert_Intento1;
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - -
  n2 <- length(NuncaCertificaron$Matricula); print(n2);head(NuncaCertificaron)
  # hay que sumar los elementos de NuncaCertificaron$NC y se divide entre el total de elementos
  # para saber si hay mas de un intento fallido por certificar, ademas de obtener el maximo y minimo
  sumaintentos <- sum(as.numeric(NuncaCertificaron$NC))
  proporcion <- sumaintentos/n2; print(proporcion)
  #ProbaMatrix[NumIter,2] <- sumaintentos; ProbaMatrix[contador,2] <- sumaintentos
  maxIntentos <- max(as.numeric(NuncaCertificaron$NC)); print(maxIntentos)
  ProbaMatrix[contador,3] <- maxIntentos; #ProbaMatrix[NumIter,3] <- maxIntentos
  IntentosPromedio <- mean(as.numeric(NuncaCertificaron$NC));print(IntentosPromedio)
  ProbaMatrix[contador,4] <- IntentosPromedio; #ProbaMatrix[NumIter,4] <- IntentosPromedio
  IntentosMediana <- median(as.numeric(NuncaCertificaron$NC));print(IntentosMediana)
  ProbaMatrix[contador,5] <- IntentosMediana; #ProbaMatrix[NumIter,5] <- IntentosMediana
  ProbNoCertNunca <- n2/n; print(ProbNoCertNunca);  #ProbaMatrix[NumIter,6] <- ProbNoCertNunca
  ProbaMatrix[contador,6] <- ProbNoCertNunca
  TablaIntentos <- table(as.numeric(NuncaCertificaron$NC)); print(TablaIntentos)
  nombre <- paste(MateriaToStudy,'_NuncaCert','.csv');  write.csv(TablaIntentos,nombre)
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - -
  n3 <- length(IntentaronCertificar$Matricula); print(n3);
  head(IntentaronCertificar)
  sumaintentos <- sum(as.numeric(IntentaronCertificar$NC)); proporcion <- sumaintentos/n3
  #ProbaMatrix[NumIter,7] <- sumaintentos; ProbaMatrix[contador,7] <- sumaintentos
  maxIntentos <- max(as.numeric(IntentaronCertificar$NC))
  ProbaMatrix[contador,7] <- maxIntentos; #ProbaMatrix[NumIter,8] <- maxIntentos
  IntentosPromedio <- mean(as.numeric(IntentaronCertificar$NC));print(IntentosPromedio)
  ProbaMatrix[contador,8] <- IntentosPromedio; #ProbaMatrix[NumIter,9] <- IntentosPromedio
  IntentosMediana <- median(as.numeric(IntentaronCertificar$NC));print(IntentosMediana)
  ProbaMatrix[contador,9] <- IntentosMediana; #ProbaMatrix[NumIter,10] <- IntentosMediana
  TablaIntentos <- table(as.numeric(IntentaronCertificar$NC)); print(TablaIntentos)
  nombre <- paste(MateriaToStudy,'_IntentosCert','.csv');  write.csv(TablaIntentos,nombre)
  head(ProbaMatrix,10)
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - -
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - -
  #---------------------------------------------------------
  # TERMINA LA ITERACION
  #---------------------------------------------------------
  #_________________________________________________________
  contador <- contador+1; print(contador)
  #NumIter <- NumIter+1; print(NumIter)
  #_________________________________________________________
}
#write.csv(ProbaMatrix,'MatrixProb.csv')
