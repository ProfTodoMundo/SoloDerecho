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
setwd("~/Documentos/MiGitHub/SoloDerecho")
load("Wkspaces/Checkpoint4.RData")
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
print(MateriaSelecc)
print(MiBDD$Materias)
#<==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
write.csv(MiBDD,"BDD/SubBDD_Derecho.csv")
library(dplyr)
NumMaterias    <- length(MiBDD$Materias); MateriaSelecc  <- sort(MiBDD$Materias)
#---------------------------------------------------------
View(MiBDD)
#---------------------------------------------------------
milista <- c()
m <- length(milista); print(milista);N <- length(ListaTemasSelectos); print(N);
K <- N-m; print(K);ProbaMatrix <- matrix(0,K,9)
colnames(ProbaMatrix) <- c('Materia','ProbCert1a',
                           'NumMaxIntentosNuncaCert','IntentosPromNuncaCert',
                           'MedianaIntentosNuncaCert','ProbNoCertNunca',
                           'NumMaxIntentosSiCert','IntentosPromedioSiCert','MedianaIntentosSiCert')
lista <- seq(1,N); print(lista); listareducida <-lista; print(listareducida)
#---------------------------------------------------------------------
NumMaterias    <- length(MiBDD$Materias); MateriaSelecc  <- sort(MiBDD$Materias)
#---------------------------------------------------------
#<==> <==> <==> <==> > <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> <==> 
contador <-1;NumIter <- listareducida[contador];MaxIter <- length(listareducida)
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
  if(length(todasmatriculas)>=2){
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
    nombre <- paste('BDD/',MateriaToStudy,'.csv');
    write.csv(Resultados,nombre)
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
    nombre <- paste('BDD/',MateriaToStudy,'_NuncaCert','.csv');  write.csv(TablaIntentos,nombre)
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
    nombre <- paste('BDD/',MateriaToStudy,'_IntentosCert','.csv');  write.csv(TablaIntentos,nombre)
    head(ProbaMatrix,10)
  }
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
write.csv(ProbaMatrix,'Matrices/MatrixProb.csv')
#_________________________________________________________
save.image("Wkspaces/Checkpoint5.RData")
#_________________________________________________________
View(ProbaMatrix)
PMatrix <- as.data.frame(ProbaMatrix); View(PMatrix)
MatrixProba <- PMatrix %>%
  filter(PMatrix$Materia != 0)
View(MatrixProba)
# Guarda el nuevo conjunto de datos
write_csv(MatrixProba, 'Matrices/MatrixProba.csv')
# <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><>
# <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><>
MatrixProbGral <- MatrixProba
View(MatrixProbGral)
write.csv(MatrixProbGral, 'Matrices/GralProbMatrix.csv')
#>><<===>><<===>><<===>><<===>><<===>><<===>><<===>><<===>><<===>><<===
MPGord <- MatrixProbGral[order(MatrixProbGral$Materia), ]; head(MPGord)
write.csv(MPGord,'Matrices/MatrizProbGralOrd.csv')
# <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><>
print(MPGord$Materia)
# <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><> <><>====<><>
MPG <- MPGord
MPG <- as.data.frame(MPG)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
colnames(MPG) <- c('Materia','ProbCert1a',
                   'MaxIntNuncaCert','MeanIntNuncaCert',
                   'ModeIntNuncaCert','ProbNuncaCert',
                   'MaxIntSiCert','MeanIntSiCert',
                   'ModeIntSiCert')
View(MPG)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert1a    <- MPG[order(MPG$ProbCert1a,decreasing= TRUE), ];       View(ResProbCert1a);    # si
ResProbNuncaCert <- MPG[order(MPG$ProbNuncaCert,decreasing= TRUE), ];    View(ResProbNuncaCert); # si
MaxIntCert       <- MPG[order(MPG$MaxIntSiCert,decreasing= TRUE), ];     View(MaxIntCert);      # si
MaxIntNuncaCertif  <- MPG[order(MPG$MaxIntNuncaCert,decreasing= TRUE), ];  View(MaxIntNuncaCertif); # si
ModaIntNuncaCert <- MPG[order(MPG$ModeIntNuncaCert,decreasing= TRUE), ]; View(ModaIntNuncaCert) # si
ModaIntSiCert    <- MPG[order(MPG$ModeIntSiCert,decreasing= TRUE), ];    View(ModaIntSiCert)    # si
MediaNuncaCert   <- MPG[order(MPG$MeanIntNuncaCert,decreasing= TRUE), ]; View(MediaNuncaCert);  # si
MediaIntSiCert   <- MPG[order(MPG$MeanIntSiCert,decreasing= TRUE), ];    View(MediaIntSiCert)   # si
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCertPrimera <- ResProbCert1a %>% filter(ResProbCert1a$ProbCert1a>=0.9); View(ResProbCertPrimera)
ResProbCertPrimera <- ResProbCertPrimera[,c(1,2)]; View(ResProbCertPrimera);
ResProbCertPrimera <- as.data.frame(ResProbCertPrimera); View(ResProbCertPrimera)
##write.csv(ResProbCertPrimera,'CSVFiles/ResultadosProbCertPrimera.csv')
xtable(ResProbCertPrimera,type="latex")#,file="Tablas/TablaRPCPrimera.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResProbNuncaCertPrimeros <- ResProbNuncaCert %>% filter(ResProbNuncaCert$ProbNuncaCert>=0.75);
ResProbNuncaCertPrimeros <- ResProbNuncaCertPrimeros[,c(1,3,4)]; View(ResProbNuncaCertPrimeros)
#write.csv(ResProbNuncaCertPrimeros,'CSVFiles/ResultadosProbNuncaCert.csv')
xtable(ResProbNuncaCertPrimeros,type="latex")#,file="Tablas/TablaRPNuncaCPrimera.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResMaxIntCert <- MaxIntCert %>% filter(MaxIntCert$MaxIntSiCert>=4);
ResMaxIntCert <- ResMaxIntCert[,c(1,8,9,11)]; View(ResMaxIntCert)
#write.csv(ResMaxIntCert,'CSVFiles/ResultadosMaximIntCertifica.csv')
xtable(ResMaxIntCert,type="latex")#,file="Tablas/TablaMaxIC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResMaxIntNuncaCert <- MaxIntNuncaCertif%>% filter(MaxIntNuncaCertif$MaxIntNuncaCert>=4);
ResMaxIntNuncaCert <- ResMaxIntNuncaCert[,c(1,4,5,2,7)]; View(ResMaxIntNuncaCert);
#write.csv(ResMaxIntNuncaCert,'CSVFiles/ResultadosMaxIntNuncaCert.csv')
xtable(ResMaxIntNuncaCert,type="latex")#,file="Tablas/TablaRMaxINC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResModaIntNuncaCert <- ModaIntNuncaCert%>% filter(ModaIntNuncaCert$ModeIntNuncaCert>=2);
ResModaIntNuncaCert <- ResModaIntNuncaCert[, c(1,7,3,4)]; View(ResModaIntNuncaCert); 
#write.csv(ResModaIntNuncaCert,'CSVFiles/ResultadosModaIntNuncaCer.csv')
xtable(ResModaIntNuncaCert,type="latex")#,file="Tablas/TablaRModaINC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResModaIntSiCert <- ModaIntSiCert%>% filter(ModaIntSiCert$MaxIntSiCert>=2);
ResModaIntSiCert <- ResModaIntSiCert[, c(1,11,8,9)]; View(ResModaIntSiCert); 
#write.csv(ResModaIntSiCert,'CSVFiles/ResultadosModaIntSiCert.csv')
xtable(ResModaIntSiCert,type="latex")#,file="Tablas/TablaRModaISC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResMediaNuncaCert <- MediaNuncaCert%>% filter(MediaNuncaCert$MeanIntNuncaCert>=2);
ResMediaNuncaCert <-ResMediaNuncaCert[, c(1,5,3,4,7)]; View(ResMediaNuncaCert); 
#write.csv(ResMediaNuncaCert,'CSVFiles/ResultadosMediaNuncaCert.csv')
xtable(ResMediaNuncaCert,type="latex")#,file="Tablas/TablaRMediaNC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ResMediaSiCert <- MediaIntSiCert%>% filter(MediaIntSiCert$MeanIntSiCert>=2)
ResMediaSiCert <- ResMediaSiCert[, c(1,9,8,10,11)]; View(ResMediaSiCert); 
#write.csv(ResMediaSiCert,'CSVFiles/ResultadosMediaIntSiCert.csv')
xtable(ResMediaSiCert,type="latex")#,file="Tablas/TablaRMediaSC.tex")
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
save.image("Repositorio/WkspreprocessingAnalisis.RData")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
xtable(ResProbCertPrimera,type="latex")#,file="Tablas/TablaRPCPa.tex")
xtable(ResProbNuncaCertPrimeros,type="latex")#,file="Tablas/TablaRPNCPa.tex")
xtable(ResMaxIntCert,type="latex")#,file="Tablas/TablaMICa.tex")
xtable(ResMaxIntNuncaCert,type="latex")#,file="Tablas/TablaRMINCa.tex")
xtable(ResModaIntNuncaCert,type="latex")#,file="Tablas/TablaRMINCa.tex")
xtable(ResModaIntSiCert,type="latex")#,file="Tablas/TablaRMISCa.tex")
xtable(ResMediaNuncaCert,type="latex")#,file="Tablas/TablaRMNCa.tex")
xtable(ResMediaSiCert,type="latex")#,file="Tablas/TablaRMSCa.tex")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
View(MiBDD)
summary(MiBDD)
#-----------------------------------------------------------------------------------------------------
MiBDD$Plantel <- factor(MiBDD$Plantel,
                        levels = c('CH','GAM','DV','IZT','SLT','PESCER1','PESCER2','PESCER3','PESCER4',
                                   'PESCER5','PESCER6','PESCER7','PESCER8'),
                        labels = c('CH','GAM','DV','IZT','SLT','PESCER','PESCER','PESCER','PESCER',
                                   'PESCER','PESCER','PESCER','PESCER'))
#-----------------------------------------------------------------------------------------------------
save.image("Repositorio/WkspceActualizado.RData")
DatosFinal <- MiBDD; #write.csv(MiBDD,"CSVFiles/BDDActual.csv")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
library(dplyr)
library(readr)
library(foreign)
library(xtable)
library(stargazer)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoPlantel <- table(MiBDD$Plantel); 
#write.csv(ConteoPlantel,'CSVFiles/TablaConteoPlantel.csv')
xtable(ConteoPlantel,type="latex")#,file="Tablas/TablaConteoPlantel.tex")
xtable(ConteoPlantel,type="latex")#,file="Tablas/aTablaConteoPlantel.tex")

ConteoLic     <- table(MiBDD$Lic)
#write.csv(ConteoLic,'CSVFiles/TablaConteoLic.csv')
xtable(ConteoLic,type="latex")#,file="Tablas/TablaConteoLic.tex")
#xtable(ConteoLic,type="latex"#,file="TablaConteoLic.tex")

ConteoAnho <- table(MiBDD$Anho)
#write.csv(ConteoAnho,'CSVFiles/TablaConteoAnho.csv')
xtable(ConteoAnho,type="latex")#,file="Tablas/TablaConteoAnho.tex")
#xtable(ConteoAnho,type="latex")#,file="TablaConteoAnho.tex")

ConteoPlantelCal <- table(MiBDD$Plantel,MiBDD$Cal)
#write.csv(ConteoPlantelCal,'CSVFiles/TablaConteoPlantelCal.csv')
xtable(ConteoPlantelCal,type="latex")#,file="Tablas/TablaConteoPlantelCal.tex")
#xtable(ConteoPlantelCal,type="latex")#,file="TablaConteoPlantelCal.tex")

ConteoPlantelLic <- table(MiBDD$Plantel,MiBDD$Lic)
#write.csv(ConteoPlantelLic,'CSVFiles/TablaConteoPlantelLic.csv')
xtable(ConteoPlantelLic,type="latex")#,file="Tablas/TablaConteoPlantelLic.tex")
#xtable(ConteoPlantelLic,type="latex"#,file="TablaConteoPlanteLic.tex")

ConteoPlantelMaterias <- table(MiBDD$Plantel,MiBDD$Materias)
#write.csv(ConteoPlantelMaterias,'CSVFiles/TablaConteoPlantelMaterias.csv')
xtable(ConteoPlantelMaterias,type="latex")#,file="Tablas/TablaConteoPlantelMaterias.tex")
#xtable(ConteoPlantelMaterias,type="latex"#,file="TablaConteoPlantelMaterias.tex")

ConteoPlantelAnho <- table(MiBDD$Plantel,MiBDD$Anho)
#write.csv(ConteoPlantelAnho,'CSVFiles/TablaConteoPlantelAnho.csv')
xtable(ConteoPlantelAnho,type="latex")#,file="Tablas/TablaConteoPlantelAnho.tex")
#xtable(ConteoPlantelAnho,type="latex"#,file="TablaConteoPlanteAnho.tex")

ConteoAnhoPlantel <- table(MiBDD$Anho,MiBDD$Plantel)
#write.csv(ConteoAnhoPlantel,'CSVFiles/TablaConteoAnhoPlantel.csv')
xtable(ConteoAnhoPlantel,type="latex")#,file="Tablas/TablaConteoAnhoPlantel.tex")
#xtable(ConteoAnhoPlantel,type="latex"#,file="TablaConteoAnhoPlante.tex")

ConteoLicGen <- table(MiBDD$Lic,MiBDD$Gen)
#write.csv(ConteoLicGen,'CSVFiles/TablaConteoLicGen.csv')
xtable(ConteoLicGen,type="latex")#,file="Tablas/TablaConteoLicGen.tex")
#xtable(ConteoLicGen,type="latex"#,file="TablaConteoLicGen.tex")

ConteoLicCal <- table(MiBDD$Lic,MiBDD$Cal)
#write.csv(ConteoLicCal,'CSVFiles/TablaConteoLicCal.csv')
xtable(ConteoLicCal,type="latex")#,file="Tablas/TablaConteoLicCal.tex")
#xtable(ConteoLicCal,type="latex"#,file="TablaConteoLicCal.tex")

ConteoLicPlantel <- table(MiBDD$Lic,MiBDD$Plantel)
#write.csv(ConteoLicPlantel,'CSVFiles/TablaConteoLicPlantel.csv')
xtable(ConteoLicPlantel,type="latex")#,file="Tablas/TablaConteoLicPlantel.tex")
#xtable(ConteoLicPlantel,type="latex"#,file="TablaConteoLicPlantel.tex")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoPlantelCalAnho <- table(MiBDD$Plantel,MiBDD$Cal,MiBDD$Anho); print(ConteoPlantelCalAnho)
ConteoAnhoPlantelCal <- table(MiBDD$Anho,MiBDD$Plantel,MiBDD$Cal); print(ConteoAnhoPlantelCal)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoPlantelGen <- table(MiBDD$Plantel,MiBDD$Gen); print(ConteoPlantelGen)
#write.csv(ConteoPlantelGen,'CSVFiles/TablaConteoPlantelGen.csv')
xtable(ConteoPlantelGen,type="latex")#,file="Tablas/TablaConteoPlantelGen.tex")
#xtable(ConteoPlantelGen,type="latex"#,file="TablaConteoPlanteGen.tex")

ConteoGenPlantel <- table(MiBDD$Gen,MiBDD$Plantel)
#write.csv(ConteoGenPlantel,'CSVFiles/TablaConteoGenPlantel.csv')
xtable(ConteoGenPlantel,type="latex")#,file="Tablas/TablaConteoGenPlantel.tex")
#xtable(ConteoGenPlantel,type="latex"#,file="TablaConteoGenPlantel.tex")
#-----------------------------------------------------------------------------------------------------
#write.csv(ConteoAnhoPlantelCal,'CSVFiles/TablaConteoAnhoPlantelCal.csv')
#write.csv(ConteoPlantelCalAnho,'CSVFiles/TablaConteoPlantelCalAnho.csv')
#-----------------------------------------------------------------------------------------------------
save.image("Repositorio/WkspceActualizado.RData")
#-----------------------------------------------------------------------------------------------------
setwd("~/Desktop/MiGithub/CertProy/ArticuloCertGral")
load("Repositorio/WkspceActualizado.RData")
#-----------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(foreign)
library(xtable)
library(stargazer)

#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
View(MPG)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert1a    <- MPG[order(MPG$ProbCert1a,decreasing= TRUE), ];       View(ResProbCert1a);    # si
summary(ResProbCert1a)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ProbCert<- ResProbCert1a %>% filter(ResProbCert1a$ProbCert1a>=0.9);
ResProbCert <- ResProbCertPrimera[,c(1,2)]; View(ResProbCert);
ResProbCert <- as.data.frame(ResProbCertPrimera); View(ResProbCert)
summary(ResProbCert)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert2 <- ResProbCert1a %>% filter(ResProbCert1a$ProbCert1a>=0.75 & 
                                           ProbCert1a < 0.9); 
ResProbCert2 <- ResProbCert2[,c(1,2)]; View(ResProbCert2);
ResProbCert2 <- as.data.frame(ResProbCert2); View(ResProbCert2)
summary(ResProbCert2)
table_latex <- xtable(ResProbCert2)
print(table_latex, type = "latex")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert3 <- ResProbCert1a %>%filter(ProbCert1a >= 0.5 & ProbCert1a < 0.75)
View(ResProbCert3)
ResProbCert3 <- ResProbCert3[,c(1,2)]; View(ResProbCert3);
ResProbCert3 <- as.data.frame(ResProbCert3); View(ResProbCert3)
summary(ResProbCert3)
table_latex <- xtable(ResProbCert3)
print(table_latex, type = "latex")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert4 <- ResProbCert1a %>%filter(ProbCert1a == 0.5 )
View(ResProbCert4)
ResProbCert4 <- ResProbCert3[,c(1,2)]; View(ResProbCert4);
ResProbCert4 <- as.data.frame(ResProbCert4); View(ResProbCert4)
summary(ResProbCert4)
table_latex <- xtable(ResProbCert4)
print(table_latex, type = "latex")

#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ResProbCert5 <- ResProbCert1a %>%filter(ProbCert1a < 0.5 )
View(ResProbCert5)
ResProbCert5 <- ResProbCert5[,c(1,2)]; View(ResProbCert5);
ResProbCert5 <- as.data.frame(ResProbCert5); View(ResProbCert5)
summary(ResProbCert5)
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
#-----------------------------------------------------------------------------------------------------
# RECORDATORIO DE TABLAS
#-----------------------------------------------------------------------------------------------------
print(ConteoPlantelCal);print(ConteoPlantelLic);print(ConteoPlantelMaterias);
print(ConteoPlantelAnho);print(ConteoAnhoPlantel);print(ConteoLicGen);
print(ConteoLicCal);print(ConteoLicPlantel);print(ConteoPlantelCalAnho);
print(ConteoAnhoPlantelCal);print(ConteoPlantelGen);print(ConteoGenPlantel);
print(ConteoPlantel);print(ConteoAnho);print(ConteoLic)
#-----------------------------------------------------------------------------------------------------
pdf("Graficas/BarPlotLic.pdf")
barplot(ConteoLic,
        legend=levels(MiBDD$Lic),
        xlab = "Licenciaturas",
        ylab = "Numero Certificaciones",
        beside = TRUE,
        main = "Certificaciones por licenciatura",
        ylim = c(0,max(ConteoLic)+10000),
        col = rainbow(19,alpha = 0.35),
        border = "blue")
grid(30, 10, lwd = 2)
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotLic.pdf")
ggplot(MiBDD, aes(Lic, fill = Lic ) ) +  geom_bar()+ coord_flip()+
  ggtitle("Certrificaciones por Licenciatura")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnho.pdf")
barplot(ConteoAnho,
        xlab = "Año",
        ylab = "Numero Certificaciones",
        beside = TRUE,
        main = "Certificaciones por año",
        ylim = c(0,max(ConteoAnho)+19000),
        col = rainbow(22,alpha = 0.35),
        border = "blue")
grid(30, 10, lwd = 2)
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnho.pdf")
ggplot(MiBDD, aes(Anho, fill = Anho ) ) +  geom_bar()+ coord_flip()+
  ggtitle("Certrificaciones por año")+
  xlab("Año")+
  ylab("Certificaciones realizadas")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/barplotPlanteles.pdf")
barplot(ConteoPlantel,
        legend=levels(MiBDD$Plantel),
        xlab = "Planteles",
        ylab = "Numero Certificaciones",
        beside = TRUE,
        main = "Certificaciones en cada uno de los planteles de la UACM",
        ylim = c(0,max(ConteoPlantel)+10000),
        col = rainbow(6,alpha = 0.35),
        border = "blue")
grid(30, 10, lwd = 2)
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlanteles.pdf")
ggplot(MiBDD, aes(Plantel, fill = Plantel ) ) +  geom_bar()+ coord_flip()+
  ggtitle("Certrificaciones por Plantel")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas")
dev.off()
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
colnames(ConteoPlantelCal) <- c("Certifica","No Certifica")
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
#------------------------------------------------------------------------------
ConteoPlantelCal <- table(MiBDD$Plantel,MiBDD$Cal)
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelCal.pdf")
ggplot(MiBDD, aes(Plantel, fill = Cal) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certrificaciones por Planteles: Certificacion vs No Certificacion")+
  xlab("Plantles")+
  ylab("Certificaciones realizadas")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelCal3.pdf")
ggplot(MiBDD, aes(Plantel, fill = Cal) ) +
  geom_bar(position = "stack")+
  ggtitle("Certrificaciones por Planteles: Certificacion vs No Certificacion")+
  xlab("Plantles")+
  ylab("Certificaciones realizadas")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelCal2.pdf")
ggplot(MiBDD, aes(Plantel, fill=Cal ) ) +
  geom_bar(position="dodge")+
  ggtitle("Certrificaciones por Planteles: Certificacion vs No Certificacion")+
  xlab("Plantles")+
  ylab("Certificaciones realizadas")
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/BarplotPlantelCal.pdf")
barplot(ConteoPlantelCal,
        col = rainbow(6,alpha = 0.35),
        ylim =c(0,max(ConteoPlantelCal)+50000),
        beside = TRUE,
        xlab = "Planeteles de la Universidad",
        ylab = "Numero de certificaciones",
        main = "Certificaciones en los Planteles",
        legend=levels(MiBDD$Plantel))
dev.off()
#------------------------------------------------------------------------------
ConteoPlantelLic <- table(MiBDD$Plantel,MiBDD$Lic)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelLic.pdf")
ggplot(MiBDD, aes(Plantel, fill = Lic) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certificaciones por Licenciatura en Planteles")+
  xlab("Plantees")+
  ylab("Certificaciones realizadas por licenciatura")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelLic2.pdf")
ggplot(MiBDD, aes(Plantel, fill = Lic) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Licenciatura")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por licenciatura")
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelLic3.pdf")
ggplot(MiBDD, aes(Plantel, fill = Lic) ) +
  geom_bar(position = "dodge")+
  ggtitle("Certificaciones en Planteles por Licenciatura")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por licenciatura")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/BarplotPlantelLic.pdf")
barplot(ConteoPlantelLic,
        col = rainbow(6,alpha = 0.35),
        ylim =c(0,max(ConteoPlantelLic)+50000),
        beside = TRUE,
        xlab = "Planeteles de la Universidad",
        ylab = "Numero de certificaciones",
        main = "Certificaciones en los Planteles",
        legend=levels(MiBDD$Plantel))
dev.off()
#------------------------------------------------------------------------------
ConteoPlantelMaterias <- table(MiBDD$Plantel,MiBDD$Materias)
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelMaterias.pdf")
ggplot(MiBDD, aes(Plantel, fill = Materias) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Materia")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Materias")
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelMaterias2.pdf")
ggplot(MiBDD, aes(Plantel, fill = Materias) ) +
  geom_bar()+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Materias")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Materias")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelMaterias3.pdf")
ggplot(MiBDD, aes(Plantel, fill = Materias) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Materias")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Materias")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotMateriasPlantel.pdf")
ggplot(MiBDD, aes(Materias, fill = Plantel) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Licenciatura")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por licenciatura")
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotMateriasPlantel2.pdf")
ggplot(MiBDD, aes(Materias, fill = Plantel) ) +
  geom_bar()+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Materias")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Materias")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotMateriasPlantel3.pdf")
ggplot(MiBDD, aes(Materias, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Materias")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Materias")
dev.off()
#----------------------------------------------------------------------------
ConteoPlantelAnho <- table(MiBDD$Plantel,MiBDD$Anho)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelAnho.pdf")
ggplot(MiBDD, aes(Plantel, fill = Anho) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Año")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Año")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelAnho.pdf")
ggplot(MiBDD, aes(Plantel, fill = Anho) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Año")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Año")
dev.off()
#----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoPlantel.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar(position = "stack")+ coord_flip()+
  ggtitle("Certificaciones por Año en Planteles")+
  xlab("Años")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoPlantel.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por Año en Planteles")+
  xlab("Año")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoPlantel2.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por año en Planteles")+
  xlab("Años")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoPlantel3.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar( )+ coord_flip()+
  ggtitle("Certificaciones por año en Planteles")+
  xlab("Años")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
ConteoPlantelGen <- table(MiBDD$Plantel,MiBDD$Gen);
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenPlantel.pdf")
ggplot(MiBDD, aes(Gen, fill = Plantel) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por generacion en Planteles")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenPlantel2.pdf")
ggplot(MiBDD, aes(Gen, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por generacion en Planteles")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#------------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelGen3.pdf")
ggplot(MiBDD, aes(Plantel, fill = Gen) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones en Planteles por generacion")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Generaciones")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotPlantelGen4.pdf")
ggplot(MiBDD, aes(Plantel, fill = Gen) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones en Planteles por Generacion")+
  xlab("Planteles")+
  ylab("Certificaciones realizadas por Generaciones")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/BarplotPlantelGen.pdf")
barplot(ConteoPlantelGen,
        col = rainbow(6,alpha = 0.35),
        ylim =c(0,max(ConteoPlantelLic)+50000),
        beside = TRUE,
        xlab = "Planeteles de la Universidad",
        ylab = "Numero de certificaciones",
        main = "Certificaciones en los Planteles",
        legend=levels(MiBDD$Plantel))
dev.off()
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoGenPlantel <- table(MiBDD$Gen,MiBDD$Plantel)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenPlantel.pdf")
ggplot(MiBDD, aes(Gen, fill = Plantel) ) +
  geom_bar(position = "stack")+  ggtitle("Certificaciones por generacion en Planteles")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenPlantel2.pdf")
ggplot(MiBDD, aes(Gen, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por generacion en Planteles")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas en Planteles")
dev.off()
#-----------------------------------------------------------------------------
ConteoGenLic <- table(MiBDD$Gen,MiBDD$Lic)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenLic.pdf")
ggplot(MiBDD, aes(Gen, fill = Lic) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por generacion en las licenciaturas")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas por licencitura")
dev.off()

pdf("Graficas/ggplotBarplotGenLic2.pdf")
ggplot(MiBDD, aes(Gen, fill = Lic) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por generacion en las licenciaturas")+
  xlab("Generaciones")+
  ylab("Certificaciones realizadas por licencitura")
dev.off()
#----------------------------------------------------------------------------
ConteoGenMaterias <- table(MiBDD$Gen,MiBDD$Materias)
#----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotGenMaterias.pdf")
ggplot(MiBDD, aes(Gen, fill = Materias) ) +
  geom_bar(position = "stack")
ggplot(MiBDD, aes(Gen, fill = Materias) ) +
  geom_bar(position = "dodge")+ coord_flip()
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoAnhoPlantel <- table(MiBDD$Anho,MiBDD$Plantel)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoPlantel.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por Año en Planteles")+
  xlab("Años")+
  ylab("Certificaciones realizadas por planteles")
dev.off()
pdf("Graficas/ggplotBarplotAnhoPlantel2.pdf")
ggplot(MiBDD, aes(Anho, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por Año en Planteles")+
  xlab("Años")+
  ylab("Certificaciones realizadas por planteles")
dev.off()
#-----------------------------------------------------------------------------
ConteoAnhoLic <- table(MiBDD$Anho,MiBDD$Lic)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoLic.pdf")
ggplot(MiBDD, aes(Anho, fill = Lic) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por Año en las licenciaturas")+
  xlab("Años")+
  ylab("Certificaciones realizadas por licenciaturas")
dev.off()
pdf("Graficas/ggplotBarplotAnhoLic2.pdf")
ggplot(MiBDD, aes(Anho, fill = Lic) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por Año en las licenciaturas")+
  xlab("Años")+
  ylab("Certificaciones realizadas por licenciaturas")
dev.off()
#----------------------------------------------------------------------------
ConteoAnhoMaterias <- table(MiBDD$Anho,MiBDD$Materias)
#----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotAnhoMaterias.pdf")
ggplot(MiBDD, aes(Anho, fill = Materias) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por Año por materias")+
  xlab("Años")+
  ylab("Certificaciones realizadas por materias")
dev.off()
pdf("Graficas/ggplotBarplotAnhoMaterias2.pdf")
ggplot(MiBDD, aes(Anho, fill = Materias) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por Año por materias")+
  xlab("Años")+
  ylab("Certificaciones realizadas por materias")
dev.off()
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>
ConteoLicGen <- table(MiBDD$Lic,MiBDD$Gen)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotLicGen.pdf")
ggplot(MiBDD, aes(Lic, fill = Gen) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por licenciatura por generacion")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas por generaciones")
dev.off()
pdf("Graficas/ggplotBarplotLicGen2.pdf")
ggplot(MiBDD, aes(Lic, fill = Gen) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por licenciatura por generacion")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas por generaciones")
dev.off()
#-----------------------------------------------------------------------------
ConteoLicCal <- table(MiBDD$Lic,MiBDD$Cal)
#-----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotLicCal.pdf")
ggplot(MiBDD, aes(Lic, fill = Cal) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por licenciatura por certificacion")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas")
dev.off()
pdf("Graficas/ggplotBarplotLicCal2.pdf")
ggplot(MiBDD, aes(Lic, fill = Cal) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por licenciatura por certificacion")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas")
dev.off()
#-----------------------------------------------------------------------------
ConteoLicPlantel <- table(MiBDD$Lic,MiBDD$Plantel)
#----------------------------------------------------------------------------
pdf("Graficas/ggplotBarplotLicPlantel.pdf")
ggplot(MiBDD, aes(Lic, fill = Plantel) ) +
  geom_bar(position = "stack")+
  ggtitle("Certificaciones por licenciatura por planteles")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas")
dev.off()
pdf("Graficas/ggplotBarplotLicPlantel2.pdf")
ggplot(MiBDD, aes(Lic, fill = Plantel) ) +
  geom_bar(position = "dodge")+ coord_flip()+
  ggtitle("Certificaciones por licenciatura por planteles")+
  xlab("Licenciaturas")+
  ylab("Certificaciones realizadas")
dev.off()
#<<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>><<==>><<==>><<==>><<==<<==>><<==>>

