library(NetCoMi)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

### Ouest Loscolo ####
OL <- filter(data,Code_point_Libelle == "Ouest Loscolo" )
rownames(OL) <- OL$Date
OL <- dplyr::select(OL,Actinoptychus:Coscinodiscophycidae)
OL[is.na(OL)]<- 0

OL <- as.matrix(OL)

net <- netConstruct(data = OL, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 35),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
     layout = "spring",
     nodeColor = "cluster", 
     nodeSize = "degree",
     nodeTrans = 0,
     title1 = "Ouest Loscolo",
     shortenLabels = "none",
     labelScale = F,
     labelFont = 1,
     nodeFilter = "none",
     rmSingles = "none",
     highlightHubs = T,
     edgeFilter = "none",
     negDiffCol = T,
     posCol = "green4",
     negCol = "red3",
     showTitle = TRUE,
     cexTitle = 1.3,
     cexLabels = 1,
     cexNodes = 1)

### Bois de la Chaise large ####
BCL <- filter(data,Code_point_Libelle == "Bois de la Chaise large" )
rownames(BCL) <- BCL$Date
BCL <- dplyr::select(BCL,Actinoptychus:Coscinodiscophycidae)
BCL[is.na(BCL)]<- 0

BCL <- as.matrix(BCL)

net <- netConstruct(data = BCL, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 34),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Bois de la Chaise large",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Teychan Bis ####
TEB <- filter(data,Code_point_Libelle == "Teychan bis" )
rownames(TEB) <- TEB$Date
TEB <- dplyr::select(TEB,Actinoptychus:Coscinodiscophycidae)
TEB[is.na(TEB)]<- 0

TEB <- as.matrix(TEB)

net <- netConstruct(data = TEB, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 36),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Teychan bis",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Men er Roue ####
MER <- filter(data,Code_point_Libelle == "Men er Roue" )
rownames(MER) <- MER$Date
MER <- dplyr::select(MER,Actinoptychus:Coscinodiscophycidae)
MER[is.na(MER)]<- 0

MER <- as.matrix(MER)

net <- netConstruct(data = MER, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 36),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Men er Roue",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Loguivy ####
LOG <- filter(data,Code_point_Libelle == "Loguivy" )
rownames(LOG) <- LOG$Date
LOG <- dplyr::select(LOG,Actinoptychus:Coscinodiscophycidae)
LOG[is.na(LOG)]<- 0

LOG <- as.matrix(LOG)

net <- netConstruct(data = LOG, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 36),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Loguivy",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### les Hébihens ####
LH <- filter(data,Code_point_Libelle == "les Hébihens" )
rownames(LH) <- LH$Date
LH <- dplyr::select(LH,Actinoptychus:Coscinodiscophycidae)
LH[is.na(LH)]<- 0

LH <- as.matrix(LH)

net <- netConstruct(data = LH, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 35),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "les Hébihens",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Géfosse ####
GEF <- filter(data,Code_point_Libelle == "Géfosse" )
rownames(GEF) <- GEF$Date
GEF <- dplyr::select(GEF,Actinoptychus:Coscinodiscophycidae)
GEF[is.na(GEF)]<- 0

GEF <- as.matrix(GEF)

net <- netConstruct(data = GEF, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 27),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Géfosse",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 3 ####
CL3 <- filter(data, cluster == 3 )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
CL3[is.na(CL3)]<- 0

CL3 <- as.matrix(CL3)

net <- netConstruct(data = CL3, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 242),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 3",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edCL1ilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cabourg ####
CAB <- filter(data,Code_point_Libelle == "Cabourg" )
rownames(CAB) <- CAB$Date
CAB <- dplyr::select(CAB,Actinoptychus:Coscinodiscophycidae)
CAB[is.na(CAB)]<- 0

CAB <- as.matrix(CAB)

net <- netConstruct(data = CAB, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 36),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cabourg",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Antifer ponton pétrolier ####
APP <- filter(data,Code_point_Libelle == "Antifer ponton pétrolier" )
rownames(APP) <- APP$Date
APP <- dplyr::select(APP,Actinoptychus:Coscinodiscophycidae)
APP[is.na(APP)]<- 0

APP <- as.matrix(APP)

net <- netConstruct(data = APP, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 27),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Antifer ponton pétrolier",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### At so ####
ATS <- filter(data,Code_point_Libelle == "At so" )
rownames(ATS) <- ATS$Date
ATS <- dplyr::select(ATS,Actinoptychus:Coscinodiscophycidae)
ATS[is.na(ATS)]<- 0

ATS <- as.matrix(ATS)

net <- netConstruct(data = ATS, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 25),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "At so",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Point 1 Boulogne ####
P1B <- filter(data,Code_point_Libelle == "Point 1 Boulogne" )
rownames(P1B) <- P1B$Date
P1B <- dplyr::select(P1B,Actinoptychus:Coscinodiscophycidae)
P1B[is.na(P1B)]<- 0

P1B <- as.matrix(P1B)

net <- netConstruct(data = P1B, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 28),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Point 1 Boulogne",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)


### Cluster 2 ####
CL2 <- filter(data, cluster == 2 )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
CL2[is.na(CL2)]<- 0

CL2 <- as.matrix(CL2)

net <- netConstruct(data = CL2, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 117),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 2",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Le Cornard ####
LCO <- filter(data,Code_point_Libelle == "Le Cornard" )
rownames(LCO) <- LCO$Date
LCO <- dplyr::select(LCO,Actinoptychus:Coscinodiscophycidae)
LCO[is.na(LCO)]<- 0

LCO <- as.matrix(LCO)

net <- netConstruct(data = LCO, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 39),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Le Cornard",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Auger ####
AUG <- filter(data,Code_point_Libelle == "Auger" )
rownames(AUG) <- AUG$Date
AUG <- dplyr::select(AUG,Actinoptychus:Coscinodiscophycidae)
AUG[is.na(AUG)]<- 0

AUG <- as.matrix(AUG)

net <- netConstruct(data = AUG, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 39),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Auger",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)


### Cluster 4 ####
CL4 <- filter(data, cluster == 4 )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
CL4[is.na(CL4)]<- 0

CL4 <- as.matrix(CL4)

net <- netConstruct(data = CL4, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 78),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 4",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Parc Leucate 2 ####
PL2 <- filter(data,Code_point_Libelle == "Parc Leucate 2")
rownames(PL2) <- PL2$Date
PL2 <- dplyr::select(PL2,Actinoptychus:Coscinodiscophycidae)
PL2[is.na(PL2)]<- 0

PL2 <- as.matrix(PL2)

net <- netConstruct(data = PL2, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 36),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Parc Leucate 2",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Barcares ####
BAR <- filter(data,Code_point_Libelle == "Barcares")
rownames(BAR) <- BAR$Date
BAR <- dplyr::select(BAR,Actinoptychus:Coscinodiscophycidae)
BAR[is.na(BAR)]<- 0

BAR <- as.matrix(BAR)

net <- netConstruct(data = BAR, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 35),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Barcares",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Sète mer ####
SM <- filter(data,Code_point_Libelle == "Sète mer")
rownames(SM) <- SM$Date
SM <- dplyr::select(SM,Actinoptychus:Coscinodiscophycidae)
SM[is.na(SM)]<- 0

SM <- as.matrix(SM)

net <- netConstruct(data = SM, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 39),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Sète mer",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)


### Bouzigues (a) ####
BZG <- filter(data,Code_point_Libelle == "Bouzigues (a)")
rownames(BZG) <- BZG$Date
BZG <- dplyr::select(BZG,Actinoptychus:Coscinodiscophycidae)
BZG[is.na(BZG)]<- 0

BZG <- as.matrix(BZG)

net <- netConstruct(data = BZG, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 42),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Bouzigues (a)",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Anse de Carteau 2 ####
ADC2 <- filter(data,Code_point_Libelle == "Anse de Carteau 2")
rownames(ADC2) <- ADC2$Date
ADC2 <- dplyr::select(ADC2,Actinoptychus:Coscinodiscophycidae)
ADC2[is.na(ADC2)]<- 0

ADC2 <- as.matrix(ADC2)

net <- netConstruct(data = ADC2, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 40),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Anse de Carteau 2",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)


### 22B - Toulon gde rade ####
TLN <- filter(data,Code_point_Libelle == "22B - Toulon gde rade")
rownames(TLN) <- TLN$Date
TLN <- dplyr::select(TLN,Actinoptychus:Coscinodiscophycidae)
TLN[is.na(TLN)]<- 0

TLN <- as.matrix(TLN)

net <- netConstruct(data = TLN, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 38),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "22B - Toulon gde rade",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Calvi ####
CAL <- filter(data,Code_point_Libelle == "Calvi")
rownames(CAL) <- CAL$Date
CAL <- dplyr::select(CAL,Actinoptychus:Coscinodiscophycidae)
CAL[is.na(CAL)]<- 0

CAL <- as.matrix(CAL)

net <- netConstruct(data = CAL, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 47),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Calvi",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)


### Diana centre ####
DIC <- filter(data,Code_point_Libelle == "Diana centre")
rownames(DIC) <- DIC$Date
DIC <- dplyr::select(DIC,Actinoptychus:Coscinodiscophycidae)
DIC[is.na(DIC)]<- 0

DIC <- as.matrix(DIC)

net <- netConstruct(data = DIC, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 40),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Diana centre",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 1 ####
CL1 <- filter(data, cluster == 1 )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
CL1[is.na(CL1)]<- 0

CL1 <- as.matrix(CL1)

net <- netConstruct(data = CL1, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 320),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 1",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

# Creer un sous réseau bloom vs non bloom
{
  data$EpBloom <- ifelse(data$P_dominance >=0, "yes", NA)
group_vec <- filter(data,Code_point_Libelle == "Ouest Loscolo" )$EpBloom
sel <- which(group_vec %in% c(NA, "yes"))
group_vec <- group_vec[sel]
group_vec[is.na(group_vec)] <- "no"
OL <- OL[sel,]

netbloom <- netConstruct(OL,dataType = "counts",measure = "spearman", 
                         group = group_vec,
                         filtTax = "numbSamp",filtTaxPar = list(numbSamp = 35),
                         filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                         normMethod = "none", dissFunc = "signed")

props_bloom <- netAnalyze(netbloom, clustMethod = "cluster_fast_greedy",
                          hubPar = c("eigenvector"),
                          graphlet = F,
                          connectivity = T,
                          centrLCC = FALSE)

summary(props_bloom)

plt <- plot(props_bloom,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Ouest Loscolo",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "inboth",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1,
            sameLayout = T,
            groupNames = c("Non bloom", "Bloom"),)

comp_bloom <- netCompare(props_bloom, 
                          permTest = T, 
                          verbose = FALSE,
                          seed = 123456)

summary(comp_bloom, 
        groupNames = c("Non bloom", "Bloom"))
}
# Creer deux réseaux pour comparer les clusters 1 et 3
{
group_vec <- filter(data,cluster == 1 | cluster == 3 )$cluster
sel <- which(group_vec %in% c(1, 3))
group_vec <- group_vec[sel]
CL13 <- filter(data,cluster == 1 | cluster == 3 )
CL13 <- CL13[sel,]

CL13 <- dplyr::select(CL13,Actinoptychus:Coscinodiscophycidae)
CL13[is.na(CL13)]<- 0

CL13 <- as.matrix(CL13)

net13 <- netConstruct(CL13,dataType = "counts",measure = "spearman", 
                         group = group_vec,
                         filtTax = "numbSamp",filtTaxPar = list(numbSamp = 562),
                         filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                         normMethod = "none", dissFunc = "signed")

props_13 <- netAnalyze(net13, clustMethod = "cluster_fast_greedy",
                          hubPar = c("eigenvector"),
                          graphlet = F,
                          connectivity = T,
                          centrLCC = FALSE)

summary(props_13)

plt <- plot(props_13,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1,
            sameLayout = T,
            groupNames = c("Cluster 1", "Cluster 3"),)

comp_bloom <- netCompare(props_13, 
                         permTest = T, 
                         verbose = FALSE,
                         seed = 123456)

summary(comp_bloom, 
        groupNames = c("Cluster 1", "Cluster 3"))

}

# Creer un sous reseau bloom et non bloom au niveau du cluster
{data$EpBloom <- ifelse(data$P_dominance >=0, "yes", NA)
group_vec <- filter(data,cluster == 3 )$EpBloom
sel <- which(group_vec %in% c(NA, "yes"))
group_vec <- group_vec[sel]
group_vec[is.na(group_vec)] <- "no"


CL3 <- CL3[sel,]

netbloom <- netConstruct(CL3,dataType = "counts",measure = "spearman", 
                         group = group_vec,
                         filtTax = "numbSamp",filtTaxPar = list(numbSamp = 242),
                         filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                         normMethod = "none", dissFunc = "signed")

props_bloom <- netAnalyze(netbloom, clustMethod = "cluster_fast_greedy",
                          hubPar = c("eigenvector"),
                          graphlet = F,
                          connectivity = T,
                          centrLCC = FALSE)

summary(props_bloom)

plt <- plot(props_bloom,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 3",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "inboth",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1,
            sameLayout = T,
            groupNames = c("Non bloom", "Bloom"),)

comp_bloom <- netCompare(props_bloom, 
                         permTest = T, 
                         verbose = FALSE,
                         seed = 123456)

summary(comp_bloom, 
        groupNames = c("Non bloom", "Bloom"))
}
