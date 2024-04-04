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
                    normMethod = "none", dissFunc = "signed",weighted = T)

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

# Creer deux réseaux pour comparer les clusters 1 et 2
{
  group_vec <- filter(data,cluster == 1 | cluster == 2 )$cluster
  sel <- which(group_vec %in% c(1, 2))
  group_vec <- group_vec[sel]
  CL12 <- filter(data,cluster == 1 | cluster == 2 )
  CL12 <- CL12[sel,]
  
  CL12 <- dplyr::select(CL12,Actinoptychus:Coscinodiscophycidae)
  CL12[is.na(CL12)]<- 0
  
  CL12 <- as.matrix(CL12)
  
  net12 <- netConstruct(CL12,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 437),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_12 <- netAnalyze(net12, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_12)
  
  plt <- plot(props_12,
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
              groupNames = c("Cluster 1", "Cluster 2"),)
  
  comp_bloom <- netCompare(props_12, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 1", "Cluster 2"))
  
}

# Creer deux réseaux pour comparer les clusters 1 et 4
{
  group_vec <- filter(data,cluster == 1 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(1, 4))
  group_vec <- group_vec[sel]
  CL14 <- filter(data,cluster == 1 | cluster == 4 )
  CL14 <- CL14[sel,]
  
  CL14 <- dplyr::select(CL14,Actinoptychus:Coscinodiscophycidae)
  CL14[is.na(CL14)]<- 0
  
  CL14 <- as.matrix(CL14)
  
  net14 <- netConstruct(CL14,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 398),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_14 <- netAnalyze(net14, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_14)
  
  plt <- plot(props_14,
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
              groupNames = c("Cluster 1", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_14, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 1", "Cluster 4"))
  
}






# Creer deux réseaux pour comparer les clusters 2 et 3
{
  group_vec <- filter(data,cluster == 2 | cluster == 3 )$cluster
  sel <- which(group_vec %in% c(2, 3))
  group_vec <- group_vec[sel]
  CL23 <- filter(data,cluster == 2 | cluster == 3 )
  CL23 <- CL23[sel,]
  
  CL23 <- dplyr::select(CL23,Actinoptychus:Coscinodiscophycidae)
  CL23[is.na(CL23)]<- 0
  
  CL23 <- as.matrix(CL23)
  
  net23 <- netConstruct(CL23,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 398),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_23 <- netAnalyze(net23, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_23)
  
  plt <- plot(props_23,
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
              groupNames = c("Cluster 2", "Cluster 3"),)
  
  comp_bloom <- netCompare(props_23, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 2", "Cluster 3"))
  
}


# Creer deux réseaux pour comparer les clusters 2 et 4
{
  group_vec <- filter(data,cluster == 2 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(2, 4))
  group_vec <- group_vec[sel]
  CL24 <- filter(data,cluster == 2 | cluster == 4 )
  CL24 <- CL24[sel,]
  
  CL24 <- dplyr::select(CL24,Actinoptychus:Coscinodiscophycidae)
  CL24[is.na(CL24)]<- 0
  
  CL24 <- as.matrix(CL24)
  
  net24 <- netConstruct(CL24,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 195),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_24 <- netAnalyze(net24, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_24)
  
  plt <- plot(props_24,
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
              groupNames = c("Cluster 2", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_24, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 2", "Cluster 4"))
  
}

# Creer deux réseaux pour comparer les clusters 3 et 4
{
  group_vec <- filter(data,cluster == 3 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(3, 4))
  group_vec <- group_vec[sel]
  CL34 <- filter(data,cluster == 3 | cluster == 4 )
  CL34 <- CL34[sel,]
  
  CL34 <- dplyr::select(CL34,Actinoptychus:Coscinodiscophycidae)
  CL34[is.na(CL34)]<- 0
  
  CL34 <- as.matrix(CL34)
  
  net34 <- netConstruct(CL34,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 328),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_34 <- netAnalyze(net34, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_34)
  
  plt <- plot(props_34,
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
              groupNames = c("Cluster 3", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_34, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 3", "Cluster 4"))
  
}


# Charger le package igraph
library(igraph)

#### Travail sur le cluster 3 #####
# Relancer le graphe global du cluster 3 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster3 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster3

# Visualisation générale avec IGRAPH
plot(cluster3)
wc <- cluster_fast_greedy(cluster3)

V(cluster3)$label <- V(cluster3)$name
#V(cluster3)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster3)$page_rank <- round(page.rank(cluster3)$vector, 2)
V(cluster3)$betweenness <- round(betweenness(cluster3), 2)
V(cluster3)$degree <- degree(cluster3)
V(cluster3)$size <- V(cluster3)$degree
V(cluster3)$comm <- membership(wc)
V(cluster3)$color <- colorize(membership(wc))
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- "black"

viz3 <- hchart(cluster3, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz3, "output/graphs/Reseaux/HTML/cluster3.html")

# Metriques réseau global 
S_net <-  vcount(cluster3) # nombre de noeuds
L_net <- ecount(cluster3) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster3, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster3,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster3, weights=E(cluster3)$weight)) 

Edge_connect <- edge.connectivity(cluster3) # Edge connectivity = adhesion

Modularity <- modularity(cluster3,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster3) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster3)) #Nombre de liens moyen

assort <- assortativity_degree(cluster3,directed = F) #assortativite

diss <- mean(1 - E(cluster3)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster3,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster3)) # Mean edge betweeness

nat_connect_good <- natural.connectivity(assoMat) # Connectivite naturel

adj <- as.matrix(as_adjacency_matrix(cluster3, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL3df <- filter(data,cluster == 3)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
for (i in 1:nrow(CL3)){ 
  i = 1
spe <- as.data.frame(CL3[i,])
colnames(spe) <- "Count" 
spe$phyto <- rownames(spe)
spe <- left_join(phyto_index,spe, by = join_by(phyto))
spe <- filter(spe,Count>0)
spe$Pindex <- as.numeric(spe$Pindex)

station <- CL3df[i,]$Code_point_Libelle
date <- CL3df[i,]$Date

vids <- spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)

# Metriques réseau global 
S_net <-  vcount(sub) # nombre de noeuds
L_net <- ecount(sub) # nombre de liens
Z_mat <- L_net / S_net # linkage density or average number of links per nodes
C_net <- edge_density(sub, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
Modularity <- modularity(sub,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
m_degree <- mean(degree(sub)) #Nombre de liens moyen
assort <- assortativity_degree(sub,directed = F) #assortativite
diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
trans <- transitivity(sub,type = "global") #Transitivity
mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

data_results_reseaux[i,1] <- station
data_results_reseaux[i,2] <- date
data_results_reseaux[i,3] <- S_net # nombre de noeuds
data_results_reseaux[i,4] <- L_net # nombre de liens
data_results_reseaux[i,5] <- Z_mat # Densite des liens
data_results_reseaux[i,6] <- C_net # connectance

data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
data_results_reseaux[i,8] <- Edge_connect # adhesion
data_results_reseaux[i,9] <- Modularity #modularite

data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
data_results_reseaux[i,11] <- assort # Assortativite
data_results_reseaux[i,12] <- diss # dissimilarite
data_results_reseaux[i,13] <- trans # transitivite

data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle

data_results_reseaux[i,16] <- length(wc) # Nombre de cluster

colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                    "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                    "Nat_connect","N_clust")

plot(sub,main = paste0(station,date),layout = layout_with_fr)
png(paste0("output/graphs/Reseaux/TS_CLUST3/",station,date,".png"))
plot(sub,main = paste0(station,date),layout = layout_with_fr)
dev.off()
htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/",station,date,".html"))
print(i/nrow(CL3)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster3.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
{chemin_repertoire <- "output/graphs/Reseaux/HTML/"

# Récupérez les noms de fichiers dans le répertoire
noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)

# Supprimez le chemin du répertoire des noms de fichiers
noms_fichiers <- basename(noms_fichiers_complets)

# Affichez les noms de fichiers
cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 3
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL3df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)
  
ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)
  
ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)
  
ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)
  
ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)
  
ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)
  
ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)
  
ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)
  
ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)
  
ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)
  
ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)
  
ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)
  
ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)
  
ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')




CL3_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')



ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')







#### Travail sur le cluster 1 #####
# Relancer le graphe global du cluster 1 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster1 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster1
# Visualisation générale avec IGRAPH
plot(cluster1)
wc <- cluster_fast_greedy(cluster1)

V(cluster1)$label <- V(cluster1)$name
V(cluster1)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster1)$page_rank <- round(page.rank(cluster1)$vector, 2)
V(cluster1)$betweenness <- round(betweenness(cluster1), 2)
V(cluster1)$degree <- degree(cluster1)
V(cluster1)$size <- V(cluster1)$degree
V(cluster1)$comm <- membership(wc)
V(cluster1)$color <- colorize(membership(wc))
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster3)$color <- "black"

viz1 <- hchart(cluster1, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz1, "output/graphs/Reseaux/HTML/cluster1.html")

# Metriques réseau global 
S_net <-  vcount(cluster1) # nombre de noeuds
L_net <- ecount(cluster1) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster1, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster1,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster1, weights=E(cluster1)$weight)) 

Edge_connect <- edge.connectivity(cluster1) # Edge connectivity = adhesion

Modularity <- modularity(cluster1,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster1) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster1)) #Nombre de liens moyen

assort <- assortativity_degree(cluster1,directed = F) #assortativite

diss <- mean(1 - E(cluster1)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster1,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster1)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster1, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL1df <- filter(data,cluster == 1)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
for (i in 1:nrow(CL1)){ 
  spe <- as.data.frame(CL1[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL1df[i,]$Code_point_Libelle
  date <- CL1df[i,]$Date
  if (nrow(spe) != 0){
  vids <- spe$Pindex
  sub <- igraph::subgraph(cluster1, vids)
  viz_sub <- hchart(sub, layout = layout_with_fr)
  
  # Metriques réseau global 
  S_net <-  vcount(sub) # nombre de noeuds
  L_net <- ecount(sub) # nombre de liens
  Z_mat <- L_net / S_net # linkage density or average number of links per nodes
  C_net <- edge_density(sub, loops = FALSE) #connectance
  
  # Average path length
  avg_path_length <- mean_distance(
    sub,
    directed = FALSE,
    unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
  )
  
  Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
  
  wc <- cluster_fast_greedy(sub)
  Modularity <- modularity(sub,membership = membership(wc)) # Modularity
  
  Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
  m_degree <- mean(degree(sub)) #Nombre de liens moyen
  assort <- assortativity_degree(sub,directed = F) #assortativite
  diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
  trans <- transitivity(sub,type = "global") #Transitivity
  mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
  adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
  diag(adj) <- 1
  nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
  
  data_results_reseaux[i,1] <- station
  data_results_reseaux[i,2] <- date
  data_results_reseaux[i,3] <- S_net # nombre de noeuds
  data_results_reseaux[i,4] <- L_net # nombre de liens
  data_results_reseaux[i,5] <- Z_mat # Densite des liens
  data_results_reseaux[i,6] <- C_net # connectance
  
  data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
  data_results_reseaux[i,8] <- Edge_connect # adhesion
  data_results_reseaux[i,9] <- Modularity #modularite
  
  data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
  data_results_reseaux[i,11] <- assort # Assortativite
  data_results_reseaux[i,12] <- diss # dissimilarite
  data_results_reseaux[i,13] <- trans # transitivite
  
  data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
  data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
  
  data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  png(paste0("output/graphs/Reseaux/TS_CLUST1/",station,date,".png"))
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  dev.off()
  htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL1/",station,date,".html"))
  print(i/nrow(CL1)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster1.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL1/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 3
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL1df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year,-Date2)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')




CL1_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')



ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')







#### Travail sur le cluster 2 #####
# Relancer le graphe global du cluster 2 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster2 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster2

# Visualisation générale avec IGRAPH
plot(cluster2)
wc <- cluster_fast_greedy(cluster2)

V(cluster2)$label <- V(cluster2)$name
V(cluster2)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster2)$page_rank <- round(page.rank(cluster2)$vector, 2)
V(cluster2)$betweenness <- round(betweenness(cluster2), 2)
V(cluster2)$degree <- degree(cluster2)
V(cluster2)$size <- V(cluster2)$degree
V(cluster2)$comm <- membership(wc)
V(cluster2)$color <- colorize(membership(wc))
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster3)$color <- "black"

viz2 <- hchart(cluster2, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz2, "output/graphs/Reseaux/HTML/cluster2.html")

# Metriques réseau global 
S_net <-  vcount(cluster2) # nombre de noeuds
L_net <- ecount(cluster2) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster2, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster2,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster2, weights=E(cluster2)$weight)) 

Edge_connect <- edge.connectivity(cluster2) # Edge connectivity = adhesion

Modularity <- modularity(cluster2,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster2) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster2)) #Nombre de liens moyen

assort <- assortativity_degree(cluster2,directed = F) #assortativite

diss <- mean(1 - E(cluster2)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster2,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster2)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster2, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL2df <- filter(data,cluster == 2)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
for (i in 1:nrow(CL2)){ 
  spe <- as.data.frame(CL2[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL2df[i,]$Code_point_Libelle
  date <- CL2df[i,]$Date
  if (nrow(spe) != 0){
    vids <- spe$Pindex
    sub <- igraph::subgraph(cluster2, vids)
    viz_sub <- hchart(sub, layout = layout_with_fr)
    
    # Metriques réseau global 
    S_net <-  vcount(sub) # nombre de noeuds
    L_net <- ecount(sub) # nombre de liens
    Z_mat <- L_net / S_net # linkage density or average number of links per nodes
    C_net <- edge_density(sub, loops = FALSE) #connectance
    
    # Average path length
    avg_path_length <- mean_distance(
      sub,
      directed = FALSE,
      unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
    )
    
    Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
    
    wc <- cluster_fast_greedy(sub)
    Modularity <- modularity(sub,membership = membership(wc)) # Modularity
    
    Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
    m_degree <- mean(degree(sub)) #Nombre de liens moyen
    assort <- assortativity_degree(sub,directed = F) #assortativite
    diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
    trans <- transitivity(sub,type = "global") #Transitivity
    mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
    adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
    diag(adj) <- 1
    nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- S_net # nombre de noeuds
    data_results_reseaux[i,4] <- L_net # nombre de liens
    data_results_reseaux[i,5] <- Z_mat # Densite des liens
    data_results_reseaux[i,6] <- C_net # connectance
    
    data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
    data_results_reseaux[i,8] <- Edge_connect # adhesion
    data_results_reseaux[i,9] <- Modularity #modularite
    
    data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
    data_results_reseaux[i,11] <- assort # Assortativite
    data_results_reseaux[i,12] <- diss # dissimilarite
    data_results_reseaux[i,13] <- trans # transitivite
    
    data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
    data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
    data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
  }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  png(paste0("output/graphs/Reseaux/TS_CLUST2/",station,date,".png"))
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  dev.off()
  htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL2/",station,date,".html"))
  print(i/nrow(CL2)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster2.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL2/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 2
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL2df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux[-c(653,654,655),] # supprimer les doublons

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')




CL2_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')



ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')




#### Travail sur le cluster 4 #####
# Relancer le graphe global du cluster 4 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster4 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster4

# Visualisation générale avec IGRAPH
plot(cluster4)
wc <- cluster_fast_greedy(cluster4)

V(cluster4)$label <- V(cluster4)$name
#V(cluster4)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster4)$page_rank <- round(page.rank(cluster4)$vector, 2)
V(cluster4)$betweenness <- round(betweenness(cluster4), 2)
V(cluster4)$degree <- degree(cluster4)
V(cluster4)$size <- V(cluster4)$degree
V(cluster4)$comm <- membership(wc)
V(cluster4)$color <- colorize(membership(wc))
E(cluster4)$width <- E(cluster4)$weight*6
E(cluster3)$color <- "black"

viz4 <- hchart(cluster4, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz4, "output/graphs/Reseaux/HTML/cluster4.html")

# Metriques réseau global 
S_net <-  vcount(cluster4) # nombre de noeuds
L_net <- ecount(cluster4) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster4, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster4,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster4, weights=E(cluster4)$weight)) 

Edge_connect <- edge.connectivity(cluster4) # Edge connectivity = adhesion

Modularity <- modularity(cluster4,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster4) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster4)) #Nombre de liens moyen

assort <- assortativity_degree(cluster4,directed = F) #assortativite

diss <- mean(1 - E(cluster4)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster4,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster4)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster4, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL4df <- filter(data,cluster == 4)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
for (i in 1:nrow(CL4)){ 
  spe <- as.data.frame(CL4[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL4df[i,]$Code_point_Libelle
  date <- CL4df[i,]$Date
  if (nrow(spe) != 0){
    vids <- spe$Pindex
    sub <- igraph::subgraph(cluster4, vids)
    viz_sub <- hchart(sub, layout = layout_with_fr)
    
    # Metriques réseau global 
    S_net <-  vcount(sub) # nombre de noeuds
    L_net <- ecount(sub) # nombre de liens
    Z_mat <- L_net / S_net # linkage density or average number of links per nodes
    C_net <- edge_density(sub, loops = FALSE) #connectance
    
    # Average path length
    avg_path_length <- mean_distance(
      sub,
      directed = FALSE,
      unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
    )
    
    Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
    
    wc <- cluster_fast_greedy(sub)
    Modularity <- modularity(sub,membership = membership(wc)) # Modularity
    
    Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
    m_degree <- mean(degree(sub)) #Nombre de liens moyen
    assort <- assortativity_degree(sub,directed = F) #assortativite
    diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
    trans <- transitivity(sub,type = "global") #Transitivity
    mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
    adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
    diag(adj) <- 1
    nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- S_net # nombre de noeuds
    data_results_reseaux[i,4] <- L_net # nombre de liens
    data_results_reseaux[i,5] <- Z_mat # Densite des liens
    data_results_reseaux[i,6] <- C_net # connectance
    
    data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
    data_results_reseaux[i,8] <- Edge_connect # adhesion
    data_results_reseaux[i,9] <- Modularity #modularite
    
    data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
    data_results_reseaux[i,11] <- assort # Assortativite
    data_results_reseaux[i,12] <- diss # dissimilarite
    data_results_reseaux[i,13] <- trans # transitivite
    
    data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
    data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
    data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
  }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  png(paste0("output/graphs/Reseaux/TS_CLUST4/",station,date,".png"))
  plot(sub,main = paste0(station,date),layout = layout_with_fr)
  dev.off()
  htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL4/",station,date,".html"))
  print(i/nrow(CL4)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster4.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL4/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 2
data_results_reseaux_copy <- data_results_reseaux

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster4.csv", 
                                               delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                               locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                               trim_ws = TRUE)

bloom <- dplyr::select(CL4df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')




CL4_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T))

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')



ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

#### Correlation avec autres parametres 
data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster4.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",number.cex = 0.7,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre métriques et diversité Cluster 4"
)


data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster1.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",number.cex = 0.7,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre métriques et diversité Cluster 1"
)

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster2.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",number.cex = 0.7,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre métriques et diversité Cluster 2"
)

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster3.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",number.cex = 0.7,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre métriques et diversité Cluster 3"
)







