library(readr)
library(RColorBrewer)
library(corrplot)
library("plotrix")
library(ggplot2)
library("ggpubr")

# Colour palette
#display.brewer.all(type = "all")
mycolor1 <- brewer.pal(10, "Spectral")
mycolor2 <- brewer.pal(10, "RdYlBu")
display.brewer.pal(10, "RdYlBu")
mycolor3 <- brewer.pal(10, "RdGy")
display.brewer.pal(10, "RdGy")
c_combine <- c(mycolor2[c(8)],mycolor1[c(8,7,5)],mycolor3[c(4)])
gradient_ramp1 <- colorRampPalette(c(mycolor1[c(7,5)], mycolor3[c(4)]))(6)
gradient_ramp2 <- colorRampPalette(c(mycolor1[c(7,8,9)]))(8)
gradient_ramp3 <- colorRampPalette(c(mycolor3[c(5,4,3)]))(8)
gradient_ramp4 <- colorRampPalette(c(mycolor1[c(6,5,4)]))(8)


table_all <- read_delim("merged_norm_lmi_greater_than_0.csv",
                          "\t", escape_double = FALSE, trim_ws = TRUE)
attach(table_all)
sapply(table_all[4:8], mean)


# 1.1.Proportion of each Dominant Modality
counts<-table(DominantModality)
counts
x <- c(66, 49, 81, 23, 524)
labels <- c('Auditory (66)', 'Gustatory (49)', 'Haptic (81)', 'Olfactory (23)','Visual (524)')
pie_percent <- paste(round(100*x/sum(x), 2), "%")
pie_percent
pie(x, labels = pie_percent, main = "Proportion of each Dominant Modality", 
    col=c_combine) 
legend("topright",labels, cex = 0.8, fill = c_combine)
warnings()

# 1.2.Proportion of each POS
counts<-table(Class)
counts
x <- c(332, 14, 397)
labels <- c('Adjective (332)', 'Mixed (14)', 'Noun (397)')
pie_percent <- paste(round(100*x/sum(x), 2), "%")
pie_percent
pie(x, labels = pie_percent, main = "Proportion of each POS Class", 
    col=c(mycolor3[c(3,4,5)])) 
legend("topright",labels, cex = 0.8, fill = c(mycolor3[c(3,4,5)]))


# 2.1.Strength Mean of Dominant Modality Auditory
Auditory <- subset(table_all, DominantModality=='Auditory')
print(head(Auditory))
Auditory_mean = sapply(Auditory[4:8], mean)
print(Auditory_mean)
barplot(Auditory_mean, names.arg = c('Auditory ', 'Gustatory', 'Haptic', 'Olfactory','Visual'),
        main = 'Strength Mean of Dominant Modality Auditory',xlab = 'Modalities', ylab = 'Strength Mean',
        col=c_combine, ylim = c(0,5))

# 2.2.Strength Mean of Dominant Modality Gustatory
Gustatory <- subset(table_all, DominantModality=='Gustatory')
Gustatory_mean = sapply(Gustatory[4:8], mean)
barplot(Gustatory_mean, names.arg = c('Auditory ', 'Gustatory', 'Haptic', 'Olfactory','Visual'),
        main = 'Strength Mean of Dominant Modality Gustatory',xlab = 'Modalities', ylab = 'Strength Mean',
        col=c_combine, ylim = c(0,5))

# 2.3.Strength Mean of Dominant Modality Haptic
Haptic <- subset(table_all, DominantModality=='Haptic')
Haptic_mean = sapply(Haptic[4:8], mean)
barplot(Haptic_mean, names.arg = c('Auditory ', 'Gustatory', 'Haptic', 'Olfactory','Visual'),
        main = 'Strength Mean of Dominant Modality Haptic',xlab = 'Modalities', ylab = 'Strength Mean',
        col=c_combine, ylim = c(0,5))

# 2.4.Strength Mean of Dominant Modality Olfactory
Olfactory <- subset(table_all, DominantModality=='Olfactory')
Olfactory_mean = sapply(Olfactory[4:8], mean)
barplot(Olfactory_mean, names.arg = c('Auditory ', 'Gustatory', 'Haptic', 'Olfactory','Visual'),
        main = 'Strength Mean of Dominant Modality Olfactory',xlab = 'Modalities', ylab = 'Strength Mean',
        col=c_combine, ylim = c(0,5))

# 2.5.Strength Mean of Dominant Modality Visual
Visual <- subset(table_all, DominantModality=='Visual')
Visual_mean = sapply(Visual[4:8], mean)
barplot(Visual_mean, names.arg = c('Auditory ', 'Gustatory', 'Haptic', 'Olfactory','Visual'),
        main = 'Strength Mean of Dominant Modality Visual',xlab = 'Modalities', ylab = 'Strength Mean',
        col=c_combine, ylim = c(0,5))

# 2.6.Strength Means of five Dominant Modalities
y1 = c(Auditory_mean[1],Gustatory_mean[1],Olfactory_mean[1],Haptic_mean[1],Visual_mean[1])
y2 = c(Auditory_mean[2],Gustatory_mean[2],Olfactory_mean[2],Haptic_mean[2],Visual_mean[2])
y3 = c(Auditory_mean[3],Gustatory_mean[3],Olfactory_mean[3],Haptic_mean[3],Visual_mean[3])
y4 = c(Auditory_mean[4],Gustatory_mean[4],Olfactory_mean[4],Haptic_mean[4],Visual_mean[4])
y5 = c(Auditory_mean[5],Gustatory_mean[5],Olfactory_mean[5],Haptic_mean[5],Visual_mean[5])
five_DM <- rbind(y1, y2, y3, y4, y5)
ylim <- c(0, max(five_DM)*1.15)
x <- barplot(five_DM, ylim = ylim, offset = 0, axis.lty = 1,
        names.arg = c('Auditory ', 'Gustatory', 'Olfactory', 'Haptic', 'Visual'),
        col=c_combine, beside = TRUE, space = c(0,2))
legend("top", legend = c('Auditory ', 'Gustatory', 'Olfactory', 'Haptic', 'Visual'),
       fill = c_combine, box.col = c_combine, horiz=TRUE,cex=0.8)
title(main = "Strength Means of five Dominant Modalities", xlab = "Five Dominant Modalities", ylab = "Strength Mean")


# 3.1.Modality Exclusivity & Class - boxplot
input <- table_all[,c('ModalityExclusivity','Class')]
print(head(input))
boxplot(ModalityExclusivity ~ Class, data = mtcars,
        xlab = 'Class', ylab = 'Modality Exclusivity',
        col = c_combine)

# 3.2.Modality Exclusivity & Class - plot
noun = subset(table_all, Class=='Noun')
adjective = subset(table_all, Class=='Adjective')
mixed = subset(table_all, Class=='Mixed')

noun_mean = sapply(noun[4:8], mean)
adjective_mean = sapply(adjective[4:8], mean)
mixed_mean = sapply(mixed[4:8], mean)

colour <- c(c_combine[1],c_combine[4],c_combine[5])
plot(noun_mean,names.arg = c('a','b','c','d','e'),
     type = "o", col = colour[1], ylim = c(0,5),
     xlab = 'Dominant Modality', ylab = 'Modality Exclusivity')
lines(mixed_mean,type = "o", col = colour[2])

lines(adjective_mean,type = "o", col = colour[3])
legend("topright", legend = c('Noun','Mixed','Adjective'),
       col = colour, lty = 1:3, box.lty = 0, cex=0.7)


# 3.3.Modality Exclusivity & Dominant Modality
input <- table_all[,c('ModalityExclusivity','DominantModality')]
print(head(input))
boxplot(ModalityExclusivity ~ DominantModality, data = mtcars,
        xlab = 'Dominant Modality', ylab = 'Modality Exclusivity',
        col = c_combine)


# 4.1.Corration between All Modality Exclusivity & Dominant Modality
A <- table_all[4:8]
print(head(A))
names(A) <- c("Auditory", "Gustatory", "Haptic", "Olfactory", "Visual")
print(head(A))
corrplot.mixed(corr = cor(A),
               tl.col = mycolor3[10],upper = "square",order = "FPC",
               upper.col = gradient_ramp2[rank(1:8)], lower.col = gradient_ramp2[rank(1:8)])
legend("left" , legend = c('                                              All'),
       col = colour, box.lty = 0, cex=0.7)

# 4.1.1 Corration between Gustatory_means & Olfactory_means
B <- data.frame(table_all[4:8])
B_G_O <- seq(from = 2, to = 4, by = 2)
data_BGO <- B[,B_G_O]
print(head(data_BGO))
names(data_BGO) <- c("Gustatory_means","Olfactory_means")
print(head(data_BGO))
attach(data_BGO)

shapiro.test(data_BGO$Gustatory_means)
shapiro.test(data_BGO$Olfactory_means)
ggqqplot(data_BGO$Gustatory_means, ylab = "Gustatory_means")
ggqqplot(data_GO$Olfactory_means, ylab = "Olfactory_means")

cor(data_BGO)
plot(Gustatory_means, Olfactory_means,
     col = gradient_ramp1[rank(7:8)],
     abline(lm(Olfactory_means~Gustatory_means),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(4,2.2,'slope = 0.7511968')
text(4,4.4,'p-value < 2.2e-16')
text(3,4.8,'Spearman\'s product-moment correlation')
cor.test(Gustatory_means, Olfactory_means, method="spearman")

# 4.1.2 Corration between Haptic_means & Visual_means
C <- data.frame(table_all[4:8])
C_H_V <- seq(from = 3, to = 5, by = 2)
data_CHV <- C[,C_H_V]
names(data_CHV) <- c("Haptic_means","Visual_means")
print(head(data_CHV))
attach(data_CHV)

shapiro.test(data_CHV$Haptic_means)
shapiro.test(data_CHV$Visual_means)

cor(data_CHV)
plot(Haptic_means, Visual_means,
     col = gradient_ramp1[rank(7:8)],
     abline(lm(Visual_means~Haptic_means),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(3.8,3.5,'slope = 0.3982328')
text(3.85,1,'p-value < 2.2e-16')
text(2.8,1.4,'Spearman\'s product-moment correlation')
cor.test(Haptic_means, Visual_means, method="spearman")

# 4.2.Corration between Noun's Modality Exclusivity & Dominant Modality
N <- data.frame(subset(table_all, Class == 'Noun'|Class == 'Mixed')[4:8])
print(head(N))
names(N) <- c("Auditory", "Gustatory", "Haptic", "Olfactory", "Visual")
print(head(N))
corrplot.mixed(corr = cor(N),
               tl.col = mycolor3[10],upper = "square",order = "FPC",
               upper.col = gradient_ramp3[rank(1:8)], lower.col = gradient_ramp3[rank(1:8)])
legend("left" , legend = c('                                         Noun + Mixed'),
       col = colour, box.lty = 0, cex=0.7)

# 4.2.1 Corration between Noun_Gustatory_means & Noun_Olfactory_means
N <- data.frame(subset(table_all, Class == 'Noun'|Class == 'Mixed')[4:8])
G_O <- seq(from = 2, to = 4, by = 2)
data_GO <- N[,G_O]
print(head(data_GO))
names(data_GO) <- c("Noun_Gustatory_means","Noun_Olfactory_means")
print(head(data_GO))
attach(data_GO)

shapiro.test(data_GO$Noun_Gustatory_means)
shapiro.test(data_GO$Noun_Olfactory_means)
ggqqplot(data_GO$Noun_Gustatory_means, ylab = "Noun_Gustatory_means")
ggqqplot(data_GO$Noun_Olfactory_means, ylab = "Noun_Olfactory_means")

cor(data_GO)
plot(Noun_Gustatory_means, Noun_Olfactory_means,
     col = gradient_ramp1[rank(7:8)],
     abline(lm(Noun_Olfactory_means~Noun_Gustatory_means),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(4,2.5,'slope = 0.6709283')
text(4,4.1,'p-value < 2.2e-16')
text(3,4.5,'Spearman\'s product-moment correlation')
cor.test(Noun_Gustatory_means, Noun_Olfactory_means, method="spearman")

# 4.2.2 Corration between Noun_Haptic_means & Noun_Visual_means
N <- data.frame(subset(table_all, Class == 'Noun'|Class == 'Mixed')[4:8])
H_V <- seq(from = 3, to = 5, by = 2)
data_HV <- N[,H_V]
names(data_HV) <- c("Noun_Haptic_means","Noun_Visual_means")
print(head(data_HV))
attach(data_HV)

shapiro.test(data_HV$Noun_Haptic_means)
shapiro.test(data_HV$Noun_Visual_means)

cor(data_HV)
plot(Noun_Haptic_means, Noun_Visual_means,
     col = gradient_ramp1[rank(7:8)],
     abline(lm(Noun_Visual_means~Noun_Haptic_means),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(3.8,3.5,'slope = 0.5380562')
text(3.85,1,'p-value < 2.2e-16')
text(2.8,1.4,'Spearman\'s product-moment correlation')
cor.test(Noun_Haptic_means, Noun_Visual_means, method="spearman")


# 4.3.Corration between Adjective's Modality Exclusivity & Dominant Modality
M <- data.frame(subset(table_all, Class == 'Adjective'|Class == 'Mixed')[4:8])
names(M) <- c("Auditory", "Gustatory", "Haptic", "Olfactory", "Visual")
print(head(M))
corrplot.mixed(corr = cor(M),
               tl.col = mycolor3[10],upper = "square",order = "FPC",
               upper.col = gradient_ramp4[rank(1:8)], lower.col = gradient_ramp4[rank(1:8)])
legend("left" , legend = c('                                   Adjective + Mixed'),
       col = colour, box.lty = 0, cex=0.7)

# 4.3.1 Corration between Adjective_Gustatory_means & Adjective_Olfactory_means
M <- data.frame(subset(table_all, Class == 'Adjective'|Class == 'Mixed')[4:8])
A_G_O <- seq(from = 2, to = 4, by = 2)
data_AGO <- M[,A_G_O]
print(head(data_AGO))
names(data_AGO) <- c("Adjective_Gustatory_means","Adjective_Olfactory_means")
print(head(data_AGO))
attach(data_AGO)

shapiro.test(data_AGO$Adjective_Gustatory_means)
shapiro.test(data_AGO$Adjective_Olfactory_means)

cor(data_AGO)
plot(Adjective_Gustatory_means, Adjective_Olfactory_means,
     col = gradient_ramp1[rank(7:8)],
     abline(lm(Adjective_Olfactory_means~Adjective_Gustatory_means),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(4,2.2,'slope = 0.7671591')
text(4,4.4,'p-value < 2.2e-16')
text(3,4.8,'Spearman\'s product-moment correlation')
cor.test(Adjective_Gustatory_means, Adjective_Olfactory_means, method="spearman")

      

# 5.1.Corration between Frequency and Modality Exclusivity
ID <- c(1:743)
input1 <- data.frame(ID, table_all[c('ModalityExclusivity')])
input2 <- data.frame(ID,log(table_all[c('Frequency')]))
input <- merge(input1, input2, BY='ID')
print(head(input[2:3]))
plot(input[2:3])

shapiro.test(input$ModalityExclusivity)
ggqqplot(input$ModalityExclusivity, mian = 'Normal distribution test_Modality Exclusivity',
         xlab = NULL, ylab = "Modality Exclusivity")

shapiro.test(input$Frequency)
ggqqplot(input$Frequency, mian = 'Normal distribution test_Frequency',
         xlab = NULL, ylab = "Frequency")

cor(ModalityExclusivity,log(Frequency))
plot(log(Frequency),ModalityExclusivity, 
     main = 'Corration between Frequency and Modality Exclusivity',
     # ylim = c(0,1),
     col = gradient_ramp1[rank(1:4)],
     abline(lm(ModalityExclusivity~log(Frequency)),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(5.95,0.55,'slope = -0.08440195')
text(5.8,0.83,'p-value = 0.04659')
text(6.87,0.9,'Spearman\'s product-moment correlation')
cor.test(input$ModalityExclusivity,log(input$Frequency), method="spearman")




# 5.2.Corration between Frequency and Noun's Modality Exclusivity
noun = subset(table_all, Class=='Noun')
ID <- c(1:397)
input1 <- data.frame(ID, noun[c('ModalityExclusivity')])
input2 <- data.frame(ID,log(noun[c('Frequency')]))
input <- merge(input1, input2, BY='ID')
print(head(input[2:3]))
plot(input[2:3])
names(input)[2] <- "Noun_ME"
names(input)[3] <- "Noun_Fq"
print(head(input[2:3]))
attach(input)
cor(input[2:3])
plot(Noun_Fq,Noun_ME, col = c_combine,
     abline(lm(Noun_ME~Noun_Fq),col = 'grey'),cex = 1.3,pch = 16)
cor.test(Noun_Fq,Noun_ME, method="pearson")

# 5.3.Corration between Frequency and Adjective's Modality Exclusivity
adjective = subset(table_all, Class=='Adjective')
ID <- c(1:332)
input1 <- data.frame(ID, adjective[c('ModalityExclusivity')])
input2 <- data.frame(ID,log(adjective[c('Frequency')]))
input <- merge(input1, input2, BY='ID')
print(head(input[2:3]))
plot(input[2:3])
names(input)[2] <- "Adjective_ME"
names(input)[3] <- "Adjective_Fq"
print(head(input[2:3]))
attach(input)

cor(input[2:3])
plot(Adjective_Fq,Adjective_ME, col = c_combine,
     abline(lm(Adjective_ME~Adjective_Fq),col = 'grey'),cex = 1.3,pch = 16)
cor.test(Adjective_Fq,Adjective_ME, method="pearson")


# 6.1.Corration between Average_cos_sim and Modality Exclusivity
ID <- c(1:743)
input1 <- data.frame(ID, table_all[c('ModalityExclusivity')])
input2 <- data.frame(ID,table_all[c('Average_cos_sim')])
input <- merge(input1, input2, BY='ID')
print(head(input[2:3]))
attach(input[2:3])
plot(input[2:3])

# shapiro.test(input$Average_cos_sim)
# ggqqplot(input$Average_cos_sim, mian = 'Normal distribution test_Average_cos_sim', xlab = NULL, ylab = "Average_cos_sim")

cor(ModalityExclusivity, Average_cos_sim)
plot(Average_cos_sim, ModalityExclusivity, 
     main = 'Corration between Average_cos_sim and Modality Exclusivity',
     # xlim = c(0,1), ylim = c(0,1),
     col = gradient_ramp1[rank(1:6)],
     abline(lm(ModalityExclusivity~Average_cos_sim),col = 'grey'),
     cex = 1.5,pch = 20, bg="blue")
text(0.26,0.6,'slope = -0.1229844')
text(0.26,0.82,'p-value = 0.0005914')
text(0.335,0.9,'Spearman\'s product-moment correlation')
cor.test(ModalityExclusivity,Average_cos_sim, method="spearman")

t.test(Average_cos_sim)

