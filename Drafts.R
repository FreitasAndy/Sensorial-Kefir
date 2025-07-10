df = Sensoral_AndersonReview
df$Amostra <- factor(df$Amostra, levels=c("239","575", "921"))
formulae <- lapply(colnames(df)[3:10], function(x) as.formula(paste0(x, " ~ Amostra")))
lshap <- lapply(df[3:10], shapiro.test)
lshap
#Normal!!!
res <- lapply(formulae, function(x) summary(aov(x, data = df)))
names(res) <- format(formulae)
res
library(tidyverse)
library(ggpubr)

# List of variables to plot
vars <- c("ImpGlobal", "Aparencia", "Aroma", "Sabor", "Cor", "Textura", "Acidez",
          "IntenCompra")

# Create a function to generate barplot with error bars
make_barplot <- function(var) {
  summary_df <- df %>%
    mutate(Amostra = factor(Amostra, levels = c("921", "575", "239"))) %>%
    group_by(Amostra) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      se = sd(.data[[var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[var]])))
    )
  
  ggplot(summary_df, aes(x = Amostra, y = mean, fill = Amostra)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.8) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    labs(title = var, y = var, x = "Amostra") +
    theme_bw() +
    theme(legend.position = "none")
}

# Generate the plots correctly
plots <- lapply(vars, function(v) make_barplot(v))

# Arrange plots in a grid
plot.save <- ggarrange(plotlist = plots, ncol = 2, nrow = ceiling(length(plots)/2))

ggplot2::ggsave(filename = "./Figures/Fig2_2.svg", 
                plot = plot.save, device = "svg", dpi = 1200,
                width = 12, height = 20,units = "cm")

#Tuckey para significantes
model <- aov(Textura~Amostra, data=df)
TukeyHSD(model, conf.level=.95)
plot(TukeyHSD(model, conf.level=.95), las = 2)
#239 é diferente de todas, mas nao ha diferenca entre 921 e 575

library(ggplot2)
ggplot(data = df, aes(x = Amostra, y = Textura)) +
  geom_boxplot()

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

ggplot(df, aes(x = ImpGlobal, fill = Amostra)) +
  scale_x_continuous(limits=c(0,10), breaks = c(0:10)) + 
  geom_histogram(color = "black", bins = 50) +
  labs(title = paste("Histograma de ImpGlobal"), x = "Impressao Global", y = "Frequency") +
  facet_wrap(~ Amostra) + 
  theme_minimal()

ggplot(df, aes(x = Aparencia, fill = Amostra)) +
  scale_x_continuous(limits=c(0,10), breaks = c(0:10)) + 
  geom_histogram(color = "black", bins = 50) +
  labs(title = paste("Histograma de Aparencia"), x = "Aparencia", y = "Frequencia (%)") +
  facet_wrap(~ Amostra) + 
  theme_minimal()

ggplot(df, aes(x = Aroma, fill = Amostra)) +
  scale_x_continuous(limits=c(0,10), breaks = c(0:10)) + 
  geom_histogram(color = "black", bins = 50) +
  labs(title = paste("Histograma de Aroma"), x = "Aroma", y = "Frequencia (%)") +
  facet_wrap(~ Amostra) + 
  theme_minimal()

ggplot(df, aes(x = Sabor, fill = Amostra)) +
  scale_x_continuous(limits=c(0,10), breaks = c(0:10)) + 
  geom_histogram(color = "black", bins = 50) +
  labs(title = paste("Histograma de Sabor"), x = "Sabor", y = "Frequencia (%)") +
  facet_wrap(~ Amostra) + 
  theme_minimal()

library(readxl)
df_pca <- read_excel("Sensoral_AndersonReview.xlsx", 
                     sheet = "Sheet2")

library(factoextra)
library(ggfortify)

df2 <- df_pca[c(2:25)]
df2[is.na(df2)] <- 0
pca_res <- prcomp(df2, scale = F)
summary(pca_res)
print(pca_res) 

biplot <- data.frame(pca_res$x, Preferencia = df_pca$`Amostra preferida`, Alcool = df_pca$Alcool)
fig.pca <- ggplot(data = biplot, aes(x = PC1, y = PC2)) +
  geom_point(size = 4, aes(color = as.factor(Preferencia))) +
  labs(
    x = "PC1 (40,74%)",
    y = "PC2 (8,97%)",
    color = "Amostra Favorita") +
  scale_color_manual(values=c("yellow", "green", "blue", "red")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  theme_bw()

ggplot2::ggsave(filename = "./Figures/Fig_PCA.svg", 
                plot = fig.pca, device = "svg", dpi = 1200,
                width = 18, height = 12,units = "cm")


#### correlation 
library(corrplot)
colnames(df_pca)
df.cor <- df_pca[-c(1, 5:22)]
df.cor[is.na(df.cor)] <- 0

M = cor(df.cor)
corrplot(M, method = 'number', type = 'upper')
corrplot(M, order = 'hclust', addrect = 2)

testRes = cor.mtest(df.cor, conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.4, order = 'alphabet', diag=FALSE)


library(tidyverse)
library(ggplot2)

df <- data.frame(
  SampleID = c("T0", "T15", "T30"),
  SurvivalRate = c(81.27, 79.43, 65.34),
  StandardDeviation = c(8.74, 5.88, 6.22)
)
plot.surv <- ggplot(df, aes(x = SampleID, y = SurvivalRate)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  geom_errorbar(aes(ymin = SurvivalRate - StandardDeviation, ymax = SurvivalRate + StandardDeviation),
                width = 0.2, color = "black") +
  labs(x = "Sample ID", y = "Survival Rate (%)", title = "Survival Rate with Standard Deviation") +
  theme_minimal()
plot.surv

ggplot2::ggsave(filename = "./Figures/Fig_Surv.svg", 
                plot = plot.surv, device = "svg", dpi = 1200,
                width = 18, height = 12,units = "cm")
