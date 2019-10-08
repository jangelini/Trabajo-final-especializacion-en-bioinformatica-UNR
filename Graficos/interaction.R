library(ggplot2)
library("cowplot")
library(ggthemes)

setwd("G:\\Especializacion en bioinformatica\\Para proyecto final\\Escrito\\ezthesis-master\\Graficos")

# width=590
# height= 474
windowsFonts(Times = windowsFont("Times New Roman"))

datos1<-read.csv("interaction.csv", sep=";")

datos1$amb<-as.factor(datos1$amb)
datos1$gen<-as.factor(datos1$gen)


a<-ggplot(datos1, aes(x=amb,y=Yield,group=gen)) +
  theme_few() +
  scale_x_discrete(name =" ",
                   breaks=c("1","2"),
                   labels=c("Ambiente 1", "Ambiente 2"))+
  scale_y_continuous(name ="Rendimiento", limits=c(0, 35), breaks = seq(0, 35, 5))+
  geom_line(aes(linetype=gen),size=1)+
  geom_point(aes(shape=gen))+
  theme(text=element_text(family="Times", size=12,colour = "black"),legend.position="none") + 
  annotate("text", x = 2.05, y = 32.5, label = "G1")+ 
  annotate("text", x = 2.05, y = 12.5, label = "G2")


datos2<-read.csv("interaction2.csv", sep=";")

datos2$amb<-as.factor(datos2$amb)
datos2$gen<-as.factor(datos2$gen)


b<-ggplot(datos2, aes(x=amb,y=Yield,group=gen)) +
  theme_few() +
  scale_x_discrete(name =" ",
                   breaks=c("1","2"),
                   labels=c("Ambiente 1", "Ambiente 2"))+
  scale_y_continuous(name ="Rendimiento", limits=c(0, 35), breaks = seq(0, 35, 5))+
  geom_line(aes(linetype=gen),size=1)+
  geom_point(aes(shape=gen))+
  theme(text=element_text(family="Times", size=12,colour = "black"),legend.position="none") + 
  annotate("text", x = 2.05, y = 33, label = "G1")+ 
  annotate("text", x = 2.05, y = 9, label = "G2")


datos3<-read.csv("interaction3.csv", sep=";")

datos3$amb<-as.factor(datos3$amb)
datos3$gen<-as.factor(datos3$gen)


c<-ggplot(datos3, aes(x=amb,y=Yield,group=gen)) +
  theme_few() +
  scale_x_discrete(name =" ",
                   breaks=c("1","2"),
                   labels=c("Ambiente 1", "Ambiente 2"))+
  scale_y_continuous(name ="Rendimiento", limits=c(0, 35), breaks = seq(0, 35, 5))+
  geom_line(aes(linetype=gen),size=1)+
  geom_point(aes(shape=gen)) + 
  theme(text=element_text(family="Times", size=12,colour = "black"),legend.position="none") + 
  annotate("text", x = 2.05, y = 33, label = "G1")+ 
  annotate("text", x = 2.05, y = 18, label = "G2")





plot_grid(a,b,c,
          ncol=3, nrow=1, labels=c("A","B","C"))


