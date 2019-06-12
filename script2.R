library(ggplot2)

# grafico 1 ---------------------------------------------------------------


tga <- read.table("data/tga_eucalyptus.txt", skip = 35,
                  col.names = c("N", "Temperatura", "DTA", "Massa"))
summary(tga)
p1 <- ggplot(tga, aes(x = Temperatura, y = Massa)) +
  geom_line() +
  theme_classic()

p1 

ggsave("Graficotga.jpg", p1, 
       width = 15, 
       height = 10, units = "cm",
       dpi = "print")

p1 + 
  scale_x_continuous("Temperatura (°C)", breaks = seq(50, 600, 50)) +
  scale_y_continuous("Massa (g)", limits = c(0, 10), breaks = c(1, 1.12, 2, 4, 5))

tga$Massa2 <- tga$Massa*1.2

p1 +
  geom_line(aes(x = 200), color = "red")


# grafico 2 ---------------------------------------------------------------
ggplot(dados_e, aes(x = Clone, y = MOE)) +
  geom_point() 
  
ggplot(dados_e, aes(x = Clone, y = MOE, 
                    group = Clone,
                    color = as.factor(Clone))) +
  geom_boxplot() + 
  facet_wrap(~PLANO)

ggplot(dados_e, aes(x = Clone, y = MOE, 
                    group = Clone,
                    color = as.factor(Clone))) +
  geom_point() + 
  facet_grid(cols = vars(Temp),
             rows = vars(PLANO), 
             scales = "free_y") 


ggplot(dados_e, aes(x = Clone, y = MOE, 
                    group = Clone, 
                    shape = PLANO,
                    color = as.factor(Clone))) +
  geom_point() + 
  facet_grid(cols = vars(Temp),
             rows = vars(PLANO), 
             scales = "free_y")  +
  scale_color_manual("AQUI PE O ROTULO0",
                     values = c("red",
                                "black",
                                12,
                                21,
                                "blue",
                                "#BC8F8F",
                                "#FF6347")) +
  scale_shape_manual("Plano de carga",
                     values = c(8, 14)) +
  theme(legend.position = c(.2, .5))


ggplot(dados_e[dados_e$PLANO == "PAR", ], 
       aes(x = MOE, y = RC)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2))

library(ggpmisc)

ggplot(dados_e[dados_e$PLANO == "PAR", ], 
       aes(x = MOE, y = RC)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2)) +
  stat_poly_eq(formula = y ~ x, parse = T,
               aes(label = paste(stat(eq.label))))

