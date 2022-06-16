library(ggplot2)
library(patchwork)
library(tidyverse)

d1 <- read.csv("questionnaire/questionnaire_atelier_codons_02.csv", sep=";")

q1 <- d1[1:4, ]

(p1 <- ggplot(data = q1,
       aes(x = Intitule, y = Nombre)) +
  geom_col(width = 0.5, fill = "#66c1bf") +
  geom_text(aes(x = Intitule, y = Nombre + 0.3, label = Nombre),
            colour = "#275662", size = 3) +
  coord_flip() +
  labs(title = "Avez-vous deja utilise le Tidyverse avant cet atelier ?", 
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8)))

q2 <- d1[5:8, ]

(p2 <- ggplot(data = q2,
              aes(x = Intitule, y = Nombre)) +
    geom_col(width = 0.5, fill = "#66c1bf") +
    geom_text(aes(x = Intitule, y = Nombre + 0.3, label = Nombre),
              colour = "#275662", size = 3) +
    coord_flip() +
    labs(title = "Cet atelier a-t-il repondu a vos attentes ?", 
         x = "", y = "") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "#275662", size = 8)))

q3 <- d1[9:18, ] %>% 
  arrange(Nombre) %>% 
  mutate(Intitule = fct_inorder(factor(Intitule)))

(p3 <- ggplot(data = q3,
       aes(x = Intitule, y = Nombre)) +
  geom_col(width = 0.5, fill = "#66c1bf") +
  geom_text(aes(x = Intitule, y = Nombre + 0.3, label = Nombre),
            colour = "#275662", size = 3) +
  coord_flip() +
  labs(title = "Parmi les items suivants, avec lesquels vous\nsentez-vous à l'aise à l'issue de cet atelier ?", 
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8)))

q4 <- d1[19:53, ] %>% 
  mutate(Intitule = fct_inorder(rev(Intitule)))
  

(p4 <- ggplot(data = q4,
       aes(x = Note, y = Intitule, fill = Nombre)) +
  geom_tile(show.legend = FALSE, colour = "#275662") +
  scale_fill_continuous(low = "white", high = "#66c1bf") +
  geom_text(aes(x = Note, y = rev(Intitule), label = Nombre),
            colour = "#275662", size = 3)  +
  labs(title = "Donnez une note aux items suivants entre\n1 (très mauvais) et 5 (très bien)", 
       y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10,
                                  margin = margin(b = 15)),
        axis.title.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8)))

p5 <- ggplot() +
  geom_text(aes(x = 0.5, y = 9.5, label = "Points positifs"), colour = "#275662", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 9, label = "- Format tutoriel / durée de l'atelier"), colour = "black", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 8.5, label = "- Ambiance constructive et bienveillante"), colour = "black", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 8, label = "- Disponibilité des animateurs"), colour = "black",  hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 6, label = "Points à améliorer"), colour = "#275662", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 5.5, label = "- Passer plus de temps sur les bases de R"), colour = "black", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 5, label = "- Démonstrations au fur et à mesure"), colour = "black", hjust = 0, size = 3) +
  geom_text(aes(x = 0.5, y = 4.5, label = "- Suivi plus personnalisé"), colour = "black",  hjust = 0, size = 3) +
  xlim(0, 10) +
  ylim(4, 10) +
  theme_void()

p <- p1 + p2 + p3 + p4 + plot_spacer() + p5 +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Ateliers codons! - 02 - Manipuler des données avec le Tidyverse",
                  subtitle = "Lundi 09/05/2022",
                  theme = theme(plot.title = element_text(colour = "#275662", hjust = 0.5),
                                plot.subtitle = element_text(colour = "#275662", hjust = 0.5)))

ggsave("questionnaire/reponses.png", p, dpi = 320, width = 12, height = 6)
