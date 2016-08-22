#-------------- EED balance spectrum

if (1 == 1){
library(EBImage)
which_mean <- function(x) x %>% which %>% mean
Join_by <- function(x, y) join(x, y, by = "wavelength")

Hoge_PSII <- readImage("~/Dropbox/Publish/2016PgEstimate/Data/Hoge_PSII.png")
Hoge_PSI <- readImage("~/Dropbox/Publish/2016PgEstimate/Data/Hoge_PSI.png")

Abs_PSII <-
  (Hoge_PSII[, , 1] < .8 & Hoge_PSII[, , 3] > .8) %>%
  .@.Data %>%
  data.frame %>%
  t %>%
  aaply(.margins = 2, .fun = which_mean) %>%
  data.frame(wavelength = seq(400, 800, by = 1), abs = (dim(Hoge_PSII)[2] - .)) %>%
  select(wavelength, PSII = abs)

Abs_PSI <-
  (Hoge_PSI[, , 1] > .8 & Hoge_PSI[, , 3] < .8) %>%
  .@.Data %>%
  data.frame %>%
  t %>%
  aaply(.margins = 2, .fun = which_mean) %>%
  data.frame(wavelength = seq(400, 800, by = 1), abs = (dim(Hoge_PSI)[2] - .)) %>%
  select(wavelength, PSI = abs)

Mole_abs <-
  Reduce(Join_by, list(Abs_PSII, Abs_PSI)) %>%
  filter(wavelength %% 1 == 0) %>%
  na.omit
}

# Mole_abs <-
#   xlsx::read.xlsx("~/Dropbox/materials/Hogewoning_et_al.2012_Data.xlsx", sheetIndex = 2, startRow = 2) %>%
#   select(wavelength = 1, PSII = 2, PSI = 4) %>%
#   arrange(wavelength)
#   
EED_dat <-
  Mole_abs %>%
  transmute(
    wavelength,
    AlltoPSII = PSII / (1.5* PSI + PSII)
  )

EEDs <-
  EED_dat %>%
  mutate(All2_for = c(tail(EED_dat$AlltoPSII, -1), 0),
         All2_back = c(0, head(EED_dat$AlltoPSII, -1)),
         AlltoPSII = (AlltoPSII + All2_for + All2_back) / 3) %>% # get moving average
  filter(wavelength > 400)  %>%
  replace_na(list(AlltoPSII = 0)) %>%
  ggplot(aes(x = wavelength, y = AlltoPSII)) +
  theme_thesis(base_family = "serif", base_size = base_size) +
  geom_hline(yintercept = 0.5, col = "grey50", linetype = 3) +
  gg_x(c(380, 770)) +
  # geom_ribbon(aes(ymin = AlltoPSII_max, ymax = AlltoPSII_min), col = "grey50") +
  geom_line() +
  scale_y_continuous(labels = c("0    ", "0.25", "0.50", "0.75", "1.00"),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(-.001, 1.001),
                     expand = c(0, 0)) +
  xlab("Wavelength [nm]") + ylab("Fraction of the excitation energy distributed to PSII")






#----------------------- two curves plot
X_div <- 101
X_range <- seq(0, 1, length.out = X_div)
x_breaks <- c(0.2, 0.4, 0.65)


InteractionDat <-
  data_frame(X = rep(X_range, times = 2),
             ETR = c(sqrt(dbeta(X_range, 4.5, 3.5)) * 1.1, sqrt(dbeta(X_range, 3, 5))),
             Trtm = rep(c("PSII-leaves", "PSI-leaves"), each = X_div))

InteractionLabel <-
  InteractionDat %>%
  group_by(Trtm) %>%
  summarise(ETR = max(ETR)) %>%
  left_join(., InteractionDat, by = c("Trtm", "ETR"))

InteractionPoint <-
  InteractionDat %>%
  filter(X %in% x_breaks)

InteractionText <-
  InteractionDat %>%
  filter(X %in% x_breaks) %>%
  mutate(labels = c("far-red LED light", "Sunlight", "blue + red LED light", rep("", length(x_breaks)))) %>%
  mutate(ETR = c(0.1, 0.1, 0.1, numeric(length(x_breaks))))

InteractionView <-
  InteractionDat %>%
  ggplot(aes(X, ETR, linetype = Trtm, fill = Trtm, group = Trtm)) +
  theme_thesis(base_family = "serif", base_size = base_size) +
  geom_vline(aes(xintercept = x_breaks[1]), linetype = 3, size = .75, col = "grey") +
  geom_vline(aes(xintercept = x_breaks[2]), linetype = 3, size = .75, col = "grey") +
  geom_vline(aes(xintercept = x_breaks[3]), linetype = 3, size = .75, col = "grey") +
  geom_line() +
  geom_point(data = InteractionPoint, col = "black", size = 3, shape = 21) +
  geom_text(data = InteractionLabel, aes(x = X, y = ETR, label = Trtm), vjust = -1, family = "serif", size = Annotate_size) +
  geom_text(data = InteractionText, aes(label = labels), vjust = 1, family = "serif", size = Annotate_size) +
  scale_fill_manual(values = c("white", "black")) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_x_continuous(breaks = c(0.07, 0.92),
                     limits = c(0.05, 0.95),
                     labels = c("PSI-biased", "PSII-biased")) +
  scale_y_continuous(breaks = NULL,
                     limits = c(-0.05, 2.05)) +
  xlab("PSII/PSI-biased level of measuring light") +
  ylab("Relative electron transport rate")

#----------------------- Spectra of major growth light

# set integrate ranges
PFD_integrate_min <- 350
PFD_integrate_max <- 750

IntegratePFD <- 100

panel_names <- c("(a)", "(b)", "(c)")

FL_Ocean_raw <-
  fread("~/Dropbox/materials/SPD_FL.csv") %>%
  transmute(wavelength = `Wavelength[nm]`, value = FL) %>%
  na.omit

FL_Ocean_PFD <-
  smooth.spline(FL_Ocean_raw$wavelength, FL_Ocean_raw$value) %>%
  predict(., seq(PFD_integrate_min, PFD_integrate_max)) %>%
  .$y %>%
  sum

FL_Ocean <-
  FL_Ocean_raw %>%
  mutate(value = value * 100 / FL_Ocean_PFD, variable = "FL")

HPS_Ocean_raw <-
  fread("~/Dropbox/materials/SPD_HPS.txt", skip = 50, nrow = 2000) %>%
  rename(wavelength = V1, value = V2)

HPS_Ocean_PFD <-
  smooth.spline(HPS_Ocean_raw$wavelength, HPS_Ocean_raw$value) %>%
  predict(., seq(PFD_integrate_min, PFD_integrate_max)) %>%
  .$y %>%
  sum

HPS_Ocean <-
  HPS_Ocean_raw %>%
  mutate(value = value * 100 / HPS_Ocean_PFD, variable = "HPS")

MH_Ocean_raw <-
  fread("~/Dropbox/materials/SPD_MH.txt", skip = 50, nrow = 2000) %>%
  rename(wavelength = V1, value = V2)

MH_Ocean_PFD <-
  smooth.spline(MH_Ocean_raw$wavelength, MH_Ocean_raw$value) %>%
  predict(., seq(PFD_integrate_min, PFD_integrate_max)) %>%
  .$y %>%
  sum

MH_Ocean <-
  MH_Ocean_raw %>%
  mutate(value = value * 100 / MH_Ocean_PFD, variable = "MH")

OceanSPDs <-
  bind_rows(FL_Ocean, HPS_Ocean, MH_Ocean) %>%
  mutate(panel = panel_names[1])

Absorptances <-
  fread("~/Dropbox/materials/SpectrumCucumber.csv") %>%
  filter(Trtm %in% c("W", "WFR", "FL")) %>%
  mutate(wavelength = as.numeric(wavelength)) %>%
  filter(variable == "absorption", Trtm == "W")

# Sun: JIS C8904-3
Sun <-
  fread("~/Dropbox/materials/Sunlight_JIS.csv") %>%
  setnames(c("wavelength", "Sunlight")) %>%
  left_join(., Absorptances, by = "wavelength") %>%
  transmute(wavelength, Sun = Sunlight, Shade = Sunlight * (100 - ave), Absorptance = ave)

LEDs <-
  fread("~/Dropbox/materials/SPD_FL.csv") %>%
  transmute(wavelength = `Wavelength[nm]`, W, FR, B = B_HL, R = R_SR) %>%
  SPDnorm(from = 350, to = 750, integratedPFD = IntegratePFD) %>%
  gather(variable, value, -wavelength) %>%
  mutate(panel = panel_names[2]) %>%
  na.omit

SPD_labels <-
  data_frame(variable = c("FL", "MH", "HPS", "B", "R", "FR", "W", "Sun", "Shade"),
             wavelength = c(435, 520, 570, 448, 660, 735, 460, 450, 700),
             value = c(2.2, .6, 3, 4, 4.2, 3.8, 1.3, 0.6, 2),
             label = c("FL", "MH", "HPS", "B", "R", "FR", "W", "Sunlight", "Leaf-transmitted sunlight"),
             panel = rep(panel_names,  c(3, 4, 2)))

SPD_panel <-
  data_frame(variable = "FL",
             wavelength = 400, value = 4,
             label = letters[1:3],
             panel = rep(panel_names))

SPDs <-
  Sun %>%
  SPDnorm(from = 350, to = 750, integratedPFD = IntegratePFD) %>%
  select(-Absorptance) %>%
  gather(variable, value, -wavelength) %>%
  mutate(panel = panel_names[3]) %>%
  bind_rows(., OceanSPDs, LEDs) %>%
  filter(between(wavelength, 380, 820)) %>%
  ggplot(aes(x = wavelength, y = value, group = variable, col = variable)) +
  theme_thesis(base_family = "serif", base_size = base_size) +
  geom_rect(aes(xmin = 700, xmax = 750, ymin = 0, ymax = +Inf), fill = "grey90", col = NA) +
  geom_line() +
  geom_text(data = SPD_labels, aes(x = wavelength, y = value, label = label), family = "serif", size = Annotate_size) +
  geom_text(data = SPD_panel, col = "black", aes(label = panel), family = "serif", size = Annotate_size * .8) +
  scale_color_manual(values = c("blue", "black", "darkred", "orange", "purple", "red", "grey60", "black", "black")) +
  facet_grid(panel ~ .) +
  gg_xy(c(380, 820, -.01, 4.5)) +
  xlab("Wavelength [nm]") + ylab(u_SPFD("Spectral photon flux density"))


SPDs_grey <-
  Sun %>%
  SPDnorm(from = 350, to = 750, integratedPFD = IntegratePFD) %>%
  select(-Absorptance) %>%
  gather(variable, value, -wavelength) %>%
  mutate(panel = panel_names[3]) %>%
  bind_rows(., OceanSPDs, LEDs) %>%
  filter(between(wavelength, 380, 820)) %>%
  ggplot(aes(x = wavelength, y = value, group = variable, linetype = variable)) +
  theme_thesis(base_family = "serif", base_size = base_size) +
  geom_rect(aes(xmin = 700, xmax = 750, ymin = 0, ymax = +Inf), fill = "grey90", col = NA) +
  geom_line() +
  geom_text(data = SPD_labels, aes(x = wavelength, y = value, label = label), family = "serif", size = Annotate_size) +
  geom_text(data = SPD_panel, col = "black", aes(label = panel), family = "serif", size = Annotate_size * .8) +
  scale_linetype_manual(values = c(1, 1, 1, 2, 3, 1, 2, 1, 1)) +
  facet_grid(panel ~ .) +
  gg_xy(c(380, 820, -.01, 4.5)) +
  xlab("Wavelength [nm]") + ylab(u_SPFD("Spectral photon flux density"))
