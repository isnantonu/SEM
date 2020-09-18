install.packages('lavaan') 	# jika sudah pernah install, lewatkan
install.packages('semPlot') 	# jika sudah pernah install, lewatkan

# panggil library
library(lavaan)
library(semPlot)

# baca data
datasiswa <- read.table('C:/Users/isnan/Documents/R/data1.txt', header = TRUE)

# deklarasi model
laju <- 'X =~ X1_1 + X1_2 + X1_3 + X1_4 + X1_5 + X1_6 + X1_7 + X1_8 + X1_9 + X1_10
          Y1 =~ Y1_1 + Y1_2 + Y1_3 + Y1_4 + Y1_5
          Y2 =~ Y2_1 + Y2_2 + Y2_3 + Y2_4 + Y2_5 + Y2_6 + Y2_7
	     Y2 ~ Y1
          Y1 ~ X'

# uji model
uji <- sem(laju, data=datasiswa)

# rangkuman uji
summary(uji, fit.measures=TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(uji, fit.measures = "all")

## gambar model
# gambar model hipotetik/tanpa angka
semPaths(uji, "path", edge.label.cex = 1, style = "lisrel", layout = "tree", nDigits = 3, sizeMan = 6, rotation = 2)
# gambar model dalam skor kasar (estimate, est)
semPaths(uji, "path","est", edge.label.cex = 1, style = "lisrel", layout = "tree", nDigits = 3, sizeMan = 6, rotation = 2)
# gambar model dalam skor baku (standardized, std)
semPaths(uji, "path","std", edge.label.cex = 1, style = "lisrel", layout = "tree", nDigits = 3, sizeMan = 6, rotation = 2)

# saran modifikasi model
modindices(uji, sort. = TRUE)
dev.off()

sink('SEM.txt', split=T)
cat('\n')
cat('Hasil Analisis Variabel dengan SEM\n')
summary(uji, fit.measures=TRUE, standardized = TRUE, rsquare = TRUE)
cat('\n')
cat('Sumarry Analisis\n')
fitMeasures(uji, fit.measures = "all")
cat('\n')
cat('Modifikasi Model\n')
modindices(uji, sort. = TRUE)
sink()

