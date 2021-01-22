library(rgdal)
library(raster)

evi = stack('evi.tif')

if (TRUE) {
  # plot(evi$EVI_2000.02.01)
  # pos <- locator(1)
  # print(pos)
  x = 345693.7
  y = 6025003
  wspMiejskie = data.frame(x = x, y = y)
  pxMiejskie = extract(evi, wspMiejskie)

  # plot(evi$EVI_2000.02.01)
  # pos <- locator(1)
  # print(pos)
  # x = pos$x
  # y = pos$y
  x = 323341.7
  y = 6030817
  wspLas = data.frame(x = x, y = y)
  pxLas = extract(evi, wspLas)

  # plot(evi$EVI_2000.02.01)
  # pos <- locator(1)
  # print(pos)
  # x = pos$x
  # y = pos$y
  x = 362632.8
  y = 6044248
  wspxMorze = data.frame(x = x, y = y)
  pxMorze = extract(evi, wspxMorze)

  png("wykres.png")
  plot(pxLas[1,], ylim = c(-0.35, 0.7), type = "l")
  lines(pxMiejskie[1,], col = "red")
  lines(pxMorze[1,], col = "green")
  dev.off()
}

datyTekst2 = substr(names(evi), 5, 99999)
datyEVI = data.frame(data = as.POSIXct(datyTekst2,
                                       "%Y.%m.%d", tz = "GMT"), miesiac =
                       as.numeric(format(as.POSIXct(datyTekst2, "%Y.%m.%d", tz = "GMT"), "%m")),
                     rok = as.numeric(format(as.POSIXct(datyTekst2,
                                                        "%Y.%m.%d", tz = "GMT"), "%Y")))
if (TRUE) {
  png("wykres2.png")
  plot(pxLas[1,], ylim = c(-0.35, 0.7), type = "l", x = datyEVI$data)
  lines(pxMiejskie[1,], col = "red", x = datyEVI$data)
  lines(pxMorze[1,], col = "green", x = datyEVI$data)
  dev.off()
}
evi2 = readGDAL('evi.tif')
tEviData = t(evi2@data) #transponujemy tabelę aby daty znalazły sie w wierszach(ułatwi to dalsza prace)
tEviDataMc = aggregate(tEviData, by = list(datyEVI$miesiac), FUN = mean)
#agregujemy piksele w tabeli tEviData za pomoca miesiecy w tabeli datyEVI(aregujemy FUNkcjia mean czyli liczymy srednia)
eviMc = evi2 #kopiujemy evi2 do nowego obiektu
eviMc@data = as.data.frame(t(tEviDataMc[, -1])) #podmieniamy slot dataitransponujemyabyznowdatybylywkolumnach
rEviMc = stack(eviMc) #funkcja stack konwertujemy GDALDataset na rasterStack (wracamy z pakietu GDAL do raster)
if (TRUE) {
  # plot(evi$EVI_2000.02.01)
  # pos <- locator(1)
  # print(pos)
  x = 345693.7
  y = 6025003
  wspMiejskie = data.frame(x = x, y = y)
  pxMiejskie = extract(rEviMc, wspMiejskie)

  # plot(evi$EVI_2000.02.01)
  # pos <- locator(1)
  # print(pos)
  # x = pos$x
  # y = pos$y
  x = 323341.7
  y = 6030817
  wspLas = data.frame(x = x, y = y)
  pxLas = extract(rEviMc, wspLas)

  # plot(evi$EVI_2000.02.01)
  # pox`s <- locator(1)
  # print(pos)
  # x = pos$x
  # y = pos$y
  x = 362632.8
  y = 6044248
  wspxMorze = data.frame(x = x, y = y)
  pxMorze = extract(rEviMc, wspxMorze)
  png("wykres3.png")
  plot(pxLas[1,], ylim = c(-0.35, 0.7), type = "l", x = 1:12)
  lines(pxMiejskie[1,], col = "red", x = 1:12)
  lines(pxMorze[1,], col = "green", x = 1:12)
  dev.off()
}
if (TRUE) {
  eviMcClass = evi2 #kopiujemy evi2 do nowego obiektu
  eviMcClass@data = data.frame(kmeans(eviMc@data, centers = 4)$cluster)
  #podmieniamy data na wynik ($cluster) clusteringu z 4 klasami
  rEviMcClass = stack(eviMcClass) #funkcja stack konwertujemy GDALDatasetnarasterStack
  writeRaster(rEviMcClass, filename = "4_kasy.tif", overwrite = TRUE)
  png("4_kasy.png")
  plot(rEviMcClass)
  dev.off()

  eviMcClass@data = data.frame(kmeans(eviMc@data, centers = 6)$cluster)
  #podmieniamy data na wynik ($cluster) clusteringu z 6 klasami
  rEviMcClass = stack(eviMcClass) #funkcja stack konwertujemy GDALDatasetnarasterStack
  writeRaster(rEviMcClass, filename = "6_kasy.tif", overwrite = TRUE)
  png("6_kasy.png")
  plot(rEviMcClass)
  dev.off()
}

tEviDataRok = aggregate(tEviData, by = list(datyEVI$rok), FUN = mean)
#agregujemy piksele w tabeli tEviData za pomoca miesiecy w tabeli datyEVI(aregujemy FUNkcjia mean czyli liczymy srednia)
eviRok = evi2 #kopiujemy evi2 do nowego obiektu
eviRok@data = as.data.frame(t(tEviDataRok[, -1])) #podmieniamy slot dataitransponujemyabyznowdatybylywkolumnach
rEviRok = stack(eviRok) #funkcja stack konwertujemy GDALDataset na rasterStack (wracamy z pakietu GDAL do raster)

getRegCoef = function(x)
{
  dane = data.frame(evi = x, rok = 2000:2017) #podstaw pod xxx zgodnie z instrukcja
  modelLin = lm(evi ~ rok, data = dane) #podstaw pod xxx zgodnie z instrukcja
  # print(modelLn) #parametry regresji, sigma a - b
  return(modelLin$coefficients[2]) #podstaw pod xxx zgodnie z instrukcją
}


print(names(rEviRok))
model = calc(rEviRok, fun = getRegCoef)
print(model)

writeRaster(model, filename = "model.tif", overwrite = TRUE)
