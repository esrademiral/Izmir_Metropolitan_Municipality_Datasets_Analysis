#Check the datasets

head(gerceklesen_sefer_sayilari)
head(emisyon_degerleri_co2)
head(enerji_tuketimi_yolcubasina)

#Is there any null values in dataset?

colSums(is.na(gerceklesen_sefer_sayilari))
colSums(is.na(emisyon_degerleri_co2))
colSums(is.na(enerji_tuketimi_yolcubasina))

#Is there any unfilled values in dataset?

colSums(gerceklesen_sefer_sayilari == "")
colSums(emisyon_degerleri_co2 == "")
colSums(enerji_tuketimi_yolcubasina == "")

#Summary of the datasets
summary(gerceklesen_sefer_sayilari
summary(emisyon_degerleri_co2)
summary(enerji_tuketimi_yolcubasina)
