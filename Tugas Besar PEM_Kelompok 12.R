library(readxl)     #Membaca file data excel
library(plm)        #untuk membuat model
library(kableExtra) #untuk tampilan tabel
library(lmtest)     #uji homoskedastisitas

# Import Data dengan klik file ->import dataset ->from Excel
data_tubes <- read_excel('F:/Mata Kuliah/Semester 5/Prinsip Ekonomi Mikro (Ekonometrika)/Tugas Besar 1.xls')
View(data_tubes)
#Pada output diatas dapat diketahui bahwa bentuk data merupakan data panel karena data dikumpulkan dalam kurun waktu tertentu dalam sejumlah objek tertentu. Data yang diinput merupakan data yang sudah dilakukan penumpukan terhadap waktu t maupun terhadap observasi.
data_tubes %>% kbl(format = "html", caption= "Data PDRB 2018-2020",align = 'c',longtable = 'T',) %>% kable_material(full_width=F,c("striped", "hover", "condensed", "responsive")) %>% scroll_box(width="100%", height="400px")
#dataPDRB<-as.numeric(dataPDRB)

#melihat deskripsi data
head(data_tubes)    #melihat 6 data awal
str(data_tubes) #Fungsi str digunakan untuk melihat deskripsi data meliputi jumlah variabel dan jumlah observasinya, tipe dari setiap variabel, dan datanya seperti apa.

#deskriptif
summary(data_tubes) #Fungsi summary digunakan untuk melihat statistik deskriptifnya meliputi min, max, median, rata-rata, dan kuartil
names(data_tubes)   #Fungsi names digunakan untuk melihat nama setiap kolom dari data.

common=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="pooling")
fixed=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="within")
pooltest(common,fixed)

# membuat model regresi panel
fixed=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="within",index = c("Perusahaan","Tahun"))
random=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="random",index = c("Perusahaan","Tahun"))

# Menguji Hausmaan
phtest(fixed,random)

#Uji Breusch Pagan
gr=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="random")

#Efek Dua Arah
plmtest(gr, effect="twoways", type="bp")

#Efek Individu/Cross Section
plmtest(gr, effect="individual", type="bp")

#Efek Waktu/Time
plmtest(gr, effect="time", type="bp")

#Pembuatan Model
# model 1
m1=plm(DTA~Size+Tang+Growth+Prof+Risk,data=data_tubes,model="random",effect="twoways",index = c("Perusahaan","Tahun"))
summary(m1)

ranef(m1)

#Pengujian Model
#Uji AUTOKORELASI
pbgtest(m1)

#uji Homoskedastisitas
bptest(m1)

#data visualization
par(mfrow=c(3,3))
boxplot(data_tubes$DTA,main='Boxplot Aktivitas Pajak Tahunan')
boxplot(data_tubes$Size,main='Boxplot Ukuran Perusahaan')
boxplot(data_tubes$Tang,main='Boxplot Sumber Daya yang Berwujud')
boxplot(data_tubes$Growth,main='Boxplot Pertumbuhan Penjualan')
boxplot(data_tubes$Prof,main='Boxplot Keuntungan')
boxplot(data_tubes$Prof,main='Boxplot Resiko')