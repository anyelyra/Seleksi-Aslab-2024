# Seleksi Aslab EDA & R_1

# Import data
df = read.csv("C:/Users/anyel/Downloads/movies_dataset.csv")
View(df)

# ===== PEMBERSIHAN DATA ===== #
# Mengidentifikasi dan menangani data yang hilang atau tidak konsisten.

# data yang hilang
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

# baris yang duplikat
duplicated_rows <- df[duplicated(df), ]
print(duplicated_rows)

num_duplicated_rows <- sum(duplicated(df))
print(paste("Jumlah baris duplikat: ", num_duplicated_rows))

# ===== STATISTIK DESKRIPTIF ===== #

# Deteksi kolom numerik
numeric_cols <- sapply(df, is.numeric)

# Melihat nama kolom numerik
numeric_col_names <- names(df)[numeric_cols]
print(numeric_col_names)

# Fitur yang dipakai: 
# "budget", "runtime", "box_office", "vote_avarage"

summary(df$budget)
summary(df$runtime)
summary(df$box_office)
summary(df$vote_average)

# ===== VISUALISASI ===== #

# === Hubungan antara Anggaran dan Box Office === #

# Membuat visualisasi untuk mengeksplorasi 
# hubungan antara anggaran dan box office
plot(df$budget, df$box_office, main="Hubungan antara Anggaran dan Box Office",
     xlab="Anggaran", ylab="Box Office", pch=19, col="blue")

# === Popularitas dan rata-rata suara di berbagai genre === #

boxplot(df$popularity ~ df$genres,
        main="Popularitas di Berbagai Genre",
        xlab="Genre", ylab="Popularitas",
        col="lightgreen", las=2)

boxplot(df$vote_average ~ df$genres,
        main="Rata-rata Vote di Berbagai Genre",
        xlab="Genre", ylab="Rata-rata Vote",
        col="lightblue", las=2)

# === Dampak Tahun Rilis terhadap Pendapatan Box Office === #

plot(df$release_year, df$box_office,
     main="Dampak Tahun Rilis terhadap Pendapatan Box Office",
     xlab="Tahun Rilis", ylab="Box Office",
     pch=19, col="red")

# === Distribusi Film Berdasarkan Bahasa dan Perusahaan Produksi === #

# Berdasarkan Bahasa
language_counts <- table(df$language)
barplot(language_counts, main="Distribusi Film Berdasarkan Bahasa",
        xlab="Bahasa", ylab="Jumlah Film",
        col="lightblue", las=2)

# Berdasarkan Perusahaan Produksi
production_counts <- table(df$production_companies)
barplot(production_counts, main="Distribusi Film Berdasarkan Perusahaan Produksi",
        xlab="Perusahaan Produksi", ylab="Jumlah Film",
        col="lightgreen", las=2, cex.names=0.7)

# === Analisis Genre === #
# 3 genre teratas berdasarkan kesuksesan box office,
# popularitas, dan rating rata-rata

average_box_office <- tapply(df$box_office, df$genre, mean)
average_popularity <- tapply(df$popularity, df$genre, mean)
average_vote <- tapply(df$vote_average, df$genre, mean)

# 3 genre teratas berdasarkan box office
top_genres_box_office <- sort(average_box_office, decreasing = TRUE)[1:3]
print(top_genres_box_office)

# 3 genre teratas berdasarkan popularitas
top_genres_popularity <- sort(average_popularity, decreasing = TRUE)[1:3]
print(top_genres_popularity)

# 3 genre teratas berdasarkan vote average
top_genres_vote <- sort(average_vote, decreasing = TRUE)[1:3]
print(top_genres_vote)


# 1. Jika John ingin mempertimbangan genre. Maka, genre yang paling populer
# adalah Action. Karena rata-rata vote antar genre tidak jauh berbeda, maka 
# angka populeritas dapat menjadi pertimbangan

# 2. Anggaran dan Box Office juga tidak terlihat memiliki korelasi yang erat.
# Bagitu juga dengan dampak tahun rilis.

# 3. Jika John ingin memilih perusahaan produksi, disarankan memilih
# perusahaan yang produktif mengeluarkan film. Terlihat bahwa di sepanjang
# tahun, Warner Bros cukup

