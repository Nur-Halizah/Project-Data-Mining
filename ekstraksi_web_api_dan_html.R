## WEB SCRAPING IMDb TOP 250
# TAHAP 1: MEMANGGIL PACKAGE
library(tidyverse)  # Pengolahan data
library(httr)       # Mengirim request ke website
library(rvest)      # Membaca dan mengekstrak HTML

# TAHAP 2: IDENTITAS DAN ALAMAT WEBSITE
ua <- user_agent("TugasDataMining/1.0 (Nur halizah; kontak: nurhalizah685@gmail.com)")
url <- "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"

# TAHAP 3: MENGAMBIL HALAMAN HTML
Sys.sleep(runif(1, 1, 3))  # Jeda acak 1-3 detik
res <- GET(url, ua)
status_code(res)  # Memeriksa status respons
html <- read_html(content(res, as = "text", encoding = "UTF-8"))

# TAHAP 4: PARSING TABEL HTML UTAMA
table_data <- html |>
  html_element("table") |>
  html_table()
names(table_data)  # Melihat nama kolom
head(table_data)   # Melihat enam data pertama

# TAHAP 5: PEMBERSIHAN TEKS DAN PEMISAHAN KOLOM
ratings <- table_data |>
  select(
    rank_title_year = `Rank & Title`,
    rating = `IMDb Rating`
  ) |>
  mutate(
    rank_title_year = str_replace_all(rank_title_year, "\\n +", " ")
  ) |>
  separate_wider_regex(
    rank_title_year,
    patterns = c(
      rank = "\\d+", "\\. ",
      title = ".+", " +\\(",
      year = "\\d+", "\\)"
    )
  )

head(ratings)

# TAHAP 6: MENGAMBIL ATRIBUT TITLE DARI HTML
ratings_complete <- ratings |>
  mutate(
    rating_n_raw = html |>
      html_elements("td strong") |>
      html_attr("title")
  ) |>
  separate_wider_regex(
    rating_n_raw,
    patterns = c(
      "[0-9.]+ based on ",
      number = "[0-9,]+",
      " user ratings"
    )
  ) |>
  mutate(number = parse_number(number))
head(ratings_complete)

# TAHAP 7: MENGUBAH TIPE DATA DAN MERAPIKAN KOLOM
data <- ratings_complete |>
  mutate(
    peringkat = as.integer(rank),
    judul = title,
    tahun = as.integer(year),
    rating = as.numeric(rating),
    jumlah_rating = number
  ) |>
  select(
    peringkat,
    judul,
    tahun,
    rating,
    jumlah_rating
  )

data

# TAHAP 8: PEMERIKSAAN KUALITAS DATA
dim(data)  # Memeriksa jumlah baris dan kolom
anyNA(data)  # Memeriksa missing value
!any(duplicated(data$peringkat))  # Memeriksa duplikasi peringkat
range(data$tahun)  # Melihat rentang tahun film

# TAHAP 9: ANALISIS STATISTIK DESKRIPTIF
# Menghitung rata-rata rating film
mean(data$rating)
# Menghitung jumlah film berdasarkan dekade
data |>
  count(dekade = (tahun %/% 10) * 10)
# Menghitung korelasi rating dan jumlah pemberi rating
cor(data$rating, log10(data$jumlah_rating))

# TAHAP 10: MENYIMPAN DATA KE CSV
folder <- "C:/Users/ASUS/OneDrive/Data Mining_Semester5/"
dir.create(folder, showWarnings = FALSE, recursive = TRUE)
write_csv(data, file.path(folder, "imdb_top250_2.csv"))


## WEB API: FREE DICTIONARY API (api.dictionaryapi.dev)

# TAHAP 1: MEMANGGIL PACKAGE
library(tidyverse)  # Pengolahan data dan grafik
library(httr)       # Mengirim request ke website
library(jsonlite)   # Membaca data JSON

# TAHAP 2: PARAMETER
endpoint <- "https://api.dictionaryapi.dev/api/v2/entries/en/"
daftar_kata <- c("data", "mining", "pattern", "cluster", "algorithm",
                 "variable", "observation", "model", "tidy", "scrape",
                 "zxqwvkj")   

folder_output <- "C:/Users/ASUS/OneDrive/Data Mining_Semester5/data web api"
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)

# TAHAP 3: IDENTITAS (USER-AGENT) DAN CEK ROBOTS.TXT
ua <- user_agent("TugasDataMining/1.0 (Nur Halizah; kontak: nurhalizah685@gmail.com)")

robots <- GET("https://api.dictionaryapi.dev/robots.txt", ua)
status_code(robots) 
if (status_code(robots) == 200) cat(content(robots, as = "text", encoding = "UTF-8"))

# TAHAP 4: MENCOBA SATU KATA (MEMAHAMI STRUKTUR JSON)
Sys.sleep(runif(1, 1, 3))                       # jeda acak 1-3 detik
res <- GET(paste0(endpoint, "data"), ua)
status_code(res)                                # 200 = berhasil

js <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
length(js)                                      # jumlah entri untuk kata ini
names(js[[1]])                                  # bidang pada entri pertama
str(js[[1]], max.level = 2, list.len = 8)       # struktur bersarang (2 level)

# TAHAP 5: MENGAMBIL SEMUA KATA
ambil_kata <- function(kata) {
  Sys.sleep(runif(1, 1, 3))                                   # jeda acak 1-3 detik
  res <- tryCatch(GET(paste0(endpoint, kata), ua, timeout(30)),
                  error = function(e) NULL)                   # galat koneksi tidak menghentikan program
  status <- if (is.null(res)) NA_integer_ else status_code(res)
  js <- NULL
  if (!is.na(status) && status == 200) {
    js <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  list(kata = kata, status = status, data = js)
}

hasil_api <- lapply(daftar_kata, ambil_kata)

log_api <- tibble(
  kata    = sapply(hasil_api, function(x) x$kata),
  status  = sapply(hasil_api, function(x) x$status),
  n_entri = sapply(hasil_api, function(x) length(x$data))
) |>
  mutate(hasil = case_when(
    is.na(status) ~ "Gagal koneksi",
    status == 200 ~ "Ditemukan",
    status == 404 ~ "Tidak ditemukan",
    TRUE          ~ paste("Status", status)
  ))
log_api

stopifnot(any(log_api$status == 200, na.rm = TRUE))   # berhenti jika semua kata gagal

# TAHAP 6: MEMBENTUK DUA TABEL TIDY
ganti_na <- function(x) if (is.null(x)) NA_character_ else x   # bagian yang tidak ada diisi NA

baris_kata <- list()
baris_definisi <- list()

for (h in hasil_api) {
  if (is.null(h$data)) next                      # lewati kata yang tidak ditemukan
  
  for (i in seq_along(h$data)) {                 # tingkat 1: entri
    e <- h$data[[i]]
    
    audio   <- unlist(lapply(e$phonetics, function(p) p$audio))
    fonetik <- ganti_na(e$phonetic)
    if (!is.na(fonetik) && fonetik == "") fonetik <- NA_character_
    
    baris_kata[[length(baris_kata) + 1]] <- tibble(
      kata      = h$kata,
      entri     = i,
      fonetik   = fonetik,
      ada_audio = any(audio != "", na.rm = TRUE),
      n_kelas   = length(e$meanings),
      lisensi   = ganti_na(e$license$name)
    )
    
    for (m in e$meanings) {                      # tingkat 2: kelas kata
      for (d in m$definitions) {                 # tingkat 3: definisi
        baris_definisi[[length(baris_definisi) + 1]] <- tibble(
          kata       = h$kata,
          entri      = i,
          kelas_kata = ganti_na(m$partOfSpeech),
          definisi   = ganti_na(d$definition),
          contoh     = ganti_na(d$example),
          n_sinonim  = length(d$synonyms),
          n_antonim  = length(d$antonyms)
        )
      }
    }
  }
}

kata_tbl <- bind_rows(baris_kata)
definisi_tbl <- bind_rows(baris_definisi) |>
  mutate(panjang_definisi = str_count(definisi, "\\S+"))   # jumlah kata pada definisi

kata_tbl
definisi_tbl

# TAHAP 7: PIVOT (BENTUK PANJANG DAN LEBAR)
jumlah_panjang <- definisi_tbl |>
  count(kata, kelas_kata, name = "n_definisi")           # bentuk panjang (tidy)

jumlah_lebar <- jumlah_panjang |>
  pivot_wider(names_from = kelas_kata, values_from = n_definisi, values_fill = 0L)

jumlah_kembali <- jumlah_lebar |>
  pivot_longer(-kata, names_to = "kelas_kata", values_to = "n_definisi") |>
  filter(n_definisi > 0)

jumlah_lebar

# Cek: apakah setelah pivot bolak-balik hasilnya sama dengan awal?
a <- jumlah_panjang |> arrange(kata, kelas_kata) |> as.data.frame()
b <- jumlah_kembali |> arrange(kata, kelas_kata) |> as.data.frame()
isTRUE(all.equal(a, b, check.attributes = FALSE))   # TRUE = tidak ada informasi hilang

# TAHAP 8: PEMERIKSAAN KUALITAS DATA (TIDY)
nrow(kata_tbl)                                                 # jumlah entri kata
nrow(definisi_tbl)                                             # jumlah definisi
!any(duplicated(kata_tbl[, c("kata", "entri")]))               # TRUE = satu baris satu entri
all(definisi_tbl$kata %in% kata_tbl$kata)                      # TRUE = tiap definisi punya kata induk
anyNA(definisi_tbl[, c("kata", "kelas_kata", "definisi")])     # FALSE = kolom inti lengkap

# TAHAP 9: ANALISIS STATISTIK DESKRIPTIF
# Ringkasan per kata
ringkas_kata <- definisi_tbl |>
  group_by(kata) |>
  summarise(
    n_definisi    = n(),
    n_kelas       = n_distinct(kelas_kata),
    dengan_contoh = sum(!is.na(contoh)),
    rata_panjang  = mean(panjang_definisi)
  ) |>
  arrange(desc(n_definisi))
ringkas_kata

# Sebaran kelas kata
kelas_tbl <- definisi_tbl |>
  count(kelas_kata, sort = TRUE) |>
  mutate(persen = round(100 * n / sum(n), 1))
kelas_tbl

mean(ringkas_kata$n_definisi)              # rata-rata definisi per kata
mean(definisi_tbl$panjang_definisi)        # rata-rata panjang definisi (kata)
mean(!is.na(definisi_tbl$contoh)) * 100    # persen definisi yang punya contoh
sum(kata_tbl$ada_audio)                    # jumlah entri yang punya audio
sum(!is.na(kata_tbl$fonetik))              # jumlah entri yang punya fonetik
unique(na.omit(kata_tbl$lisensi))          # lisensi data yang dilaporkan API

# TAHAP 10: VISUALISASI
jumlah_panjang |>
  mutate(kata = fct_reorder(kata, n_definisi, .fun = sum)) |>
  ggplot(aes(x = kata, y = n_definisi, fill = kelas_kata)) +
  geom_col() +
  coord_flip() +
  labs(
    title   = "Jumlah Definisi per Kata menurut Kelas Kata",
    x = "Kata", y = "Jumlah definisi", fill = "Kelas kata",
    caption = "Sumber: api.dictionaryapi.dev"
  ) +
  theme_minimal()

# TAHAP 11: MENYIMPAN DATA KE CSV
write_csv(kata_tbl, file.path(folder_output, "kamus_kata.csv"))
write_csv(definisi_tbl, file.path(folder_output, "kamus_definisi.csv"))
list.files(folder_output)   # memastikan kedua berkas sudah ada
