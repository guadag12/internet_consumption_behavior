#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Wrangling of the data and replaces node's names              #
#################################################################


# Packages -----------------------------------------------------------------


library(stringr)
library(tidyverse)
library(readr)

rm(list = ls())
options(scipen = 999)



# Import database ---------------------------------------------------------

flow_interaction_arg <- read_csv("~/GitHub/ideology_consumption_network/01_data/flow_interaction_arg.csv")

flow_interaction_arg$first_interaction <- sub("/.*", "", flow_interaction_arg$first_interaction)
flow_interaction_arg$second_interaction <- sub("/.*", "", flow_interaction_arg$second_interaction)
data_all <- as.data.frame(flow_interaction_arg)

# rename "first_interaction" column
data_all <- data_all %>% rename(value=first_interaction) 

# replave values "google.com.ar" for "google.com"
data_all$value  <- ifelse(data_all$value == ".google.com.ar", ".google.com", data_all$value )

data_all_group_ <- data_all
data_all_group <- as.data.frame(data_all_group_)


# Data Wrangling ----------------------------------------------------------

# replace values to unify the node name
data_all_group$value <- gsub(".*facebook\\.com$", "facebook.com", data_all_group$value)
data_all_group$value <- gsub("facebook.com.ar", "facebook.com", data_all_group$value)
data_all_group$value <- gsub("facebook.co", "facebook.com", data_all_group$value)
data_all_group$value <- gsub("facebook.c", "facebook.com", data_all_group$value)
data_all_group$value <- gsub("comomm", "com", data_all_group$value)
data_all_group$value <- gsub("comom", "com", data_all_group$value)

# generate a new column to classify the nodes
data_all_group$category_first <- NA
data_all_group$category_second <- NA


# Facebook ----------------------------------------------------------------


# generate lists with the values to unify
# this process will be repetead by all applications/websites

webs_fb<- c("facebook.video.downloader.fb.videosaver", "facebookblueprint.com","facebookviewpoints.perksplus.com","megazebra-facebook-trails.mega-zebra.com","sparkarcertification.facebookblueprint.com")
valores_fb <- c("Facebook","Facebook Lite","Facebook Messenger")

valores_work <- c("Facebook at Work","Facebook Pages Manager", "Facebook Ads Manager", "Facebook Viewpoints","Study from Facebook")

valores_download <- c("Bajar Videos de Facebook y Redes Sociales",
                      "Bajar ViÂ­deos de Facebook y Redes Sociales",
                      "Video Downloader for Facebook - FB Video Download",
                      "Video downloader for Facebook-Fastget",
                      "Post Maker for Facebook|Story Saver for Facebook",
                      "Download Story For Facebook",
                      "Bajar ViÂdeos de Facebook y Redes Sociales",
                      "MyVideoDownloader for Facebook",
                      "Story Saver For Facebook Stories Download For FB",
                      "Video Downloader for Facebook",
                      "Video Downloader for Facebook - FB Video Download",
                      "Video Downloader for Facebook Video Downloader",
                      "Video Downloader for Instagram and Facebook",
                      "Video downloader for Facebook",
                      "Video downloader for Facebook-Fastget",
                      "Story Saver for Facebook", "Post Maker for Facebook",
                      "Friendly for Facebook")


# change the values with "case_when()" and assign a category to the node:
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% valores_fb ~  "Facebook App",
    value %in% valores_work ~  "Facebook for Work",
    TRUE ~ value
  ),
  category_first = ifelse((value %in% valores_download|
                             value %in% valores_fb|
                             value %in% valores_work|
                             value %in% webs_fb |
                             value %in% c("Facebook at Work","Facebook for Work", "Facebook App")), "Facebook", category_first),
  category_second  = ifelse((value %in% valores_download|
                               value %in% valores_fb|
                               value %in% valores_work|
                               value %in% webs_fb|
                               value %in% c("Facebook at Work","Facebook for Work", "Facebook App")), "Social Media", category_second)
  )


# Instagram ---------------------------------------------------------------


ig <- c( "instagram.vom", "l.instagram.com", "platform.instagram.com", "www.instagram","applink.instagram.com", 
         "edge-chat.instagram.com",  "help.instagram.com", "i.instagram.com", "instagram.cln", "instagram.co", "instagram.com")

ig_work <- c( "business.instagram.com", "graph.instagram.com", "accountscenter.instagram.com")

ig_others <- c("instagram.faep15-1.fna.fbcdn.net", "instagram.faep15-2.fna.fbcdn.net", "instagram.faep6-1.fna.fbcdn.net", "instagram.faep9-1.fna.fbcdn.net", "instagram.fcor10-3.fna.fbcdn.net", "instagram.feze12-1.fna.fbcdn.net", "instagram.foyo1-1.fna.fbcdn.net", "instagram.fpra1-1.fna.fbcdn.net", 
               "instagram.fros1-1.fna.fbcdn.net", "instagram.fros2-2.fna.fbcdn.net", "instagram.fsde2-1.fna.fbcdn.net", "instagram.uptodown.com", 
               "scontent-ams4-1.cdninstagram.com", "scontent-amt2-1.cdninstagram.com", "scontent-yyz1-1.cdninstagram.com", "scontent.cdninstagram.com",
               "z-p15.www.instagram.com", "z-p42.www.instagram.com")

ig_orig <- c("Instagram Lite", "Instagram")
ig_follow <- c( "Follower Analyzer (Instagram)", "Follower Insight for Instagram", "Followers Reports for Instagram", 
                "Unfollow Users for  Instagram", "Unfollow for Instagram - Non followers & Fans", "Unfollowers 4 Instagram - Check who unfollowed you", "Unfollowers for Instagram", "Unfollowers for Instagram & Twitter - Nomesigue", "Unfollowers for Instagram.lost", "Video Downloader for Instagram", "Video Downloader for Instagram - Repost IG Photo", "Video Downloader for Instagram - iG Story Saver")
ig_download <- c( "HD Photo & Video Downloader for Instagram-IG Saver", "Photo & Video Downloader for Instagram- InstaSaver", 
                  "Video Downloader for Instagram", "Video Downloader for Instagram - Repost IG Photo", "Video Downloader for Instagram - iG Story Saver", "Video Downloader for Instagram. Reels. IG Saver", 
                  "Video Downloader for Instagram. Story Saver", "Video Downloader for Instagram. Video Locker", "Photo & Video Downloader for Instagram - Repost IG",
                  "Story Save - Instagram Stories Download", "Story Saver Instagram - IG Story Downloader Repost", "Story Saver for Instagram", "Story Saver for Instagram - Ins Video Downloader", "Story Saver for Instagram - Story Downloader", "Story Saver for Instagram - Story Manager", "Story Saver for Instagram - Video Downloader", 
                  "Story Saver for Instagram - Video Downloader Two", "Story saver. Video Downloader for Instagram", "StoryArt - Insta story editor for Instagram", "StoryLab - insta story art maker for Instagram", "Tuval - Story & Post Templates for Instagram",
                  "Story Saver Instagram - IG Story Downloader Repost")

ig_photo <- c("Highlight Cover Maker for Instagram Story", "Boomerang from Instagram","InsLikes+ Get Likes for Instagram", "Insget - Download Photos & Videos From Instagram", "Inspiry - Stories Editor for Instagram", "Insta Story - Instagramstories Ig Story Art Maker", "Insta Story Art - Instagram stories maker", 
              "InstaGrabber for Instagram","Instameter for Instagram", "Layout from Instagram", "MoStory - animated story art editor for Instagram", "No Crop for Instagram", "NoSeen for Instagram", 
              "Photo & Video Downloader for Instagram- InstaSaver", "Photo Grids - Crop photos and Image for Instagram", "Preview - Plan your Instagram", "Preview for Instagram Feed - Free Planner App", "Repost for Instagram 2019 - Repost Video & Photo", 
              "Reposta - Repost for Instagram", "Space - Spaces for Instagram", "Video Editor Instagram No Crop", "Video Splitter for WhatsApp Status. Instagram", "mojo Ã¢Â\200Â“ Video Stories Editor for Instagram",
              "Cool Fonts for Instagram Bio", "Feed Preview for Instagram", "Instagram Photo Editor")
ig_other <- c("Boomerang from Instagram")
data_all_group[grepl("Instagram",data_all_group$value),]

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% ig ~  "instagram.com",
    value %in% ig_work ~  "instagram.com",
    value %in% ig_orig ~ "Instagram", 
    TRUE ~ value
  ),
  category_first = ifelse((value %in% ig |
                             value %in% ig_work |
                             value %in% ig_others |
                             value %in% ig_orig |
                             value %in% ig_follow |
                             value %in% ig_download |
                             value %in% ig_photo), "Instagram", category_first),
  category_second  = ifelse((value %in% ig |
                               value %in% ig_work |
                               value %in% ig_others |
                               value %in% ig_orig |
                               value %in% ig_follow |
                               value %in% ig_download |
                               value %in% ig_photo), "Social Media", category_second)
  
  )


# Twitter -----------------------------------------------------------------


twitter_mkt <- c("ads-api.twitter.com","api.twitter.com", "marketing.twitter.com")
twittercom <- c("analytics.twitter.com","blog.twitter.com", "help.twitter.com", "mobile.twitter.com", "publish.twitter.com", "ton.twitter.com", "tweetdeck.twitter.com", "twitter.com")
twitter_download <- c("downloadtwittervideo.com")
tw_down <- c("Download Twitter Videos - Save Twitter & GIF", "Download Twitter Videos - Twitter video downloader")

tw <- c("Twitter", "Twitter Lite")
tw_unf<- c("Unfollow Today for Twitter")


data_all_group<- data_all_group %>%
  mutate(value = case_when(
    #  value %in% twitter_download ~  "twitter.com Downloader",
    value %in% twitter_mkt ~  "twitter.com MKT",
    value %in% twittercom ~  "twitter.com",
    
    #   value %in% tw_down ~  "Twitter Downloader",
    value %in% tw ~  "Twitter",
    value %in% tw_unf ~  "Twitter Unfollow",
    TRUE ~ value
  ),
  category_first = ifelse((value %in% twitter_download |
                             value %in% twitter_mkt |
                             value %in% twittercom |
                             value %in% tw_down |
                             value %in% tw |
                             value %in% tw_unf | value %in% c("Twitter Unfollow")), "Twitter", category_first),
  category_second  = ifelse((value %in% twitter_download |
                               value %in% twitter_mkt |
                               value %in% twittercom |
                               value %in% tw_down |
                               value %in% tw |
                               value %in% tw_unf | value %in% c("Twitter Unfollow")), "Social Media", category_second)
  )


# YouTube -----------------------------------------------------------------


yt <- c(".youtube.com", "accounts.youtube.com", "com.android.youtube", "com.gold.android.youtube", "creatoracademy.youtube.com", "families.youtube.com","img.youtube.com", "kids.youtube", "kids.youtube.c", "kids.youtube.com", "m.youtube.com",
        "music.youtube.com", "studio.youtube.com","www.youtube.com","youtube.com", "youtube.com.mx", "youtube.com"  )

yt_down <- c( "mp3youtube.download", "tubemate-youtube-downloader.mrdownload.com", "tubemate-youtube-downloader.uptodown.com",  "youtube4kdownloader.com")

yt_others <- c("httpswwwyoutubecomwatchvr32idx01ceu.nuevomix.net", 
               "inyoutube.net", "suggestqueries-clients6.youtube.com", "youtube-nocookie.com", "youtube-video.download", "youtube.fandom.com", "youtubego.com", "youtubekids.com", "youtubep.com", "youtubepp.com", "youtubesave.org", "youtubetrimmer.com")

yt_app_other <- c("Free music for YouTube", "Minimizer for YouTube Classic - Background Music")
yt_app <- c("YouTube Creator Studio", "YouTube Go", "YouTube Kids", "YouTube Music", "YouTube VR")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% yt ~  "youtube.com",
    value %in% yt_app ~ "YouTube",
    
    TRUE ~ value
  ),
  category_first = ifelse((value %in% yt |
                             value %in% yt_others |
                             value %in% yt_down |
                             value %in% yt_app_other |
                             value %in% yt_app |
                             value == "YouTube"), "YouTube", category_first),
  category_second  = ifelse((value %in% yt |
                               value %in% yt_others |
                               value %in% yt_down |
                               value %in% yt_app_other |
                               value %in% yt_app  |
                               value == "YouTube"), "Music", category_second)
  )


# Spotify -----------------------------------------------------------------


st<- c("accounts.spotify.com", "apresolve.spotify.com", "community.spotify.com", "guc3-spclient.spotify.com", "open.spotify.com", "play.spotify.com", "spotify-international.sheerid.com", "spotify.app.link", "spotify.com", "spotifycodes.com", "support.spotify.com", "wl.spotify.com")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% c("Spotify", "Spotify Lite", "Spotify Music") ~  "Spotify",
    value %in% st ~  "spotify.com",
    TRUE ~ value
  ),
  
  category_first = ifelse(  (value %in% c("Spotify", "Spotify Lite", "Spotify Music") |
                               
                               value == "spotify.com" |
                               value %in% st |
                               
                               value == "Spotify"), "Spotify", category_first),
  
  category_second = ifelse( (value %in% c("Spotify", "Spotify Lite", "Spotify Music") |
                               value == "spotify.com" |
                               value %in% st |
                               value == "Spotify"),  "Music", category_second)
  )



# TikTok ------------------------------------------------------------------

tk <- c("m.tiktok.com","support.tiktok.com",  "tiktok.com","vm.tiktok.com")
tk_oth <- c("lf16-tiktok-common.ibytedtos.com", "lf16-tiktok-web.ttwstatic.com",  "mcs-va.tiktok.com", "mssdk-va.tiktokv.com", 
            "p16-sign-sg.tiktokcdn.com", "p16-sign-va.tiktokcdn.com", "p77-sign-va.tiktokcdn.com", "s16.tiktokcdn.com", "s20.tiktokcdn.com", "tiktokcdn.com", 
            "tiktokv.com", "v16-web.tiktok.com", "v19-us.tiktokcdn.com", "v77.tiktokcdn.com")
tk_down <- c("SSS Video Downloader for TikTok - No Watermark","TikTok Wall Picture")
tk_app<- c("TikTok - Make Your Day", "TikTok - Trends Start Here", "TikTok Lite")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% tk ~  "tiktok.com",
    #value %in% tk_oth ~  "sites related with tiktok.com",
    #value %in% tk_down ~ "TikTok Downloader",
    value %in% tk_app ~ "TikTok",
    TRUE ~ value
  ),
  category_first = ifelse((value %in% tk |
                             value %in% tk_down |
                             value %in% tk_oth |
                             value %in% tk_app |
                             value == "TikTok"), "TikTok", category_first),
  category_second  = ifelse((value %in% tk |
                               value %in% tk_down |
                               value %in% tk_oth |
                               value %in% tk_app |
                               value == "TikTok"), "Social Media", category_second)
  )


# Netflix & Movies Platforms ----------------------------------------------

nt <- c("help.netflix.com", "ichnaea-web.netflix.com", "netflix.com", "top10.netflix.com")
pr <- c("app.primevideo.com", "primevideo.com", "www-primevideo-com.translate.goog")
hbo <- c("activate.hbomax.com", "hbolatam.com", "hbomax.com", "hbomax.onelink.me", "play.hbomax.com")
disney <- c("click.mail.disneyplus.com", "disneyplus.bn5x.net", "disneyplus.com", "help.disneyplus.com")
cuevana_pelis <- c("api.cuevana3.io", "cuevana-3.com", "cuevana-3.wtf", "cuevana.nz", "cuevana.pro", "cuevana.tv", "cuevana2.club", "cuevana2.io", "cuevana2.nl",
                   "cuevana2espanol.com", "cuevana3.cc", "cuevana3.club", "cuevana3.icu", "cuevana3.io", "cuevana3.mobi", "cuevana3.pro", "cuevana3.so", "online.cuevana3.live", 
                   "ver.cuevana3.cc", "web.cuevana2.nl", "www1.cuevana3.so",
                   "7pelis.xyz", "bajarpelisgratis.com", "blogdepelis.io", "bonipelis.com", "geekpelishd.blogspot.com", "gloria-guida.azpelis.com", "linfermiera-di-notte.azpelis.com", "locopelis.com", "newpelis.nl", "pelis-123.net", "pelis-online.net", "pelis-online.tv", "pelis24.com", "pelis24.gratis", "pelis24.red", "pelis24.se", "pelis24plus.com", "pelisenhd.net", "peliseries.live", "peliserieshd.com", "pelisflix.club", "pelisflix.li", "pelisgratis.nu", "pelisgratishd.com", "pelishd4k.com", "peliskstdpats.blogspot.com", 
                   "pelislatino.org", "pelismaraton.com", "pelismart.com", "pelisnow.to", "pelispanda.com", "pelispe.com", "pelispedia-v1.wtf", "pelispedia-venom-2-en-espanol.tumblr.com", "pelispedia.de", "pelispedia.io", "pelispedia.is", "pelispedia2.me", "pelispedia2.vip", "pelisplay.co", "pelisplay.tv", "pelisplus-hd.net", "pelisplus.icu", "pelisplus.io", "pelisplus.live", "pelisplus.me", "pelisplus.online", "pelisplus.so", "pelisplus.watch", "pelisplus2.io", "pelisplus2.me", "pelisplus2.org", "pelisplus3.com", 
                   "pelisplushd.com", "pelisplushd.net", "pelisplusoficial.com", "pelisplustv.co", "pelisplustv.live", "pelispop.me", "pelispop.org", "pelispophd.com", "pelispunto.net", "pelisr.com", "pelisxeyuiorb.blogspot.com", "pelisyserieshd.com", "ppelisplus.com", "repelis.net", "repelis.red", "repelis2.co", "repelises.com", "repelishd.me", "repelisplus.fans", "repelisplus.id", "repelisplus.li", "repelisrkkurr.blogspot.com", "rpelis.net", "v4.pelispluss.org", "venom2carnageliberado-pelis.tumblr.com", "verpelisgratis.net", 
                   "verpelisonlineeqxym.blogspot.com", "verpelisonlineztkyb.blogspot.com", "www6.pelisplus.movie")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% nt ~  "netflix.com",
    value %in% pr ~  "primevideo.com",
    value %in% hbo ~  "hbomax.com",
    value %in% disney ~  "disneyplus.com",
    TRUE ~ value
  ),
  category_first = case_when(
    value %in% nt | value == "Netflix" ~  "Netflix",
    value %in% pr | value == "Amazon Prime Video"~  "Amazon Prime Video",
    value %in% hbo | value == "HBO NOW" ~  "HBO NOW",
    value %in% disney | value == "Disney+" ~  "Disney+",
    #   value %in% cuevana_pelis | value == "Netflix" ~  "Cuevana",
    T ~ category_first
  ),
  category_second  = ifelse((value %in% nt |
                               value %in% pr |
                               value %in% hbo |
                               value %in% disney |
                               value %in% cuevana_pelis |
                               value == "Netflix" |
                               value == "Disney+" |
                               value == "HBO NOW" |
                               value == "Amazon Prime Video" |
                               value %in% c("Cuevana 3 Prime", "Cuevana 3 Prime Peliculas y series" )
  ), "Movies", category_second)
  )


# Shopping online ---------------------------------------------------------

ml <- c("addresses.mercadolibre.com.ar", "ads.mercadolibre.com.ar", "api.mercadolibre.com", "articulo.mercadolibre.cl", "articulo.mercadolibre.com.ar", "articulo.mercadolibre.com.co", "articulo.mercadolibre.com.mx", "articulo.mercadolibre.com.pe", "articulo.mercadolibre.com.uy", "auth-identity.mercadolibre.com.ar", "auth.mercadolibre.com.ar", "auto.mercadolibre.com.ar", "autos.mercadolibre.com.ar", "ayuda.mercadolibre.com.ar", "bolsos.mercadolibre.com.ar", "camaras-digitales.mercadolibre.com.ar", "careers-meli.mercadolibre.com", 
        "casa.mercadolibre.com.ar", "catalog-domains-admin.mercadolibre.com.ar", "celulares.mercadolibre.com.ar", "celulares.mercadolibre.com.co", "chat.mercadolibre.com.ar", "click1.mercadolibre.com.ar", "coleccionables.mercadolibre.com.ar", "computacion.mercadolibre.com.ar", "computacion.mercadolibre.com.mx", "data.mercadolibre.com", "departamento.mercadolibre.com.ar", "deportes.mercadolibre.com.ar", "deportes.mercadolibre.com.mx", "electronica.mercadolibre.com.ar", "electronica.mercadolibre.com.mx", "encuestas.mercadolibre.com", 
        "envios.mercadolibre.com.ar", "eshops.mercadolibre.com.ar", "events.mercadolibre.com", "feedback.mercadolibre.com.ar", "fiesta.mercadolibre.com.ar", "fotografia.mercadolibre.com.mx", "hogar.mercadolibre.com.ar", "ideas.mercadolibre.com", "inmueble.mercadolibre.com.ar", "inmuebles.mercadolibre.com.ar", "instrumentos.mercadolibre.com.ar", "jobs.mercadolibre.com", "joyas.mercadolibre.com.ar", "joyas.mercadolibre.com.mx", "juegos-juguetes.mercadolibre.com.ar", "libros.mercadolibre.com.ar", "libros.mercadolibre.com.mx", 
        "listado.mercadolibre.cl", "listado.mercadolibre.com.ar", "listado.mercadolibre.com.co", "listado.mercadolibre.com.mx", "listado.mercadolibre.com.pe", "listado.mercadolibre.com.uy", "listado.mercadolibre.com.ve", "liveness.mercadolibre.com.ar", "matt.mercadolibre.com.ar", "mediations.mercadolibre.com.ar", "melidata.mercadolibre.com", "melidata.mercadolibre.com.ar", "mercadolibre-oauth.firebaseapp.com", "mercadolibre.cl", "mercadolibre.com", "mercadolibre.com.ar", "mercadolibre.com.mx", "mercadolibre.com.pe", 
        "mercadolibre.com.uy", "mercadolibre.com.xn--ar-5ia", "mercadolibre.eightfold.ai", "mercadolibreparacoger.ar", "moto.mercadolibre.com.ar", "motos.mercadolibre.com.ar", "myaccount.mercadolibre.com.ar", "ofertas.mercadolibre.com.ar", "ofertas.mercadolibre.com.mx", "perfil.mercadolibre.com.ar", "places.mercadolibre.com.ar", "pmstrk.mercadolibre.com.ar", "registration.mercadolibre.com.ar", "relojes.mercadolibre.com.ar", "resell.mercadolibre.com.ar", "reviews.mercadolibre.com.ar", "ropa.mercadolibre.com.ar", 
        "ropa.mercadolibre.com.mx", "servicio.mercadolibre.com.ar", "servicios.mercadolibre.com.ar", "shipping-frontend.mercadolibre.com.ar", "survey.mercadolibre.com", "telefonia.mercadolibre.com.ar", "telefonos.mercadolibre.com.co", "televisores.mercadolibre.com.ar", "terreno.mercadolibre.com.ar", "tienda.mercadolibre.com.ar", "tuning.mercadolibre.com.ar", "vehiculo.mercadolibre.com.ar", "vehiculos.mercadolibre.com.ar", "vehiculos.mercadolibre.com.mx", "vendedores.mercadolibre.com.ar", "vender.mercadolibre.com.ar", 
        "videojuegos.mercadolibre.com.ar", "zapatillas.mercadolibre.com.ar", "zapatos.mercadolibre.com.ar")

mp <- c("api.mercadopago.com", "auth-identity.mercadopago.com.ar", "auth.mercadopago.com.ar", "calculadora-mercadopago.com.ar", "chat.mercadopago.com.ar", "clicktocall.mercadopago.com.ar", "liveness.mercadopago.com.ar", "melidata.mercadopago.com", "melidata.mercadopago.com.ar", "mercadopago.com", "mercadopago.com.ar", "mercadopago.com.mx")

amazon_shop <- c("amazon.co.uk", "amazon.com", "amazon.com.br", "amazon.com.mx", "amazon.de", "amazon.es")

amazon_cloud <- c("console.aws.amazon.com", "digital-meetup-signed-users.s3-eu-west-1.amazonaws.com", "digital-meetup-signed-users.s3-eu-west1.amazonaws.com", "dl.amazon.com", "docs.aws.amazon.com", "ec2-184-72-180-246.compute-1.amazonaws.com", "europae-media.s3.amazonaws.com", "gaming.amazon.com", "higherlogicdownload.s3-external-1.amazonaws.com", "hotmart.s3.amazonaws.com", "idp.federate.amazon.com", "images-cienradios-arc.s3.amazonaws.com", "images-na.ssl-images-amazon.com", 
                  "ka-perseus-images.s3.amazonaws.com", "m.media-amazon.com", "moonactive.s3.amazonaws.com", "music.amazon.com.mx", "netivooregon.s3.amazonaws.com", "online-versions-prod.s3.amazonaws.com", "packglobal-publico.s3.amazonaws.com", "passport.amazon.jobs", "pay.amazon.es", "pdf-ceg-input.s3.amazonaws.com", "petroamazonas.gob.ec", "prismic-io.s3.amazonaws.com", "rtvc-assets-radionica3.s3.amazonaws.com", "s3-sa-east-1.amazonaws.com", "s3-us-west-2.amazonaws.com", "s3.amazonaws.com", "s3.console.aws.amazon.com", 
                  "sa-east-1.console.aws.amazon.com", "sa-east-1.signin.aws.amazon.com", "signin.aws.amazon.com", "simplesolutionscloud.s3.sa-east-1.amazonaws.com", "smile.amazon.com", "swissjust-platform.s3.amazonaws.com", "the-snake-game.s3.us-east-2.amazonaws.com", "tycsportsplay.auth.us-east-1.amazoncognito.com", "unagi-na.amazon.com", "unagi.amazon.com", "unagi.amazon.com.mx", "us-east-2.console.aws.amazon.com", "us-west-1.console.aws.amazon.com", "us-west-2.console.aws.amazon.com", "vendiendoporamazon.com", 
                  "wellbin-uploads.s3.us-west-2.amazonaws.com")

amazon_oth <- c("aan.amazon.com", "aan.amazon.it", "aax-us-east.amazon-adsystem.com", "account-status.amazon.es", "account.amazon.jobs", "afiliados.amazon.es",
                "amazon.jobs", "amazon.syf.com", "amazongames.com", "assessments.amazon.jobs", "aws.amazon.com", "backend-appuploads.s3.amazonaws.com", "branding.s3.us-west-2.amazonaws.com", "caelum-online-public.s3.amazonaws.com", "cesim-download.s3-website.eu-west-3.amazonaws.com", 
                "com.amazon.firelauncher", "com.amazon.smartgenie")
shein <- c("es.shein.com", "gorten-shein.com", "m.shein.com")

ebay <- c("accountsettings.ebay.de", "ar.ebay.com", "cart.ebay.com", "cart.payments.ebay.com", "donwebayuda.com", "ebay.ca", "ebay.co.uk", "ebay.com", "ebay.de", "ebay.es", "i.ebayimg.com", "mypiratebay.net", "order.ebay.com", "pineapplebay.com.ar", "piratebay.party", "pr.ebay.com", "reg.ebay.com", "rover.ebay.com", "signin.ebay.com", "signin.ebay.de", "sizebay.com", "thepiratebay.cc", "thepiratebay.org", "thepiratebay.party", "thepiratebay0.org", "thepiratebay10.org", "ww1.piratebay1.top", "ww25.piratebay-proxylist.se", 
          "www1.thepiratebay3.to")

anses <- c( "anses.go", "anses.gob.ar", "anses.gov.ar", "ansesresponde.anses.gob.ar", "ansessoporte-nlinea.com", "ansesturnos.ar", "ansesturnos.org", "atenciones.anses.gob.ar", "atenciones.anses.gov.ar", "certificado-escolar.anses.gob.ar", "encuestas.anses.gob.ar", "encuestas.anses.gov.ar", "mi-anses.com.ar", "multimedia.anses.gob.ar", "noticias.anses.gob.ar", "prestamos-anses.org", "servicioscorp.anses.gob.ar", "servicioswww.anses.gob.ar", "servicioswww.anses.gov.ar")
arg <- c("argentina.gob.ar", "calculadora-del-cuidado.argentina.gob.ar", "form-ddjj-turismo.argentina.gob.ar", "form-ddjj-verano.argentina.gob.ar", "form-docentes.argentina.gob.ar", "formulario-ddjj.argentina.gob.ar", "id.argentina.gob.ar", "mi.argentina.gob.ar", "mipieza.argentina.gob.ar", "resultadosargentina.gob", "tarjetaalimentaria.argentina.gob.ar", "turnos.argentina.gob.ar")



data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% ml ~  "mercadolibre.com",
    value %in% mp ~  "mercadopago.com",
    
    value %in% amazon_shop ~  "amazon.com",
    value %in% amazon_cloud ~  "console.aws.amazon.com",
    #   value %in% amazon_oth ~  "sites related with amazon.com",
    value %in% shein ~  "shein.com",
    value %in% ebay ~  "ebay.com",
    value %in% anses ~  "anses.gob.ar",
    value %in% arg ~  "argentina.gob.ar",
    
    TRUE ~ value
  ),
  category_first = case_when(
    value %in% ml | value == "mercadolibre.com" | value == "MercadoLibre"~  "Mercado Libre",
    value %in% mp | value == "mercadopago.com" | value == "MercadoPago" ~  "Mercado Pago",
    value %in% amazon_shop | value == "amazon.com" | value == "Amazon Shopping" ~  "Amazon Shopping",
    value == "console.aws.amazon.com" ~  "Amazon Cloud",
    value == "shein.com" ~  "Shein",
    value == "ebay.com" ~  "Ebay",
    T ~ category_first
  ),
  category_second  = ifelse((value %in% ml |
                               value %in% mp |
                               value %in% amazon_shop |
                               value %in% shein |
                               value %in% ebay |
                               value == "MercadoLibre" |
                               value == "MercadoPago" |
                               value == "Amazon Shopping" ), 
                            "Online Shopping", category_second)
  )



# Google's environment ----------------------------------------------------


google_books<- c("books.google.com.ar", "books.google.com.br", "books.google.com.co", "books.google.es", "books.google.tm")


google_drive<- c("drive.google.com",  "docs.google.com", "colab.research.google.com")

google_translate <-c("translate.google.cn", "translate.google.co.ve", "translate.google.com", "translate.google.com.ar", "translate.google.com.br",
                     "translate.google.com.co", "translate.google.com.cu","translate.google.com.mx",
                     "translate.google.es", "translate.googleusercontent.com")


googles <- c("google.br",".google.com", "google.c", "google.co", "google.co.uk", "google.com","google.com.ar","google.com.br", "google.com.mx", "google.com.uy",
             "google.com.vn", "google.de", "google.es", "google.gr", "search.google.com",   "www.google.com", "www.google.com.ar")

google_usercontent <- c("doc-00-08-apps-viewer.googleusercontent.com", "doc-00-0k-apps-viewer.googleusercontent.com", "doc-00-0s-apps-viewer.googleusercontent.com", "doc-00-14-apps-viewer.googleusercontent.com", "doc-00-1g-apps-viewer.googleusercontent.com", "doc-00-1o-apps-viewer.googleusercontent.com", "doc-00-20-apps-viewer.googleusercontent.com", "doc-00-24-apps-viewer.googleusercontent.com", "doc-00-2c-apps-viewer.googleusercontent.com", "doc-00-2o-apps-viewer.googleusercontent.com", "doc-00-2s-prod-00-apps-viewer.googleusercontent.com", 
                        "doc-00-30-apps-viewer.googleusercontent.com", "doc-00-38-apps-viewer.googleusercontent.com", "doc-00-3c-apps-viewer.googleusercontent.com", "doc-00-3k-apps-viewer.googleusercontent.com", "doc-00-40-apps-viewer.googleusercontent.com", "doc-00-4c-apps-viewer.googleusercontent.com", "doc-00-4g-apps-viewer.googleusercontent.com", "doc-00-4s-apps-viewer.googleusercontent.com", "doc-00-50-apps-viewer.googleusercontent.com", "doc-00-54-apps-viewer.googleusercontent.com", "doc-00-5o-apps-viewer.googleusercontent.com", 
                        "doc-00-6k-apps-viewer.googleusercontent.com", "doc-00-6o-apps-viewer.googleusercontent.com", "doc-00-74-apps-viewer.googleusercontent.com", "doc-00-7g-apps-viewer.googleusercontent.com", "doc-00-88-apps-viewer.googleusercontent.com", "doc-00-8o-apps-viewer.googleusercontent.com", "doc-00-8s-apps-viewer.googleusercontent.com", "doc-00-9g-apps-viewer.googleusercontent.com", "doc-00-a4-apps-viewer.googleusercontent.com", "doc-00-b4-apps-viewer.googleusercontent.com", "doc-00-bc-apps-viewer.googleusercontent.com", 
                        "doc-04-00-apps-viewer.googleusercontent.com", "doc-04-0c-apps-viewer.googleusercontent.com", "doc-04-0s-apps-viewer.googleusercontent.com", "doc-04-14-apps-viewer.googleusercontent.com", "doc-04-1g-apps-viewer.googleusercontent.com", "doc-04-20-apps-viewer.googleusercontent.com", "doc-04-2s-prod-00-apps-viewer.googleusercontent.com", "doc-04-30-apps-viewer.googleusercontent.com", "doc-04-38-prod-00-apps-viewer.googleusercontent.com", "doc-04-3g-apps-viewer.googleusercontent.com", "doc-04-3k-apps-viewer.googleusercontent.com", 
                        "doc-04-40-apps-viewer.googleusercontent.com", "doc-04-4o-apps-viewer.googleusercontent.com", "doc-04-4s-docs.googleusercontent.com", "doc-04-54-apps-viewer.googleusercontent.com", "doc-04-58-apps-viewer.googleusercontent.com", "doc-04-5o-apps-viewer.googleusercontent.com", "doc-04-6o-apps-viewer.googleusercontent.com", "doc-04-78-apps-viewer.googleusercontent.com", "doc-04-7g-apps-viewer.googleusercontent.com", "doc-04-7k-apps-viewer.googleusercontent.com", "doc-04-98-apps-viewer.googleusercontent.com", 
                        "doc-04-b4-apps-viewer.googleusercontent.com", "doc-04-bc-apps-viewer.googleusercontent.com", "doc-04-bo-apps-viewer.googleusercontent.com", "doc-04-c0-docs.googleusercontent.com", "doc-08-08-apps-viewer.googleusercontent.com", "doc-08-0c-apps-viewer.googleusercontent.com", "doc-08-0k-apps-viewer.googleusercontent.com", "doc-08-0s-apps-viewer.googleusercontent.com", "doc-08-10-apps-viewer.googleusercontent.com", "doc-08-1g-apps-viewer.googleusercontent.com", "doc-08-1k-apps-viewer.googleusercontent.com", 
                        "doc-08-20-apps-viewer.googleusercontent.com", "doc-08-24-apps-viewer.googleusercontent.com", "doc-08-2s-apps-viewer.googleusercontent.com", "doc-08-30-apps-viewer.googleusercontent.com", "doc-08-3k-apps-viewer.googleusercontent.com", "doc-08-4c-apps-viewer.googleusercontent.com", "doc-08-4o-apps-viewer.googleusercontent.com", "doc-08-4s-apps-viewer.googleusercontent.com", "doc-08-5c-apps-viewer.googleusercontent.com", "doc-08-5g-docs.googleusercontent.com", "doc-08-5o-apps-viewer.googleusercontent.com", 
                        "doc-08-68-apps-viewer.googleusercontent.com", "doc-08-6k-apps-viewer.googleusercontent.com", "doc-08-7c-prod-00-apps-viewer.googleusercontent.com", "doc-08-80-apps-viewer.googleusercontent.com", "doc-08-80-docs.googleusercontent.com", "doc-08-9k-apps-viewer.googleusercontent.com", "doc-08-a4-apps-viewer.googleusercontent.com", "doc-08-a8-apps-viewer.googleusercontent.com", "doc-08-bc-apps-viewer.googleusercontent.com", "doc-08-bo-prod-00-apps-viewer.googleusercontent.com", "doc-0c-0k-apps-viewer.googleusercontent.com", 
                        "doc-0c-0s-apps-viewer.googleusercontent.com", "doc-0c-14-apps-viewer.googleusercontent.com", "doc-0c-1g-apps-viewer.googleusercontent.com", "doc-0c-20-apps-viewer.googleusercontent.com", "doc-0c-24-apps-viewer.googleusercontent.com", "doc-0c-2c-apps-viewer.googleusercontent.com", "doc-0c-2g-apps-viewer.googleusercontent.com", "doc-0c-30-apps-viewer.googleusercontent.com", "doc-0c-38-apps-viewer.googleusercontent.com", "doc-0c-3g-apps-viewer.googleusercontent.com", "doc-0c-3k-apps-viewer.googleusercontent.com", 
                        "doc-0c-44-prod-03-apps-viewer.googleusercontent.com", "doc-0c-4o-apps-viewer.googleusercontent.com", "doc-0c-4s-apps-viewer.googleusercontent.com", "doc-0c-50-apps-viewer.googleusercontent.com", "doc-0c-5o-apps-viewer.googleusercontent.com", "doc-0c-6k-apps-viewer.googleusercontent.com", "doc-0c-74-apps-viewer.googleusercontent.com", "doc-0c-78-apps-viewer.googleusercontent.com", "doc-0c-7k-apps-viewer.googleusercontent.com", "doc-0c-80-apps-viewer.googleusercontent.com", "doc-0c-8c-apps-viewer.googleusercontent.com", 
                        "doc-0c-94-apps-viewer.googleusercontent.com", "doc-0c-9c-apps-viewer.googleusercontent.com", "doc-0c-a8-apps-viewer.googleusercontent.com", "doc-0c-b4-apps-viewer.googleusercontent.com", "doc-0c-bg-apps-viewer.googleusercontent.com", "doc-0c-c4-apps-viewer.googleusercontent.com", "doc-0c-c8-prod-00-apps-viewer.googleusercontent.com", "doc-0g-0k-apps-viewer.googleusercontent.com", "doc-0g-0s-apps-viewer.googleusercontent.com", "doc-0g-1k-apps-viewer.googleusercontent.com", "doc-0g-1o-apps-viewer.googleusercontent.com", 
                        "doc-0g-20-apps-viewer.googleusercontent.com", "doc-0g-24-apps-viewer.googleusercontent.com", "doc-0g-2s-apps-viewer.googleusercontent.com", "doc-0g-2s-docs.googleusercontent.com", "doc-0g-30-apps-viewer.googleusercontent.com", "doc-0g-3s-apps-viewer.googleusercontent.com", "doc-0g-4c-apps-viewer.googleusercontent.com", "doc-0g-4o-docs.googleusercontent.com", "doc-0g-4s-apps-viewer.googleusercontent.com", "doc-0g-50-apps-viewer.googleusercontent.com", "doc-0g-68-apps-viewer.googleusercontent.com", 
                        "doc-0g-6c-apps-viewer.googleusercontent.com", "doc-0g-6k-apps-viewer.googleusercontent.com", "doc-0g-6o-apps-viewer.googleusercontent.com", "doc-0g-74-apps-viewer.googleusercontent.com", "doc-0g-80-apps-viewer.googleusercontent.com", "doc-0g-88-apps-viewer.googleusercontent.com", "doc-0g-8c-apps-viewer.googleusercontent.com", "doc-0g-98-apps-viewer.googleusercontent.com", "doc-0g-b4-apps-viewer.googleusercontent.com", "doc-0g-bc-apps-viewer.googleusercontent.com", "doc-0g-bs-apps-viewer.googleusercontent.com", 
                        "doc-0g-c4-apps-viewer.googleusercontent.com", "doc-0k-0k-apps-viewer.googleusercontent.com", "doc-0k-0s-apps-viewer.googleusercontent.com", "doc-0k-10-apps-viewer.googleusercontent.com", "doc-0k-18-apps-viewer.googleusercontent.com", "doc-0k-1c-apps-viewer.googleusercontent.com", "doc-0k-1g-apps-viewer.googleusercontent.com", "doc-0k-1k-apps-viewer.googleusercontent.com", "doc-0k-20-apps-viewer.googleusercontent.com", "doc-0k-2c-apps-viewer.googleusercontent.com", "doc-0k-30-apps-viewer.googleusercontent.com", 
                        "doc-0k-38-apps-viewer.googleusercontent.com", "doc-0k-3k-apps-viewer.googleusercontent.com", "doc-0k-3s-apps-viewer.googleusercontent.com", "doc-0k-3s-prod-00-apps-viewer.googleusercontent.com", "doc-0k-4g-apps-viewer.googleusercontent.com", "doc-0k-4o-apps-viewer.googleusercontent.com", "doc-0k-4s-apps-viewer.googleusercontent.com", "doc-0k-4s-docs.googleusercontent.com", "doc-0k-54-apps-viewer.googleusercontent.com", "doc-0k-58-apps-viewer.googleusercontent.com", "doc-0k-58-prod-01-apps-viewer.googleusercontent.com", 
                        "doc-0k-5o-apps-viewer.googleusercontent.com", "doc-0k-5o-docs.googleusercontent.com", "doc-0k-64-prod-02-apps-viewer.googleusercontent.com", "doc-0k-68-apps-viewer.googleusercontent.com", "doc-0k-6k-apps-viewer.googleusercontent.com", "doc-0k-6o-apps-viewer.googleusercontent.com", "doc-0k-6s-apps-viewer.googleusercontent.com", "doc-0k-7g-apps-viewer.googleusercontent.com", "doc-0k-7k-apps-viewer.googleusercontent.com", "doc-0k-80-apps-viewer.googleusercontent.com", "doc-0k-88-apps-viewer.googleusercontent.com", 
                        "doc-0k-8s-apps-viewer.googleusercontent.com", "doc-0k-94-apps-viewer.googleusercontent.com", "doc-0k-9k-apps-viewer.googleusercontent.com", "doc-0k-a8-apps-viewer.googleusercontent.com", "doc-0k-bc-apps-viewer.googleusercontent.com", "doc-0k-bs-apps-viewer.googleusercontent.com", "doc-0o-0c-apps-viewer.googleusercontent.com", "doc-0o-0k-apps-viewer.googleusercontent.com", "doc-0o-0s-apps-viewer.googleusercontent.com", "doc-0o-1g-apps-viewer.googleusercontent.com", "doc-0o-20-apps-viewer.googleusercontent.com", 
                        "doc-0o-24-apps-viewer.googleusercontent.com", "doc-0o-2c-prod-03-apps-viewer.googleusercontent.com", "doc-0o-2g-apps-viewer.googleusercontent.com", "doc-0o-2s-apps-viewer.googleusercontent.com", "doc-0o-30-apps-viewer.googleusercontent.com", "doc-0o-3g-apps-viewer.googleusercontent.com", "doc-0o-3k-apps-viewer.googleusercontent.com", "doc-0o-4c-apps-viewer.googleusercontent.com", "doc-0o-4o-apps-viewer.googleusercontent.com", "doc-0o-50-apps-viewer.googleusercontent.com", "doc-0o-64-apps-viewer.googleusercontent.com", 
                        "doc-0o-6k-apps-viewer.googleusercontent.com", "doc-0o-70-apps-viewer.googleusercontent.com", "doc-0o-7c-apps-viewer.googleusercontent.com", "doc-0o-7o-docs.googleusercontent.com", "doc-0o-7s-apps-viewer.googleusercontent.com", "doc-0o-80-apps-viewer.googleusercontent.com", "doc-0o-8o-apps-viewer.googleusercontent.com", "doc-0o-98-apps-viewer.googleusercontent.com", "doc-0o-9k-apps-viewer.googleusercontent.com", "doc-0o-a8-apps-viewer.googleusercontent.com", "doc-0o-as-apps-viewer.googleusercontent.com", 
                        "doc-0o-b4-apps-viewer.googleusercontent.com", "doc-0o-bc-apps-viewer.googleusercontent.com", "doc-0s-00-apps-viewer.googleusercontent.com", "doc-0s-08-apps-viewer.googleusercontent.com", "doc-0s-0k-apps-viewer.googleusercontent.com", "doc-0s-0s-apps-viewer.googleusercontent.com", "doc-0s-14-apps-viewer.googleusercontent.com", "doc-0s-1k-apps-viewer.googleusercontent.com", "doc-0s-20-apps-viewer.googleusercontent.com", "doc-0s-24-apps-viewer.googleusercontent.com", "doc-0s-2c-apps-viewer.googleusercontent.com", 
                        "doc-0s-2s-apps-viewer.googleusercontent.com", "doc-0s-30-apps-viewer.googleusercontent.com", "doc-0s-38-docs.googleusercontent.com", "doc-0s-3g-apps-viewer.googleusercontent.com", "doc-0s-3k-apps-viewer.googleusercontent.com", "doc-0s-44-apps-viewer.googleusercontent.com", "doc-0s-44-prod-03-apps-viewer.googleusercontent.com", "doc-0s-4o-apps-viewer.googleusercontent.com", "doc-0s-4s-apps-viewer.googleusercontent.com", "doc-0s-5o-apps-viewer.googleusercontent.com", "doc-0s-6k-apps-viewer.googleusercontent.com", 
                        "doc-0s-6o-apps-viewer.googleusercontent.com", "doc-0s-6s-apps-viewer.googleusercontent.com", "doc-0s-7c-apps-viewer.googleusercontent.com", "doc-0s-80-apps-viewer.googleusercontent.com", "doc-0s-88-apps-viewer.googleusercontent.com", "doc-0s-8o-apps-viewer.googleusercontent.com", "doc-0s-8s-apps-viewer.googleusercontent.com", "doc-0s-98-apps-viewer.googleusercontent.com", "doc-0s-9c-apps-viewer.googleusercontent.com", "doc-0s-a8-apps-viewer.googleusercontent.com", "doc-0s-bo-apps-viewer.googleusercontent.com", 
                        "doc-0s-c8-apps-viewer.googleusercontent.com", "doc-10-00-apps-viewer.googleusercontent.com", "doc-10-0k-apps-viewer.googleusercontent.com", "doc-10-0o-apps-viewer.googleusercontent.com", "doc-10-0s-apps-viewer.googleusercontent.com", "doc-10-14-apps-viewer.googleusercontent.com", "doc-10-18-apps-viewer.googleusercontent.com", "doc-10-1g-prod-03-apps-viewer.googleusercontent.com", "doc-10-1k-apps-viewer.googleusercontent.com", "doc-10-20-apps-viewer.googleusercontent.com", "doc-10-24-apps-viewer.googleusercontent.com", 
                        "doc-10-2s-apps-viewer.googleusercontent.com", "doc-10-3g-apps-viewer.googleusercontent.com", "doc-10-3k-apps-viewer.googleusercontent.com", "doc-10-40-apps-viewer.googleusercontent.com", "doc-10-4s-apps-viewer.googleusercontent.com", "doc-10-50-apps-viewer.googleusercontent.com", "doc-10-5c-apps-viewer.googleusercontent.com", "doc-10-5o-apps-viewer.googleusercontent.com", "doc-10-64-apps-viewer.googleusercontent.com", "doc-10-6k-apps-viewer.googleusercontent.com", "doc-10-70-apps-viewer.googleusercontent.com", 
                        "doc-10-84-apps-viewer.googleusercontent.com", "doc-10-88-apps-viewer.googleusercontent.com", "doc-10-8c-apps-viewer.googleusercontent.com", "doc-10-8o-prod-02-apps-viewer.googleusercontent.com", "doc-10-98-apps-viewer.googleusercontent.com", "doc-10-9o-apps-viewer.googleusercontent.com", "doc-10-a8-apps-viewer.googleusercontent.com", "doc-10-as-apps-viewer.googleusercontent.com", "doc-10-b4-apps-viewer.googleusercontent.com", "doc-10-c0-apps-viewer.googleusercontent.com", "doc-10-c8-apps-viewer.googleusercontent.com", 
                        "doc-14-08-apps-viewer.googleusercontent.com", "doc-14-0k-apps-viewer.googleusercontent.com", "doc-14-0s-apps-viewer.googleusercontent.com", "doc-14-14-apps-viewer.googleusercontent.com", "doc-14-20-apps-viewer.googleusercontent.com", "doc-14-24-apps-viewer.googleusercontent.com", "doc-14-2c-apps-viewer.googleusercontent.com", "doc-14-2s-apps-viewer.googleusercontent.com", "doc-14-30-apps-viewer.googleusercontent.com", "doc-14-3c-apps-viewer.googleusercontent.com", "doc-14-3k-apps-viewer.googleusercontent.com", 
                        "doc-14-40-apps-viewer.googleusercontent.com", "doc-14-4c-apps-viewer.googleusercontent.com", "doc-14-4o-apps-viewer.googleusercontent.com", "doc-14-4o-docs.googleusercontent.com", "doc-14-4s-apps-viewer.googleusercontent.com", "doc-14-50-apps-viewer.googleusercontent.com", "doc-14-58-apps-viewer.googleusercontent.com", "doc-14-5o-apps-viewer.googleusercontent.com", "doc-14-68-apps-viewer.googleusercontent.com", "doc-14-6k-apps-viewer.googleusercontent.com", "doc-14-6s-apps-viewer.googleusercontent.com", 
                        "doc-14-7c-apps-viewer.googleusercontent.com", "doc-14-7k-apps-viewer.googleusercontent.com", "doc-14-80-apps-viewer.googleusercontent.com", "doc-14-94-apps-viewer.googleusercontent.com", "doc-14-98-apps-viewer.googleusercontent.com", "doc-14-9g-apps-viewer.googleusercontent.com", "doc-14-9o-apps-viewer.googleusercontent.com", "doc-14-a8-apps-viewer.googleusercontent.com", "doc-14-a8-docs.googleusercontent.com", "doc-14-c4-apps-viewer.googleusercontent.com")

google_video <- c("r1---sn-bg07dnk6.googlevideo.com", "r1---sn-bg07dnkd.googlevideo.com", "r1---sn-bg07dnsr.googlevideo.com", "r1---sn-bg07dnze.googlevideo.com", "r2---XXXXXXXX.googlevideo.com", "r2---sn-bg07dn6k.googlevideo.com", "r2---sn-bg0eznls.googlevideo.com", "r3---XXXXXXXX.googlevideo.com", "r3---sn-bg07dnzl.googlevideo.com", "r4---XXXXXXXX.googlevideo.com", "r4---sn-4g5e6nsy.googlevideo.com", "r4---sn-bg07dnkr.googlevideo.com", "r4---sn-h5qzen7s.googlevideo.com", "r4---sn-x1x7dn7k.googlevideo.com", "r5---sn-x1x7dn7s.googlevideo.com")

google_scholar <- c("scholar.google.com", "scholar.google.com.ar", "scholar.google.es")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% googles ~  "google.com",
    value %in% google_translate ~  "translate.google.com",
    
    value %in% google_drive ~  "drive.google.com",
    value %in% google_books ~  "books.google.com",
    value %in% google_usercontent ~"googleusercontent.com",
    value %in% google_video ~ "googlevideo.com",
    value %in% google_scholar ~"scholar.google.com",
    TRUE ~ value
  ),
  category_first = case_when(
    value %in% googles |   value %in% google_translate |  value %in% google_drive  |  value %in% google_books |
      value %in% google_books  |  value %in% google_usercontent  |  value %in% google_video |  value %in% google_scholar ~  "Google",
    T ~ category_first
  ), 
  category_second = case_when(
    value %in% googles |   value %in% google_translate |  value %in% google_drive  |  value %in% google_books |
      value %in% google_books  |  value %in% google_usercontent  |  value %in% google_video |  value %in% google_scholar ~  "Google",
    T ~ category_second
  ), 
  
  
  
  )


# Linkedin ----------------------------------------------------------------


link <- c("ar.linkedin.com", "business.linkedin.com", "co.linkedin.com", "de.linkedin.com", "e.linkedin.com", "es.linkedin.com", "linkedin.com",
          "ni.linkedin.com", "py.linkedin.com", "sv.linkedin.com")

link_rel <- c( "triunfaconlinkedin.com",  "linkedinmobileapp.com")

link_ <- c("LinkedIn", "LinkedIn Lite", "LinkedIn Recruiter")
zona_jobs <-  c("link.zonajobs.com.ar", "zonajobs.com.ar")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% link ~  "linkedin.com",
    #  value %in% link_rel ~  "sites related with linkedin.com",
    value %in% link_ ~  "LinkedIn",
    value %in% zona_jobs ~ "zonajobs.com.ar",
    TRUE ~ value
  ),
  category_first = case_when(
    value %in% link |   value %in% link_rel |  value %in% link_  ~  "LinkedIn",
    T ~ category_first
  ),
  category_second = case_when(
    value %in% link |   value %in% link_rel |  value %in% link_ | value %in%  zona_jobs ~  "Jobs",
    T ~ category_second
  )
  )



# Telegram ----------------------------------------------------------------


tel <- c("Telegram", "Telegram Messenger", "Telegram+")

tel_web <- c( "desktop.telegram.org", "org.telegram.messenger.web", "telegram.me", "telegram.org",  "web.telegram.org", "webk.telegram.org")

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% tel ~  "Telegram",
    
    value %in% tel_web ~ "web.telegram.org",
    TRUE ~ value
  ),
  category_first = case_when(
    value %in% tel |   value %in% tel_web  ~  "Telegram",
    T ~ category_first
  ),
  category_second = case_when(
    value %in% tel |   value %in% tel_web   ~  "Social Media",
    T ~ category_second
  )
  )


# WhatsApp Messenger ------------------------------------------------------

whatsapp <- c("api.whatsapp.com", "chat.whatsapp", "chat.whatsapp.com", "faq.whatsapp.com", "web.whatsapp.com", "whatsapp.com")

wht_stickers <- c("Animated Sticker For WhatsApp", "Animated Sticker Maker for WhatsApp WAStickerApps", "Animated Sticker Maker from WhatsApp WAStickerApps", "Animated Stickers Maker for WhatsApp", "Animated Stickers for WhatsApp Free (WAStickerApp)", "Argentine Characters for WhatsApp WAStickerApps", "Argentine Stickers for WhatsApp WAStickerApps", "Brazilian Memes Stickers - WhatsApp WAStickerApps", "Chat Translator for WhatsApp", "Cleaner for WhatsApp", "Create stickers for WhatsApp - StickerFactory", "HD Stickers packs for WhatsApp - WAStickersApps", 
                  "Opus Player  for WhatsApp", "Own Sticker Maker for WhatsApp. WhatsApp Stickers", "Personal stickers for WhatsApp", "Status Saver for WhatsApp - Image Video Downloader", "Sticker Make for WhatsApp", "Sticker Maker - Make Sticker for WhatsApp stickers", "Sticker Studio - Sticker Maker for WhatsApp", "Sticker.ly for WhatsApp", "Text Sticker 2021 for WhatsApp - WAStickerApps", "Video Splitter for WhatsApp Status. Instagram", "Video stickers for WhatsApp", "Wemoji - WhatsApp Sticker Maker", "What Sticker Maker-WAStickerApps-sticker WhatsApp")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% whatsapp ~  "web.whatsapp.com",
    value == "WhatsApp Business" ~ "WhatsApp Business",
    # value %in% wht_stickers ~ "WhatsApp Stickers",
    value == "WhatsApp Messenger" ~ "WhatsApp Messenger",
    TRUE ~ value
  ),
  
  category_first = case_when(
    value %in% whatsapp |   value %in% wht_stickers | value %in% c("WhatsApp Messenger", "WhatsApp Business")  ~  "WhatsApp",
    T ~ category_first
  ),
  category_second = case_when(
    value %in% whatsapp |   value %in% wht_stickers | value %in% c("WhatsApp Messenger", "WhatsApp Business") ~  "Social Media",
    T ~ category_second
  )
  
  )



# Delivery Apps -----------------------------------------------------------

pedidosya_empleos <- c("empleos.pedidosya.com"
)

pedidosya_com <- c( "envios.pedidosya.com.uy","pedidosya.cl", "pedidosya.co", 
                    "pedidosya.cok", "pedidosya.com", "pedidosya.com.ar", "pedidosya.com.do", "portal.pedidosya.com"
)

pedidosya_ <-c("PedidosYa - Comida a Domicilio",  "PedidosYa - Pedidos de Comida")

rappi_ <- c("help.partners.rappi.com", "link.hello.rappi.com.ar", "rappi.com.ar" )
rappi <-c("Rappi", "Rappi - Food & Market Delivery")
rappi_repartidos<- c("RappiAliado", "Soy Rappitendero")
ubereats <- c("Uber Eats")

uber_app <- c("Uber", "Uber Lite", "UberPartner")
uber <- c( "accounts.uber.com", "auth.uber.com", "blogapi.uber.com",  "email.uber.com", "help.uber.com", "m.uber.com", "trip.uber.com", 
           "uber.com","unsubscribe.uber.com")
cabify <- c("cabify.com", "help.cabify.com")
beat <- "Beat Driver"
taxi_app <- c("99Taxis - Taxi cab app", "APT Taxis La Plata.", "BA Taxi", "BA Taxi - Conductor", "Crazy Taxi Free", "Easy Taxi - Taxi via Mobile", "Movi Taxi", "Pide Tu Taxi", "Radio Taxi San Juan", "Tappsi - Safe Taxi", "Taxi Control", "Taxi Premium", "Taxi Puerto", "TaxiApp Aniversario", "Taxiar V3.0", "Taxibeat - Get your Taxi")
taxi_web <- c("es.shemale.taxi", "movitaxi.com.ar", "shemale.taxi", "sintaxis.org", "taxi.booking.com", "taxiarg.com", "taxipremium.com.ar", "taxisbarzola.com.ar")
pedisos_ya_repartidos <-  ("PedidosYa - Partners Pics")

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% pedidosya_empleos ~  "empleos.pedidosya.com",
    value %in% pedidosya_ ~  "PedidosYa",
    value %in% pedidosya_com ~  "pedidosya.com",
    value %in% rappi_ ~   "rappi.com.ar",
    value %in% uber ~  "uber.com",
    value %in% uber_app ~  "Uber",
    value %in% rappi ~  "Rappi",
    value %in% rappi_repartidos ~  "RappiAliado",
    value %in% ubereats ~  "Uber Eats",
    value %in% pedisos_ya_repartidos ~  "PedidosYa - Partners Pics",
    T ~ value
  ),
  category_first = case_when(
    value %in% pedidosya_empleos |  value %in% pedidosya_com | value %in% pedisos_ya_repartidos~  "PedidosYa",
    value %in% rappi | value %in% rappi_ | value %in% rappi_repartidos ~  "Rappi",
    value %in% uber | value %in%  uber_app ~  "Uber",
    value %in% ubereats  ~  "Uber Eats",
    T ~ category_first
  ),
  category_second = case_when(
    value %in% pedidosya_empleos | value %in% pedidosya_com | value %in% pedisos_ya_repartidos | value %in% rappi | 
      value %in% rappi_ | value %in% rappi_repartidos | value %in% ubereats   ~  "Delivery food",
    value %in% c("Uber", "Cabify - Your private driver") | value %in% beat | value %in%  taxi_app | value %in% taxi_web |
      value %in% c("Uber",  "uber.com") ~  "Private Driver or Taxi",
    
    T ~ category_second
  )
  )



# HomeBanking Apps ------------------------------------------------------------

patagonia_web <- c("ahorrosybeneficios.bancopatagonia.com.ar", "bancopatagonia.com.ar", "bancopatagonia.hiringroom.com", "chateaconpadi.bancopatagonia.com.ar", "ebankempresas.bancopatagonia.com.ar", "ebankpersonas.bancopatagonia.com.ar", "links.info.bancopatagonia.com.ar", "tunuevacuenta.bancopatagonia.com.ar", "turnosweb.bancopatagonia.com.ar", "tuseguroaltoque.bancopatagonia.com.ar")
patagonia <- c("Patagonia Movil")

galicia_web <-c("appsbg.bancogalicia.com.ar", "bancogalicia.com", "cobranzas.bancogalicia", "cobranzas.bancogalicia.com.ar", "cuentas.bancogalicia.com.ar", "eticagrupogalicia.lineaseticas.com", "galicia.com.ar", "galiciamove.com.ar", "hacetegalicia.bancogalicia.com.ar", "inversiones.bancogalicia.com.ar", "lacocinadefrabisa.lavozdegalicia.es", "onlinebanking.bancogalicia.com.ar", "pagosyrecargas.bancogalicia.com.ar", "prestamos.bancogalicia.com.ar", "sacatutarjeta.bancogalicia.com.ar", "t.email.galicia.ar", "tarjetas.bancogalicia.com.ar", 
                "transferencias.bancogalicia.com.ar", "turnero.bancogalicia.com.ar", "wsec06.bancogalicia.com.ar")
galicia <- c("Banco Galicia", "Banco Galicia App", "Galicia Office Token")

bbva <- c("BBVA Frances net cash | AR", "BBVA Frances | Banca Movil AR", "BBVA | Espana", "Pagos BBVA Uruguay")
bbva_web <- c("apps.bbva.com.ar", "assets.caasbbva.com", "bbva", "bbva.com", "bbva.com.ar", "bbva.csod.com", "bbva.docuprint.com", "bbva.es", "bbva.mx", "bbva.viajes.despegar.com.ar", "bbvaexperiencia.knack.com.ar", "bbvaopenmind.com", "bbvaseguros.com.ar", "buscador.bbva.com.ar", "edufin.bbva.com.ar", "go.bbva.com.ar", "netcash.bbva.com.ar", "online.bbva.com.ar", "plansueldo.bbva.com.ar", "sacatutarjeta.bbva.com.ar", "simulatucredito.bbva.com.ar", "smetrics.bbva.com.ar")

santander <- c("Mi Tarjeta Santander", "Santander RiÂ­o", "Santander RiÂ­o Empresas")
santander_web <- c("app.becas-santander.com", "autoloans.santanderautos.com.ar", "ayuda.santander.com.ar", "banco.santander.com.ar", "banco.santanderrio.com.ar", "bancosantander.es", "becas-santander.com", "cash.santander.com.ar", "emp.santander.com.ar", "empresas.santander.com.ar", "empresas3.gruposantander.es", "m1.novedades.santander.com.ar", "mya.santanderrio.com.ar", "personas.santander.com.ar", "productos.santander.com.ar", "riopi.santander.com.ar", "santander-argentina.english.britishcouncil.org", "santander.com", 
                   "santander.com.ar", "santander.com.uy", "santander.queue-it.net", "santander.wd3.myworkdayjobs.com", "santanderrio.com.ar", "santanderx.com", "shopping.santander.com.ar", "sso.becas-santander.com", "supernet.santander.com.uy", "t1.novedades.santander.com.ar", "tienda.santander.com.ar", "www2.personas.santander.com.ar")

ciudad_web <- c("bancociudad.com.ar", "hb.bancociudad.com.ar", "newsbcba-bancociudad.notifications-icommkt.com", "subastas.bancociudad.com.ar")
ciudad <- c("Banca Movil Ciudad")
credicoop_web <- c("ayudaenlinea.bancocredicoop.coop", "bancainternet.bancocredicoop.coop", "bancocredicoop.coop", "beneficios.bancocredicoop.coop", "cabal.credicoop", "credicoopmovil.coop", "encuestasenlinea.bancocredicoop.coop", "puntoscredicoop.coop")
credicoop <- c("Credicoop Movil")

provincia_web <- c("bancoprovincia.bancainternet.com.ar", "bancoprovincia.com.ar", "bipmovil.bancoprovincia.com.ar")
provincia <- c("BIP Mobile", "BIP Token")

bna_web <- c("bna.com", "bna.com.a", "bna.com.ar", "bnajubilados.tiendaexo.com", "bnanet.bna.com.ar", "especiales.tiendabna.com.ar", "eventos.bna.com.ar", "simplificateconbna.com.ar", "thumbnailsave.com", "tiendabna.com.ar", "www.bna")
bna <- "BNA+"
otros_bancos <- c("APP Banco San Juan", "APP Banco Santa Fe",  "HomeBanking Banco Formosa", "Banca Movil Banco Hipotecario","Banco Municipal APP", "Banco Provincia del Neuquen", "Banco Rioja APP", "Banco de Corrientes", "Banco del Sol", "Bancon", "Bind24-Banco Industrial - PEI", "HomeBanking Banco Formosa", "Nuevo Banco del Chaco", "i-Movil Banco Bica S.A")

bancos_webs <- c("ap01.bancoentrerios.ar", "app.bancorfondos.com.ar", "apps.bancodino.com",  
                 "banco.bradesco", "banco.ciu", "banco.n",  "banco.wilobank.com", "bancoazteca.com.mx", "bancobica.com.ar", 
                 "bancochubut.com.ar", "bancocoinag.com", "bancocolumbia.com.ar",  "bancodecorrientes.com.ar", 
                 "bancodelagente-cba-gov-ar.web.app", "bancodelapampa.com.ar", "bancodeserviciosfinancieros.com.ar", "bancodevenezuela.com",
                 "bancodino.com", "bancoentrerios.com.ar", "bancoformosa.com.ar",  "bancohipotecario.surveykiwi.com", "bancomeridian.com.ar", 
                 "bancon.bancor.com.ar","bancopiano.com.ar", "bancor.com.ar", "bancor.network", "bancorio.com.ar", 
                 "bancoroela.com.ar", "bancos.salud.gob.ar", "bancosaenz.com.ar", "bancosanjuan.com", "bancosantacruz.com",
                 "bancosantafe.com.ar", "bffpe01.bancoentrerios.ar", "clubbancor.com.ar", "digital.bancoentrerios.ar", "digital.bancosanjuan.ar", 
                 "digital.bancosantacruz.ar", 
                 "digital.bancosantafe.ar", "envio.superviellebanco.com.ar", "experienciabancor.com.ar", "grupobancolombia.com", "hb.bancoformosa.com.ar", "i-bica.bancobica.com.ar", "mailing.bancosantafe.net", "newsbcba-bancociudad.notifications-icommkt.com", "old.bancosantafe.com", 
                 "online.turnero.bancor.com.ar", "open.bancodelsol.com", "relaytrk.alertasbancor.com.ar", "secure.bancocolumbia.com.ar", "servicios.bancodelapampa.com.ar", "servicios.bancoentrerios.com.ar", "tramitesbancodelagente.cba.gov.ar", 
                 "tuseguroaltoque.bancopatagonia.com.ar", "urbancomercial.cl", "www--bancoentrerios--com--ar.insuit.net", "zonabancos.com")



data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% patagonia_web ~  "bancopatagonia.com.ar",
    value %in% patagonia ~  "Patagonia Movil",
    value %in% galicia_web ~  "bancogalicia.com",
    value %in% galicia ~   "Banco Galicia",
    value %in% bbva ~  "BBVA Frances",
    value %in% bbva_web ~  "bbva.com",
    value %in% santander ~  "Mi Tarjeta Santander",
    value %in% santander_web ~  "santander.com.ar",
    value %in% ciudad ~  "Banca Movil Ciudad",
    value %in% ciudad_web ~  "bancociudad.com.ar",
    value %in% credicoop_web ~  "bancocredicoop.coop",
    value %in% credicoop ~  "Credicoop Movil",
    value %in% provincia_web ~  "bancoprovincia.com.ar",
    value %in% provincia ~  "BIP Mobile",
    value %in% bna_web ~  "bna.com",
    value %in% bna ~  "BNA+",
    #    value %in% otros_bancos ~  "Others apps of banks ",
    #    value %in% bancos_webs ~  "Other Websites of banks",
    T ~ value
  ),
  category_first = case_when(
    value %in% patagonia_web ~  "Banco Patagonia",
    value %in% patagonia ~  "Banco Patagonia",
    value %in% galicia_web ~  "Banco Galicia",
    value %in% galicia ~   "Banco Galicia",
    value %in% bbva ~  "BBVA Frances",
    value %in% bbva_web ~  "BBVA Frances",
    value %in% santander ~  "Santander",
    value %in% santander_web ~  "Santander",
    value %in% ciudad ~  "Banca Movil Ciudad",
    value %in% ciudad_web ~  "Banca Movil Ciudad",
    value %in% credicoop_web ~  "Credicoop",
    value %in% credicoop ~  "Credicoop",
    value %in% provincia_web ~  "Banco Provincia",
    value %in% provincia ~  "Banco Provincia",
    value %in% bna_web ~  "BNA",
    value %in% bna ~  "BNA",
    value %in% otros_bancos ~  "Others banks",
    value %in% bancos_webs ~  "Others banks",
    T ~ category_first),
  category_second = case_when(
    value %in% patagonia_web | value %in%  patagonia | value %in% galicia_web | value %in% galicia |
      value %in% bbva | value %in% bbva_web | value %in% santander |
      value %in% santander_web |value %in% ciudad |value %in% ciudad_web | value %in% credicoop_web |
      value %in% credicoop | value %in% provincia_web | value %in% provincia |   value %in% provincia_web |  
      value %in% bna_web  |  value %in% bna  | value %in% otros_bancos  | value %in% bancos_webs ~  "Banking",
    T ~ category_second
  )
  )



# Newspapers --------------------------------------------------------------


lanacion <- c("api-ingresar.lanacion.com.ar", 
              "blogs.lanacion.com.ar", "club.lanacion.com.ar", "colecciones.lanacion.com.ar", "contacto.lanacion.com.ar", "edicionimpresa.lanacion.com.ar", "elecciones2021generales.lanacion.com.ar",
              "envio.fundacionlanacion.org.ar", "especialeslntools.lanacion.com.ar", "especialess3.lanacion.com.ar", "experienciasblack.lanacion.com.ar", "ingresar.lanacion.com.ar",
              "lanacion.com", "lanacion.com.ar", "lanacionar-prod-cdn.video-api.arcpublishing.com", 
              "lanacionar.video-player.arcpublishing.com", "lnmas.lanacion.com.ar", "micuenta.lanacion.com.ar", "myaccount.lanacion.com.ar", "quinielanacional1.com.ar",
              "servicios.lanacion.com.ar", "suscripciones.lanacion.com.ar")
clarin <- c("Clarin.com","365.clarin.com", "api-editoriales.clarin.com", "app.noticias.clarin.com", "bi.clarin.com", "biblioteca.clarin", "biencasero.clarin.com", "buenos-aires.guia.clarin.com", "capital-federal.guia.clarin.com", "chaco.guia.clarin.com", "clarin.com", "clarin.com.ar", "clasificados.clarin.com", "comopublicarenclarin.com.ar", "data-ecp.clarin.com", "ecp.clarin.com", "elle.clarin.com", "empleos.clarin.com", "filer.365.clarin.com", "grandt.clarin.com", "grupoclarin.com", "guia.clarin.com", "images.clarin.com", 
            "inmuebles.clarin.com", "kiosco.clarin.com", "login.grandt.clarin.com", "mendoza.guia.clarin.com", "micuenta.clarin.com", "misiones.guia.clarin.com", "mt-wzb.clarin.com", "raul.clarin.com", "recetas.clarin.com", "santa-fe.guia.clarin.com", "static.clarin.com", "suscripcion.365.clarin.com", "tiendacolecciones.clarin.com", "tucuman.guia.clarin.com", "www.clarin")
pagina <- c("aw.pagina12.com.ar", "pagina12.com.ar",  "pdf.pagina12.com.ar")
cronica <- c("diariocronica.com.ar", "cronica.com.ar", "cronica.com.mx")

ambito <- c( "elecciones.ambito.com", "media.ambito.com", "mercados.ambito.com", "usuarios.ambito.com", "ambito.com")
infobae <- c("blogs.infobae.com", "data-ecp.infobae.com", "ecp.infobae.com", "infobae.com", "infobae.com.ar", "inhouse.infobae.com", "opinion.infobae.com")
tn <- c("elections-widget.tn.com.ar", "tn.com.ar", "tn.com")
TN <- "TN"
diariopopular <- c("diariopopular.com.ar")
minuto1 <- c("media.minutouno.com", "minutouno.com", "minutouno.com.ar")
lagaceta <- c("clasificados.lagaceta.com.ar", "club.lagaceta.com", "cuenta.lagaceta.com.ar", "gaceta.es", "gacetaeronautica.com", "gacetamarinera.com.ar", "gacetamercantil.com", "inmuebles.lagaceta.com.ar", "lagaceta.com.ar", "lagacetand.com.ar", "lagacetasalta.com.ar", "publicar.lagaceta.com.ar")
elpais <- c("blogs.elpais.com", "cincodias.elpais.com", "elcomidista.elpais.com", "elpais.com", "elpais.com.co", "elpais.com.uy", "elpaisdiario.com.ar", "elpaisdigital.com.ar", "encasa.elpais.com.uy", "imagenes.elpais.com", "motor.elpais.com", "pxlctl.elpais.com", "rocanroldelpais.com", "seguro.elpais.com", "smetrics.elpais.com", "smoda.elpais.com", "viajes.elpais.com.uy")
washingtonpost <- c("subscription.washingtonpost.com", "washingtonpost.com")
newyorker <- "newyorker.com"
nytimes <- c("a.et.nytimes.com", "nytimes.com")

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% lanacion ~  "lanacion.com.ar",
    value %in% clarin ~  "clarin.com",
    value %in% pagina ~  "pagina12.com.ar",
    value %in% cronica ~  "cronica.com.ar",
    value %in% ambito ~  "ambito.com",
    value %in% infobae ~  "infobae.com",
    value %in% tn ~  "tn.com",
    value %in% TN ~  "TN",
    value %in% diariopopular ~  "diariopopular.com.ar",
    value %in% minuto1 ~  "minutouno.com",
    value %in% lagaceta ~  "gaceta.es",
    value %in% elpais ~  "elpais.com",
    value %in% washingtonpost ~  "washingtonpost.com",
    value %in% newyorker ~  "newyorker.com",
    value %in% nytimes ~  "nytimes.com",
    T ~ value
  ),
  
  category_first = case_when(
    value %in% lanacion ~  "lanacion.com.ar",
    value %in% clarin ~  "clarin.com",
    value %in% pagina ~  "pagina12.com.ar",
    value %in% cronica ~  "cronica.com.ar",
    value %in% ambito ~  "ambito.com",
    value %in% infobae ~  "infobae.com",
    value %in% tn ~  "tn.com",
    value %in% TN ~  "TN",
    value %in% diariopopular ~  "diariopopular.com.ar",
    value %in% minuto1 ~  "minutouno.com",
    value %in% lagaceta ~  "gaceta.es",
    value %in% elpais ~  "elpais.com",
    value %in% washingtonpost ~  "washingtonpost.com",
    value %in% newyorker ~  "newyorker.com",
    value %in% nytimes ~  "nytimes.com",
    T ~ category_first
  ),
  
  category_second = case_when(
    value %in% lanacion | value %in%  clarin | value %in% pagina | value %in% cronica |
      value %in% ambito | value %in% infobae | value %in% tn |
      value %in% TN |value %in% diariopopular |value %in% minuto1 | value %in% lagaceta |
      value %in% elpais | value %in% washingtonpost | value %in% newyorker |   value %in% nytimes  ~  "News",
    T ~ category_second
  )
  )


twitch <- c("api.twitch.tv", "clips.twitch.tv", "countess.twitch.tv", "dashboard.twitch.tv", "gql.twitch.tv", "help.twitch.tv", "id.twitch.tv", "m.twitch.tv", "player.twitch.tv", "pubsub-edge.twitch.tv", "twitch.tv")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% twitch ~  "twitch.tv",
    T ~ value
  ),
  
  category_first = case_when(
    value %in% twitch | value == "Twitch" ~  "Twitch",
    T ~ category_first
  ),
  
  category_second = case_when(
    value %in% twitch | value == "Twitch"  ~  "Video",
    T ~ category_second
  ),
  
  
  )



# Crypto ------------------------------------------------------------------

crypto_others <- c("ablink.news.crypto.com", "alertcrypto.eu", "ambcrypto.com", "ambcrypto.email", "americancryptoassociation.com", "app.cryptoblades.io", "app.tiendacrypto.com", "auth.crypto.com", "beincrypto.com", "crypto-ferm.fun", "crypto-strategy.club", "crypto.com", "crypto.omegapro.world", "crypto.onelink.me", "cryptoad.org", "cryptoarby.xyz", "cryptobeegame.site", "cryptoboom.cc", "cryptobrowser.site", "cryptobuyingtips.com", "cryptocars.me", "cryptocarsworld.com", "cryptocoinsad.com", "cryptocurrency10.com", 
                   "cryptodirectories.com", "cryptogeek.info", "cryptoid.com.ar", "cryptokitties.co", "cryptoloaded.com", "cryptology.com", "cryptomarket24option.com", "cryptomines.app", "cryptomkt.com", "cryptomoneyco.in", "cryptopampa.ar", "cryptoplants.club", "cryptorotator.website", "cryptosaurio.com", "cryptoshitcompra.com", "cryptosoul.io", "cryptotabbrowser.com", "cryptotabstart.com", "cryptovantage.com", "cryptowin.io", "docs-es.cryptomines.app", "docs.cryptomines.app", "es.ambcrypto.com", "es.beincrypto.com", 
                   "exchange.cryptomkt.com", "expresscrypto.io", "faucetcrypto.com", "getcryptopay.com", "getcryptoprofitz.com", "help.crypto.com", "hitcrypto.global", "legitfx-cryptotrade.com", "play.cryptomines.app", "theblockcrypto.com", "tiendacrypto.com", "whitepaper.cryptoplants.club")
crypto_app <- c("Bitso Alpha - Crypto trader Pro", "Celsius Network Ã¢Â\200Â“ Crypto Wallet", "CropBytes - Crypto Farming Game", "Crypto", "Crypto Cards - Collect and Earn", "Crypto Planet", "Crypto Treasures", "Crypto.com Wallet", "CryptoCurrency - Bitcoin Altcoin Price", "CryptoPop -  Earn Free ETH", "CryptoTab Browser", "CryptoTab Browser Pro", "CryptoTab Lite Ã¢Â\200Â” Get BTC Without Investments", "CryptoTab VPN - unlimited access to global net", "CryptoWin - Earn Real Bitcoin Free", 
                "HitBTC Ã¢Â\200Â” The Most Advanced Cryptocurrency Exchange", "Merge CatsÃ¯Â¼Â\215Collect Blockchain Crypto Tokens No Ads", "Monaco - Cryptocurrency in Every Wallet", "Nexo - Crypto Banking Account", "Nodle Cash | Earn Crypto", "Poloniex Crypto Exchange", "SafePal - Crypto wallet BTC ETH LTC BNB Tron EOS")
binance <- c("academy.binance.com", "accounts.binance.com", "accounts.binance.me", "binance.com", "binance.org", "c2c.binance.com", "freebinancecoin.com", "launchpad.binance.com", "p2p.binance.com", "pay.binance.com", "pool.binance.com", "research.binance.com", "testnet.binance.org")
Binance <-c("Binance - Cryptocurrency Exchange")
bullmarket <- c("bullmarketbrokers.com", "bullmarketbrokers.com#", "help.bullmarketbrokers.com", "inversor.bullmarketbrokers.com")
bullmarkets_brokers <- c("Bull Market Brokers")
data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% binance ~  "binance.com",
    value %in% Binance ~  "Binance - Cryptocurrency Exchange",
    value %in% bullmarket ~  "bullmarketbrokers.com",
    value %in% bullmarkets_brokers ~  "Bull Market Brokers",
    T ~ value
  ),
  category_first = case_when(
    (value %in% crypto_others | value %in%  crypto_app | value %in% binance | value %in% Binance | value %in% bullmarket |value %in% bullmarkets_brokers) ~  "Crypto",
    T ~ category_first
  ),
  
  category_second = case_when(
    ( value %in% crypto_others | value %in%  crypto_app | value %in% binance | value %in% Binance |
        value %in% bullmarket |value %in% bullmarkets_brokers ) ~  "Crypto",
    T ~ category_second
  )
  )


happn <- c("ablink.news.happnmail.com", "happn")
tinder <- c("api.gotinder.com", "help.tinder.com", "images-ssl.gotinder.com", "policies.tinder.com", "tinder.com")
grindr <- "Grindr - Gay. bi & curious guy"
grindr__ <- c("grindr.com", "neo-account.grindr.com")
bubmle <- c("bumble.com", "us1.bumble.com")
others_apps <- c("appdater - Breaking and Trending News", "sweetadults.date", "us1.chatdate.app")

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% tinder ~  "tinder.com",
    value == "Tinder" ~ "Tinder",
    value == "Bumble App" ~ "Bumble App",
    value %in% happn ~ "happn",
    value %in% grindr ~ "Grindr - Gay. bi & curious guy",
    value %in% grindr__ ~ "grindr.com",
    value %in% bubmle ~ "bumble.com",
    value %in% others_apps ~ "appdater",
    T ~ value
  ),
  category_first = case_when(
    value %in% tinder ~  "Tinder",
    value == "Tinder" ~ "Tinder",
    value == "Bumble App" ~ "Bumble",
    value %in% happn ~ "happn",
    value %in% grindr ~ "Grindr",
    value %in% grindr__ ~ "Grindr",
    value %in% bubmle ~ "Bumble",
    value %in% others_apps ~ "appdater",
    T ~ category_first
  ),
  
  category_second = case_when(
    value %in% tinder | value %in%  happn | value %in% grindr | value %in% grindr__ |
      value %in% bubmle | value %in% others_apps | value %in% c("Tinder", "Bumble App") ~  "Dating",
    T ~ category_second
  )
  )


# Dating Apps -------------------------------------------------------------



happn <- c("ablink.news.happnmail.com", "happn")
tinder <- c("api.gotinder.com", "help.tinder.com", "images-ssl.gotinder.com", "policies.tinder.com", "tinder.com")
grindr <- "Grindr - Gay. bi & curious guy"
grindr__ <- c("grindr.com", "neo-account.grindr.com")
bubmle <- c("bumble.com", "us1.bumble.com")
others_apps <- c("appdater - Breaking and Trending News", "sweetadults.date", "us1.chatdate.app")

data_all_group<- data_all_group %>%
  mutate(value = case_when(
    value %in% tinder ~  "tinder.com",
    value == "Tinder" ~ "Tinder",
    value == "Bumble App" ~ "Bumble App",
    value %in% happn ~ "happn",
    value %in% grindr ~ "Grindr - Gay. bi & curious guy",
    value %in% grindr__ ~ "grindr.com",
    value %in% bubmle ~ "bumble.com",
    value %in% others_apps ~ "appdater",
    T ~ value
  ),
  category_first = case_when(
    value %in% tinder ~  "Tinder",
    value == "Tinder" ~ "Tinder",
    value == "Bumble App" ~ "Bumble",
    value %in% happn ~ "happn",
    value %in% grindr ~ "Grindr",
    value %in% grindr__ ~ "Grindr",
    value %in% bubmle ~ "Bumble",
    value %in% others_apps ~ "appdater",
    T ~ category_first
  ),
  
  category_second = case_when(
    value %in% tinder | value %in%  happn | value %in% grindr | value %in% grindr__ |
      value %in% bubmle | value %in% others_apps | value %in% c("Tinder", "Bumble App") ~  "Dating",
    T ~ category_second
  )
  )



# googlestore_1 <- read_csv("C:/Users/User/Downloads/Part1.csv/Part1.csv")
# googlestore_2 <- read_csv("C:/Users/User/Downloads/Part2.csv/Part2.csv",     col_names = FALSE)
# googlestore_3 <- read_csv("C:/Users/User/Downloads/Part3.csv/Part3.csv",     col_names = FALSE)
# names(googlestore_2)[1:3] <- c( "App Name","App Id", "Category")
# names(googlestore_3)[1:3] <- c( "App Name","App Id", "Category")
# 
# googlestore_1 <- googlestore_1[,c(1:3)]
# googlestore_2 <- googlestore_2[,c(1:3)]
# googlestore_3 <- googlestore_3[,c(1:3)]
# 
# names(googlestore_1)
# names(googlestore_2)
# names(googlestore_3)
# 
# googlestore <- rbind(googlestore_1, googlestore_2, googlestore_3)
# games_type <- c("Adventure" , "Racing", "Puzzle", "Arcade", "Trivia", "Action", "Strategy", "Simulation", "Action", "Casino", "Card")
# 
# googlestore_filter <- googlestore %>% filter(Category %in% games_type)
# 
# googlestore_filter <- as.data.frame(googlestore_filter)
# 
# head(data_all_group)
# data_all_group <- data_all_group %>% mutate(
#   
#   category_first = case_when(
#     
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Adventure", "App Name"]  ) ~ "Adventure - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Racing", "App Name"]     ) ~ "Racing - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Puzzle", "App Name"]     ) ~ "Puzzle - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Arcade", "App Name"]     ) ~ "Arcade - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Trivia", "App Name"]     ) ~ "Trivia - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Action", "App Name"]     ) ~ "Action - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Strategy", "App Name"]   ) ~ "Strategy - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Simulation", "App Name"] ) ~ "Simulation - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Casino", "App Name"]     ) ~ "Casino - Games",
#     value %in% unique( googlestore_filter[googlestore_filter$Category == "Card", "App Name"]       ) ~ "Card - Games",
#     T ~ category_first
#     
#   ),
#   category_second = case_when(
#     value %in% unique(googlestore_filter$`App Name`) ~ "Games",
#     T ~ category_second
#   )
#   
# )
library(lubridate)


# change column name
data_all_group_target <- data_all_group %>% rename(category_first_target=category_first, category_second_target = category_second, target=value)
data_all_group <- data_all_group %>% rename(category_first_target=category_first, category_second_target = category_second, target=value)

head(data_all_group)

#delete duplicates
unique_rows <- !duplicated(data_all_group[, c("panelist_id", "datetime_first","datetime_second","source", "target",  "first_type",  "second_type","seconds_between")])
data_all_group <- data_all_group[unique_rows, ]

#calculate the time interevent
data_all_group$time_interevent<- ymd_hms(data_all_group$datetime_second)-ymd_hms(data_all_group$datetime_first)
data_all_group <- data_all_group %>% rename(rest_time=seconds_between)

# bring the data of the survey
read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)
read_wave <- distinct(read_wave,panelist_id, ideology, Edad, Genero, Provincia, QSDTrabaja, Educacion, QSDBeneficios_1, personasHogar )

#join the data from the survey and the browsing data
data_all_group <- data_all_group %>% left_join(read_wave[,c("panelist_id", "ideology", "Edad", "Genero", "Provincia", "QSDTrabaja", "Educacion", "QSDBeneficios_1", "personasHogar")])

#save file
#write.csv(data_all_group, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_argentina_new_classification.csv", row.names = FALSE)

#import file
flow_interaction_argentina_new_classification <- read.csv("C:/Users/User/Documents/GitHub/ideology_consumption_network/01_data/flow_interaction_argentina_new_classification.csv")
head(flow_interaction_argentina_new_classification)

#remove punctuation from the source and target column
remove_punctuation_except_periods <- function(text) {
  str_replace_all(text, "[[:punct:]&&[^.]]", "")
}

data_all_group <- data_all_group %>%
  mutate(source = remove_punctuation_except_periods(source),
         target = remove_punctuation_except_periods(target),
         
  )

#modify the value of "consultacivil.justiciasalta.gov.ar.gov.ar8080"
data_all_group$source <- gsub("consultacivil.justiciasalta.gov.ar8080", "consultacivil.justiciasalta.gov.ar", data_all_group$source)
data_all_group$target <- gsub("consultacivil.justiciasalta.gov.ar8080", "consultacivil.justiciasalta.gov.ar", data_all_group$target)

# export the file
write.csv(data_all_group, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_argentina_new_classification.csv", row.names = FALSE)
