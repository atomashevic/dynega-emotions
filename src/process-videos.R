### setup the transforEmotion package

install_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  } else {
    message(paste0(pkg, " is already installed"))
  }
}

install_if_not_installed("transforEmotion")

library(transforEmotion)

setup_miniconda()

### load the urls

urls <- read.csv("data/videos-faces-selection.csv")


### define emotion labels

emotions <- c("excitement",
              "happiness",
              "pride",
              "anger",
              "fear",
              "sadness",
              "neutral")

n <-  nrow(urls) - 1 # ids start at 0

### go through all urls, process videos
### and save the results as csv files

for (i in 0:n){
  print(paste("Processing video", i))
  video <- urls[i, ]
  print(video$url)
  if (video$Face == 0) {
    result <- try({
      if (video$crop == "no") {
        video_scores(video$url, classes = emotions,
                     nframes = 300, save_video = FALSE,
                     save_frames = TRUE, video_name =
                       paste("video", i, sep = "-"),
                     uniform = TRUE)
      } else {
        st <- video$start
        en <- video$end
        video_scores(video$url, classes = emotions,
                     nframes = 30, save_video = FALSE,
                     save_frames = TRUE, video_name =
                       paste("video", i, sep = "-"),
                     start = st, end = en, uniform = TRUE)
      }
    }, silent = TRUE)
    if (class(result) == "try-error") {
      print(paste("Error occurred at video", i, ": ", result))
    } else {
      write.csv(result,
                file = paste("data/revision/video", i, "300.csv", sep = "-"))
    }
  }
}