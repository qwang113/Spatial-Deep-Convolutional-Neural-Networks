install.packages("tensorflow")
install.packages("reticulate")
install.packages("keras")
library(reticulate)
library(tensorflow)
library(keras)


# # Check/Install Python
 path_to_python <- install_python("3.9.10", force = TRUE)
 virtualenv_remove("r-reticulate")
 
 
 virtualenv_create("r-reticulate", python = path_to_python)

# 
# 
# # Install Tensorflow
 install_keras(envname = "r-reticulate")



# If activate GPU, use instructions from: 
 # https://letyourmoneygrow.com/2021/02/20/howto-install-tensorflow-gpu-with-keras-in-r-a-manual-that-worked-on-2021-02-20-and-likely-will-work-in-future/ 