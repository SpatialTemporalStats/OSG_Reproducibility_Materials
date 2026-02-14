#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#%% Document
# This file uses the VAE architecture tuned in file "Wind_VAE_Tuning _Parameters.py" to test the performance of VAE trained by initial data blocks of differet sizes: first month (or first year, or first nine years). The trained model is evaluated by data from the last year. 

#%% Setup
import os                                      # access to environment variables and file paths
# os.environ["TF_USE_CUDNN_AUTOTUNE"] = "0"
os.environ["KERAS_BACKEND"] = "tensorflow"     # tell keras to use tensorflow as the backend

import numpy as np
import tensorflow as tf
# Disable XLA compilation completely
# tf.config.optimizer.set_jit(False)
import keras
from keras import ops
from keras import layers                       # brings in buiding blocks
# import Data_Treatment
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd
from matplotlib.path import Path


#%% Import and preprocess the data
# Read data from 2014 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2014.nc")
lon=daT.variables["longitude"][:]   # Longitude
lat=daT.variables["latitude"][:]    # Latitude
Dat_loc=np.column_stack([np.tile(lon,len(lat)),np.repeat(lat,len(lon))]) # Locations
R=len(daT.variables["number"][:])   # Numer of ensembles
NT=len(daT.variables["time"][:])    # Number of time points in 2014
Dat_ru=daT.variables["u10"][:]      # u-component (10m)
Dat_rv=daT.variables["v10"][:]      # v-component (10m)
Dat_ru=Dat_ru.values.astype("float32")    # Tarnsfer to numpy array
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)           # Flatten the first two dimensions (time, ensemble) by iterating all ensembles at each time point
Dat_ru=np.expand_dims(Dat_ru,axis=-1)     # Add one more dimension of dimension 1 (add channel)
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Wind_ARP=np.concatenate([Dat_ru,Dat_rv],axis=3) # Put U and V variables into two channels

# Read data from 2015 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2015.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0)  

# Read data from 2016 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2016.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=np.delete(Dat_ru,np.s_[472:480],axis=0)      # (31+28)*8+1=473
Dat_rv=np.delete(Dat_rv,np.s_[472:480],axis=0)      # #(31+29)*8=480
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2017 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2017.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2018 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2018.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2019 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2019.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2020 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2020.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=np.delete(Dat_ru,np.s_[472:480],axis=0)      # (31+28)*8+1=473
Dat_rv=np.delete(Dat_rv,np.s_[472:480],axis=0)      # #(31+29)*8=480
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2021 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2021.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2022 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2022.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Read data from 2023 into Wind_ARP
daT=xr.open_dataset("Data/WindSpeeduv_ARP_2023.nc")
Dat_ru=daT.variables["u10"][:]      
Dat_rv=daT.variables["v10"][:]      
Dat_ru=Dat_ru.values.astype("float32")  
Dat_rv=Dat_rv.values.astype("float32")
Dat_ru=Dat_ru.reshape(-1,37,55)   
Dat_rv=Dat_rv.reshape(-1,37,55)
Dat_ru=np.expand_dims(Dat_ru,axis=-1)
Dat_rv=np.expand_dims(Dat_rv,axis=-1)
Dat_ru=np.concatenate([Dat_ru,Dat_rv],axis=3)
Wind_ARP=np.concatenate([Wind_ARP,Dat_ru],axis=0) 

# Mask the datasets
ARP_bd=pd.read_csv("Data/ARP_Boundary.csv")                       # Import ARP polygon boundary
ARP_polygon=list(zip(ARP_bd["x_dense"],ARP_bd["y_dense"]))        
poly_path=Path(ARP_polygon)                                       # Connect boundary points in order and form a polygon 
inside_flat=poly_path.contains_points(Dat_loc,radius=-1e-9)       # Test if points in Dat_loc are inside the path, radius=-1e-9 means we include the points on path
inside_mask_2d=inside_flat.reshape(37, 55)                        # Reshape the mask 
ARP_mask=inside_mask_2d.astype(Wind_ARP.dtype)  # Convert (TRUE,FALSE) to (1.0,0.0) 
ARP_mask=ARP_mask[np.newaxis,:,:,np.newaxis]    # Add dimensions to ARP_mask so that it can match the data tensor 
Wind_ARP=Wind_ARP*ARP_mask

# Standardize the datasets
Wind_mean=Wind_ARP.mean(axis=(0,1,2),keepdims=True)    # Compute mean over samples and space, per channel
Wind_std=Wind_ARP.std(axis=(0,1,2),keepdims=True)      # Compute  over samples and space, per channel
Wind_ARP=(Wind_ARP-Wind_mean)/Wind_std

N=Wind_ARP.shape[0]
x_train=Wind_ARP[:int(31*8)]            # set the data of the first month as the training set
# x_train=Wind_ARP[:int(0.1*N)]         # set the data of the first year as the training set
# x_train=Wind_ARP[:int(0.9*N)]         # set the data of the first nine years as the training set

#%% Construct VAE and optimizer
# Creating a sampling layer 
class Sampling(layers.Layer):           # define a custom keras layer for sampling
    """Uses (z_mean, z_log_var) to sample z, the vector encoding a digit."""

    def __init__(self, **kwargs):       # _init_: initialize the layer; **kwargs: pass any keyword argument to this layer
        super().__init__(**kwargs)      # super()._init_: set up keras layer internals
        self.seed_generator=keras.random.SeedGenerator(1337)   # fix seed for reproducibility

    def call(self, inputs):
        z_mean, z_log_var=inputs      # outputs of encoder, the \mu(x) and \log(\sigma(x)^2)
        batch=ops.shape(z_mean)[0]    # number of samples in mini-batch
        dim=ops.shape(z_mean)[1]      # dimension of latent vactor
        epsilon=keras.random.normal(shape=(batch,dim),seed=self.seed_generator) # generate epsilon
        return z_mean+ops.exp(0.5*z_log_var)*epsilon      # transfer to sampled latent vector
    

# Build the encoder
latent_dim=64                                # dimension of latent vector: 2A
encoder_inputs=keras.Input(shape=(37,55,2))   # each input is an image of size 37 * 55 * 2 
x=layers.Conv2D(32,3,activation="relu",strides=2,padding="same")(encoder_inputs)
x=layers.Conv2D(64,5,activation="relu",strides=2,padding="same")(x)
x=layers.Flatten()(x)
x=layers.Dense(1024,activation="relu")(x)
z_mean=layers.Dense(latent_dim,name="z_mean")(x)
z_log_var=layers.Dense(latent_dim,name="z_log_var")(x)
z=Sampling()([z_mean,z_log_var])              # sample latent vector z
encoder=keras.Model(encoder_inputs,[z_mean,z_log_var,z],name="encoder")  # build the encoder model
encoder.summary()


# Build the decoder
latent_inputs=keras.Input(shape=(latent_dim,))
x=layers.Dense(1024,activation="relu")(latent_inputs)
x=layers.Dense(37*55*96,activation="relu")(x)
x=layers.Reshape((37,55,96))(x)
x = layers.Conv2D(96,5,activation="relu",padding="same")(x)
decoder_outputs=layers.Conv2D(2,3,padding="same")(x)
decoder=keras.Model(latent_inputs, decoder_outputs, name="decoder")     # build the decoder
decoder.summary()


# Define the VAE as a Model with a custom train_step
class VAE(keras.Model):
    def __init__(self, encoder, decoder, **kwargs):   # pass in encoder and decoder built before
        super().__init__(**kwargs)
        self.encoder=encoder          # internal model in VAE
        self.decoder=decoder          # internel model in VAE
        self.total_loss_tracker = keras.metrics.Mean(name="total_loss")    # track total loss
        self.reconstruction_loss_tracker = keras.metrics.Mean(
            name="reconstruction_loss"
        )                                                                  # track recnstruction loss
        self.kl_loss_tracker = keras.metrics.Mean(name="kl_loss")          # track KL loss

    @property
    def metrics(self):
        return [
            self.total_loss_tracker,              # keras uses this to know which metric to reset each epoch and which metric to log
            self.reconstruction_loss_tracker,
            self.kl_loss_tracker,
        ]

    def train_step(self, data):                   # define one training update using one mini-batch, where data are of size (batch_size,28,28,1)
        with tf.GradientTape() as tape:           # record operations in this block for gradient
            z_mean, z_log_var, z = self.encoder(data)  # encode
            reconstruction = self.decoder(z)           # decode
            reconstruction_loss = ops.mean(            # calculate the reconstruction loss using "BCE"
                ops.sum(
                    ops.square(data-reconstruction),
                    axis=(1,2,3),
                )
            )
            kl_loss=-0.5*(1+z_log_var-ops.square(z_mean)-ops.exp(z_log_var)) # calculate the KL divergence
            kl_loss=ops.mean(ops.sum(kl_loss,axis=1))
            total_loss = reconstruction_loss +0.7* kl_loss
        grads=tape.gradient(total_loss,self.trainable_weights)    # calculate gradient
        self.optimizer.apply_gradients(zip(grads,self.trainable_weights)) # tell the optimizer to update each weight/parameter using its corresponding gradient
        self.total_loss_tracker.update_state(total_loss)             # keep track of the average loss across all batches so far
        self.reconstruction_loss_tracker.update_state(reconstruction_loss)
        self.kl_loss_tracker.update_state(kl_loss)
        return {
            "loss": self.total_loss_tracker.result(),
            "reconstruction_loss": self.reconstruction_loss_tracker.result(),
            "kl_loss": self.kl_loss_tracker.result(),
        }
    

#%% Train the VAE and test on the testing data
vae=VAE(encoder, decoder)           # wrap the encoder and decoder using the VAE model class defined before
vae.compile(optimizer=keras.optimizers.Adam(learning_rate=0.0001))  # choose the optimizer and compile
vae.fit(x_train,epochs=30,batch_size=32) # train the vae using only data of the first month

Ntest=int(0.1*N)
x_test=Wind_ARP[(9*Ntest):(10*Ntest)]
z_mean_test, z_log_var_test, z_test =vae.encoder.predict(x_test,verbose=0)
x_test_recon=vae.decoder.predict(z_test,verbose=0)
test_recon_loss=np.mean((x_test-x_test_recon)**2,axis=0)*Wind_std.reshape(1,1,2)*Wind_std.reshape(1,1,2)
np.save("VAE/test_recon_loss_1m.npy",test_recon_loss)
# np.save("VAE/test_recon_loss_1y.npy",test_recon_loss)
# np.save("VAE/test_recon_loss_9y.npy",test_recon_loss)
















