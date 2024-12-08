#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec  8 02:35:55 2024

@author: theahellen
"""

import librosa
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import NMF

#Set the basis
n_components = 6

#Load the audio file
audio_file = '/Users/theahellen/Documents/Uni/Year 4/Statistical Machine Learning/NMF/NMF music seperation/crepuscolo_sul_mare_chunk.wav'
y, sr = librosa.load(audio_file, sr=None)  # y = audio signal, sr = sample rate

#Perform Short-Time Fourier Transform (STFT)
n_fft = 2048  #Length of each window for FFT
hop_length = 512  #Number of samples between successive frames
D = librosa.stft(y, n_fft=n_fft, hop_length=hop_length)

#Convert the complex values to magnitude (amplitude)
magnitude, _ = librosa.magphase(D)

#Convert the magnitude to decibels (dB)
DB = librosa.amplitude_to_db(magnitude, ref=np.max)

#Knowing that the max of DB is 0 and the min is -80, convert to non-negative data
DB = np.abs(DB)

#Plot the spectrogram (frequency over time)
plt.figure(figsize=(10, 6))
librosa.display.specshow(DB, sr=sr, x_axis='time', y_axis='log')
plt.colorbar(format='%+2.0f dB')
plt.title('Spectrogram of Audio')
plt.show()

print("Spectrogram shape:", DB.shape)

#Define a range for the number of components
component_range = range(1, 100)  #Test 1 to 100 ranks
errors = []  #List to store errors

#Train NMF for each number of components and calculate error
for n_components in component_range:
    model = NMF(n_components=n_components, init='random', random_state=0)
    W = model.fit_transform(DB)
    H = model.components_
    reconstruction = np.dot(W, H)  #Reconstructed matrix
    error = np.linalg.norm(DB - reconstruction, 'fro')  #Frobenius norm of the error
    errors.append(np.sqrt(error**2/1025)) #RMSE


#Plot the RMSE
plt.figure(figsize=(8, 5))
plt.plot(component_range, errors, marker='o', linestyle='-')
plt.axvline(x=10, color='grey', linestyle='--')
plt.xlabel('Rank')
plt.ylabel('RMSE')
plt.title('Optimal Rank for NMF')
plt.grid()
plt.show()


