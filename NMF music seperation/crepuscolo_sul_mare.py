#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec  8 01:21:36 2024

@author: theahellen
"""

import librosa
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import NMF
import soundfile as sf

#set the basis
n_components = 10

#Load the audio file
audio_file = '/Users/theahellen/Documents/Uni/Year 4/Statistical Machine Learning/NMF/NMF music seperation/crepuscolo_sul_mare_chunk.wav'
y, sr = librosa.load(audio_file, sr=None)  # y = audio signal, sr = sample rate

#Perform Short-Time Fourier Transform (STFT)
n_fft = 2048  # Length of each window for FFT
hop_length = 512  # Number of samples between successive frames
D = librosa.stft(y, n_fft=n_fft, hop_length=hop_length)

#Convert the complex values to magnitude (amplitude)
magnitude, _ = librosa.magphase(D)

#Convert the magnitude to decibels (dB)
DB = librosa.amplitude_to_db(magnitude, ref=np.max)

#Knowing that the max of DB is 0 and the min is -80, convert to non-negative data
DB = DB+80

#Factorise using NMF
model = NMF(n_components, init='random', solver='mu', beta_loss='frobenius')
W = model.fit_transform(DB)
H = model.components_

#%% PLOTS

#Plot the spectrogram (frequency over time)
plt.figure(figsize=(10, 6))
librosa.display.specshow(DB, sr=sr, x_axis='time', y_axis='log')
plt.colorbar(format='%+2.0f dB')
plt.xlabel('Time (s)')
plt.ylabel('Frequency (Hz)')
#plt.title('Spectrogram of Audio')
plt.show()

print("Spectrogram shape:", DB.shape)

print("W matrix:\n", W)
print("H matrix:\n", H)

print("W shape:", W.shape)

#Plot each basis in W
freq_bins = np.linspace(0, sr / 2, W.shape[0])  #Map frequency bins to Hz
colors = ['maroon', 'red', 'orange', 'gold', 'yellowgreen', 'green', 'teal', 'blue', 'indigo', 'purple']  #Colors for each basis
labels = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10']  #Labels for the basis components
x_positions = np.arange(n_components)  #Horizontal positions for each basis

#Plot the basis spectra
plt.figure(figsize=(8, 6))
for i in range(n_components):
    plt.plot(x_positions[i] + W[:, i] / np.max(W[:, i]) * 0.3, freq_bins / 1000,  #Scale and shift each basis
             color=colors[i], linewidth=1.5, label=labels[i])

#Adjust the plot appearance
plt.xticks(x_positions, labels, ha='center')  #Set custom x-axis labels
plt.tick_params(axis='x', which='both', bottom=False, top=False)  #Remove x-axis ticks
plt.ylabel('Frequency (kHz)')
plt.xlabel('Basis')
#plt.title('Basis Matrix (W)')
plt.ylim([0, 3])  #Limit y-axis to 3 kHz as in the image
plt.xlim([-0.5, n_components - 0.5])  #Add padding around bases
plt.tight_layout()
plt.show()

#Plot the  H horizontally
plt.figure(figsize=(10, 6))

time = np.arange(H.shape[1])  #Time indices

#Convert time frames to seconds
time_in_seconds = np.arange(H.shape[1]) * hop_length / sr

y_positions = np.arange(n_components)  #Vertical positions for each basis

for i in range(n_components):
    #Plot the zero line for each basis
    plt.axhline(y=y_positions[i], color='gray', linestyle='--', linewidth=0.8, alpha=0.7)
    
    #Plot each row of H horizontally, shifted vertically
    plt.plot(time_in_seconds, y_positions[i] + H[i, :] / np.max(H[i, :]) * 0.5,  #Scale and shift activations
             color=colors[i], linewidth=1.5, label=labels[i])
    
'''
#Plot the  H horizontally
plt.figure(figsize=(25, 5))

time = np.arange(H.shape[1])  #Time indices

#Convert time frames to seconds
time_in_seconds = np.arange(H.shape[1]) * hop_length / sr

y_positions = np.arange(n_components)  #Vertical positions for each basis

for i in range(n_components):
    #Plot the zero line for each basis
    plt.axhline(y=y_positions[i], color='gray', linestyle='--', linewidth=0.8, alpha=0.7)
    
    #Plot each row of H horizontally, shifted vertically
    plt.plot(time_in_seconds, H[i, :] / np.max(H[i, :]) * 0.5,  #Scale and shift activations
             color=colors[i], linewidth=1.5, label=labels[i])
'''

#Adjust plot appearance
plt.yticks(y_positions, labels)
plt.xlabel('Time (s)')
plt.ylabel('Basis')
#plt.title('Activations of Basis Components (H)')
plt.xlim([0, np.max(time_in_seconds)])
plt.ylim([0-0.5, n_components-0.25])  #Add padding around components
plt.grid(True, axis='x', linestyle='--', alpha=0.5)  #Add vertical grid lines for clarity
plt.tight_layout()
plt.show()

#Reconstruct the spectrogram from W and H
reconstructed_DB = np.dot(W, H)  # Multiply W and H
#Convert back to decibels for comparison
reconstructed_DB_db = librosa.amplitude_to_db(reconstructed_DB, ref=np.max)
reconstructed_DB_db = reconstructed_DB_db +80
#Plot the reconstructed spectrogram
plt.figure(figsize=(10, 6))
librosa.display.specshow(reconstructed_DB_db, sr=sr, hop_length=hop_length, x_axis='time', y_axis='log')
plt.colorbar(format='%+2.0f dB')
#plt.title('Reconstructed Spectrogram (WH)')
plt.xlabel('Time (s)')
plt.ylabel('Frequency (Hz)')
plt.tight_layout()
plt.show()

#%% RECONSTRUCT BY EYE

#Create a copy of H
Hc = np.copy(H)
#Set - by eye - some basis to be 0


#Plot the  H horizontally
plt.figure(figsize=(10, 6))

time = np.arange(Hc.shape[1])  #Time indices

#Convert time frames to seconds
time_in_seconds = np.arange(Hc.shape[1]) * hop_length / sr

y_positions = np.arange(n_components)  #Vertical positions for each basis

for i in range(n_components):
    #Plot the zero line for each basis
    plt.axhline(y=y_positions[i], color='gray', linestyle='--', linewidth=0.8, alpha=0.7)
    
    #Plot each row of H horizontally, shifted vertically
    plt.plot(time_in_seconds, y_positions[i] + Hc[i, :] / np.max(Hc[i, :]) * 0.5,  #Scale and shift activations
             color=colors[i], linewidth=1.5, label=labels[i])

#Adjust plot appearance
plt.yticks(y_positions, labels)
plt.xlabel('Time (s)')
plt.ylabel('Basis')
#plt.title('Activations of Basis Components (H)')
plt.xlim([0, np.max(time_in_seconds)])
plt.ylim([-0.5, n_components - 0.5])  #Add padding around components
plt.grid(True, axis='x', linestyle='--', alpha=0.5)  #Add vertical grid lines for clarity
plt.tight_layout()
plt.show()

#Reconstruct the spectrogram from W and H
reconstructed_DB = np.dot(W, Hc)  # Multiply W and H
#Convert back to decibels for comparison
reconstructed_DB_db = librosa.amplitude_to_db(reconstructed_DB, ref=np.max)
reconstructed_DB_db = reconstructed_DB_db +80
#Plot the reconstructed spectrogram
plt.figure(figsize=(10, 6))
librosa.display.specshow(reconstructed_DB_db, sr=sr, hop_length=hop_length, x_axis='time', y_axis='log')
plt.colorbar(format='%+2.0f dB')
#plt.title('Reconstructed Spectrogram (WH)')
plt.xlabel('Time (s)')
plt.ylabel('Frequency (Hz)')
plt.tight_layout()
plt.show()

#Reconstruct the magnitude spectrogram
reconstructed_complex = np.dot(W, Hc)

#Perform the inverse STFT to get the waveform
reconstructed_waveform = librosa.istft(reconstructed_complex, hop_length=hop_length)

#Save reconstructed waveform to a .wav file
output_file = 'reconstructed_audio_4.wav'
sf.write(output_file, reconstructed_waveform, sr)
print(f"Reconstructed audio saved to: {output_file}")


#%% END OF PLOTS

#Reconstruct the magnitude spectrogram
reconstructed_complex = np.dot(W, H)

#Perform the inverse STFT to get the waveform
reconstructed_waveform = librosa.istft(reconstructed_complex, hop_length=hop_length)

#Save reconstructed waveform to a .wav file
output_file = 'reconstructed_audio_full_nmf.wav'
sf.write(output_file, reconstructed_waveform, sr)
print(f"Reconstructed audio saved to: {output_file}")


