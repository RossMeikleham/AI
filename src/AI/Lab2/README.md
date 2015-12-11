#Lab 2

Processes files in the `samples` folder, 50 files contain samples of
audio with speech and the other 50 files contain sample of audio
with no speech. Generates  gaussian discriminant functions with k-fold validation
to be able to predict whether a sample contains speech or not.

##Required
- Python 3.x
- MatPlotLib and Numpy libraries for Python
- GHC 4.6 or newer

##Building
- Ensure the `AI` package from the root of the repository is installed
- Run `build.sh` to generate the executable

##Running
To generate signal data for plotting, run the Haskell
program as follows:

```
./Lab2
```

This should return an average loss between 0 and 1 (1 being nothing was predicted correctly,
and 0 being that everything was predicted correctly). Also produces csv files for
the log of the average Magnitude, Energy and the log of the Zero Crossing Rate over
all the samples.


Then plot the data using the python program:

```
python plotting.py

``` 

#Laboratory Exercises

The `samples` folder contains 100 audio files, each of which has a duration of 300ms, extracted from telephone conversations. 
For each of those 100 audio files (in .wav format), the zip file contains one corresponding file in the same .dat format 
as the previous exercise.

The files were extracted from segments of speech and silence, and are named accordingly. 
The zip file contains 50 files for the "Silence" class and 50 files for the "Speech" class.

The goals of the exercise are as follows:

- extract features summarizing each audio file
- use those features to build a classifier able to automatically discriminate between the "Silence" and "Speech" classes
- assess the performance of the classification approach
- In order to achieve those goals, the tasks to be performed are as follows:


- For each of the 100 signals, compute and store in a text file the following three feature values (that we will denote by 'E', 'M', and 'Z'):
  - 'E' - Logarithm of the average value of the short-term energy signal
  - 'M' - Logarithm of the average value of the short-term magnitude signal
  - 'Z' - Average value of the Zero Crossing Rate signal
- Produce three scatter plots, showing all the audio signals, using the following three combinations of features:
  - 'E' versus 'M'
  - 'E' versus 'Z'
  - 'M' versus 'Z'
- Repeat the following steps in a cross-validation fashion, using K-fold validation with K=10:
  - Split the data set into training and test sets keeping the training set balanced, namely with half "Speech" and half "Silence" audio samples
  - Train Gaussian discriminant functions for decisions "Speech" and "Silence"
  - Compute the average loss over the test set
  - Compute the average loss of the K-fold validation procedure
