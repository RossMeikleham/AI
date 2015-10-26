#Lab 1

Processes a file containing a list of sound samples taken over
300ms, and generates graphs based on the data.

##Required
- Python 3.x
- MatPlotLib and Numpy libraries for Python
- GHC 4.6 or newer

##Building
either execute build.sh or manually from the command line enter:
````
cabal configure
cabal install --only-dependencies
cabal build
```

##Running
To generate signal data for plotting, run the Haskell
program as follows:

```
dist/build/lab1/lab1
```

Then plot the data using the python program:

```
python plotting.py
``` 
#Laboratory Exercises

The data to be used in the laboratory is available in the following file "laboratory.dat"

The file contains the samples extracted from the first 300 ms of the following audio file "audiosample.wav"

The tasks to be performed are as follows:
- Determine the Sampling Rate (the samples in the file account for 300 ms of speech)
- Apply the ideal delay operator with delay 5, 10 and 15 ms
- Apply the moving average with k1=k2=5,10 and 15 ms
- Convolve the signal with a window of length 10 ms
- Extract the short-term energy signal from the signal in "laboratory.dat" (window length 30 ms)
- Extract the short-term magnitude signal from the signal in "laboratory.dat" (window length 30 ms)
- Extract the short-term Zero Crossing Rate (ZCR) signal from the signal in "laboratory.dat" (window length 30 ms)
- Plot the original signal, the energy signal, the magnitude signal and the ZCR signal as a function of time


You are free to use any programming language and any program to plot the signals.
Note that it might be a good idea to scale the numbers in the .dat file in order to avoid overflow issues.



#Graphs

##Original Signals
![signals](images/samples.png?raw=true)

##Extracting Data From Signals
![data](images/functions.png?raw=true)

##Ideal Delay Applied to original Signals
![idealDelay](images/idealDelay.png?raw=true)

##Moving Averages Applied to original Signals
![movingAverage](images/movingAverage.png?raw=true)

##Convolution of 10ms window applied to original Signals
![signals](images/convolution.png?raw=true)

