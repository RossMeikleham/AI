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
