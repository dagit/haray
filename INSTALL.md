Install
-------

At the moment, we require an unreleased version of
[Juicy.Pixels](https://github.com/Twinside/Juicy.Pixels/).  Assuming you use
`cabal-dev`, you can do the following to install the dependencies:

```
git clone https://github.com/dagit/haray.git
git clone https://github.com/Twinside/Juicy.Pixels.git
cd haray
cabal-dev install ../Juicy.Pixels
cabal-dev install
```

If you use plain `cabal`:

```
git clone https://github.com/dagit/haray.git
git clone https://github.com/Twinside/Juicy.Pixels.git
cd Juicy.Pixels
cabal install
cd ../haray
cabal install
```

