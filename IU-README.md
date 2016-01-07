# How to do the thing

```
git clone -b nofib-testing --recursive https://github.com/iu-parfunc/ghc
cd nofib/
make clean
make boot
make 2&>1 | tee nofib-log
```
