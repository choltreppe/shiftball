# Shift Ball

A little mobile puzzle game made with rylib in nim.

![](https://raw.githubusercontent.com/choltreppe/shiftball/master/screenshots/playing.jpg) ![](https://raw.githubusercontent.com/choltreppe/shiftball/master/screenshots/hole.jpg)

You can **install it from playstore**: https://play.google.com/store/apps/details?id=foo.chol.shiftball

or
## build from source

**setup**:
```bash
nimble convertSvgs
nimble buildFirstBoard
```

### build desktop test version
```bash
nimble build
```

### build android version
- get java and android sdk/ndk 
- make sure `ANDROID_HOME` `ANDROID_NDK` enviorment variables are set
```bash
nimble exportIcon
nimble setupAndroid
nimble buildAndroid
```
- connect device
```bash
nimble deploy
```