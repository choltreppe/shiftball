package {{ApplicationId}}

import android.app.NativeActivity

class NativeLoader: NativeActivity() {
  companion object {
    init {
      System.loadLibrary("main")
    }
  }
}