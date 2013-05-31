;;; kzkn-android.el --- android-mode settings

(require 'android-mode)
(when (getenv "ANDROID_SDK_HOME")
  (setq android-mode-sdk-dir (expand-file-name (getenv "ANDROID_SDK_HOME"))))

(provide 'kzkn-android)
