;;; kzkn-android.el --- android-mode settings

(require 'android-mode)
(setq android-mode-sdk-dir (expand-file-name (getenv "ANDROID_SDK_HOME")))

(provide 'kzkn-android)
