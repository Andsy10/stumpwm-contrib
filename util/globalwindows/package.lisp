;;;; package.lisp

(defpackage #:globalwindows
  (:use #:cl :stumpwm))

(in-package #:globalwindows)

(import '(
  stumpwm::*window-format*
  stumpwm::completing-read
  stumpwm::current-group
  stumpwm::current-screen
  stumpwm::current-window
  stumpwm::focus-all
  stumpwm::group-focus-window
  stumpwm::group-windows
  stumpwm::move-window-to-group
  stumpwm::pull-window
  stumpwm::select-window-from-menu
  stumpwm::sort-groups
  stumpwm::sort1
  stumpwm::window-group
  stumpwm::window-name
  ))
