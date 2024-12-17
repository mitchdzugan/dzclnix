(defsystem "rename-me"
  :build-operation :program-op
  :class :package-inferred-system
  :depends-on ((:version "uiop" "3.3.4")
               "rename-me/main")
  :entry-point "RENAME-ME/MAIN:MAIN")
