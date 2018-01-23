(asdf:defsystem
  :server-helpers
  :depends-on
  (:iolib :uiop :iterate :cl-ppcre :bordeaux-threads :local-time alexandria)
  :serial nil
  :components
  (
   (:file "shell")
   (:file "file-locks")
   (:file "vt" :depends-on ("shell" "file-locks"))
   (:file "references")
   (:file "fbterm-requests" :depends-on ("vt" "file-locks"))
   (:file "safe-read")
   (:file "auth-data" :depends-on ("shell"))
   )
  )
