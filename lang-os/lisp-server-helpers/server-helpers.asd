(asdf:defsystem
  :server-helpers
  :depends-on
  (:iolib :uiop :iterate :cl-ppcre :bordeaux-threads :local-time alexandria
          :clsql :clsql-sqlite3)
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
   (:file "timestamp")
   (:file "network" :depends-on ("shell"))
   (:file "socket-command-server" :depends-on ("safe-read" "references"))
   (:file "daemon" :depends-on ("shell" "timestamp"))
   (:file "global-sqlite")
   )
  )
