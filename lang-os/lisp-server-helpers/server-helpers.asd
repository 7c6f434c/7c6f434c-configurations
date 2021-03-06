(asdf:defsystem
  :server-helpers
  :depends-on
  (
   :iolib :iolib/os :iolib/syscalls
   :uiop :iterate :cl-ppcre :bordeaux-threads :local-time alexandria
   :clsql :clsql-sqlite3
   )
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
   (:file "daemon" :depends-on ("shell" "timestamp"))
   (:file "global-sqlite")
   (:file "nix" :depends-on ("shell"))
   (:file "unix-users")
   (:file "socket-command-server" :depends-on ("safe-read" "references" "auth-data" "fbterm-requests"))
   (:file "subuser" :depends-on ("shell" "timestamp" "global-sqlite"))
   )
  )
