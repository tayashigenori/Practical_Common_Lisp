
(defun hello-world ()
  (format t "Hello, world!"))

; CL-USER> (load "c:/Users/staya/Documents/GitHub/Practical_Common_Lisp/02_REPL_Tour/2_6_save_files.lisp")
; #P"c:/Users/staya/Documents/GitHub/Practical_Common_Lisp/02_REPL_Tour/2_6_save_files.lisp"
; CL-USER> (hello-world)
; Hello, world!
; NIL
; CL-USER> (load (compile-file "c:/Users/staya/Documents/GitHub/Practical_Common_Lisp/02_REPL_Tour/2_6_save_files.lisp"))
; #P"c:/Users/staya/Documents/GitHub/Practical_Common_Lisp/02_REPL_Tour/2_6_save_files.wx32fsl"
