(in-package :keepit-egg-hunt)
;;
;; this has to be a deep dive into a lisp world. a magical world where i experience the in and outs of the language and greet the ones came before me. i am walking in their shadows.
;; well there has to be something about the user i am running right?

;; 27 jan. happy birthday, berkay. known as @berukaimerito. may this challange will serve you on your path to become a 10x engineer.

;;(defun they-call-it-lisp (list)
  ;;(format t "show me your car and show me your cdr ~a . ~a~%" (car list) (cdr list)))
;;
;;(defun recursion1 (value)
  ;;(format t "entering eggcursion~(~a~)~\%" value)
  ;;(if (< value 5 )
      ;;(recursion1 (1+ value))))
;;
;;;; here i am trying desperately to find an egg
;;(defun make-omlette (butter bread pepper egg)
  ;;(list :butter butter :bread bread :pepper pepper :egg egg))
;;
;;(defvar *table* nil)
;;
;;(defun add-plate (omlette) (push omlette *table*))
;;
;;(defun open-file (path)
  ;;(with-open-file (stream path)
    ;;(loop for line = (read-line stream nil 'foo)
          ;;until (eq line 'foo)
          ;;do (print line))))

(defvar *rot13-translate-table*
  (let ((str (make-string 127 :initial-element #\null)))
    (dotimes (i 127)
      (setf (elt str i) (code-char i)))
    (dotimes (i 26)
      (setf (elt str (+ i #1=#.(char-code #\a)))
            (code-char (+ (mod (+ i 13) 26) #1#)))
      (setf (elt str (+ i #2=#.(char-code #\a)))
            (code-char (+ (mod (+ i 13) 26) #2#))))
    str)
  "string table for rot13 translation.")

(defun rot13-char (char)
  (handler-case
      (elt *rot13-translate-table* (char-int char))
    (error (e) char)))

(defun rot13-string (string)
  (map 'string #'rot13-char string))

;;lambda-list: (&rest args &key high low go come wind-direction
              ;;(mamaaaaaaaa :ooo-oo-oo-oooooohhhh))
;;derived type: (function
               ;;(&rest t &key
                ;;(:high
                 ;;(or (single-float #1=(0.0)) (double-float #2=(0.0d0))
                     ;;(rational #3=(0))))
                ;;(:low
                 ;;(or (single-float #1#) (double-float #2#)
                     ;;(rational #3#)))
                ;;(:go (member . #4=(:hard :medium :easy)))
                ;;(:come (member . #4#)) (:wind-direction t)
                ;;(:mamaaaaaaaa t))
               ;;(values simple-string &optional))
;;documentation:
  ;;is this the real life? is this just fantasy?
;;source file: /home/mhr/egg-hunt.lisp

(print(third (macroexpand-1 '*poof-and-its-gone*)))

(defun my-favorite-egg-function ()
 (let ((s1 "s") (s2 "o") (s3 "m") (s4 "e") (s5 "t")
       (s6 "i") (s7 "m") (s8 "e") (s9 "s") (s10 "y")
       (s11 "o") (s12 "u") (s13 "m") (s14 "u") (s15 "s")
       (s16 "t") (s17 "c") (s18 "h") (s19 "a") (s20 "n")
       (s21 "g") (s22 "e") (s23 "t") (s24 "h") (s25 "e")
       (s26 "s") (s27 "o") (s28 "u") (s29 "r") (s30 "c")
       (s31 "e") (s32 "c") (s33 "o") (s34 "d") (s35 "e"))
   (concatenate 'string
               s1 s2 s3 s4 s5
               s6 s7 s8 s9
               s10 s11 s12
               s13 s14 s15 s16
               s17 s18 s19 s20 s21 s22
               s23 s24 s25
               s26 s27 s28 s29 s30 s31
               s32 s33 s34 s35)))

(defun lexicographical-str (str)
  (let ((the-str (copy-seq str)))
    (stable-sort the-str #'string)))

(remove-method #'initialize-instance
               (find-method #'initialize-instance '(:before)
                           (list (find-class 'egg-vault-key))))

;;(defun check-slots (class-name)
 ;;(mapcar #'slot-definition-name
         ;;(class-direct-slots (class-of (make-instance class-name)))))
(defun reference-to-gta ()
  (loop repeat 5 do
        (verify (no-easter-eggs-here))))


(defvar *golden-key* :golden-key)

(defmethod locked-and-unopenable (&key key)
  (funcall #'locked-and-unopenable :key key))

(defun unlock-it ()
 (apply #'locked-and-unopenable :allow-other-keys t '(:key :very-detailed)))

(defun print-key ()
  (format t "~a" :key)
  :golden-key)

(defun wrap-keys (&key game eggs)
  (apply #'locked-and-unopenable
         (list  :game game
                :eggs eggs)))

;; (make-package 'temporary :nicknames '("temp" "temp")) =>  #<package "temporary">
(call-with-temporary-packegge (lambda () (format t "~a~%" *package*)))

;; CALL WITH EGG CONTEXT
;; basic local dynamic binding
;; global dynamic
;; progressive dynamic


(defvar *context* "EGG")

(defun eggtext ()
 (let ((context 'special-context))
   (format t "here it is ~a~%" context)))

(defun local-macro-binding (var-name fn)
  (symbol-macrolet ((var-name nil))
    (funcall fn)))

(defun debugger ()
  (break))


(defparameter *dynamic-var* nil) ; special variable for dynamic binding

(defun multiple-binding-dispatch (fn)
  ;; first binding: lexical
  (let ((lexical-binding nil))
    ;; second binding: using special binding
    (let ((*dynamic-var* nil))
      (declare (special *dynamic-var*))
      ;; third binding: using symbol macrolet
      (symbol-macrolet ((macro-binding nil))
        (let ((result (funcall fn)))
          (case result
            (:lexical lexical-binding)
            (:dynamic *dynamic-var*)
            (:macro macro-binding)))))))

;; POST-ORDER-EGGS
(defun postorder (tree)
 (reverse (format nil "~{~A~}" (funcall #'flatten tree)) ))

(defun flatten (list)
  (nreverse (%flatten list '())))

(defun %flatten (list accumulator)
  ;; base case if the list is empty, return the accumulator
  (if (null list)
      accumulator
      ;; handle first element
      (if (atom list)
          ;; if it is atomic push it to the accumulator and pass the xdr of the list
          (cons list accumulator)
          ;; if a cons cell, flatten it and process rest
          (%flatten (cdr list) (%flatten (car list) accumulator)))))


(defvar *ALL-SOLVED* NIL)

(defun egeneric-func ()
  (let* ((methods (loop for method in (closer-mop:generic-function-methods #'eggeneric-function)
                       collect (method-qualifiers method)))
        ;; TODO
        (names '()))
    ))


(defun sort-qualifiers ()
  (let ((sorted-methods (list "HEN" "CLUCKING" "ROOSTER" "CHICKEN" "COOP")))
    (sort sorted-methods #'string<)))

(defun concatenate-methods ()
  (reduce (lambda (x y) (concatenate 'string x y))
          (sort-qualifiers)))


(defun a-funcall (function)
  (let ((*eyes-wide-shut* nil))
    (makunbound '*eyes-wide-shut*)
    (funcall function)))

;;("LOCALLYUNBOUNDVARIABLESAREAREALTHING" "CHICKENCLUCKINGCOOPHENROOSTER"
 ;;"JUSTSWITCHCARANDCDRAROUND" "YOUAREGETTINGGOODATLOCKPICKING"
 ;;"THISONEISSIMPLEGOGRABSOMETHINGHARDER" "THESPRINGISTRULYINFULLFORCE"
 ;;"YOUCANEXPECTEGGSINALLSORTSOFPLACES" "THISONEISAFREEBIEDONTCHATHINK"
 ;;"MACROEXPANDINGTHINGSCANBEVERYUSEFULYOUKNOW"
 ;;"LETFREDDIEMERCURYLIVEFOREVERINOURHEARTS" "THEYWEREDOINGHIGHRADIXMATHINLISP"
 ;;"SYMBOLSCANPRETENDTOBEVARIABLESBUTBEMACROSINSTEAD"
 ;;"WHYDIDYOUINTERRUPTMEIWASINTHEMIDDLEOFSOMETHING"
 ;;"SOMETIMESYOUMUSTCHANGETHESOURCECODE"
 ;;"RUNNINGUNTRUSTEDSOFTWAREONYOURMACHINEISNOTAGOODIDEA"
 ;;"THISEGGHUNTISBECOMINGWEIRDERBYTHESECOND" "THEREISNOVAULTKEYDONOTLOOKFORIT"
 ;;"ITSIMPOSSIBLEHOWDIDYOUDOTHAT" "THEVAULTISFULLOFEGGSBUTYOUMAYONLYHAVEONE"
 ;;"PLEASEDONOTBREAKPRINTREADCONSISTENCY" "YOUARELUCKYDISASSEMBLYWORKSONTHISONE"
 ;;"GOTTAESTABLISHTHEPROPERCATCHFORTHAT" "HYPNOTICVIBESARENOTGOODFORYOU!"
 ;;"BUTALSOPLEASELOOKINTOCLASSDEFINITIONSYOUKNOW"
 ;;"THINKOUTSIDETHEBOXLOOKOUTSIDETHEPACKAGE" "YOUWOULDNTDOWNLOADANOSTRICHEGG"
 ;;"SOMETIMESDOINGTHESAMETHINGOVERANDOVERISNOTMADNESS"
 ;;"FIXINGBUGSGIVESYOULOTSOFSTREETCRED" "ATRUEHACKERMANBYSPIRITAREYOUNOT"
 ;;"PACKAGENAMESCANBEEGGSTOOYOUKNOW" "YOUCANPEEKINTOTHEMACHINEASITISRUNNING"
 ;;"ANDSYMBOLNAMESCANMAKEVERYGOODEGGSASWELL" "WHATWASFIRSTTHEEGGORTHECHICKEN")
