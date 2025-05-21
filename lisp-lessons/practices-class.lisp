(defclass entity ()
  ((health :initarg :health
           :accessor health
           :initform 100)
   (name :initarg :name
         :accessor name)
   (weapons :initarg :weapons
           :accessor weapons
           :initform nil
           :type list)))


(defclass aesir (entity)
  ((health :initarg :health
           :accessor health
           :initform 42000))
  )

(defclass undead (entity)
  ((regeneration-count :initform 0
                       :accessor regeneration-count)))


(defclass zombie (undead) ())

(defclass player (entity)
  ((health :initarg :health
           :accessor health
           :initform 300)
   (name :initarg :name
         :accessor name
         :initform "Viking")
   ))

(defclass thor (aesir)
  ((weapons :initarg :weapons
            :accessor weapons
            :initform (make-instance 'mjolnir))))


(defclass spartan (entity)
  ((health :initarg :health
           :accessor health
           :initform 8000)))

(defclass troll (entity)
  ((health :initarg :health
           :accessor health
           :initform 1500)))


(defclass ghost (undead)
  ((health :initarg health
           :accessor health
           :initform 140)))


(defclass kratos (spartan) ())


(defclass weapon ()
  ((name :initarg :name
         :accessor weapon-name)
   (base-damage :initarg :base-damage
           :accessor base-damage
           :initform 10)
   (affinities :initarg :affinities
               :initform nil
               :accessor weapon-affinities)))


(defclass magical-mixin ()
  ((multiplier :initarg :multiplier
               :accessor multiplier
               :initform 1)))


(defclass holy-mixin (magical-mixin)
  ((protection :accessor protection
               :initarg :protection
               :initform 0.8)))



(defclass draugr (entity) ())

(defclass baldur (aesir) ())

(defclass heimdall (aesir) ())

(defclass axe (weapon) ())

(defclass sword (weapon) ())

(defclass spear (weapon) ())

(defclass holy-sword (holy-mixin sword) ())

(defclass epic-axe (holy-mixin axe) ())

(defclass epic-spear (holy-mixin spear) ())

(defclass magical-sword (magical-mixin sword) ())


(defclass draupnir (epic-spear)
  ((base-damage :initarg :base-damage
                :accessor base-damage
                :initform 1300)
   (protection :initarg :protection
               :accessor protection
               :initform 1.6)))


(defclass leviathan (epic-axe)
  ((base-damage :initarg :base-damage
                :accessor base-damage
                :initform 400)
   (affinities :initarg  :affinities
               :accessor weapon-affinities
               :initform '(:heimhall-frost)
               :type list)))


(defclass mjolnir (epic-axe)
  ((base-damage :initarg :base-damage
                :accessor base-damage
                :initform 2000)
   (affinities :initarg  :affinities
               :accessor weapon-affinities
               :initform :heimhall-frost)))


(defun affinity-damage (weapon)
  (let ((affinities (weapon-affinities weapon)))
    (* (base-damage weapon)
       (reduce #'*
               (mapcar (lambda (aff)
                         (case aff
                           (:ice 1.2)
                           (:fire 1.2)
                           (:poison 1.1)
                           (:bifrost 2)
                           (:heilheim-frost 10)
                           (:lightning 1.5)
                           (otherwise 1)))
                       affinities)))))


(defgeneric calculate-damage (weapon target)
  (:method ((w holy-mixin) target)
    (affinity-damage w))
  (:method ((w leviathan) target)
    (affinity-damage w)
    )
  (:method ((w holy-mixin) (target undead))
    (* 2 (affinity-damage w))
    )
  (:method ((w holy-mixin) target)
    (* 1.1 (affinity-damage w))
    )
  (:method ((w draupnir) (target heimdall))
    (affinity-damage w))
  (:method ((w weapon) (target heimdall))
    ())
  (:method ((w weapon) target)
    (affinity-damage w)))


(defgeneric take-damage (entity weapon)
  (:documentation "Apply weapon damage to entity and handle health reduction"))


(defmethod take-damage ((e entity) (w weapon))
  (let ((damage-taken (base-damage w)))
    (setf (health e) (max 0 (decf (health e) damage-taken)))
    damage-taken))


(defmethod take-damage :after ((e undead) (w weapon))
  (when (and (> (health e) 0)
             (< (health e) 300)
             (< (regeneration-count e) 4))
   (incf (regeneration-count e))
   (incf (health e) (regeneration e))))


(defmethod take-damage :after ((e spartan) (w weapon))
  (when (< (health e) 800)
   (incf (health e) (regeneration e))))


(defgeneric damage (weapon))

(defmethod damage ((weapon weapon))
  (base-damage weapon))

(defgeneric attack (entity weapon))


;; Most specific - Heimdall with Draupnir
(defmethod attack ((entity heimdall) (weapon draupnir))
  (take-damage entity weapon)
  (format t "~%~a took ~d damage from ~a!~%" entity (damage weapon) weapon))


;; Heimdall with any other weapon - keep as is since it's a special case
(defmethod attack ((entity heimdall) (weapon weapon))
  (format t "~%Your peasant weapon cannot harm mighty Heimdall!~%")
  nil)


;; Aesir with special weapons
(defmethod attack ((entity aesir) (weapon leviathan))
  (take-damage entity weapon)
  (format t "~%~a took ~d damage from ~a!~%" entity (damage weapon) weapon))


(defmethod attack ((entity aesir) (weapon mjolnir))
  (take-damage entity weapon)
  (format t "~%~a took ~d damage from ~a!~%" entity (damage weapon) weapon))


;; Base method for all successful attacks
(defmethod attack ((e entity) (w weapon))
  (take-damage e w)
  (format t "~%~a took ~d damage from ~a!~%" e (damage w) w))

#|
This is a multi-line comment
It can span several lines
And everything between #| and |# is ignored
|#


;; Keep the after method for troll transformation
(defmethod attack :after ((trol troll) (w weapon))
  (when (zerop (health trol))
    (change-class trol 'ghost)))


(defgeneric regeneration (entity)
 (:method-combination +))

(defmethod regeneration + ((entity draugr))
   20)

(defmethod regeneration + ((entity zombie))
  20)

(defmethod regeneration + ((entity ghost))
   20)

(defmethod regeneration + ((entity kratos))
  (+ ( health entity ) 150))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +affinities+
    '(:bifrost :fire :ice :lightning :poison :helheim-frost)
    :documentation "List of all possible affinities"
    :test #'equal))

(deftype affinity ()
  `(member ,@+affinities+))

(defgeneric add-affinity (object affinity-type)
  (:documentation "Add an affinity to an object"))

(defmethod add-affinity ((weapon weapon) aff-type)
  (check-type aff-type affinity)
  (pushnew aff-type (weapon-affinities weapon)))


;; call applicable classes
(defgeneric damage-type (weapon)
  (:method-combination append))

(defmethod damage-type append ((weapon axe))
  '(:cleaving))

(defmethod damage-type append ((weapon sword))
  '(:slashing))

(defmethod damage-type append ((weapon spear))
  '(:piercing))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +entities+
    '(thor draugr zombie troll ghost heimdall kratos player)
    :documentation "List of all possible entities"
    :test #'equal))



(deftype entities ()
  `(member ,@+entities+))
(defvar *random-entities*
  '(thor draugr zombie troll ghost heimdall kratos player))


(defun create-random-entity ()
  (let ((type (alexandria:random-elt +entities+)))
    (if (eql type 'thor)
        (make-instance 'thor :weapons (make-instance 'mjolnir))
        (make-instance type
                       :weapons (create-random-weapon)))))


(defvar *random-weapons*
  '(leviathan sword axe spear holy-sword epic-axe epic-spear magical-sword
    draupnir mjolnir))


(defun random-affinities ()
  (let ((n (random (1+ (length +affinities+)))))
    (subseq (alexandria:shuffle (copy-list +affinities+)) 0 n)))


(defun create-random-weapon ()
  (let* ((weapon-type (alexandria:random-elt *random-weapons*)))
    (make-instance weapon-type :affinities (random-affinities))))


(defun player-dead? (player)
  (<= (health player) 0))

(defun enemy-dead? (enemy)
  (<= (health enemy) 0))


(defun make-special-attack (user target)
  (setf (base-damage (weapons user)) (calculate-damage (weapons user) target)))


(defun reset-slot-to-initform (instance slot-name)
  (let* ((class (class-of instance))
         (slot (find (alexandria:ensure-symbol slot-name)
                     (closer-mop:class-slots class)
                     :key #'closer-mop:slot-definition-name)))
    (alexandria:when-let (initform (and slot
                               (closer-mop:slot-definition-initform slot)))
      (setf (slot-value instance slot-name) initform))))


(defun regular-attack (user target)
  (let ((weapon-of-user (weapons user)))
    (reset-slot-to-initform weapon-of-user 'base-damage)
    (attack target weapon-of-user)))


(defun process-player-turn (player enemy)
  (let ((attack-special (read-char nil)))
    (if (char= #\Q attack-special)
        (progn
          (make-special-attack player enemy)
          (attack enemy (weapons player)))
        (regular-attack player enemy))))


(defun process-enemy-turn (enemy player)
  ;; difficulty level could be added in the future by simple increasing roll on enemy-turn
  (let ((roll (random 100)))
    (cond
      ((< roll 10) (make-special-attack enemy player))
      (t (regular-attack enemy player)))))


(defmethod print-object ((entity entity) stream)
  (print-unreadable-object (entity stream :type nil :identity nil)
    (format stream "~a" (type-of entity))))


(defmethod print-object ((weapon weapon) stream)
  (print-unreadable-object (weapon stream :type nil :identity nil)
    (format stream "~a" (type-of weapon))))


;; Add a compact formatting helper
(defun print-battle-status (player enemy)
  (format t "~%Status: ~a(HP:~d) vs ~a(HP:~d)~%"
          (type-of player) (health player)
          (type-of enemy) (health enemy)))


(defun enemy-turn-delay ()
  (sleep 2))


(defun game-loop (player enemy)
  (loop until (or (enemy-dead? enemy)
                  (player-dead? player))
         do (print-battle-status player enemy)
         do (progn
              (format t "Your turn! (Press Q for special attack, any other key for regular attack)~%")
              (process-player-turn player enemy)
              (when (enemy-dead? enemy)
                (print-battle-status player enemy)
                (return-from game-loop :player-wins))

              (format t "~%Enemy's turn!~%")
              (enemy-turn-delay)
              (process-enemy-turn enemy player)
              (when (player-dead? player)
                (print-battle-status player enemy)
                (return-from game-loop :enemy-wins)))
        finally (return :game-over)))


(defun simulate-battle ()
  (let* ((player (create-random-entity))
         (enemy (create-random-entity)))
    (format t "~%A new battle begins!~%")
    (format t "You are a ~a~%" (type-of player))
    (format t "Your opponent is a ~a~%" (type-of enemy))
    (case (game-loop player enemy)
      (:player-wins
       (format t "~%Congratulations! You have vanquished ~a!~%" (type-of enemy)))
      (:enemy-wins
       (format t "~%You have been slain by ~a! Your journey ends here.~%" (type-of enemy)))
      (:game-over
       (format t "~%The battle has ended in a stalemate.~%")))))
