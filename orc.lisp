(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "당신은 죽었다. 게임 오버."))
  (when (monsters-dead)
    (princ "축하한다! 당신은 적을 모두 무찔렀다.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 8))

(defun show-player ()
  (format t "~&당신은 생명이 ~A, 재주가 ~A, 힘이 ~A인 용감한 기사다."
          *player-health*
          *player-agility*
          *player-strength*))

(defun player-attack ()
  (format t "~&공격형태: [1]찌르기 [2]두 번 베기 [3]회전검 :")
  (case (read)
    (1 (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (2 (let ((x (randval (truncate (/ *player-strength* 6)))))
         (format t "~&당신은 ~A 만큼의 강도로 두 번 벤다.~&" x)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
      (random-monster)
      m)))

(defun pick-monster ()
  (format t "~&공격 대상 #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
      (progn (format t "적 번호를 잘못 입력했다.~&")
             (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
          (progn (format t "그 적은 이미 죽었다.~&")
                 (pick-monster))
          m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (format t "~&적들:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (format t "~&~4D. " (incf x))
           (if (monster-dead m)
             (format t "**사망**")
             (progn (format t "(생명=~3D) " (monster-health m))
                    (monster-show m))))
         *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (format t "당신은 ~A를 죽인다!~&" (type-of m))
    (format t "당신은 ~A를 공격해 ~A 피해를 입힌다!~&" (type-of m) x)))

(defmethod monster-show (m)
  (format t "무시무시한 ~A" (type-of m)))

(defmethod monster-attack (m))

(defstruct (오크 (:include monster)) (club-level (randval 8)))

(push #'make-오크 *monster-builders*)

(defmethod monster-show ((m 오크))
  (format t "몽둥이+~A로 무장한 난폭한 오크" (오크-club-level m)))

(defmethod monster-attack ((m 오크))
  (let ((x (randval (오크-club-level m))))
    (format t "오크가 당신에게 몽둥이를 휘둘러 생명을 ~A 떨어트린다!~&" x)
    (decf *player-health* x)))

(defstruct (히드라 (:include monster)))

(push #'make-히드라 *monster-builders*)

(defmethod monster-show ((m 히드라))
  (format t "머리 ~A개 달린 잔인한 히드라" (monster-health m)))

(defmethod monster-hit ((m 히드라) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (format t "모든 머리가 잘린 히드라의 시체가 바닥에 힘없이 무너진다!~&")
    (format t "당신은 히드라 머리 ~A개를 잘라낸다!~&" x)))

(defmethod monster-attack ((m 히드라))
  (let ((x (randval (ash (monster-health m) -1))))
    (format t "히드라가 당신을 물어뜯어 생명을 ~A 떨어트린다! 그리고 머리가 하나 더 자란다!~&" x)
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (점균류 (:include monster)) (sliminess (randval 5)))

(push #'make-점균류 *monster-builders*)

(defmethod monster-show ((m 점균류))
  (format t "~A 만큼 끈적한 점균류" (점균류-sliminess m)))

(defmethod monster-attack ((m 점균류))
  (let ((x (randval (점균류-sliminess m))))
    (format t "점균류가 당신의 다리를 휘감아 당신의 재주를 ~A 떨어트린다!~&" x)
    (decf *player-agility* x)
    (when (zerop (random 2))
      (format t "그리고 당신의 얼굴에 점액을 발사해, 생명을 1 떨어트린다!~&")
      (decf *player-health*))))

(defstruct (산적 (:include monster)))

(push #'make-산적 *monster-builders*)

(defmethod monster-attack ((m 산적))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (format t "산적이 당신에게 새총을 발사해 생명을 2 떨어트린다!~&")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (format t "산적이 당신의 다리에 채찍을 감아 재주를 2 떨어트린다!~&")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (format t "산적이 당신의 팔에 채찍을 휘둘러 힘을 2 떨어트린다!~&")
           (decf *player-strength* 2)))))

