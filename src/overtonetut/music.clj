(ns overtonetut.music
  (:use [overtone.core]
        [overtone.inst.sampled-piano]))

;; (load "music")
;; (in-ns 'overtonetut.music)
;; to refresh - (require 'overtonetut.music :reload)


; Converts pitch to hertz
(defn note->hz [music-note]
	(midi->hz (note music-note)))


; Plays chord that takes frequencies as parameters
(defn hz-chord
  ([instrument root chord-name]
    (doseq [note (chord root chord-name)]
      (instrument (note->hz note))))
  ([instrument root chord-name inv]
    (doseq [note (chord root chord-name inv)]
      (instrument (note->hz note)))))


(defn piano-chord
  ([root chord-name]
    (doseq [note (chord root chord-name)]
      (sampled-piano note 0.5)))
  ([root chord-name inv]
    (doseq [note (chord root chord-name inv)]
      (sampled-piano note 0.5))))

; (def scale-degrees [:i :ii :iii :iv :v :vi :vii])
    
; (def pitches (degrees->pitches scale-degrees :pentatonic :C4))
; (def pitches (scale :c3 :pentatonic))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)


(def cymbal (sample (freesound-path 13254)))


(defn string
  [freq duration]
  (with-overloaded-ugens
    (* (line:kr 1 1 duration FREE)
       (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
              1 1 (/ 1 freq) (* duration 2) 0.25))))


(definst harpsichord [freq 440]
  (let [duration 1
        snd  (string freq duration)
        t1   (* 0.2 (string (* 2/1 freq) duration))
        t2   (* 0.15 (string (* 3/2 freq) duration))
        t3   (* 0.1 (string (* 4/3 freq) duration))
        t4   (* 0.1 (string (* 5/4 freq) duration))
        snd  (+ snd (mix [t1 t2 t3 t4]))]
    snd))


(definst hi-hat1 [duration 0.1]
  (pan2 (* (env-gen (perc 0 duration :curve -9)) (white-noise))))


; hi pass filter, multiply by 2
(definst hi-hat2 [duration 0.2]
  (pan2 (* 2 (env-gen (perc 0 duration :curve -9)) (hpf (white-noise) 9000))))


(defn play-pattern [cur-t sep-t seq sound]
  (at cur-t (when (first seq) (apply sound (first seq))))
  (let [new-t (+ cur-t sep-t)]
    (apply-by new-t #'play-pattern [new-t sep-t (rest seq) sound])))


; (play-pattern (now) 200 (cycle [[] nil [] nil [] nil [0.5] nil]) hi-hat2)
; (play-pattern (now) 200 (cycle [[] nil nil nil [] nil [0.5] nil]) hi-hat2)


; SEQUENCES

(def hi-hat1-seq [nil [] nil [] nil [] nil []])

(def hi-hat2-seq [nil nil [] nil nil nil [] nil])

(def kick-seq [[] nil [] nil [] nil [] nil])

(def piano-seq [
                [:c3 :major] nil nil nil [:c3 :7] nil nil nil
                [:f3 :major] nil nil nil [:f3 :m6] nil nil nil
                [:c3 :major] nil nil nil [:c3 :7] nil nil nil
                [:f3 :major] nil nil nil [:f3 :m6] nil nil nil
                [:c3 :major] nil nil nil [:a2 :minor] nil nil nil
                [:d3 :minor] nil nil nil [:g2 :7] nil nil nil
                [:c3 :major] nil nil nil [:a2 :minor7] nil nil nil
                [:d3 :minor] nil nil nil [:g2 :7] nil nil nil
                ])

(def melody-seq [
                [(note :e4)] [(note :f4)] [(note :g4)] nil [(note :g4)] nil nil [(note :g4)]
                [(note :a4)] [(note :b4)] [(note :c5)] nil [(note :c5)] nil nil [(note :c5)]
                [(note :e4)] [(note :f4)] [(note :g4)] nil [(note :g4)] nil [(note :g4)] nil
                [(note :a4)] [(note :g4)] [(note :f4)] nil [(note :f4)] nil nil nil
                [(note :e4)] nil [(note :g4)] nil [(note :c4)] nil [(note :e4)] nil
                [(note :d4)] nil [(note :f4)] nil nil nil [(note :b3)] nil
                [(note :c4)] nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil [(note :g4)]
])

(def cymbal-seq [
                nil nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil nil
                nil nil nil nil nil nil nil nil
                nil nil nil nil [] nil nil nil
                nil nil nil nil nil nil nil nil
])


(defn play-all [sep-t patterns]
  (let [t (+ (now) 200)]
    (doseq [[sound pattern] patterns]
      (play-pattern t sep-t pattern sound))))


(defn play-music [time]
  (play-all time {
            kick (cycle kick-seq)
            hi-hat1 (cycle hi-hat1-seq)
            ; hi-hat2 (cycle hi-hat2-seq)
            piano-chord (cycle piano-seq)
            sampled-piano (cycle melody-seq)
            cymbal (cycle cymbal-seq)
            }))