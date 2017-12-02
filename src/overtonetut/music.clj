(ns overtonetut.music
  (:use [overtone.core]
        [overtone.inst.sampled-piano]))
;;tested in Overtone 0.6-dev
;;add [overtone.sc.machinery.defcgen] to ns :use clause for 0.5.0

; (boot-external-server)

;;http://computermusicresource.com/Simple.bell.tutorial.html
(def dull-partials
  [
   0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])

;; http://www.soundonsound.com/sos/Aug02/articles/synthsecrets0802.asp
;; (fig 8)
(def partials
  [
   0.5
   1
   3
   4.2
   5.4
   6.8])

;; we make a bell by combining a set of sine waves at the given
;; proportions of the frequency. Technically not really partials
;; as for the 'pretty bell' I stuck mainly with harmonics.
;; Each partial is mixed down proportional to its number - so 1 is
;; louder than 6. Higher partials are also supposed to attenuate
;; quicker but setting the release didn't appear to do much.

(defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (env-gen (perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))


(definst dull-bell [freq 220 dur 1.0 vol 1.0]
  (let [snd (* vol (bell-partials freq dur dull-partials))]
    (detect-silence snd :action FREE)
    snd))

(definst pretty-bell [freq 220 dur 1.0 vol 1.0]
  (let [snd (* vol (bell-partials freq dur partials))]
    (detect-silence snd :action FREE)
    snd))

;; TUNE - Troika from Lieutenant Kije by Sergei Prokofiev
;; AKA the Sleigh song
;; AKA that tune they play in most Christmas adverts

(def bell-metro  (metronome 500))

;; Two lines - the i-v loop that sort of sounds right
;; and the melody. _ indidcates a rest, we don't have to worry
;; about durations as this is percussion!
(def kije-troika-intervals
  (let [_ nil]
    [[ :i++ :v++ ]
     [_     _    _     _    _     _   _   _
      _     _    _     _    _     _  :v   _
      :i+  :vii  :vi  :vii  :i+   _  :vi  _
      :v    _     :vi  _   :iii   _  :v   _
      :vi  :v     :iv  _   :i+   _   :vii :i+
      :v   _      _    _   _     _   :iv  :iii
      :ii  _      :vi  _  :v     _   :iv  _   :v :iv
      :iii :iv    :v   _  :i+   :vi :iv  _   :iii  :iv :v _ :v _ :i ]]))

;; Playing in C major
(def troika-hz
  "Map all nested kije troika intervals to hz using the major scale with root C5"
  (let [scale [:major :C5]]
    (letfn [(intervals->hz [intervals]
              (map #(when % (midi->hz %)) (apply degrees->pitches intervals scale)))]
      (map intervals->hz kije-troika-intervals))))

;; Plays the tune endlessly
(defn play-bells
  "Recursion through time over an sequence of infinite sequences of hz notes
  (or nils representing rests) to play with the pretty bell at the specific
  time indicated by the metronome"
  [beat notes]
  (let [next-beat     (inc beat)
        notes-to-play (remove nil? (map first notes))]
    (at (bell-metro beat)
        (dorun
         (map #(pretty-bell % :vol 0.5) notes-to-play)))
    (apply-at (bell-metro next-beat) #'play-bells [next-beat (map rest notes)])))

;; Start the bells ringing...
(defn runner
  "Start up the play-bells recursion with a repeating troika melody and baseline"
  []
  (play-bells (bell-metro) (map cycle troika-hz)))

;; (pretty-bell 440) ;; sounds a bit woodblock
;; (pretty-bell 2000 7.00) ;; diiiiiiiiinnng
;; (dull-bell 600 5.0) ;;  ddddddonnnngg
;; (runner) ;; happy xmas
;; (stop)

;; cd kausta
;; jooksuta (load "music")
;; (in-ns 'overtonetut.music)
;; refreshimiseks (require 'overtonetut.music :reload)
(defn piano-chord
  ([root chord-name]
    (doseq [note (chord root chord-name)]
      (sampled-piano note)))
  ([root chord-name inv]
    (doseq [note (chord root chord-name inv)]
      (sampled-piano note))))

(defn play-harmony []
  (let [time (now)]
    (at         time  (piano-chord :c3 :major7))
    (at (+ 2000 time) (piano-chord :d3 :minor7))
    (at (+ 4000 time) (piano-chord :e3 :minor7))
    (at (+ 6000 time) (piano-chord :f3 :major7))))


(defn bossa-harmony []
  (let [time (now)]
    (at time (piano-chord :d3 :m9))
    (at (+ 2000 time) (piano-chord :a3 :m6))
    (at (+ 4000 time) (piano-chord :e3 :m9))
    (at (+ 6000 time) (piano-chord :b3 :m6))))


(def scale-degrees [:i :ii :iii :iv :v :vi :vii])
    
(def pitches (degrees->pitches scale-degrees :pentatonic :C4))
; (def pitches (scale :c3 :pentatonic))

(defn play [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (sampled-piano note))
    (let [next-time (+ time sep)]
      (apply-at next-time play [next-time (rest notes) sep])))))

(defn play-notes [time notes sep]
  (at time (sampled-piano (first notes))))


(def metro (metronome 160))

;; We can use recursion to keep playing the chord progression
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (piano-chord :C4 :major))
  (at (m (+ 4 beat-num)) (piano-chord :G3 :major))
  (at (m (+ 8 beat-num)) (piano-chord :A3 :minor))
  (at (m (+ 12 beat-num)) (piano-chord :F3 :major))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) [])
)
; (chord-progression-beat metro (metro))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

;(c-hat)

(definst hi-hat1 [strum 0.1]
  (pan2 (* (env-gen (perc 0 strum :curve -9)) (white-noise))))


; hi pass filter, multiply by 2
(definst hi-hat2 [strum 0.2]
  (pan2 (* 2 (env-gen (perc 0 strum :curve -9)) (hpf (white-noise) 9000))))

(defn play-pattern [cur-t sep-t seq sound]
  (at cur-t (when (first seq) (apply sound (first seq))))
  (let [new-t (+ cur-t sep-t)]
    (apply-by new-t #'play-pattern [new-t sep-t (rest seq) sound])))

; (play-pattern (now) 200 (cycle [[] nil [] nil [] nil [0.5] nil]) hi-hat2)
; (play-pattern (now) 200 (cycle [[] nil nil nil [] nil [0.5] nil]) hi-hat2)

(def hi-hat-seq [nil [] nil [] nil [] nil []])
(def kick-seq [[] nil [] nil [] nil [] nil])
; (def piano-seq [[60] [62] [65] [67] [69] [72] [74] [77]])
(def piano-seq [[:c3 :major7] nil nil nil [:c3 :m7] nil nil nil])

(defn play-all [sep-t patterns]
  (let [t (+ (now) 200)]
    (doseq [[sound pattern] patterns]
      (play-pattern t sep-t pattern sound))))

(play-all 250 {kick (cycle kick-seq)
           hi-hat2 (cycle hi-hat-seq)
           piano-chord (cycle piano-seq)})