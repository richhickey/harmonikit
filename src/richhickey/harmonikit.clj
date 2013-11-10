(ns richhickey.harmonikit)

;; aargh with the using
;; temporary until we can find where everything lives (not easy)
(use 'overtone.live)

;; a dummy patch that will be used to calc offsets and mappings
(def patch
     {:harmonikit/rev 1
      :name "Patch 42"

      :master {:toggle 1.0}
      :master-curves {:attack 0.5 :decay 0.5 :fade 0.75 :release 0.0}
      :high-harmonics {:toggle 1.0 :taper 0.0}
      :freq-envelope {:toggle 0.0 :init 0.0 :rate 0.1 :freq 0.0 :return 0.1
                      :freq-fscale 0.0 :rate-fscale 0.0 :freq-ascale 0.0 :rate-ascale 0.0}
      :lfo {:toggle 1.0 :rate 0.1 :fscale 0 :ramp 0.2 :amp-mod 0 :freq-mod 0 :depth 0}
      
      :master-env
      {:gain {:val 1.0 :fscale 0.0 :ascale 0.0}
       :delay {:val 0.0 :fscale 0.0 :ascale 0.0}
       :attack {:val 0.5 :fscale 0.0 :ascale 0.0}
       :decay {:val 0.5 :fscale 0.0 :ascale 0.0}
       :sustain {:val 0.25 :fscale 0.0 :ascale 0.0}
       :fade {:val 0.5 :fscale 0.0 :ascale 0.0}
       :release {:val 0.5 :fscale 0.0 :ascale 0.0}}
      
      :harmonics
      {:gain [0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75
              0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75]
       :delay [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :attack [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :decay [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :sustain [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :fade [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :release [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :toggle [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0]
       :ascale [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
       :fscale [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]}
      
      :resonances
      {:toggle [0.0 0.0 0.0 0.0]
       :freq [0.1 0.1 0.1 0.1]
       :width [0.1 0.1 0.1 0.1]
       :gain [0.0 0.0 0.0 0.0]}})

(defn patch-offsets [offset level]
  (cond
   (map? level) (reduce-kv (fn [[o ret] k v]
                             (let [[o nv] (patch-offsets o v)]
                               [o (assoc ret k nv)]))
                           [offset {}] level)
   (vector? level) (reduce (fn [[o ret] v]
                             (let [[o nv] (patch-offsets o v)]
                               [o (conj ret nv)]))
                           [offset []] level)
   :else [(inc offset) offset]))

(let [[nparams offsets] (patch-offsets 0 patch)]
  (def offsets offsets)
  (def nparams nparams))

(def BASE_HARMS 24)
(def NHARMS 64)

;;todo - make buffers part of synth objects to support multitimbral
(def b (buffer nparams))

(buffer-id b)
;(buffer-set! b 0 0.02)
;(buffer-set! b 3 1)
;(buffer-set! b 6 0.5)
;(buffer-set! b 9 0.5)
;(buffer-set! b 12 2)
;(buffer-set! b 15 0.1)
;(buffer-set! b 18 0.5)

(defn scaler [n pbuf aratio fratio]
  (with-overloaded-ugens
    (* (+ 1.0 (index:kr pbuf n))
       (+ 1.0 (* aratio (index:kr pbuf (+ 1 n))))
       (+ 1.0 (* fratio (index:kr pbuf (+ 2 n)))))))

(defn harm [h freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel]
  (with-overloaded-ugens
    (let [base (* 24 h)
          scale (/ 1.0 h)
          hr1 (* r1 (scaler (+ base 0) pbuf aratio fratio))
          hl1 (* l1 (scaler (+ base 3) pbuf aratio fratio))
          hr2 (* r2 (scaler (+ base 6) pbuf aratio fratio))
          hl2 (* l2 (scaler (+ base 9) pbuf aratio fratio))
          hr3 (* r3 (scaler (+ base 12) pbuf aratio fratio))
          hl3 (* l3 (scaler (+ base 15) pbuf aratio fratio))
          hrel (* rel (scaler (+ base 18) pbuf aratio fratio))
          hectl (envelope [0 hl1 hl2 hl3 0] [hr1 hr2 hr3 hrel] :linear 3)
          henv (env-gen:kr hectl gate)]
      (* henv scale (sin-osc (* h freq))))))

(defn ugen-deps
  "Returns a set of the deps (arguments) of this ugen that are themselves
  upstream ugens."
  [ug]
  (set (#'overtone.sc.synth/ugen-children ug)))

(defn topological-sort-ugens-x
  "Sort into a vector where each node in the directed graph of ugens
  will always be preceded by its upstream dependencies.
  Depth first, from:

  http://en.wikipedia.org/wiki/Topological_sorting,

  following the advice here:

  http://supercollider.svn.sourceforge.net/viewvc/supercollider/trunk/common/build/Help/ServerArchitecture/Synth-Definition-File-Format.html

  'For greatest efficiency:

  Unit generators should be listed in an order that permits efficient
  reuse of connection buffers, which means that a depth first
  topological sort of the graph is preferable to breadth first.'"
  [ugens]
  (let [visit (fn visit [[ret visited path :as acc] ug]
                (cond
                 (visited ug) acc
                 (path ug) (throw (Exception. "ugen graph contains cycle"))
                 :else
                 (let [[ret visited path :as acc]
                       (reduce visit [ret visited (conj path ug)] (ugen-deps ug))]
                   [(conj ret ug) (conj visited ug) path])))]
    (first (reduce visit [[] #{} #{}] ugens))))

(alter-var-root #'overtone.sc.synth/topological-sort-ugens (constantly topological-sort-ugens-x))

(defn load-synthdef-x
  "Synchronously load an Overtone synth definition onto the audio
  server. The synthdef is also stored so that it can be re-loaded if the
  server gets rebooted. If the server is currently not running, the
  synthdef loading is delayed until the server has succesfully
  connected."
  [sdef]
  (assert (overtone.sc.machinery.synthdef/synthdef? sdef))
  (dosync (alter overtone.sc.machinery.synthdef/loaded-synthdefs* assoc (:name sdef) sdef))

  (when (server-connected?)
    (let [bytes (overtone.sc.machinery.synthdef/synthdef-bytes sdef)]
      (overtone.sc.machinery.server.comms/with-server-sync
        (if (< (count bytes) (- overtone.osc.util/BUFFER-SIZE 4))
          #(snd "/d_recv" bytes)
          (let [path (str (System/getProperty "java.io.tmpdir")  (-> (:name sdef) symbol name) ".scsyndef")]
            (overtone.sc.machinery.synthdef/synthdef-write sdef path)
            #(snd "/d_load" path)))
        (str "whilst loading synthdef " (:name sdef))))))

(alter-var-root #'overtone.sc.machinery.synthdef/load-synthdef (constantly load-synthdef-x))

(defn patch->buf [patch buf]
  (let [arr (double-array nparams)
        blit (fn blit [level offset]
               (cond
                (map? level) (reduce-kv (fn [_ k v]
                                          (blit v (offset k)))
                                        nil level)
                (vector? level) (map-indexed (fn [i v]
                                               (let [off (nth offset i)]
                                                 (aset arr off v)))
                                             level)
                (number? level) (aset-double arr offset level)))]
    (blit patch offsets)
    (buffer-write! buf arr)))

(patch->buf patch b)

(defn bget
  ([bid k] (index:kr bid (-> offsets k)))
  ([bid k1 k2] (index:kr bid (-> offsets k1 k2)))
  ([bid k1 k2 k3] (index:kr bid (-> offsets k1 k2 k3))))

(defn scaled [aval scale ratio]
  (with-overloaded-ugens
    (* aval (exp (* scale ratio)))))

(defn master-scaled [aval bid k aratio fratio]
  (with-overloaded-ugens
    (* aval
       (exp (* (bget bid :master-env k :ascale) aratio))
       (exp (* (bget bid :master-env k :fscale) fratio)))))

(definst harmonikit
  [bid (buffer-id b)
   freq 220
   amp 0.75
   gate 1.0]
  (let [abase 0.125
        fbase 220
        aratio (log (/ amp abase))
        fratio (log (/ freq fbase))
        gain (master-scaled (bget bid :master-env :gain :val) bid :gain aratio fratio)
        delay (bget bid :master-env :delay :val)
        attack (master-scaled (lin-lin (bget bid :master-env :attack :val) 0.0 1.0 0.0 4.0)
                              bid :attack aratio fratio)
        decay (master-scaled (lin-lin (bget bid :master-env :decay :val) 0.0 1.0 0.0 4.0)
                             bid :decay aratio fratio)
        sustain (master-scaled (bget bid :master-env :sustain :val) bid :sustain aratio fratio)
        fade (master-scaled (lin-lin (bget bid :master-env :fade :val) 0.0 1.0 0.0 20.0)
                            bid :fade aratio fratio)
        release (master-scaled (lin-lin (bget bid :master-env :release :val) 0.0 1.0 0.0 4.0)
                               bid :release aratio fratio)
        ectl (envelope [0 0 1.0 sustain 0 0]
                       [delay attack decay fade release]
                       [0
                        (bget bid :master-curves :attack)
                        (bget bid :master-curves :decay)
                        (bget bid :master-curves :fade)
                        (bget bid :master-curves :release)]
                       4)
              env (env-gen:kr ectl gate amp :action FREE)]
    (* gain env (sin-osc freq))))


(time (definst harmonikit
        [pbuf (buffer-id b)
         freq 220
         amp 0.75
         gate 1.0
         gain 1.0
         freq-lfo 0
         amp-lfo 0]
        (let [fbase 220
              abase 0.25
              aratio (/ (- amp abase) abase)
              fratio (/ (- freq fbase) fbase)
              r1 (scaled 0 pbuf aratio fratio)
              l1 (* amp (scaled 3 pbuf aratio fratio))
              r2 (scaled 6 pbuf aratio fratio)
              l2 (* l1 (scaled 9 pbuf aratio fratio))
              r3 (scaled 12 pbuf aratio fratio)
              l3 (* l1 (scaled 15 pbuf aratio fratio))
              rel (scaled 18 pbuf aratio fratio)
              ectl (envelope [0 l1 l2 l3 0] [r1 r2 r3 rel] :linear 3)
              env (env-gen:kr ectl gate amp :action FREE)
              mixed (* gain
             (+
              (harm 1 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 2 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 3 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 4 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 5 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 6 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 7 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 8 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 9 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 10 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 11 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 12 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 13 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 14 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 15 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 16 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 17 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 18 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 19 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 20 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 21 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 22 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 23 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 24 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 25 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 26 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 27 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 28 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 29 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 30 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 31 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 32 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 33 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 34 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 35 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 36 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 37 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 38 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 39 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 40 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 41 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 42 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 43 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 44 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 45 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 46 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 47 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 48 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 49 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 50 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 51 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 52 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 53 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 54 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 55 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 56 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 57 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 58 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 59 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 60 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 61 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 62 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 63 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 64 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ))
              rez (resonz mixed 750 0.01)]
          (+ mixed (* 4 rez)))))

(time (definst harmonikit
        [pbuf 0 ;(buffer-id b)
         freq 220
         amp 0.75
         gate 1.0
         gain 1.0
         freq-lfo 0
         amp-lfo 0]
        (let [fbase 220
              abase 0.25
              aratio (/ (- amp abase) abase)
              fratio (/ (- freq fbase) fbase)
              r1 (scaled 0 pbuf aratio fratio)
              l1 (* amp (scaled 3 pbuf aratio fratio))
              r2 (scaled 6 pbuf aratio fratio)
              l2 (* l1 (scaled 9 pbuf aratio fratio))
              r3 (scaled 12 pbuf aratio fratio)
              l3 (* l1 (scaled 15 pbuf aratio fratio))
              rel (scaled 18 pbuf aratio fratio)
              ectl (envelope [0 l1 l2 l3 0] [r1 r2 r3 rel] :linear 3)
              env (env-gen:kr ectl gate amp :action FREE)
              ]
          (* gain
             (+
              (harm 1 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 2 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 3 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 4 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 5 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 6 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 7 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 8 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 9 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 10 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 11 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 12 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 13 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 14 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 15 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 16 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 17 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 18 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 19 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 20 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 21 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 22 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 23 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 24 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 25 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 26 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 27 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 28 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 29 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 30 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 31 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              ;(harm 32 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 33 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 34 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 35 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 36 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 37 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 38 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 39 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 40 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 41 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 42 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 43 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 44 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 45 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 46 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 47 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 48 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 49 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 50 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 51 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 52 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 53 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 54 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 55 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 56 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 57 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 58 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 59 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 60 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 61 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 62 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 63 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              (harm 64 freq gate pbuf aratio fratio r1 l1 r2 l2 r3 l3 rel)
              )))))

(harmonikit (buffer-id b) 440)
(stop)