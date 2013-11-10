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
      {:gain {:val 1.0 :fscale 0.15 :ascale 0.0}
       :delay {:val 0.0 :fscale 0.0 :ascale 0.0}
       :attack {:val 0.5 :fscale 0.5 :ascale -0.75}
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

(defn scaler [n pbuf aratio fratio]
  (with-overloaded-ugens
    (* (+ 1.0 (index:kr pbuf n))
       (+ 1.0 (* aratio (index:kr pbuf (+ 1 n))))
       (+ 1.0 (* fratio (index:kr pbuf (+ 2 n)))))))

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
                (vector? level) (dorun (map-indexed (fn [i v]
                                                      (let [off (nth offset i)]
                                                        (blit v off)))
                                                    level))
                (number? level) (aset-double arr offset level)))]
    (blit patch offsets)
    (buffer-write! buf arr)))

(patch->buf patch b)

(defn bget
  ([bid k] (index:kr bid (-> offsets (get k))))
  ([bid k1 k2] (index:kr bid (-> offsets (get k1) (get k2))))
  ([bid k1 k2 k3] (index:kr bid (-> offsets (get k1) (get k2) (get k3)))))

(defn scaled [aval scale ratio]
  (with-overloaded-ugens
    (* aval (exp (* scale ratio)))))

(defn master-scaled [aval bid k aratio fratio]
  (with-overloaded-ugens
    (* aval
       (exp (* (bget bid :master-env k :ascale) aratio))
       (exp (* (bget bid :master-env k :fscale) fratio)))))

(defn hscaled [aval bid h k]
  (with-overloaded-ugens
    (* aval (pow 2 (lin-lin (bget bid :harmonics k h) -1.0 1.0 -3.0 3.0)))))

(defn harm [h bid freq gate aratio fratio delay attack decay sustain fade release]
  (with-overloaded-ugens
    (let [amp (/ (scaled
                  (scaled (bget bid :harmonics :gain h) (bget bid :harmonics :ascale h) aratio)
                  (bget bid :harmonics :ascale h) fratio)
                 (+ 1 h))
          delay (+ delay (bget bid :harmonics :delay h))
          attack (hscaled attack bid h :attack)
          decay (hscaled decay bid h :decay)
          sustain (hscaled sustain bid h :sustain)
          fade (hscaled fade bid h :fade)
          release (hscaled release bid h :release)
          ectl (envelope [0 0 1.0 sustain 0 0]
                         [delay attack decay fade release]
                         [0
                          (bget bid :master-curves :attack)
                          (bget bid :master-curves :decay)
                          (bget bid :master-curves :fade)
                          (bget bid :master-curves :release)]
                         4)
          env (env-gen:kr ectl gate)
          hfreq (* (+ 1 h) freq)]
      (* (bget bid :harmonics :toggle h)
         amp env
         (sin-osc hfreq)
         ;;anti-alias
         (select:kr (< hfreq 20000) [0.0 1.0])))))

(definst harmonikit
  [bid (buffer-id b)
   freq 220
   amp 0.5
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
    (* gain
       (+
        (harm 0 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 1 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 2 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 3 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 4 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 5 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 6 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 7 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 8 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 9 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 10 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 11 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 12 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 13 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 14 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 15 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 16 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 17 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 18 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 19 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 20 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 21 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 22 bid freq gate aratio fratio delay attack decay sustain fade release)
        (harm 23 bid freq gate aratio fratio delay attack decay sustain fade release)))))

(harmonikit (buffer-id b) 440)
(stop)
