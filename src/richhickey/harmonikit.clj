;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns richhickey.harmonikit
  (:require [overtone.osc :as osc]
            [overtone.osc.util :as osc-util]
            [overtone.sc.buffer :as buf]
            [overtone.sc.server :as srv]
            [overtone.sc.ugens :as ugen]
            [overtone.sc.machinery.synthdef :as sdef]
            [overtone.sc.machinery.server.comms :as com]
            [overtone.studio.inst :as inst]
            [overtone.sc.envelope :as env]
            [overtone.sc.cgens.line :as lin]
            [overtone.sc.node :as node]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            ;; Needed to boot Overtone (Temporary)
            [overtone.studio.mixer :as omix]))

;; Boot the Overtone Server for live demo
(defonce __AUTO-BOOT__
  (when (srv/server-disconnected?)
    (omix/boot-server-and-mixer)))

;; a dummy patch that will be used to calc offsets and mappings
(def patch
     {:harmonikit/rev 1
      :name "Patch 42"

      :master {:toggle 1.0}
      :master-curves {:attack 0.5 :decay 0.5 :fade 0.75 :release 0.0}
      :high-harmonics {:toggle 1.0 :taper 0.0}
      :freq-envelope {:toggle 1.0 :init 0.5 :rate 0.5 :freq -0.5 :return 0.5
                      :freq-fscale 0.0 :rate-fscale 0.0 :freq-ascale 0.0 :rate-ascale 0.0}
      :lfo {:toggle 0.0 :rate 0.2 :fscale 0.5 :ramp 0.5 :amp-mod 0.9 :freq-mod 0.5 :depth 0.5}
      
      :master-env
      {:gain {:val 0.5 :fscale 0.15 :ascale 0.0}
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
      {:toggle [1.0 1.0 0.0 0.0]
       :freq [0.5 0.65 0.1 0.1]
       :width [0.01 0.01 0.1 0.1]
       :gain [0.95 0.95 0.0 0.0]}})

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
(def NHARMS 100)

;;todo - make buffers part of synth objects to support multitimbral
(def b (buf/buffer nparams))

(buf/buffer-id b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; patching overtone ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (assert (sdef/synthdef? sdef))
  (dosync (alter sdef/loaded-synthdefs* assoc (:name sdef) sdef))

  (when (srv/server-connected?)
    (let [bytes (sdef/synthdef-bytes sdef)]
      (com/with-server-sync
        (if (< (count bytes) (- osc-util/BUFFER-SIZE 4))
          #(srv/snd "/d_recv" bytes)
          (let [path (str (System/getProperty "java.io.tmpdir")  (-> (:name sdef) symbol name) ".scsyndef")]
            (sdef/synthdef-write sdef path)
            #(srv/snd "/d_load" path)))
        (str "whilst loading synthdef " (:name sdef))))))

(alter-var-root #'overtone.sc.machinery.synthdef/load-synthdef (constantly load-synthdef-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end patching overtone ;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (buf/buffer-write! buf arr)))

(patch->buf patch b)

(defn bget
  ([bid k] (ugen/index:kr bid (-> offsets (get k))))
  ([bid k1 k2] (ugen/index:kr bid (-> offsets (get k1) (get k2))))
  ([bid k1 k2 k3] (ugen/index:kr bid (-> offsets (get k1) (get k2) (get k3)))))

(defn scaled [aval scale ratio]
  (ugen/with-overloaded-ugens
    (* aval (ugen/exp (* scale ratio)))))

(defn master-scaled [aval bid k aratio fratio]
  (ugen/with-overloaded-ugens
    (* aval
       (ugen/exp (* (bget bid :master-env k :ascale) aratio))
       (ugen/exp (* (bget bid :master-env k :fscale) fratio)))))

(defn scale8 [scale]
  (ugen/with-overloaded-ugens
    (ugen/pow 2 (* 3.0 scale))))

(defn hscaled [aval bid h k]
  (ugen/with-overloaded-ugens
    (* aval (ugen/pow 2 (lin/lin-lin (bget bid :harmonics k h) -1.0 1.0 -3.0 3.0)))))

(defn harm [h hf bid freq gate aratio fratio delay attack decay sustain fade release]
  (ugen/with-overloaded-ugens
    (let [amp (/ (scaled
                  (scaled (bget bid :harmonics :gain h) (bget bid :harmonics :ascale h) aratio)
                  (bget bid :harmonics :ascale h) fratio)
                 hf)
          delay (+ delay (bget bid :harmonics :delay h))
          attack (hscaled attack bid h :attack)
          decay (hscaled decay bid h :decay)
          sustain (hscaled sustain bid h :sustain)
          fade (hscaled fade bid h :fade)
          release (hscaled release bid h :release)
          ectl (env/envelope [0 0 1.0 sustain 0 0]
                              [delay attack decay fade release]
                              [0
                               (bget bid :master-curves :attack)
                               (bget bid :master-curves :decay)
                               (bget bid :master-curves :fade)
                               (bget bid :master-curves :release)]
                              4)
          env (ugen/env-gen:kr ectl gate)
          hfreq (* hf freq)]
      (* (bget bid :harmonics :toggle h)
         amp env
         (ugen/sin-osc hfreq)
         ;;anti-alias
         (ugen/select:kr (< hfreq 20000) [0.0 1.0])))))

(defmacro primary-harms [n bid freq gate aratio fratio delay attack decay sustain fade release]
  (let [hs (map (fn [h]
                 `(harm ~h (+ 1 ~h) ~bid ~freq ~gate ~aratio ~fratio ~delay ~attack ~decay ~sustain ~fade ~release))
                (range n))]
    (cons '+ hs)))

(defmacro high-harms [n bid freq gate aratio fratio delay attack decay sustain fade release]
  (let [hs (map (fn [h]
                  `(harm 23 ~h ~bid ~freq ~gate ~aratio ~fratio ~delay ~attack ~decay ~sustain ~fade ~release))
                (range 24 n))]
    (cons '+ hs)))

(defn lfo [bid depth fratio]
  (ugen/with-overloaded-ugens
    (let [freq (scaled (lin/lin-lin (bget bid :lfo :rate) 0.0 1.0 0.0 20.0)
                       fratio (bget bid :lfo :fscale))
          ectl (env/envelope [0 1.0]
                         [(bget bid :lfo :ramp)]
                         :linear)
          env (ugen/env-gen:kr ectl)]
      (* (+ depth (bget bid :lfo :depth)) env (bget bid :lfo :toggle) (ugen/sin-osc:kr freq)))))

(defn res [sig bid n]
  (ugen/with-overloaded-ugens
    (* 10
       (bget bid :resonances :toggle n)
       (bget bid :resonances :gain n)
       (ugen/resonz sig
               (ugen/octcps (lin/lin-lin (bget bid :resonances :freq n) 0.0 1.0 1.0 10.0))
               (bget bid :resonances :width n)))))

(defn fine-rate [r]
  (ugen/with-overloaded-ugens
    (->> r (ugen/pow 2) (+ -1.0) ugen/squared)))

(defn fenv [fin bid aratio fratio]
  (ugen/with-overloaded-ugens
    (let [ffscale (scale8 (bget bid :freq-envelope :freq-fscale))
          fascale (scale8 (bget bid :freq-envelope :freq-ascale))
          rfscale (scale8 (bget bid :freq-envelope :rate-fscale))
          rascale (scale8 (bget bid :freq-envelope :rate-ascale))
          f0 (bget bid :freq-envelope :init);;(scaled (scaled (bget bid :freq-envelope :init) ffscale fratio) fascale aratio)
          r0 (fine-rate (bget bid :freq-envelope :rate));;(scaled (scaled (bget bid :freq-envelope :rate) rfscale fratio) rascale aratio)
          f1 (bget bid :freq-envelope :freq);;(scaled (scaled (bget bid :freq-envelope :freq) ffscale fratio) fascale aratio)
          r1 (fine-rate (bget bid :freq-envelope :return));;(scaled (scaled (bget bid :freq-envelope :return) rfscale fratio) rascale aratio)
          ectl (env/envelope [f0 f1 0] ;;[1.0 -1.0 0]
                         [r0 r1] ;;[1.0 1.0]
                         :linear)
          env (ugen/env-gen:kr ectl)]
      (-> fin ugen/cpsoct (+ (* env (bget bid :freq-envelope :toggle))) ugen/octcps))))

(inst/definst harmonikit
  [bid (buf/buffer-id b)
   note 60
   amp 0.25
   gate 1.0
   lfo-depth 0.0]
  (let [abase 0.125
        fbase 220
        freq (ugen/midicps note)
        aratio (ugen/log (/ amp abase))
        fratio (ugen/log (/ freq fbase))
        lfo (lfo bid lfo-depth fratio)
        envfreq (fenv freq bid aratio fratio)
        mfreq (-> envfreq ugen/cpsoct (+ (* (bget bid :lfo :freq-mod)
                                       (* 0.1 lfo)))
                  ugen/octcps)
        mamp (-> amp ugen/ampdb (+ (* (bget bid :lfo :amp-mod)
                                       (* 60.0 lfo)))
                 ugen/dbamp)
        
        gain (master-scaled (bget bid :master-env :gain :val) bid :gain aratio fratio)
        delay (bget bid :master-env :delay :val)
        attack (master-scaled (fine-rate (bget bid :master-env :attack :val))
                              bid :attack aratio fratio)
        decay (master-scaled (* 4 (fine-rate (bget bid :master-env :decay :val)))
                             bid :decay aratio fratio)
        sustain (master-scaled (bget bid :master-env :sustain :val) bid :sustain aratio fratio)
        fade (master-scaled (* 20.0 (bget bid :master-env :fade :val))
                            bid :fade aratio fratio)
        release (master-scaled (* 4.0 (bget bid :master-env :release :val))
                               bid :release aratio fratio)
        ectl (env/envelope [0 0 1.0 sustain 0 0]
                            [delay attack decay fade release]
                            [0
                             (bget bid :master-curves :attack)
                             (bget bid :master-curves :decay)
                             (bget bid :master-curves :fade)
                             (bget bid :master-curves :release)]
                            4)
        env (ugen/env-gen:kr ectl gate mamp :action ugen/FREE)
        sig (* (bget bid :master :toggle)
               env gain
               (+ (primary-harms 24 bid mfreq gate aratio fratio delay attack decay sustain fade release)
                  (high-harms 100 bid mfreq gate aratio fratio delay attack decay sustain fade release)))]
    (+ sig
       (res sig bid 0)
       (res sig bid 1)
       (res sig bid 2)
       (res sig bid 3))))

(defn transmit-patch [ch patch]
  (let [tx-scalars (fn [k]
                     (doseq [[attr v] (k patch)]
                       (async/put! ch [(str "/" (name k) "/" (name attr)) [(float v)]])))]
    (async/put! ch ["/name" [(:name patch)]])
    (doseq [[k attrs] (:master-env patch)]
      (doseq [[attr v] attrs]
        (async/put! ch [(str "/master-env/" (name k) "/" (name attr)) [(float v)]])))
    (dorun (map tx-scalars [:master :master-curves :freq-envelope :lfo :high-harmonics]))
    (doseq [[attr vs] (-> patch :harmonics (dissoc :toggle))]
      (async/put! ch [(str "/harmonics/" (name attr)) (map float vs)]))
    (doseq [[attr vs] (:resonances patch)]
      (doseq [i (range 4)]
        (async/put! ch [(str "/resonances/" (name attr) "/" i) [(float (vs i))]])))
    (async/put! ch [(str "/harmonics/toggle/0") (map float (subvec (-> patch :harmonics :toggle) 0 12))])
    (async/put! ch [(str "/harmonics/toggle/1") (map float (subvec (-> patch :harmonics :toggle) 12))])))

(defn edit-patch [patch {:keys [^String path args]}]
  (let [[s1 s2 s3] (.split (subs path 1) "/")]
    (case s1
          "master-env"
          (assoc-in patch [(keyword s1) (keyword s2) (keyword s3)] (first args))

          ("master" "master-curves" "freq-envelope" "lfo" "high-harmonics")
          (assoc-in patch [(keyword s1) (keyword s2)] (first args))

          "harmonics"
          (if (= s2 "toggle")
            (let [off (* 12 (edn/read-string s3))]
              (update-in patch [(keyword s1) (keyword s2)]
                         #(reduce-kv (fn [ret i v] (assoc ret (+ i off) v)) % (vec args))))
            (assoc-in patch [(keyword s1) (keyword s2)] (vec args)))

          "resonances"
          (assoc-in patch [(keyword s1) (keyword s2) (edn/read-string s3)] (first args))

          patch)))

(defn chan->client [chan client]
  (async/go
   (loop []
     (let [[path args] (async/<! chan)]
       (apply osc/osc-send client path args)
       (recur)))))

(defn server->chan [server chan]
  (osc/osc-listen server (fn [msg] (async/put! chan msg)) chan))


;;fiddle
;;to/from ./resources for now
(defn save-patch [patch patch-name]
  (with-open [file (io/writer (str "./resources/" patch-name ".edn"))]
    (binding [*out* file] (pp/pprint patch))))

(defn load-patch [patch-name]
  (with-open [file (io/reader (str "./resources/" patch-name ".edn"))]
    (binding [*in* (java.io.PushbackReader. file)] (edn/read))))

(def apatch (atom patch))
(def server (osc/osc-server 4242))

(def client (osc/osc-client "Aristotle.local" 8000))
(def cchan (async/chan 10))
(chan->client cchan client)

(reset! apatch (load-patch "testpatch"))
(patch->buf @apatch b)
(transmit-patch cchan @apatch)

(osc/osc-rm-all-listeners server)
(def schan (async/chan 10))
(server->chan server schan)

(async/go
 (loop []
   (let [msg (async/<! schan)]
     ;;(prn msg)
     (swap! apatch edit-patch msg)
     (patch->buf @apatch b)
     (recur))))

(def qserver (osc/osc-server 8899))
(osc/osc-rm-all-listeners qserver)
(def qchan (async/chan 100))
(server->chan qserver qchan)
(def notes (atom {}))
;;(osc-listen qserver (fn [msg] (println "Listener: " msg)) :debug)
(async/go
 (loop []
   (let [{:keys [path args]} (async/<! qchan)]
     ;;(prn msg)
     (when (= path "/qunexus/keys/note_and_velocity")
       (let [[note vel] args]
         (if (zero? vel)
           (when-let [synth (get @notes note)]
             (node/ctl synth :gate 0.0))
           (swap! notes assoc note (harmonikit (buf/buffer-id b) note (/ vel 127.0))))))
     (recur))))

(comment
  ;; Load a patch
  (patch->buf patch b)

  ;; Play a note
  (harmonikit (buffer-id b) 70)

  ;; Stop the note
  (node/ctl *1 :gate 0)

  ;; Stop all notes
  (srv/stop))
