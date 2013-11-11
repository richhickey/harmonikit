;;jvm opts cribbed from overtone itself

(require 'leiningen.core.eval)

(def JVMOPTS
  "Per os jvm options. Options common to all cases go under
  `:any`. Options specific to one OS go under the key returned by
  `leiningen.core.eval/get-os` for that system. Temporarily disabled
  options can be kept under `:disabled`."
  {:any
   ["-Xms512m" "-Xmx1g"           ; Minimum and maximum sizes of the heap
    "-XX:+UseParNewGC"            ; Use the new parallel GC in conjunction with
    "-XX:+UseConcMarkSweepGC"     ;  the concurrent garbage collector
    "-XX:+CMSConcurrentMTEnabled" ; Enable multi-threaded concurrent gc work (ParNewGC)
    "-XX:MaxGCPauseMillis=20"     ; Specify a target of 20ms for max gc pauses
    "-XX:+CMSIncrementalMode"     ; Do many small GC cycles to minimize pauses
    "-XX:MaxNewSize=257m"         ; Specify the max and min size of the new
    "-XX:NewSize=256m"            ;  generation to be small
    "-XX:+UseTLAB"                ; Uses thread-local object allocation blocks. This
                                  ;  improves concurrency by reducing contention on
                                  ;  the shared heap lock.
    "-XX:MaxTenuringThreshold=0"] ; Makes the full NewSize available to every NewGC
                                  ;  cycle, and reduces the pause time by not
                                  ;  evaluating tenured objects. Technically, this
                                  ;  setting promotes all live objects to the older
                                  ;  generation, rather than copying them.
   :macosx
   ["-Xdock:name=Overtone"]
   :disabled
   ["-XX:ConcGCThreads=2"         ; Use 2 threads with concurrent gc collections
    "-XX:TieredCompilation"       ; JVM7 - combine both client and server compilation
                                  ;  strategies
    "-XX:CompileThreshold=1"      ; JIT each function after one execution
    "-XX:+PrintGC"                ; Print GC info to stdout
    "-XX:+PrintGCDetails"         ;  - with details
    "-XX:+PrintGCTimeStamps"]})   ;  - and timestamps

(defn jvm-opts
  "Return a complete vector of jvm-opts for the current os."
  [] (let [os (leiningen.core.eval/get-os)]
       (vec (set (concat (get JVMOPTS :any)
                         (get JVMOPTS os))))))

(defproject harmonikit "0.1.0-SNAPSHOT"
  :description "An additive synth in Clojure using overtone"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [overtone "0.8.1" :exclusions [org.clojure/clojure]]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]]
  :jvm-opts ^:replace ~(jvm-opts))
