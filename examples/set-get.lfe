(defmodule set-get
  (export all)
  (import (from ledis (make-client 0)))
  (import (rename ledis ((get 1) ledis-get)
                        ((set 2) ledis-set))))

; From the REPL, we could simply do this:
;   > (slurp '"src/ledis.lfe")
;   #(ok ledis)
;   > (set client (make-client))
;   #Fun<lfe_eval.10.53503600>
;   > (get client '"fooz-4")
;   "barz-4"
;
; This is in contrast to the usage of the underlying Erlang Redis library:
;   > (set (tuple 'ok client) (: eredis start_link))
;   #(ok <0.31.0>)
;   > (: eredis q client '("GET" "fooz-4"))
;   #(ok #B(98 97 114 122 45 52))
(defun do-gets-and-sets ()
  (let ((client (make-client)))
    (set client "fooz-1" "barz-1")
    (set client "fooz-2" "barz-2")
    (set client "fooz-3" "barz-3")
    (set client "fooz-4" "barz-4")
    (set client "fooz-5" "barz-5")
    (get client "fooz-4")))