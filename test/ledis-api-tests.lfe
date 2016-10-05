(defmodule ledis-api-tests
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest start-up
  (is (ledis:start-link)))

(deftest set
  (is-equal (ledis:set 'foo 'bar)
            #(ok #"OK"))
  (is-equal (ledis:set 'foo-string 'bar '(#(return-type string)))
            #(ok "OK")))

(deftest get
  (is-equal (ledis:get 'not-here)
            #(ok undefined))
  (is-equal (ledis:get 'foo)
            #(ok #"bar"))
  (is-equal (ledis:get 'foo '(#(return-type string)))
            #(ok "bar")))

(deftest bitcount
  (is-equal (ledis:bitcount 'not-here)
            #(ok #"0"))
  (is-equal (ledis:bitcount 'foo)
            #(ok #"10"))
  (is-equal (ledis:bitcount 'foo '(#(start 2) #(end 4)))
            #(ok #"4")))

(deftest bitop
  (is-equal (ledis:set 'a "asdf")
            #(ok #"OK"))
  (is-equal (ledis:set 'b "sdfg")
            #(ok #"OK"))
  (is-equal (ledis:bitop 'AND 'c '(a b))
            #(ok #"4"))
  (is-equal (ledis:get 'c)
            #(ok #"a`df")))

;" <-- this is a syntax high-lighting "fix" for Sublime Text

; XXX The following test is commented out until this ticket is fixed:
;     https://github.com/lfex/ledis/issues/11
;
; (deftest bitops
;   (is-equal (ledis:set 'd #b(#xff #xf0 #x00))
;             #(ok #"OK"))
;   (is-equal (ledis:bitops 'd 0)
;             #(ok #"12"))
;   (is-equal (ledis:set 'd #b(#x00 #xff #xf0))
;             #(ok #"OK"))
;   (is-equal (ledis:bitops 'd 1 '(#(start 1)))
;             #(ok #"8"))
;   (is-equal (ledis:bitops 'd 1 '(#(start 2)))
;             #(ok #"16"))
;   (is-equal (ledis:set 'd #b(#x00 #x00 #x00))
;             #(ok #"OK"))
;   (is-equal (ledis:bitpos 'd 1)
;             #(ok #"-1")))

; XXX The following test is commented out until this ticket is fixed:
;     https://github.com/lfex/ledis/issues/10
;
; (deftest blpop
;   (is-equal (progn
;               (ledis:rpush 'e "banana")
;               (ledis:rpush 'e "tranbar")
;               (ledis:blpop 'e 0))
;             #(ok (#"e" #"banana")))
;   (is-equal (ledis:blpop 'e 0)
;             #(ok (#"e" #"tranbar"))))
