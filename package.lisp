;;;; package.lisp

(defpackage #:utils
  (:use #:cl)
  (:nicknames #:u)
  (:export :head
           :read-lines
           :tail
           :sum
           :mappend
           :mapconcat
           :with-gensyms
           :fn
           :condlet
           :any
           :all
           :aif
           :alambda
           :array-map
           :group
           :iota
           :range
           :flatten
           :memoize
           :nif
           :allf
           :nilf
           :tf
           :toggle
           :_f
           :->
           :->>
           :as->
           :cond->
           :cond->>
           :pull
           :pull-if
           :popn
           :split-list
           :split-string
           :extract-if
           :on-trees
           :concf
           :in))
