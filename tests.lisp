(in-package #:template-refine)

(def-suite template-refine)
(in-suite template-refine)

(test mk-fragment
  (let (f)
    (finishes
      (setf f (make-fragment-from-string "<p>This is a <b>test</b></p>")))))

(test compose-fragments
  (let ((f (make-fragment-from-string "<p>foo"))
        (g (make-fragment-from-string "<p>bar"))
        h)
      (finishes (setf h (compose-fragments f g)))
      (is (equal '("foo" "bar")
                 (stp:find-recursively-if
                  (lambda (node) (eql (type-of node) 'stp:text))
                  (fragment h))))))

