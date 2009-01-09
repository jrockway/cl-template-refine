(in-package #:template-refine)

(defvar +html-namespace+ "http://www.w3.org/1999/xhtml"
  "Namespace for HTML elements.")

(defclass fragment ()
  ((fragment :type stp:document :reader fragment :initarg :fragment))
  (:documentation "Represents a fragment of an HTML body.  For an entire document, see the `html-document` class."))

(defgeneric make-fragment-from-string (string &optional class)
  (:documentation "Parse an HTML string into a `fragment`.  Anything in the <head> is ignored, and the HTML need not be valid.  The parser will do its best to create a sane document from it."))

(defun html-to-cxml-document (html)
  "Internal function to convert HTML to an stp DOM."
  (closure-html:parse html (stp:make-builder)))

(defmethod make-fragment-from-string ((html string) &optional class)
  (make-instance (or class 'fragment) :fragment (html-to-cxml-document html)))

(defgeneric compose-fragments (a b)
  (:documentation "Composes two fragments into one fragment, appending the contents of b to a and returing a new fragment.  The operands are not modified, and their nodes are not shared with the result."))

(defun make-html-document (&rest elements)
  "Returns an stp:document representing an HTML page."
  (destructuring-bind (html body)
      (iter (for e in '("html" "body"))
            (collect (stp:make-element e +html-namespace+)))
    (stp:append-child html body)
    (iter (for e in elements) (stp:append-child body e))
    (stp:make-document html)))

(defmethod compose-fragments ((a fragment) (b fragment))
  (make-instance
   (class-of a) ;; todo: look for first differing class in (reverse (linearized-isa ...
   :fragment
   (apply #'make-html-document
          (iter
            (for f in (list a b))
            (nconcing (mapcar #'stp:copy
                              (stp:list-children
                               (stp:find-recursively-if (stp:of-name "body" +html-namespace+)
                                                        (fragment f)))))))))

(defclass matcher () ()
  (:documentation "A rule that translates a fragment into 'relevant nodes'"))

(defgeneric match (matcher document)
  (:documentation "Apply a matcher to document, generating a list of nodes."))

(defclass replacer () ()
  (:documentation "A rule that transforms one node into its replacement"))

(defgeneric apply-replacement (replacement node)
  (:documentation "Apply a replacement rule to a node, generating a (list-of) replacement nodes."))

(defclass rule ()
  ((matcher :type matcher :reader matcher :initarg :matcher)
   (replacer :type replacer :reader replacer :initarg :replacer)))

(defgeneric apply-rule (fragment rule)
  (:documentation "Translates fragment into a new fragment using rule.  The fragment will not be modified."))

(defun make-rule (matcher replacer)
  (make-instance 'rule :matcher matcher :replacer replacer))

(defclass xpath-matcher (matcher)
  ((pattern :reader pattern :initarg :pattern))
  (:documentation "Declare a matcher that matches nodes with an XPath expression"))

(defmethod match ((matcher xpath-matcher) (document stp:document))
  ;; XXX: for some reason xpath doesn't work against the STP doc, as per the docs.
  (let (result)
    (xpath:do-node-set (node (xpath:evaluate (pattern matcher)
                                             (stp:serialize document (cxml-dom:make-dom-builder))))
      (setf result (cons node result)))
    result))