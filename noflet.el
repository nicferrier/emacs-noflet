
(require 'cl-macs)

(defun noflet|expand (bindings &rest forms)
  "Expand BINDINGS and evaluate FORMS.

Used by `noflet' to expand it's contents.

Example:

 (noflet|expand
  '((find-file-noselect (file-name)
           (if t
               (this-fn \"/tmp/mytest\")
               (this-fn file-name)))
    (expand-file-name (file-name &optional thing)
     (if t
         (concat \"/tmp/\" file-name)
         (funcall this-fn file-name thing))))
  '(progn (expand-file-name \"/home/nferrier/thing\")))

It should not be necessary ever to call this.  Hence the exotic
name."
  (let*
      ((fsets
        (cl-loop
           for i in bindings
           collect
             (cl-destructuring-bind (name args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(fset (quote ,name)
                          (lambda ,args
                            (let ((this-fn ,saved-func-namev))
                              ,@body))))))))
       (fresets
        (cl-loop
             for i in bindings
             collect
             (cl-destructuring-bind (name args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(fset (quote ,name) ,saved-func-namev))))))
       (lets
        (cl-loop
           for i in bindings
           collect
             (cl-destructuring-bind (name test-arg args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(,saved-func-namev
                     (symbol-function (quote ,name)))))))))
    `(let ,lets
       (unwind-protect
            (progn
              (progn ,@fsets)
              ,@form)
         (progn ,@fresets)))))
 
(defmacro noflet (bindings &rest body)
  "Make local function BINDINGS allowing access to the original.

Each of the BINDINGS is done like in `flet':

  (noflet
    ((expand-file-name (file-name &optional default-dir)
       (concat defaulr-dir file-name)))
    (expand-file-name \"~/test\"))

In each of the BINDINGS the original function is accessible with
the name `this-fn':

  (noflet
    ((expand-file-name (file-name &optional default-dir)
       (if (string-match-p \"/fake.*\" file-name)
          (concat default-dir file-name)
          (funcall this-fn file-name default-dir))))
    (expand-file-name \"~/test\"))

This is great for overriding in testing and such like."
  (apply 'noflet|expand bindings body))



;; (expand-file-name "~/")
