(require 'f)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar code-path
  (f-parent test-path))

(add-to-list 'load-path code-path)

(require 'noflet)
