(defun declare-class (class)
  (format nil "class ~a {~%" class))

(defun read-attr (lst n)
  (format t "~%  Attribut ~a~%" n)
  (format t "  Typ eingeben        :  ")
  (finish-output nil)
  (push (string-downcase (read)) lst)
  (format t "  Attribut eingeben   :  ")
  (finish-output nil)
  (push (string-downcase (read)) lst)
  (format t "  Noch eins (j/n)?       ")
  (finish-output nil)
  (cond ((eql 'j (read)) (read-attr lst (+ n 1)))
        (t (reverse lst))))

(defun declare-attributes (lst)
  (cond ((null lst) (format nil "~%"))
        (t (concatenate 'string  (format nil "    ~a ~a;~%" (car lst) (cadr lst))
                                 (declare-attributes (cddr lst))))))

(defun declare-constructor (lst)
  (cond ((null (cddr lst)) (format nil "~a ~a);~%" (car lst) (cadr lst)))
	(t (concatenate 'string (format nil "~a ~a, " (car lst) (cadr lst))
			        (declare-constructor (cddr lst))))))

(defun define-constructor-parameters (lst)
  (cond ((null (cddr lst)) (format nil "~a ~a) {~%" (car lst) (cadr lst)))
	(t (concatenate 'string (format nil "~a ~a, " (car lst) (cadr lst))
			        (define-constructor-parameters (cddr lst))))))  

(defun define-constructor-attributes (lst)
  (cond ((null lst) (format nil "}~%"))
	(t (concatenate 'string (format nil "    this->~a = ~a;~%" (cadr lst) (cadr lst))
			        (define-constructor-attributes (cddr lst))))))

(defun declare-methods (lst)
  (cond ((null lst) (format nil "};~%"))
        (t (concatenate 'string (format nil "    ~a get~a();~%"
                                  (car lst) (string-capitalize (cadr lst)))
                                (format nil "    void set~a(~a ~a);~%"
                                  (string-capitalize (cadr lst)) (car lst) (cadr lst))
                                (declare-methods (cddr lst))))))

(defun define-methods (lst class)
  (let ((type (car lst))
        (attr (cadr lst))
        (next-pair (cddr lst)))
    (cond ((null lst) (format nil ""))
          (t (concatenate 'string (format nil "~a ~a::get~a() {~%    return this->~a;~%}~%"
                                    type class (string-capitalize attr) attr)
                                  (format nil "void ~a::set~a(~a ~a) {~%    this->~a = ~a;~%}~%"
                                    class (string-capitalize attr) type attr attr attr)
                                  (define-methods next-pair class))))))

(defun write-file (path str)
  (with-open-file
    (stream path :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format stream "~a" str)))

(defun write-files ()
  (format t "~%  Name der Klasse eingeben, (für gewünschte Groß- und Kleinschreibung in
  Anführungszeichen!) :  ")
  (let* ((class (read))
         (attr-list (read-attr '() 1))
         (class-h (declare-class class))
	 (constructor (concatenate 'string (format nil "~a::~a(" class class)
                                           (define-constructor-parameters attr-list)
				           (define-constructor-attributes attr-list)))
         (class-cpp (define-methods attr-list class))
         (source (format nil "#include <~a.h>~%~%" class)))
    (setf class-h (concatenate 'string class-h
			               (format nil "  private:~%")
                                       (declare-attributes attr-list)
                                       (format nil "  public:~%")
                                       (format nil "    ~a(" class)
			               (declare-constructor attr-list)
                                       (declare-methods attr-list)))
    (setf source (concatenate 'string source (format nil "int main() {~%~%}")))
    (write-file (format nil "./~a.h" class) class-h)
    (write-file (format nil "./~a.cpp" class) (concatenate 'string constructor class-cpp))
    (write-file "source.cpp" source)
    (format t "~%  Taste drücken zum Beenden...")
    (finish-output nil)
    (read-char)))

(sb-ext:save-lisp-and-die "class-writer.exe"
  :executable t
  :toplevel 'write-files)

;; (setq path "D:/Programmieren/Common LISP/class-writer.lisp")
