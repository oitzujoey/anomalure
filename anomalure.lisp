
(defpackage #:anomalure
  (:use #:cl)
  (:import-from #:alexandria
                #:copy-hash-table)
  (:export #:eval
           #:lib-bool
           #:lib-cons)
  (:shadow #:eval))
(in-package #:anomalure)


(defun handle-lambda (form)
  (if (= 3 (length form))
      form
      (print "Lambda requires exactly two arguments")))

(defun handle-funcall (form)
  (if (= 3 (length form))
      (eval (let ((function (eval (second form)))
                  (argument (eval (third form))))
              (let ((keyword (first function)))
                (case keyword
                  (lambda
                      (let ((parameter (second function))
                            (body (third function)))
                        (labels ((expand-from-binding (form name value)
                                   (cond
                                     ((symbolp form)
                                      (if (eq form name) value form))
                                     ((listp form)
                                      (let ((keyword (first form)))
                                        (case keyword
                                          (lambda
                                              (let ((parameter (second form))
                                                    (body (third form)))
                                                ;; Stop descending?
                                                (if (eq parameter name)
                                                    form
                                                    `(lambda ,parameter
                                                       ,(expand-from-binding body name value)))))
                                          (funcall
                                           `(funcall ,(expand-from-binding (second form) name value)
                                                     ,(expand-from-binding (third form) name value)))
                                          (maccall
                                           `(maccall ,(expand-from-binding (second form) name value)
                                                     ,(expand-from-binding (third form) name value)))
                                          (otherwise
                                           (format t
                                                   "funcall:expand-from-binding: Invalid keyword \"~A\"~%"
                                                   keyword)))))
                                     (t form))))
                          (expand-from-binding body parameter argument))))
                  (funcall
                   (handle-funcall function))
                  (maccall
                   (handle-maccall function))
                  (otherwise (format t "funcall: Invalid keyword \"~A\"~%" keyword))))))
      (print "Lambda requires exactly two arguments")))

(defun handle-maccall (form)
  (if (= 3 (length form))
      (eval (let ((function (eval (second form)))
                  (argument (eval (third form))))
              (let ((keyword (first function)))
                (case keyword
                  (lambda
                      (let ((parameter (second function))
                            (body (third function)))
                        (labels ((expand-from-binding (form name value)
                                   (cond
                                     ((symbolp form)
                                      (if (eq form name) value form))
                                     ((listp form)
                                      (let ((keyword (first form)))
                                        (case keyword
                                          (lambda
                                              (let ((parameter (second form))
                                                    (body (third form)))
                                                (if (eq parameter name)
                                                    form
                                                    `(lambda ,parameter ,(expand-from-binding body name value)))))
                                          (funcall
                                           `(funcall ,(expand-from-binding (second form) name value)
                                                     ,(expand-from-binding (third form) name value)))
                                          (maccall
                                           `(maccall ,(expand-from-binding (second form) name value)
                                                     ,(expand-from-binding (third form) name value)))
                                          (otherwise
                                           (format t
                                                   "maccall:expand-from-binding: Invalid keyword \"~A\"~%"
                                                   keyword)))))
                                     (t form))))
                          (expand-from-binding body parameter argument))))
                  (funcall
                   (handle-funcall `(funcall ,function ,argument)))
                  (maccall
                   (handle-maccall `(maccall ,function ,argument)))
                  (otherwise (format t "maccall: Invalid keyword \"~A\"~%" keyword))))))
      (print "Lambda requires exactly two arguments")))

;; The language is pretty simple.
(defun eval (form)
  (cond
    ((symbolp form)
     form)
    ((listp form)
     (let ((keyword (first form)))
       (case keyword
         (lambda (handle-lambda form))
         (funcall (handle-funcall form))
         (maccall (handle-maccall form))
         (otherwise (print "Invalid keyword")))))
    (t form)))

(defun lib-bool (form)
  `(funcall (lambda true
              (funcall (lambda false
                         (funcall (lambda and
                                    (funcall (lambda or
                                               (funcall (lambda not
                                                          (funcall (lambda if
                                                                     ,form)
                                                                   ;; if
                                                                   (lambda condition
                                                                     (lambda then
                                                                       (lambda else
                                                                         (funcall (funcall (funcall condition)
                                                                                           then)
                                                                                  else))))))
                                                        ;; not
                                                        (lambda p
                                                          (funcall (funcall p false) true))))
                                             ;; or
                                             (lambda p (lambda q
                                                         (funcall (funcall p p) q)))))
                                  ;; and
                                  (lambda p (lambda q
                                              (funcall (funcall p q) p)))))
                       ;; false
                       (lambda first (lambda second second))))
            ;; true
            (lambda first (lambda second first))))

(defun lib-cons (form)
  (lib-bool `(funcall (lambda nil
                        (funcall (lambda null?
                                   (funcall (lambda cons
                                              (funcall (lambda car
                                                         (funcall (lambda cdr
                                                                    ,form)
                                                                  ;; cdr
                                                                  (lambda cons (funcall cons false))))
                                                       ;; car
                                                       (lambda cons (funcall cons true))))
                                            ;; cons
                                            (lambda a (lambda d (lambda f
                                                                  (funcall (funcall f a) d))))))
                                 ;; null?
                                 (lambda cons
                                   (funcall cons (lambda a (lambda d false))))))
                      ;; nil
                      (lambda cons true))))

(defun lib-let (form)
  `(funcall (lambda let
              ,form)
            (lambda name (lambda value (lambda body
                                         (cons (quote funcall) (cons (cons (quote lambda) (cons name
                                                                                                (cons body nil)))
                                                                     (cons value nil))))))))

(defun lib-lisp (form)
  )
