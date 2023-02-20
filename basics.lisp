
(in-package #:anomalure)

(eval (lib-cons '(funcall car (funcall (funcall cons x) y))))

'(maccall (maccall (maccall let eval)
           (lambda))
  (maccall eval (let ((x 5))
                  5)))
