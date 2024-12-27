;;;; ./mnas-sheet-formats.lisp

(defpackage :mnas-sheet-formats
  (:use #:cl)
  (:export *A0*
           *B0*
           *C0*)
  (:export fmt)
  (:export print-formats))

(in-package :acad-formats)

(defparameter *A0*
  (list (/ 1000 (sqrt (sqrt 2)))
        (* 1000 (sqrt (sqrt 2))))
  "Формат размера A0")

(defparameter *B0*
  (list 1000.0 (* 1000 (sqrt 2)))
    "Формат размера B0")

(defparameter *C0*
  (list (sqrt (*  (first *A0*)(first *B0*) ))
        (sqrt (* (second *A0*)(second *B0*) )))
    "Формат размера C0")

(defun fmt (base-firmat-sizes number &optional (mult 1))
  "@b(Описание:) функция @b(fmt) возвращает размеры листа,
основанного на базовом формате с размерами base-firmat-sizes, со
степеньнью его деления пополам number, и кратностью mult.  Следует
иметь ввиду, что кратность 2 допустима только для форматов A0, B0 и C0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fmt *A0* 0)   => (840 1189)
 (fmt *A0* 4 1) => (210 297)
 (fmt *A0* 4 3) => (297 630)
@end(code) "
  (let ((rez (copy-list base-firmat-sizes)))
    (loop :for i :from 0 :below number
          :do
             (setf rez (reverse rez)
                   (first rez) (/ (first rez) 2)))
    (when (>= mult 2)
      (setf (first rez) (* mult (first rez))
            rez (reverse rez)))
    (mapcar
     #'(lambda (el) (floor el))
     rez)))

(defun print-formats (format-name size-from size-to mult-from mult-to)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (print-formats \"C\" 0 4 1 9)
@end(code)
  "
  (let* ((name format-name)
         (format-sizes
           (cond ((string= "A" format-name) *A0*)
                 ((string= "B" format-name) *B0*)
                 ((string= "C" format-name) *C0*))))
  (loop :for sz :from size-from :to size-to
        :do
           (loop :for mult :from mult-from :to mult-to
                   :do
                      (when (or (= sz 0) (/= mult 2))
                        (format t "~A~Ax~A (~{~A~^x~})~%"
                                name sz mult (fmt format-sizes sz mult)))))))

(print-formats "C" 0 4 1 9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


