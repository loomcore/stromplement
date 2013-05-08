; stromplement v0.0.1
; Copyright (C) 2013 P. M. Yeeles
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see
; <http://www.gnu.org/licenses/>.

(require-extension srfi-13) ; Provides string-upcase

; Assume that A=0, B=1, C=2, ...
; Assume that character codes are modulo 26 (25=Z, 26=A, 27=B, ...)

(define a (char->integer #\A))
(define space (char->integer #\space))

(define (str->bin s)
  (let ((l (string->list (string-upcase s))))
    (map (lambda (c) (- (char->integer c) a)) l)))

(define (comp-code c)
  (cond ((= c (- space a)) c)
         (else (modulo (bitwise-and (bitwise-not c) #b11111) 26))))

(define (decode n)
  (cond ((= n (- space a)) #\space)
         (else (integer->char (+ n a)))))

(define (stromplement s)
  (list->string (map decode (map comp-code (str->bin s)))))
