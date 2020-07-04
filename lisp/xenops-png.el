;;; xenops-png.el --- Functions for manipulating binary PNG data -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun xenops-png-set-phys-chunk (png-string ppi)
  "Set the pHYs chunk in PNG data.

Input PNG-STRING is a unibyte string containing the PNG data. The
return value is a unibyte string specifying an equivalent PNG
image, but with the pHYs chunk set according to PPI.

If PPI is nil, return an exact copy of the input PNG data."
  (let* ((m/i 0.0254)
         (ppm (and ppi (round (/ ppi m/i))))
         (in-bytes (append png-string nil))
         (out-bytes)
         (phys-type-bytes '(#x70 #x48 #x59 #x73)))  ;; p H Y s
    ;; Emit the initial PNG signature (8 bytes) and IHDR chunk. The IHDR chunk is:
    ;; length (4 bytes), type (4 bytes), data (13 bytes), crc (4 bytes).
    (dotimes (_ (+ 8 4 4 13 4))
      (push (pop in-bytes) out-bytes))
    ;; Emit the pHYs chunk
    (if ppm
        (dolist (phys-byte (xenops-png-make-phys-chunk ppm))
          (push phys-byte out-bytes)))
    ;; Now, emit everything, except pHYs chunks, if there are any.
    (while in-bytes
      (let (length type data crc)
        (dotimes (_ 4)
          (push (pop in-bytes) length))
        (dotimes (_ 4)
          (push (pop in-bytes) type))
        (dotimes (_ (xenops-png-pack-quartet (nreverse (append length nil))))
          (push (pop in-bytes) data))
        (dotimes (_ 4)
          (push (pop in-bytes) crc))
        (unless (and ppm (equal type (nreverse (append phys-type-bytes nil))))
          (dolist (byte (apply #'append (mapcar #'nreverse (list length type data crc))))
            (push byte out-bytes)))))
    (apply #'unibyte-string (nreverse out-bytes))))

(defun xenops-png-make-phys-chunk (ppm)
  "Construct the pHYs chunk for the requested size PPM.

Return value is a list of bytes."
  (let ((phys-crc-message)
        (bytes nil))
    ;; The length (4 bytes) is always 9. It is not included in the CRC message.
    (dolist (byte '(0 0 0 9))
      (push byte bytes))
    (dolist (title-byte '(#x70 #x48 #x59 #x73))
      (push title-byte bytes)
      (push title-byte phys-crc-message))
    (dolist (_ '(x y))
      (dolist (ppm-byte (xenops-png-unpack-word ppm))
        (push ppm-byte bytes)
        (push ppm-byte phys-crc-message)))
    (push 1 bytes)
    (push 1 phys-crc-message)
    (dolist (crc-byte (xenops-png-crc (nreverse phys-crc-message)))
      (push crc-byte bytes))
    (nreverse bytes)))

(defun xenops-png-crc (bytes)
  "Return crc32 checksum of BYTES.

This is a translation of the C code at
https://www.w3.org/TR/PNG-Structure.html#CRC-algorithm."
  (let ((register #xFFFFFFFF))
    (dolist (byte bytes)
      (setq register (logxor
                      (xenops-png-crc-one-byte (logand (logxor register byte) #xFF))
                      (lsh register -8))))
    (xenops-png-unpack-word (logxor register #xFFFFFFFF))))

(defun xenops-png-crc-one-byte (n)
  "Return PNG crc32 for the single byte N."
  (let* ((c n)
         (polynomial #xEDB88320))
    (dotimes (_ 8)
      (if (eq (logand c 1) 1)
          (setq c (logxor polynomial (lsh c -1)))
        (setq c (lsh c -1))))
    c))

(defun xenops-png-pack-quartet (quartet)
  "Convert list of 4 bytes to 32-bit word.

 QUARTET is a list of 4 integers in the range 0-255."
  (unless (equal (length quartet) 4)
    (error "Input must have length 4"))
  (let ((masks '(#xFF000000 #xFF0000 #xFF00 #xFF))
        (register 0))
    (cl-loop for (index . (byte mask)) in (-zip '(0 1 2 3) quartet masks) do
             (setq register
                   (logior register (logand mask (lsh byte (xenops-png-byte-offset index))))))
    register))

(defun xenops-png-unpack-word (word)
  "Convert 32-bit WORD to a list of 4 bytes.

A byte is represented as an integer in the range 0-255."
  (unless (< word #xFFFFFFFF)
    (error "Input must fit in 4 bytes"))
  (-map-indexed (lambda (index mask) (lsh (logand mask word) (- (xenops-png-byte-offset index))))
                '(#xFF000000 #xFF0000 #xFF00 #xFF)))

(defun xenops-png-byte-offset (n)
  "Return bit offset of N-th byte.

 N=0 for the most significant byte, and N=3 for the least significant."
  (* 8 (- 4 (1+ n))))

(provide 'xenops-png)

;;; xenops-png.el ends here
