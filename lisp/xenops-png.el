;;; xenops-png.el --- Functions for manipulating binary PNG data -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun xenops-png-set-phys (ppi in-file out-file)
  "Write PNG data with new pHYs chunk to OUT-FILE.

PNG data is read from IN-FILE, and altered such that the pHYs
chunk specifies size according to PPI."
  (let* ((m/i 0.0254)
         (ppm (round (/ ppi m/i)))
         (in-bytes (f-read-bytes in-file))
         (out-bytes)
         (i 0))
    (assert (< ppm #xFFFFFFFF))
    (dolist (byte (append in-bytes nil))
      (setq i (1+ i))
      (cond
       ((or (< i 42)
            (> i 54))
        (push byte out-bytes))
       ((= i 42)
        ;; Push Data and CRC bytes for the pHYS chunk.
        (setq phys-crc-message nil)
        (dolist (title-byte '(#x70 #x48 #x59 #x73))  ;; p H Y s
          (push title-byte phys-crc-message))
        (dolist (_ '(x y))
          (dolist (ppm-byte (xenops-png-unpack-word ppm))
            (push ppm-byte out-bytes)
            (push ppm-byte phys-crc-message)))
        (push 1 out-bytes)
        (push 1 phys-crc-message)
        (dolist (crc-byte (xenops-png-crc (nreverse phys-crc-message)))
          (push crc-byte out-bytes)))))
    (f-write-bytes (apply #'unibyte-string (nreverse out-bytes)) out-file)))

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
    (dolist (_ (number-sequence 1 8))
      (if (eq (logand c 1) 1)
          (setq c (logxor polynomial (lsh c -1)))
        (setq c (lsh c -1))))
    c))

(defun xenops-png-unpack-word (word)
  "Convert 32-bit WORD to a list of 4 bytes.

A byte is represented as an integer in the range 0-255."
  (-map-indexed (lambda (index mask) (lsh (logand mask word) (- (* 8 (- 4 (1+ index))))))
                '(#xFF000000 #xFF0000 #xFF00 #xFF)))

(provide 'xenops-png)

;;; xenops-png.el ends here
