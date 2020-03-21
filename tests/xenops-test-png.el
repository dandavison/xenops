;; -*- lexical-binding: t -*-

(ert-deftest xenops-test-png-pack-quartet--test ()
  (should (equal (xenops-png-pack-quartet '(0 0 0 0)) 0))
  (should (equal (xenops-png-pack-quartet '(0 0 0 1)) 1))
  (should (equal (xenops-png-pack-quartet '(0 0 0 255)) 255))
  (should (equal (xenops-png-pack-quartet '(0 0 1 0)) 256)))

(ert-deftest xenops-test-png-unpack-word--test ()
  (should (equal (xenops-png-unpack-word 0) '(0 0 0 0)))
  (should (equal (xenops-png-unpack-word 1) '(0 0 0 1)))
  (should (equal (xenops-png-unpack-word 255) '(0 0 0 255)))
  (should (equal (xenops-png-unpack-word 256) '(0 0 1 0))))

(ert-deftest xenops-test-png-crc ()
  (should (equal
           (xenops-png-crc '(0))
           '(#xD2 #x02 #xEF #x8D)))
  ;; Consider pHYs chunk data
  ;; <-- type -> <--- x ---> <--- y ---> unit
  ;; 70 48 59 73 00 00 93 a3 00 00 93 a3 01
  ;; The crc32 of this is 0xD1B2F0C6:
  ;; "%x" % binascii.crc32(b"\x70\x48\x59\x73\x00\x00\x93\xa3\x00\x00\x93\xa3\x01")
  ;; ==> 'd1b2f0c6'
  (should (equal
           (xenops-png-crc '(#x70 #x48 #x59 #x73 #x00 #x00 #x93 #xa3 #x00 #x00 #x93 #xa3 #x01))
           '(#xD1 #xB2 #xF0 #xC6))))

;; This is the output of
;; (append (f-read-bytes "tests/assets/png/5966a90e6deef0b402896a569e2d077ceea22298.png") nil)
(setq xenops-test-png--png-bytes-1
      '(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 8 0 0 0 10 4 3 0 0 0 123 233 2 179 0 0
            0 42 80 76 84 69 255 255 255 0 0 0 102 102 102 118 118 118 186 186 186 204 204 204 238
            238 238 152 152 152 170 170 170 136 136 136 16 16 16 84 84 84 220 220 220 50 50 50 0
            231 51 65 0 0 0 9 112 72 89 115 0 0 14 196 0 0 14 196 1 149 43 14 27 0 0 0 51 73 68 65
            84 8 29 99 96 96 80 102 0 2 87 16 1 4 108 197 17 83 25 216 89 23 236 96 224 225 216 0
            228 51 77 0 18 188 1 7 24 138 78 37 52 48 116 150 150 39 0 0 159 154 9 151 38 199 60
            218 0 0 0 0 73 69 78 68 174 66 96 130))

(ert-deftest xenops-test-png-set-phys-chunk--creates-valid-png ()
  (let* ((in-file (make-temp-file "xenops-test-png" nil "png"))
         (out-file (make-temp-file "xenops-test-png" nil "png"))
         (expected-file-output-regex "[^:]+.png: PNG image data, 8 x 10, 4-bit colormap, non-interlaced")
         (in-png-string (apply #'unibyte-string xenops-test-png--png-bytes-1))
         in-file-file-output out-png-string out-file-file-output)
    (f-write-bytes in-png-string in-file)
    (setq in-file-file-output (shell-command-to-string (format "file %s" in-file)))
    (should (string-match expected-file-output-regex in-file-file-output))
    (setq out-png-string (xenops-png-set-phys-chunk (f-read-bytes in-file) 1))
    (f-write-bytes out-png-string out-file)
    (setq out-file-file-output (shell-command-to-string (format "file %s" out-file)))
    (should (string-match expected-file-output-regex out-file-file-output))))
