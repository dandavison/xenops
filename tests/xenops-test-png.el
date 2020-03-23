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

(ert-deftest xenops-test-png-make-phys-chunk ()
  "Test construction of pHYs chunk for 37795 pixels per meter in x and y dimensions."
  (let ((ppm 37795))
    (should (equal (xenops-png-make-phys-chunk 37795)
                   '(#x00 #x00 #x00 #x09           ;; length
                          #x70 #x48 #x59 #x73      ;; type
                          #x00 #x00 #x93 #xa3      ;; x pixels-per-meter
                          #x00 #x00 #x93 #xa3      ;; y pixels-per-meter
                          #x01                     ;; meter unit
                          #xD1 #xB2 #xF0 #xC6))))) ;; crc

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

(ert-deftest xenops-test-png-set-phys-chunk--makes-no-changes-when-ppi-is-nil ()
  "When the PPI argument is nil, `png-set-phys-chunk' should make no changes."
  (let* ((in-png-string (apply #'unibyte-string xenops-test-png--png-bytes-1))
         (out-png-string (xenops-png-set-phys-chunk in-png-string nil))
         (out-png-bytes (append out-png-string nil)))
    (should (equal out-png-bytes xenops-test-png--png-bytes-1))))

(ert-deftest xenops-test-png-set-phys-chunk--test-file-report ()
  "Test that the UNIX `file` utility recognizes the file as PNG."
  (let* ((in-file (make-temp-file "xenops-test-png" nil "png"))
         (out-file (make-temp-file "xenops-test-png" nil "png"))
         (expected-file-output-regex "[^:]+.png: PNG image data, 8 x 10, 4-bit colormap, non-interlaced")
         (in-png-string (apply #'unibyte-string xenops-test-png--png-bytes-1))
         in-file-file-output out-png-string out-file-file-output)

    ;; Check that `file` reports that the input is a valid PNG
    (f-write-bytes in-png-string in-file)
    (setq in-file-file-output (shell-command-to-string (format "file %s" in-file)))
    (should (string-match expected-file-output-regex in-file-file-output))

    ;; Check that `file` reports that the output is a valid PNG with the same properties.
    (setq out-png-string (xenops-png-set-phys-chunk (f-read-bytes in-file) 1))
    (f-write-bytes out-png-string out-file)
    (setq out-file-file-output (shell-command-to-string (format "file %s" out-file)))
    (should (string-match expected-file-output-regex out-file-file-output))))

(ert-deftest xenops-test-png-set-phys-chunk--test-imagemagick-report ()
  "Test that `imagemagick` reports that the intended change to the pHYs chunk has been made."
  (let* ((in-file (make-temp-file "xenops-test-png" nil "png"))
         (out-file (make-temp-file "xenops-test-png" nil "png"))
         (in-png-string (apply #'unibyte-string xenops-test-png--png-bytes-1))
         (meters/inch 0.0254)
         (pixels-per-inch 2540)
         in-file-file-output out-png-string out-file-file-output)

    ;; Check that `imagemagick` reports the expected pHYs chunk properties for the input.
    (f-write-bytes in-png-string in-file)
    (xenops-test-png--test-imagemagick-report in-file 3780)

    ;; Check that `imagemagick` reports the expected pHYs chunk properties in the output.
    (setq out-png-string (xenops-png-set-phys-chunk (f-read-bytes in-file) pixels-per-inch))
    (f-write-bytes out-png-string out-file)
    (xenops-test-png--test-imagemagick-report out-file 100000)))

(defun xenops-test-png--test-imagemagick-report (file expected-resolution)
  "Use `imagemagick` to assert that the pHYs chunk in FILE matches EXPECTED-RESOLUTION.

The `imagemagick` executable used is named `identify`."
  (with-temp-buffer
    (let ((output-buffer (current-buffer)))
      (with-temp-buffer
        (let ((error-buffer (current-buffer)))
          (shell-command (format "identify -verbose %s | grep -F png:pHYs" file)
                         output-buffer error-buffer)
          (with-current-buffer error-buffer
            (should (equal (buffer-string) "")))
          (with-current-buffer output-buffer
            (should (equal (buffer-string)
                           (format "    png:pHYs: x_res=%s, y_res=%s, units=1\n"
                                   expected-resolution expected-resolution)))))))))
