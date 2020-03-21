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
