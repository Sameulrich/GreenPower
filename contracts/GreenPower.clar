;; GreenPower - Renewable energy certificate tracking system
(define-map energy-certificates uint {
  generator: principal,
  facility-name: (string-utf8 64),
  generation-specs: (string-utf8 256),
  production-date: uint,
  facility-location: (string-utf8 64),
  output-verified: bool
})

(define-map generator-facilities principal (list 100 uint))
(define-map energy-auditors principal bool)
(define-data-var certificate-id-tracker uint u0)

;; Error codes
(define-constant err-not-generator (err u400))
(define-constant err-not-auditor (err u401))
(define-constant err-certificate-not-found (err u402))
(define-constant err-access-restricted (err u403))
(define-constant err-facility-limit-exceeded (err u404))
(define-constant err-invalid-auditor-address (err u405))
(define-constant err-invalid-facility-name (err u406))
(define-constant err-invalid-generation-specs (err u407))
(define-constant err-invalid-production-date (err u408))
(define-constant err-invalid-facility-location (err u409))
(define-constant err-invalid-certificate-id (err u410))

;; Grid administrator
(define-constant grid-administrator tx-sender)

;; Register energy auditor
(define-public (register-energy-auditor (auditor principal))
  (begin
    ;; Check if sender is grid administrator
    (asserts! (is-eq tx-sender grid-administrator) err-access-restricted)
    
    ;; Validate auditor principal
    (asserts! (not (is-eq auditor 'SP000000000000000000002Q6VF78)) err-invalid-auditor-address)
    
    ;; Add auditor to registry
    (ok (map-set energy-auditors auditor true))
  )
)

;; Register energy certificate
(define-public (register-energy-certificate 
  (facility-name (string-utf8 64)) 
  (generation-specs (string-utf8 256)) 
  (production-date uint) 
  (facility-location (string-utf8 64)))
  (let
    ((certificate-id (var-get certificate-id-tracker))
     (generator tx-sender)
     (current-facilities (default-to (list) (map-get? generator-facilities generator))))
    
    ;; Validate inputs
    (asserts! (> (len facility-name) u0) err-invalid-facility-name)
    (asserts! (> (len generation-specs) u0) err-invalid-generation-specs)
    (asserts! (> production-date u0) err-invalid-production-date)
    (asserts! (> (len facility-location) u0) err-invalid-facility-location)
    
    ;; Check facility limit
    (asserts! (< (len current-facilities) u100) err-facility-limit-exceeded)
    
    ;; Store certificate information
    (map-set energy-certificates certificate-id {
      generator: generator,
      facility-name: facility-name,
      generation-specs: generation-specs,
      production-date: production-date,
      facility-location: facility-location,
      output-verified: false
    })
    
    ;; Update generator's facility list
    (let 
      ((updated-facilities (unwrap-panic (as-max-len? (concat (list certificate-id) current-facilities) u100))))
      (map-set generator-facilities generator updated-facilities)
    )
    
    ;; Increment certificate ID tracker
    (var-set certificate-id-tracker (+ certificate-id u1))
    
    (ok certificate-id)))

;; Verify energy output
(define-public (verify-energy-output (certificate-id uint))
  (begin
    ;; Validate certificate ID
    (asserts! (< certificate-id (var-get certificate-id-tracker)) err-invalid-certificate-id)
    
    (let
      ((certificate (unwrap! (map-get? energy-certificates certificate-id) err-certificate-not-found)))
      
      ;; Check if sender is energy auditor
      (asserts! (default-to false (map-get? energy-auditors tx-sender)) err-not-auditor)
      
      ;; Update certificate verification status
      (ok (map-set energy-certificates certificate-id (merge certificate {output-verified: true})))
    )
  )
)

;; Get certificate details
(define-read-only (get-certificate-details (certificate-id uint))
  (map-get? energy-certificates certificate-id))

;; Get generator's facilities
(define-read-only (get-generator-facilities (generator principal))
  (default-to (list) (map-get? generator-facilities generator)))

;; Check auditor status
(define-read-only (is-energy-auditor (address principal))
  (default-to false (map-get? energy-auditors address)))
