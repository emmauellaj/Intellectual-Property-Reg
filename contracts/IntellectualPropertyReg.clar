;; IntellectualPropertyRegistry - Patent and trademark protection system

(define-map intellectual-properties uint {
  inventor: principal,
  ip-title: (string-utf8 64),
  technical-description: (string-utf8 256),
  filing-date: uint,
  research-institution: (string-utf8 64),
  patent-granted: bool
})

(define-map inventor-portfolio principal (list 100 uint))
(define-map patent-examiners principal bool)
(define-data-var ip-application-id uint u0)

;; Error constants
(define-constant err-unauthorized-inventor (err u700))
(define-constant err-unauthorized-examiner (err u701))
(define-constant err-ip-application-missing (err u702))
(define-constant err-operation-forbidden (err u403))
(define-constant err-portfolio-capacity-exceeded (err u704))
(define-constant err-invalid-principal-format (err u705))
(define-constant err-empty-ip-title (err u706))
(define-constant err-empty-technical-description (err u707))
(define-constant err-invalid-filing-date (err u708))
(define-constant err-empty-research-institution (err u709))
(define-constant err-invalid-application-id (err u710))

;; IP registry administrator
(define-constant ip-registry-admin tx-sender)

;; Register patent examiner
(define-public (register-patent-examiner (examiner principal))
  (begin
    (asserts! (is-eq tx-sender ip-registry-admin) err-operation-forbidden)
    (asserts! (not (is-eq examiner 'SP000000000000000000002Q6VF78)) err-invalid-principal-format)
    (ok (map-set patent-examiners examiner true))
  ))

;; File IP application
(define-public (file-ip-application
  (ip-title (string-utf8 64))
  (technical-description (string-utf8 256))
  (filing-date uint)
  (research-institution (string-utf8 64)))
  (let
    ((application-id (var-get ip-application-id))
     (inventor tx-sender)
     (current-portfolio (default-to (list) (map-get? inventor-portfolio inventor))))
    
    (asserts! (> (len ip-title) u0) err-empty-ip-title)
    (asserts! (> (len technical-description) u0) err-empty-technical-description)
    (asserts! (> filing-date u0) err-invalid-filing-date)
    (asserts! (> (len research-institution) u0) err-empty-research-institution)
    (asserts! (< (len current-portfolio) u100) err-portfolio-capacity-exceeded)
    
    (map-set intellectual-properties application-id {
      inventor: inventor,
      ip-title: ip-title,
      technical-description: technical-description,
      filing-date: filing-date,
      research-institution: research-institution,
      patent-granted: false
    })
    
    (let
      ((updated-portfolio (unwrap-panic (as-max-len? (concat (list application-id) current-portfolio) u100))))
      (map-set inventor-portfolio inventor updated-portfolio)
    )
    
    (var-set ip-application-id (+ application-id u1))
    (ok application-id)))

;; Grant patent
(define-public (grant-patent (application-id uint))
  (begin
    (asserts! (< application-id (var-get ip-application-id)) err-invalid-application-id)
    (let
      ((ip-application (unwrap! (map-get? intellectual-properties application-id) err-ip-application-missing)))
      (asserts! (default-to false (map-get? patent-examiners tx-sender)) err-unauthorized-examiner)
      (ok (map-set intellectual-properties application-id (merge ip-application {patent-granted: true})))
    )
  ))

;; Get IP application details
(define-read-only (get-ip-application-details (application-id uint))
  (map-get? intellectual-properties application-id))

;; Get inventor portfolio
(define-read-only (get-inventor-portfolio (inventor principal))
  (default-to (list) (map-get? inventor-portfolio inventor)))

;; Check examiner status
(define-read-only (is-patent-examiner (address principal))
  (default-to false (map-get? patent-examiners address)))
