(define-non-fungible-token certificate uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-invalid-certificate (err u102))

(define-data-var certificate-counter uint u0)
(define-data-var institution-admin principal tx-sender)

(define-map certificates
    uint
    {
        recipient: principal,
        institution: (string-ascii 64),
        course: (string-ascii 64),
        issue-date: uint,
        certificate-hash: (string-ascii 64),
        verified: bool,
    }
)

(define-map institutions
    principal
    {
        name: (string-ascii 64),
        verified: bool,
    }
)

(define-read-only (get-certificate-by-id (certificate-id uint))
    (map-get? certificates certificate-id)
)

(define-read-only (get-institution (institution-principal principal))
    (map-get? institutions institution-principal)
)

(define-read-only (get-certificate-count)
    (var-get certificate-counter)
)

(define-public (register-institution (institution-name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set institutions tx-sender {
            name: institution-name,
            verified: true,
        })
        (ok true)
    )
)

(define-public (set-institution-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (var-set institution-admin new-admin)
        (ok true)
    )
)

(define-public (issue-certificate
        (recipient principal)
        (institution (string-ascii 64))
        (course (string-ascii 64))
        (certificate-hash (string-ascii 64))
    )
    (let (
            (certificate-id (+ (var-get certificate-counter) u1))
            (institution-data (unwrap! (get-institution tx-sender) err-not-authorized))
        )
        (asserts! (get verified institution-data) err-not-authorized)
        (try! (nft-mint? certificate certificate-id recipient))
        (map-set certificates certificate-id {
            recipient: recipient,
            institution: institution,
            course: course,
            issue-date: stacks-block-height,
            certificate-hash: certificate-hash,
            verified: true,
        })
        (var-set certificate-counter certificate-id)
        (ok certificate-id)
    )
)

(define-public (transfer-certificate
        (certificate-id uint)
        (recipient principal)
    )
    (let ((current-owner tx-sender))
        (try! (nft-transfer? certificate certificate-id current-owner recipient))
        (ok true)
    )
)

(define-public (revoke-certificate (certificate-id uint))
    (let ((current-owner tx-sender))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (try! (nft-burn? certificate certificate-id current-owner))
        (ok true)
    )
)
