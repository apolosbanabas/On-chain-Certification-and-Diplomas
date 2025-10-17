(define-non-fungible-token certificate uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-invalid-certificate (err u102))
(define-constant err-certificate-expired (err u103))
(define-constant err-not-renewable (err u104))
(define-constant err-batch-limit-exceeded (err u105))

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
        expiration-date: (optional uint),
        renewable: bool,
    }
)

(define-map institutions
    principal
    {
        name: (string-ascii 64),
        verified: bool,
        reputation-score: uint,
        certificates-issued: uint,
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

(define-read-only (is-certificate-valid (certificate-id uint))
    (match (get-certificate-by-id certificate-id)
        certificate-data (match (get expiration-date certificate-data)
            expiry-block (<= stacks-block-height expiry-block)
            true
        )
        false
    )
)

(define-public (register-institution (institution-name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set institutions tx-sender {
            name: institution-name,
            verified: true,
            reputation-score: u100,
            certificates-issued: u0,
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
        (expiration-blocks (optional uint))
        (renewable bool)
    )
    (let (
            (certificate-id (+ (var-get certificate-counter) u1))
            (institution-data (unwrap! (get-institution tx-sender) err-not-authorized))
            (expiry-date (match expiration-blocks
                blocks (some (+ stacks-block-height blocks))
                none
            ))
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
            expiration-date: expiry-date,
            renewable: renewable,
        })
        (map-set institutions tx-sender
            (merge institution-data { certificates-issued: (+ (get certificates-issued institution-data) u1) })
        )
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

(define-public (renew-certificate
        (certificate-id uint)
        (new-expiration-blocks (optional uint))
    )
    (let (
            (certificate-data (unwrap! (get-certificate-by-id certificate-id)
                err-invalid-certificate
            ))
            (institution-data (unwrap! (get-institution tx-sender) err-not-authorized))
            (new-expiry-date (match new-expiration-blocks
                blocks (some (+ stacks-block-height blocks))
                none
            ))
        )
        (asserts! (get verified institution-data) err-not-authorized)
        (asserts! (get renewable certificate-data) err-not-renewable)
        (map-set certificates certificate-id
            (merge certificate-data {
                expiration-date: new-expiry-date,
                issue-date: stacks-block-height,
            })
        )
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

(define-read-only (get-certificate-score (certificate-id uint))
    (match (get-certificate-by-id certificate-id)
        certificate-data (let (
                (institution-principal (get recipient certificate-data))
                (institution-data (default-to {
                    name: "",
                    verified: false,
                    reputation-score: u0,
                    certificates-issued: u0,
                }
                    (get-institution institution-principal)
                ))
                (age-score (if (< (- stacks-block-height (get issue-date certificate-data))
                        u1000
                    )
                    u90
                    u70
                ))
                (validity-score (if (is-certificate-valid certificate-id)
                    u100
                    u30
                ))
                (institution-score (get reputation-score institution-data))
            )
            (some (+ (/ (* age-score u30) u100)
                (+ (/ (* validity-score u40) u100)
                    (/ (* institution-score u30) u100)
                )))
        )
        none
    )
)

(define-read-only (get-institution-score (institution-principal principal))
    (match (get-institution institution-principal)
        institution-data (some (get reputation-score institution-data))
        none
    )
)

(define-public (update-institution-reputation
        (institution-principal principal)
        (new-score uint)
    )
    (let ((institution-data (unwrap! (get-institution institution-principal) err-invalid-certificate)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= new-score u100) err-invalid-certificate)
        (map-set institutions institution-principal
            (merge institution-data { reputation-score: new-score })
        )
        (ok true)
    )
)

(define-private (issue-single-batch-certificate
        (recipient principal)
        (context {
            institution: (string-ascii 64),
            course: (string-ascii 64),
            expiration-blocks: (optional uint),
            renewable: bool,
            institution-data: {
                name: (string-ascii 64),
                verified: bool,
                reputation-score: uint,
                certificates-issued: uint,
            },
            current-counter: uint,
            issued-count: uint,
        })
    )
    (let (
            (certificate-id (+ (get current-counter context) u1))
            (expiry-date (match (get expiration-blocks context)
                blocks (some (+ stacks-block-height blocks))
                none
            ))
        )
        (match (nft-mint? certificate certificate-id recipient)
            success (begin
                (map-set certificates certificate-id {
                    recipient: recipient,
                    institution: (get institution context),
                    course: (get course context),
                    issue-date: stacks-block-height,
                    certificate-hash: "batch",
                    verified: true,
                    expiration-date: expiry-date,
                    renewable: (get renewable context),
                })
                {
                    institution: (get institution context),
                    course: (get course context),
                    expiration-blocks: (get expiration-blocks context),
                    renewable: (get renewable context),
                    institution-data: (get institution-data context),
                    current-counter: certificate-id,
                    issued-count: (+ (get issued-count context) u1),
                }
            )
            error
            context
        )
    )
)

(define-public (batch-issue-certificates
        (recipients (list 25 principal))
        (institution (string-ascii 64))
        (course (string-ascii 64))
        (expiration-blocks (optional uint))
        (renewable bool)
    )
    (let (
            (institution-data (unwrap! (get-institution tx-sender) err-not-authorized))
            (initial-counter (var-get certificate-counter))
            (initial-context {
                institution: institution,
                course: course,
                expiration-blocks: expiration-blocks,
                renewable: renewable,
                institution-data: institution-data,
                current-counter: initial-counter,
                issued-count: u0,
            })
        )
        (asserts! (get verified institution-data) err-not-authorized)
        (asserts! (<= (len recipients) u25) err-batch-limit-exceeded)
        (let ((final-context (fold issue-single-batch-certificate recipients initial-context)))
            (var-set certificate-counter (get current-counter final-context))
            (map-set institutions tx-sender
                (merge institution-data { certificates-issued: (+ (get certificates-issued institution-data)
                    (get issued-count final-context)
                ) }
                ))
            (ok {
                total-issued: (get issued-count final-context),
                final-certificate-id: (get current-counter final-context),
            })
        )
    )
)
