(define-constant err-unauthorized (err u200))
(define-constant err-already-indexed (err u201))
(define-constant err-not-found (err u202))

(define-data-var admin principal tx-sender)

(define-map cert-hash-index
    { hash: (buff 32) }
    {
        certificate-id: uint,
        owner: principal,
        revoked: bool,
    }
)

(define-read-only (get-admin)
    (var-get admin)
)

(define-read-only (get-by-hash (hash (buff 32)))
    (map-get? cert-hash-index { hash: hash })
)

(define-read-only (has-hash (hash (buff 32)))
    (is-some (get-by-hash hash))
)

(define-read-only (verify-by-hash (hash (buff 32)))
    (match (get-by-hash hash)
        entry (if (get revoked entry)
            err-not-found
            (ok true)
        )
        err-not-found
    )
)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-unauthorized)
        (var-set admin new-admin)
        (ok true)
    )
)

(define-public (index-cert-hash
        (hash (buff 32))
        (certificate-id uint)
        (owner principal)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-unauthorized)
        (asserts! (is-none (get-by-hash hash)) err-already-indexed)
        (map-set cert-hash-index { hash: hash } {
            certificate-id: certificate-id,
            owner: owner,
            revoked: false,
        })
        (ok true)
    )
)

(define-public (set-revoked-by-hash
        (hash (buff 32))
        (revoked bool)
    )
    (let ((entry (unwrap! (get-by-hash hash) err-not-found)))
        (asserts!
            (or
                (is-eq tx-sender (var-get admin))
                (is-eq tx-sender (get owner entry))
            )
            err-unauthorized
        )
        (map-set cert-hash-index { hash: hash }
            (merge entry { revoked: revoked })
        )
        (ok true)
    )
)

(define-public (remove-by-hash (hash (buff 32)))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-unauthorized)
        (asserts! (is-some (get-by-hash hash)) err-not-found)
        (map-delete cert-hash-index { hash: hash })
        (ok true)
    )
)
