//Muhammad Shariq
//Abdur Rehman
(define-non-fungible-token non-fungible-token int)
(define-map tokens-spender((token_id int))((spender principal)))
(define-map tokens-count((owner principal))((count int)))
(define-map accounts-operator((operator principal) (account principal))((is-approved bool)))
	

(define-private (balance-of (account principal))
	(default-to 0
	(get count
	(map-get? tokens-count (owner account)))))
	

(define-public (owner-of? (token_id int))
  (ok (nft-get-owner? non-fungible-token token_id)))

(define-private (is-spender-approved (spender principal) (token_id int))
(let ((approved-spender
      (unwrap! (get spender
	                          (map-get? tokens-spender ((token_id token_id))))
                  false)))
	    (is-eq spender approved-spender)))
	
(define-private (is-operator-approved (account principal) (operator principal))
	  (unwrap!
	    (get is-approved
	      (map-get? accounts-operator ((operator operator) (account account)))
	    )
	    false))
	
(define-private (is-owner (actor principal) (token_id int))
	  (is-eq actor
	    (unwrap! (nft-get-owner? non-fungible-token token_id) false)))
	

(define-private (can-transfer (actor principal) (token_id int))
	  (or
	   (is-owner actor token_id)
	   (is-spender-approved actor token_id)
	   (is-operator-approved (unwrap! (nft-get-owner? non-fungible-token token_id) false) actor)))
	

(define-private (register-token (new-owner principal) (token_id int))
	  (let ((current-balance (balance-of new-owner)))
	    (begin
	      (nft-mint? non-fungible-token token_id new-owner)
	      (map-set tokens-count
	        ((owner new-owner))
	        ((count (+ 1 current-balance))))
	      true)))
	

(define-private (release-token (owner principal) (token_id int))
	  (let ((current-balance (balance-of owner)))
	    (begin
	      (map-delete tokens-spender
	        ((token_id token_id)))
	      (map-set tokens-count
	        ((owner owner))
	        ((count (- current-balance 1))))
	      true)))
	

(define-constant same-spender-err (err 1))
(define-constant not-approved-spender-err (err 2))
(define-constant failed-to-move-token-err (err 3))
(define-constant unauthorized-transfer-err (err 4))
(define-constant failed-to-mint-err (err 5))
	

(define-public (set-spender-approval (spender principal) (token_id int))
	 (if (is-eq spender tx-sender)
	      same-spender-err
	      (if (or (is-owner tx-sender token_id)
	              (is-operator-approved
	               (unwrap! (nft-get-owner? non-fungible-token token_id) not-approved-spender-err)
	               tx-sender))
	          (begin
	            (map-set tokens-spender
	                        ((token_id token_id))
	                        ((spender spender)))
	            (ok token_id))
				not-approved-spender-err)))
	

(define-public (set-operator-approval (operator principal) (is-approved bool))
	  (if (is-eq operator tx-sender)
	      same-spender-err
	      (begin
	        (map-set accounts-operator
	                    ((operator operator) (account tx-sender))
	                    ((is-approved is-approved)))
	        (ok true))))
	

(define-public (transfer-from (owner principal) (recipient principal) (token_id int))
	  (if (and
	        (can-transfer tx-sender token_id)
	        (is-owner owner token_id)
	        (not (is-eq recipient owner)))
	      (if
	        (and
	          (unwrap-panic (nft-transfer? non-fungible-token token_id owner recipient))
	          (map-set tokens-count
	            ((owner recipient))
	            ((count (+ 1 (balance-of recipient)))))
	          (map-set tokens-count
	            ((owner owner))
	            ((count (- (balance-of owner) 1)))))
	        (ok token_id)
		failed-to-move-token-err)
	    unauthorized-transfer-err))

(define-public (transfer (recipient principal) (token_id int))
	  (transfer-from tx-sender recipient token_id))

(define-private (mint! (owner principal) (token_id int))
	  (if (register-token owner token_id)
	      (ok token_id)
	      failed-to-mint-err))

(begin
(mint! 'ST398K1WZTBVY6FE2YEHM6HP20VSNVSSPJTW0D53M 10001)
(mint! 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 10002)
(mint! 'S02J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKPVKG2CE 10003))

