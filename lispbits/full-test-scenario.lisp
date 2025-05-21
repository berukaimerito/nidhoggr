(5am:test filter-audit-area
  "Push audit records - check filtering by area"
  (5am:with-fixture com.keepit.tests:create-partner-account ()
    (5am:with-fixture com.keepit.tests:create-portfolio-product-config
        ("test product" '(("resource" ("name" "users") ("limit" "5"))))
      (5am:with-fixture com.keepit.tests:create-user-account ()
        (let* ((product-guid com.keepit.tests::product-guid))
          ()
          )))))
