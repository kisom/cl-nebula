(in-package #:nebula)

(defun startup (&key (port 3000))
  (load-credentials)
  (connect)
  (restas:debug-mode-on)
  (format t "starting up on port ~A~%" port)
  (restas:start '#:nebula :port port))

(restas:define-route index-route ("/" :method :get)
  (log4cl:log-info "GET /")
  (who:with-html-output-to-string (out nil :prologue t :indent t)
      (:html
       (:head
        (:title "nebula demo filestore")
        (:meta :http-equiv "Content-Type"
               :content    "text/html;charset=utf-8"))
       (:body
          (:h1 "WILLKOMMEN ZU NEBULA")
          (:p "Endpoints")
          (:ul
           (:li "POST /entry :: upload new blob"
            (:ul
             (:li "put the file contents in the \"file\" parameter")
             (:li (:code "curl -F file=@file.txt /entry"))
             (:li "returns the UUID of the file entry. don't lose this, as otherwise
              this is the only way to access the file.")))
           (:li "GET /entry/:uuid :: retrieve the blob stored under uuid."
            (:ul
             (:li "for example, if the upload returned the UUID 2181203d-7c99-4cf3-8461-f0702565819b,")
             (:li (:code "curl /entry/2181203d-7c99-4cf3-8461-f0702565819b"))
             (:li "currently serves the file as application/octet-stream (I think)")))
           (:li "POST /entry/:uuid :: upload new blob with parent"
            (:ul
             (:li "this uploads some data with `uuid` as the parent entry. for example, with the previous UUID,")
             (:li (:code "curl -F file=@updated-file.txt /entry/2181203d-7c99-4cf3-8461-f0702565819b"))
             (:li "this also returns the UUID of the created file."))
            (:li "GET /entry/:uuid/proxy :: proxy an entry"
             (:ul
              (:li "use this to share a file without giving away its original UUID")
              (:li "returns the UUID of the proxy")
              (:li (:code "curl /entry/2181203d-7c99-4cf3-8461-f0702565819b/proxy")))))
           (:li "DELETE /entry/:uuid :: remove an entry"
            (:ul
             (:li "remove the entry from the store")
             (:li "this will perform garbage collection, removing the backing blob if it's no longer needed and any proxied entries that will now be invalidated.")
             (:li "returns the UUID of the deleted entry")
             (:li (:code "curl -X DELETE /entry/2181203d-7c99-4cf3-8461-f0702565819b"))))
           (:li "GET /entry/:uuid/lineage :: retrieve an entry's lineage"
            (:ul
             (:li "returns a list of UUIDs representing the history of the entry")
             (:li (:code "curl /entry/2181203d-7c99-4cf3-8461-f0702565819b/lineage"))))
           (:li "GET /entry/:uuid/lineage/proxy :: proxy a lineage"
            (:ul
             (:li "proxies an entry and all its parents.")
             (:li (:code "curl /entry/2181203d-7c99-4cf3-8461-f0702565819b/lineage/proxy")))))))))

(defvar last-blob)

(restas:define-route post-entry ("/entry" :method :post)
  (log4cl:log-info "POST /entry")
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (setq last-blob body)
    (let ((uuid (upload-blob body)))
      (log4cl:log-info "created entry " uuid)
      uuid)))

(restas:define-route get-entry ("/entry/:uuid" :method :get)
  (log4cl:log-info "GET /entry/" uuid)
  (flexi-streams:octets-to-string
   (resolve-target uuid)))

(restas:define-route post-entry ("/entry/:uuid" :method :post)
  (log4cl:log-info "POST /entry/" uuid)
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (setq last-blob body)
    (let ((created (upload-blob body :parent uuid)))
      (log4cl:log-info "created entry " created)
      created)))

(restas:define-route proxy-entry-route ("/entry/:uuid/proxy" :method :get)
  (log4cl:log-info "GET /entry/" uuid "/proxy")
  (proxy uuid))

(restas:define-route delete-entry-route ("/entry/:uuid" :method :delete)
  (log4cl:log-info "DELETE /entry/" uuid)
  (if (remove-entry uuid)
      "OK"
      "failed"))

(restas:define-route get-entry-route ("/entry/:uuid/lineage" :method :get)
  (log4cl:log-info "GET /entry/" uuid "/lineage")
  (st-json:write-json-to-string
   (entry-history uuid)))

(restas:define-route entry-info-route ("/entry/:uuid/info" :method :get)
  (log4cl:log-info "GET /entry/" uuid "/info")
  (entry-info uuid))

(restas:define-route get-entry-route ("/entry/:uuid/lineage/proxy" :method :get)
  (log4cl:log-info "GET /entry/" uuid "/lineage/proxy")
  (st-json:write-json-to-string
   (proxy-all uuid)))
