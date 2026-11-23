;; Copyright (C) 2026 M.R. Siavash Katebzadeh <mr@katebzadeh.xyz>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require-builtin steel/process)
(require-builtin steel/ports)
(require-builtin steel/core/result)
(require (only-in "helix/editor.scm"
                  editor-focus editor->doc-id
                  editor-document->path editor-document->language))
(require (only-in "helix/misc.scm"
                  cursor-position get-active-lsp-clients lsp-client-name
                  lsp-client-initialized? lsp-client-offset-encoding
                  send-lsp-command set-error!))
(require (only-in "helix/static.scm" offset->lsp))

(define (latex-file-uri path)
  (with-handler
    (lambda (err)
      (set-error! (string-append "Cannot prepare LaTeX preview: " (error-object-message err)))
      #f)
    (let ([spawned
           (spawn-process
             (with-stdout-piped
               (command
                 "python3"
                 (list "-B" "-c"
                       "import sys; from pathlib import Path; sys.stdout.write(Path(sys.argv[1]).absolute().as_uri())"
                       path))))])
      (unless (Ok? spawned)
        (error (string-append "Starting python3: " (to-string (Err->value spawned)))))
      (let* ([child (Ok->value spawned)]
             [uri (#%read-port-to-string (child-stdout child))]
             [waited (wait child)])
        (if (and (Ok? waited)
                 (number? (Ok->value waited))
                 (= (Ok->value waited) 0)
                 (> (string-length uri) 0))
            uri
            (error (string-append "Python URI conversion failed: " (to-string waited))))))))

(define (latex-preview-result result)
  (define status (and (hash? result) (hash-try-get result 'status)))
  (cond
    [(not (number? status)) (set-error! "Texlab returned an invalid preview result")]
    [(= status 0) #f]
    [(= status 1) (set-error! "Compile LaTeX first; Texlab could not resolve the master PDF")]
    [(= status 2) (set-error! "Texlab could not launch Zathura; check its executable/PATH")]
    [(= status 3) (set-error! "Configure Texlab forwardSearch executable and args")]
    [else (set-error! "Texlab returned an unknown preview status")]))

;;@doc
;; Open the LaTeX master PDF in Zathura at the current source position.
(define (latex-view)
  (define doc (editor->doc-id (editor-focus)))
  (define path (editor-document->path doc))
  (define clients
    (filter (lambda (client) (equal? (lsp-client-name client) "texlab"))
            (get-active-lsp-clients)))
  (cond
    [(not (equal? (editor-document->language doc) "latex"))
     (set-error! "Preview needs a LaTeX document")]
    [(not (and (string? path) (is-file? path)))
     (set-error! "Save and compile this LaTeX document before previewing")]
    [(null? clients)
     (set-error! "No Texlab attached; check the LaTeX language-server configuration")]
    [(not (lsp-client-initialized? (car clients)))
     (set-error! "Texlab is still initializing; retry shortly")]
    [else
     (define position
       (offset->lsp (lsp-client-offset-encoding (car clients)) (cursor-position)))
     (define uri (latex-file-uri path))
     (when uri
       (send-lsp-command
         "texlab" "textDocument/forwardSearch"
         (hash "textDocument" (hash "uri" uri)
               "position" (hash "line" (car position) "character" (cadr position)))
         latex-preview-result))]))

(provide latex-view)
