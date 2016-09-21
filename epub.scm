
(define-module epub
  (use srfi-1)
  (use srfi-19)
  (use sxml.serializer)
  (use text.tree)
  (use gauche.record)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.mop.singleton)
  (use zip-archive)
  (use rfc.zlib)
  (use util.match)
  (use www.css)
  (export <book>
          book-open
          book-close
          add-html
          add-style
          add-image
          chapter-begin
          chapter-end))

(select-module epub)

(define (container)
  "<?xml version='1.0' ?>
<container
  xmlns='urn:oasis:names:tc:opendocument:xmlns:container'
  version='1.0'>
<rootfiles>
<rootfile
  full-path='OEBPS/content.opf'
  media-type='application/oebps-package+xml' />
</rootfiles>
</container>"
  )

(define-class <book> ()
  ((zip :init-keyword :zip)
   (id :init-keyword :id)
   (filename :init-keyword :filename)
   (page-progression-direction :init-keyword :page-progression-direction
                               :init-value "default")
   (series :init-value #f)
   (has-toc :init-keyword :has-toc :init-value #t)
   (title :init-keyword :title :init-value "")
   (author :init-keyword :author :init-value #f)
   (description :init-keyword :description :init-value #f)
   (episode-id :init-value 0)
   (chapter-id :init-value 0)
   (items :init-value '())
   (styles :init-value '())
   (images :init-value '())))

(define-class <chapter-begin> ()
  ((title :init-keyword :title)
   (id :init-keyword :id)))

(define-method write-object ((obj <chapter-begin>) port)
  (display #"#<chapter-begin \"~(~ obj 'title)\">" port))

(define (make-chapter-begin title id)
  (make <chapter-begin> :title title :id id))

(define (chapter-begin? obj)
  (is-a? obj <chapter-begin>))

(define-class <chapter-end> ()
  ()
  :metaclass <singleton-meta>)

(define (make-chapter-end)
  (make <chapter-end>))

(define (chapter-end? obj)
  (is-a? obj <chapter-end>))

(define-class <episode> ()
  ((title :init-keyword :title)
   (filename :init-keyword :filename)
   (id :init-keyword :id)))

(define (make-episode title filename id)
  (make <episode> :title title :filename filename :id id))

(define-method write-object ((obj <episode>) port)
  (display #"#<episode \"~(~ obj 'title)\">" port))

(define (episode? obj)
  (is-a? obj <episode>))

(define (sxhtml-display xml . rest)
  (let-optionals* rest
      ((port (current-output-port)))
    (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" port)
    (display "<!DOCTYPE html>\n" port)
    (write-tree
     (srl:parameterizable xml
                          #f
                          '(omit-xml-declaration . #t)
                          '(indent . #f))
     port)))

(define (sxhtml->string xml)
  (call-with-output-string (pa$ sxhtml-display xml)))

(define-method initialize ((obj <book>) initargs)
  (next-method obj initargs)
  (unless (slot-bound? obj 'filename)
    (error "filename must be set to create book"))
  (unless (slot-bound? obj 'title)
    (error "title must be set to create book"))
  (unless (slot-bound? obj 'id)
    (error "id must be set to create book"))
  (set! (~ obj 'zip) (open-output-zip-archive (~ obj 'filename)))
  (zip-add-entry (~ obj 'zip)
                 "mimetype" "application/epub+zip"
                 :compression-level Z_NO_COMPRESSION)
  (zip-add-entry (~ obj 'zip)
                 "META-INF/container.xml" (container)
                 :compression-level Z_BEST_COMPRESSION))

(define (book-open filename id . rest)
  (let-optionals* rest
      ((title "")
       (author #f)
       (description #f))
    (rlet1 book
        (make <book>
          :id id
          :title title
          :author author
          :filename filename
          :description description))))

(define (book? x)
  (is-a? x <book>))

(define (chapter-begin book title)
  (push! (~ book 'items) (make-chapter-begin title (~ book 'chapter-id)))
  (inc! (~ book 'chapter-id)))

(define (chapter-end book)
  (push! (~ book 'items) (make-chapter-end)))

(define-method add-html
    ((book <book>) (filename <string>) (title <string>) (content <list>))
  (zip-add-entry (~ book 'zip)
                 #"OEBPS/contents/~|filename|" (sxhtml->string content))
  (push! (~ book 'items) (make-episode title filename (~ book 'episode-id)))
  (inc! (~ book 'episode-id)))

(define-method add-html
    ((book <book>) (filename <string>) (title <string>) (content <string>))
  (zip-add-entry (~ book 'zip)
                 #"OEBPS/contents/~|filename|" content)
  (push! (~ book 'items) (make-episode title filename (~ book 'episode-id)))
  (inc! (~ book 'episode-id)))

(define (add-image book filename content)
  (zip-add-entry (~ book 'zip) #"OEBPS/contents/~|filename|" content
                 :compression-level Z_NO_COMPRESSION)
  (push! (~ book 'images) filename))

(define-method add-style ((book <book>) (filename <string>) (content <string>))
  (zip-add-entry (~ book 'zip)
                 #"OEBPS/contents/~|filename|" content)
  (push! (~ book 'styles) filename))

(define (construct-css-string lst)
  (call-with-output-string (cut construct-css lst <>)))

(define-method add-style ((book <book>) (filename <string>) (content <list>))
  (add-style book filename
             (construct-css-string content)))

(define-method title-style ((book <book>))
  (if (equal? (~ book 'page-progression-direction) "rtl")
      (construct-css-string
       '((style-rule body
                     (-epub-writing-mode vertical-rl)
                     (writing-mode vertical-rl))))
      ""))

(define (title-page title . rest)
  (let-optionals* rest
      ((author #f)
       (description #f))
    (sxhtml->string
     `(*TOP*
       (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                (xml:lang "ja"))
             (head (title ,title)
                   (link (@ (rel "stylesheet") (href "style.css"))))
             (body
              (h1 ,title)
              ,@(if author `((h2 "作者") (p ,author)) '())
              ,@(if description
                    `((h2 "概要") (p ,description))
                    '())))))))

(define (half-title-page title . rest)
  (apply title-page title rest))

(define (make-image-manifest book)
  (map-with-index
   (lambda(index item)
     `(item (@ (id ,#"image_~|index|")
               (href ,#"contents/~|item|")
               (media-type
                ,(rxmatch-cond
                   ((rxmatch #/.gif$/ item) (#f) "image/gif")
                   ((rxmatch #/.jpg$/ item) (#f) "image/jpeg")
                   ((rxmatch #/.png$/ item) (#f) "image/png"))))))
   (~ book 'images)))

(define (make-text-manifest book)
  (filter-map
   (lambda(item)
     (match item
       ((@ <chapter-begin> (id id))
        `(item (@ (id ,#"chapter_~(~ item 'id)")
                  (href ,#"navigation/chapter~(~ item 'id).xhtml")
                  (media-type "application/xhtml+xml"))))
       ((@ <chapter-end>) #f)
       ((@ <episode> (id id) (filename filename))
        `(item (@ (id ,#"id_~(~ item 'id)")
                  (href ,#"contents/~(~ item 'filename)")
                  (media-type "application/xhtml+xml"))))))
   (reverse (~ book 'items))))

(define (make-style-manifest book)
  (map-with-index
   (lambda(idx item)
     `(item (@ (id ,#"style_~|idx|")
               (href ,#"contents/~|item|")
               (media-type "text/css"))))
   (reverse (~ book 'styles))))

(define (make-spine book)
  (filter-map
   (lambda(item)
     (match item
       ((@ <chapter-begin> (id id))
        `(itemref (@ (idref ,#"chapter_~(~ item 'id)"))))
       ((@ <chapter-end>) #f)
       ((@ <episode> (id id) (filename filename))
        `(itemref (@ (idref ,#"id_~(~ item 'id)"))))))
   (reverse (~ book 'items))))

(define (make-opf book)
  (define style-manifest (make-style-manifest book))
  (define image-manifest (make-image-manifest book))
  (define text-manifest (make-text-manifest book))

  (sxhtml->string
   `(*TOP*
     (@@ (*NAMESPACES*
          (dc "http://purl.org/dc/elements/1.1/")
          (dcterms "http://purl.org/dc/terms/")
          (xsi "http://www.w3.org/2001/XMLSchema-instance")
          (opf "http://www.idpf.org/2007/opf")))
     (package
      (@ (xmlns "http://www.idpf.org/2007/opf")
         (unique-identifier "BookId")
         (version "3.0"))
      (metadata
       (dc:title ,(~ book 'title))
       (dc:creator ,(~ book 'author))
       (dc:language "ja")
       (dc:identifier (@ (id "BookId")) ,#"urn:uuid:~(~ book 'id)")
       (dc:subject "General Fiction")
       ,@(if (~ book 'description)
             `((dc:description ,(~ book 'description)))
             '())
       (meta (@ (property "dcterms:modified"))
             ,(date->string (current-date) "~Y-~m-~dT~H:~M:~SZ"))
       ,@(if-let1 series-title (~ book 'series)
           `((meta (@ (name "calibre:series") (content ,series-title)))
             (meta (@ (name "calibre:series_index") (content "0"))))
           '()))
       (manifest
        (item (@ (id "toc")
                 (href "toc.ncx")
                 (media-type "application/x-dtbncx+xml")))
        (item (@ (id "nav")
                 (href "navigation/nav.xhtml")
                 (media-type "application/xhtml+xml")
                 (properties "nav")))
        (item (@ (id "title")
                 (href "navigation/title.xhtml")
                 (media-type "application/xhtml+xml")))
        (item (@ (id ,#"basestyle")
                 (href ,#"navigation/style.css")
                 (media-type "text/css")))
        ,@style-manifest
        ,@image-manifest
        ,@text-manifest)
       (spine (@ (toc "toc")
                  (page-progression-direction
                   ,(~ book 'page-progression-direction)))
               (itemref (@ (idref "title")))
               ,@(if (~ book 'has-toc)
                     '((itemref (@ (idref "nav"))))
                     '())
               ,@(make-spine book))
        (guide
         (reference (@ (type "title")
                       (title "title")
                       (href "navigation/title.xhtml"))))))))

(define (unflatten e g lst)

  (define (%element lst)
    (let ((item (car lst)))
      (cond ((chapter-begin? item)
             (receive (i r)
                 (%group (cdr lst))
               (values (g item i) r)))
            ((chapter-end? item)
             (error "too many chapter end specifier"))
            ((episode? item)
             (values (e item) (cdr lst)))
            (else (error "unknown element" item)))))

  (define (%group lst)
    (let loop ((result '())
               (lst lst))
      (cond ((null? lst)
             (error "chapter end specifier is not enogh"))
            ((chapter-end? (car lst))
             (values (reverse! result) (cdr lst)))
            (else (receive (v r)
                      (%element lst)
                    (loop (cons v result) r))))))

  (let loop ((result '())
             (lst lst))
    (if (null? lst)
        (reverse! result)
        (receive (v r)
            (%element lst)
          (loop (cons v result) r)))))

(define (make-ncx book)

  (define (nav)
    (unflatten
     (lambda(item)
       `(navPoint (@ (id ,#"id_~(~ item 'id)")
                     (playOrder ,#"~(+ 1 (~ item 'id))"))
                  (navLabel (text ,(~ item 'title)))
                  (content (@ (src ,#"contents/~(~ item 'filename)")))))
     (lambda(chapter items)
       (zip-add-entry (~ book 'zip)
                      #"OEBPS/navigation/chapter~(~ chapter 'id).xhtml"
                      (half-title-page (~ chapter 'title)))
       `(navPoint
         (@ (id ,#"chapter_~(~ chapter 'id)"))
         (navLabel (text ,(~ chapter 'title)))
         (content
          (@ (src ,#"navigation/chapter~(~ chapter 'id).xhtml")))
         ,@items))
     (reverse (~ book 'items))))
  
  (with-output-to-string
    (^[]
      (display "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n")
      (write-tree
       (srl:parameterizable
        `(*TOP*
          (ncx (@ (xmlns "http://www.daisy.org/z3986/2005/ncx/")
                  (xml:lang "en")
                  (version "2005-1"))
            (head
             (meta (@ (name "dtb:uid") (content ,#"urn:uuid:~(~ book 'id)")))
             (meta (@ (name "dtb:depth") (content "1")))
             (meta (@ (name "dtb:totalPageCount") (content "0")))
             (meta (@ (name "dtb:maxPageNumber") (content "0"))))
            (docTitle
             (text ,(~ book 'title)))
            (navMap ,@(nav))
            ))
        #f
        '(indent . #f)
        )))))

(define (format-link title filename)
  `(li (a (@ (href ,filename)) ,title)))

(define (topic-grouping book)
  (cons 'ol
        (unflatten
         (lambda(item)
           (format-link (~ item 'title) #"../contents/~(~ item 'filename)"))
         (lambda(chapter items)
           `(li (span (a (@ (href ,#"chapter~(~ chapter 'id).xhtml"))
                         ,(~ chapter 'title)))
                (ol ,@items)))
         (reverse (~ book 'items)))))

(define (topic-page book)
  (sxhtml->string
   `(*TOP*
     (@@ (*NAMESPACES*
          (epub "http://www.idpf.org/2007/ops")))
     (html (@ (xmlns "http://www.w3.org/1999/xhtml")
              (xml:lang "ja"))
           (head (title "目次")
                 (link (@ (rel "stylesheet") (href "style.css"))))
           (body
            (section
             (@ (epub:type "frontmatter toc"))
             (h1 "目次")
             (nav (@ (epub:type "toc")
                     (id "toc"))
                  ,(topic-grouping book))))))))

(define (book-close book)
  (define archive (~ book 'zip))
  (zip-add-entry archive "OEBPS/navigation/style.css"
                 (title-style book)
                 :compression-level Z_BEST_COMPRESSION)
  (zip-add-entry archive "OEBPS/navigation/title.xhtml"
                 (title-page (~ book 'title)
                             (~ book 'author)
                             (~ book 'description))
                 :compression-level Z_BEST_COMPRESSION)
  (zip-add-entry archive "OEBPS/navigation/nav.xhtml" (topic-page book)
                 :compression-level Z_BEST_COMPRESSION)
  (zip-add-entry archive "OEBPS/content.opf" (make-opf book)
                 :compression-level Z_BEST_COMPRESSION)
  (zip-add-entry archive "OEBPS/toc.ncx" (make-ncx book)
                 :compression-level Z_BEST_COMPRESSION)
  (zip-close archive))

(provide "epub")
