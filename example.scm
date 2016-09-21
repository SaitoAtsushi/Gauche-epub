
(use epub)

(define (make-html title content-text)
  `(*TOP*
    (html (@ (xmlns "http://www.w3.org/1999/xhtml")
             (xml:lang "ja"))
          (head
           (title ,title)
           (link (@ (rel "stylesheet")
                    (type "text/css")
                    (href "style.css"))))
          (body
           (h2 ,title)
           (p ,content-text)))))

(define style
  '((style-rule body
                (-epub-writing-mode vertical-rl)
                (writing-mode vertical-rl))
    (style-rule p
                (margin 0)
                (line-height (170 %)))))

(define-class <my-book> (<book>)
  ())

(define-method add-html ((book <my-book>)
                         (filename <string>)
                         (title <string>)
                         (content <string>))
  (add-html book filename title (make-html title content)))

(let ((book (make <my-book>
              :filename "example.epub"
              :id "541aec457c7a89c6ce88ad1757b9c1a8"
              :title "表題"
              :author "作者"
              :page-progression-direction "rtl"
              :description "概要")))
  (add-style book "style.css" style)
  (add-html book "001.xhtml" "まえがき" "まえがき本文")
  (chapter-begin book "第1部 導入編")
  (add-html book "002.xhtml" "プロローグ" "プロローグ本文")
  (chapter-begin book "第1章")
  (add-html book "003.xhtml" "第1章 第1話 最初の話" "ほにゃらら")
  (add-html book "004.xhtml" "第1章 第2話 次の話" "ほげほげ")
  (add-html book "005.xhtml" "第1章 第3話 一段落" "ふがふが")
  (chapter-end book)
  (chapter-begin book "第2章")
  (add-html book "006.xhtml" "第2章 第1話 次の事件" "わはー")
  (add-html book "007.xhtml" "第2章 第2話 解決" "むすー")
  (chapter-end book)
  (chapter-end book)
  (chapter-begin book "第2部 発展編")
  (chapter-begin book "第3章")
  (add-html book "008.xhtml" "第3章 第1話 開始" "かくかく")
  (add-html book "009.xhtml" "第3章 第2話 開始" "しかじか")
  (chapter-end book)
  (chapter-end book)
  (add-html book "010.xhtml" "あとがき" "あとがき本文")
  (book-close book))

