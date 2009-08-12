#lang scheme/gui

(require "mark.ss"
         "plot.ss")

(define (plot-file file-name frame)
  (define setup (new ps-setup%))
  (send setup set-file file-name)
  (send setup set-mode 'file)
  (parameterize ([current-ps-setup setup])
    (define dc (new post-script-dc% [interactive #f]))
    (send dc start-doc file-name)
    (send dc start-page)
    ((make-plotter frame) #f dc)
    (send dc end-page)
    (send dc end-doc)))

(provide plot-file)