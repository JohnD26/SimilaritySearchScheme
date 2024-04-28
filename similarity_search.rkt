#lang racket
#|d contient une liste du chemin vers tout les fichier du dossier
imageDataset2_15_20|#
(define openDirectory(lambda(dName) (directory-list dName)))

#|(pathToString pathList dName) retourne une liste de string contenant
le chemin de tout les fichiers dans le dossier dName |#
(define pathToString
  (lambda (pathList dName)
    (if (null? pathList)
        '()
        (cons
         (string-append dName (path->string (car pathList)))
         (pathToString (cdr pathList) dName)
         ))))

#|(filterImages d) retourne d en enlevant de la liste
tout les string qui ne se terminent pas par t (garde les .txt)|#
(define filterImages
  (lambda(d) (filter
              (lambda(x)(equal? (string-ref x (- (string-length x) 1)) #\t)
                ) d)))
#|(histogramPaths dName)renvoi une liste contenant le
chemin vers tout les histogram d'un dossier |#
(define histogramPaths (lambda (dName)
  (filterImages (pathToString (openDirectory dName) dName))))

;(openImage filename) ouvre filename
(define openImage (lambda(filename) (open-input-file filename)))
;(fermeImage openedFile) ferme le fichier openedFile
(define fermeImage (lambda(openedFile) (close-input-port openedFile)))

#|(readHist filePath) retourne une liste contenant l'histogram
du fichier contenu a l'adresse filePath |#
(define readHist
  (lambda (filePath)
    (let((openedFile (openImage filePath)))
       (readFileHist openedFile (read openedFile)))))
#|(readFileHist openedFile n) retourne une liste contenant
les n premieres valeurs de openedFile |#
(define readFileHist
  (lambda(openedFile n)
    (let ((value (read openedFile)))
      (if (= n 0)
          '() 
          (cons value (readFileHist openedFile (- n 1)))
          ))))
#| (compareH H1 H2) retourne le score de la comparaison
des deux histogram |#
(define compareH
  (lambda(H1 H2)
    (compareH-aux H1 H2 (sum H1) (sum H2))
    ))
(define compareH-aux
  (lambda(H1 H2 NP1 NP2)
    (if (null? H1) 0.0
        (+ (min (exact->inexact (/ (car H1) NP1) ) (exact->inexact (/ (car H2) NP2)))
           (compareH-aux (cdr H1) (cdr H2) NP1 NP2)))
        ))
;(sum lst)calcule la somme des elements dans lst
(define (sum lst) (apply + lst))

#| (compareHists queryHist directoryFiles) retourne le resultat de la comparaison
 de queryHist avec tout les histogram de directoryFiles |#
(define compareHists
  (lambda (queryHist directoryFiles)
    (if (null? directoryFiles) '()
        (cons (cons
               (car directoryFiles)
               (compareH queryHist (readHist (car directoryFiles))))
              (compareHists queryHist (cdr directoryFiles))))))

#|(split L) retourne deux liste, respectivement
la premiere et seconde moitiee de L |#
(define (split L)
  ; division de la liste en 2:
  ; retourne ((1ere moitié)(2nde moitié))
(let ((len (length L)))
 (cond ((null? L) '(() ()))
       ((= len 1) (list L '() ))
       (else (list (prefix L (/ len 2))
                   (suffix L (/ len 2)))))))
#|(prefix L N) retourne une liste contenant
les N premier elements de L |#
(define (prefix L N)
  (if (= N 0)
      null
      (if (or (= N 1) (< N 2))
          (list (car L))
          ;else
          (cons (car L) (prefix (cdr L) (- N 1))))))
#|(suffix L N) retourne la liste L en enlevant
les N premier elements |#
(define (suffix L N)
  (if (= N 0) L
      (if (or (= N 1) (< N 2))
      (cdr L)
      ;else
      (suffix (cdr L) (- N 1)))
      )) 
#|(mergelists L M) retourne une liste contenant
la fusion triee selon l'indice de similarite a la position
des deux listes L et M |#
(define (mergelists L M)
; supposer L et M déjà triés
(cond ( (null? L) M)
 ( (null? M) L)
 ( (> (cdr (car L))(cdr (car M))) (cons (car L)
                                        (mergelists (cdr L)M)))
 (else (cons (car M) (mergelists L (cdr M)))) ) )

#|(mergesort L) retourne la liste L triee selon
l'indice de similarite qui est a la deuxieme position
de chaque sous liste|# 
(define (mergesort L)
(cond ((null? L) '())
      ((= 1 (length L)) L)
      ((= 2 (length L)) (mergelists (list (car L))
                                    (cdr L)))
      (else (mergelists (mergesort (car (split L)) )
                        (mergesort (car (cdr (split L))))))
      ))
#|(similaritySearch queryHistogramFilename imageDatasetDirectory)
retourne une liste contenant les 5 images les plus similaires
a queryHistogramFilename dans imageDatasetDirectory |#
(define similaritySearch
  (lambda (queryHistogramFilename imageDatasetDirectory)
    (prefix
     (mergesort
      (compareHists
       (readHist queryHistogramFilename)
       (histogramPaths imageDatasetDirectory))) 5)))


