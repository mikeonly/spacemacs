;; Source: http://stackoverflow.com/a/13879078/3885799
;; Initial step to font specification

(create-fontset-from-fontset-spec
 "-*-consolas-*-*-*-*-12-*-*-*-*-*-fontset-consolas,
    ascii:-*-consolas-*-*-*-*-12-*-*-*-*-*-iso8859-1,
    latin-iso8859-1:-*-consolas-*-*-*-*-12-*-*-*-*-*-iso8859-1,
    latin-iso8859-15:-*-consolas-*-*-*-*-12-*-*-*-*-*-iso8859-15")

(setq default-frame-alist '((width . 100) 
                            (height . 44) 
                            (top . 50) ;pixels
                            (left . 50) ;pixels
                            (font . "fontset-consolas")
                            ))
