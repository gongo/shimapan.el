(eval-when-compile (require 'cl))

(defconst shimapan:var-buffer-name "*Shimapan*")

(defconst shimapan:var-waist-padding 12
  "shimapan の腰部分と、画面との隙間の長さ(left + right)

|---| <-here->  |---|

|   =============   |
|    ===========    |
|     =========     |
|      =======      |
|       =====       |
|        ===        |
|         =         |")

(defun shimapan:waist-width ()
  "現在の window に適した shimapan の腰部分の長さを返す"
  (let ((w (- (window-width) shimapan:var-waist-padding)))
    (when (evenp w) (decf w))
    (if (< w 0) 0 w) ))

(defun shimapan:height (width)
  "\
shimapan の腰部分の長さから、shimapan の高さを返す。
WIDTH が1未満だったり偶数だったら nil を返す。

Example:
  (shimapan:height 5)
    ;; => 3
  (shimapan:height 81)
    ;; => 41
  (shimapan:height 0)
    ;; => nil
  (shimapan:height 4)
    ;; => nil
"
  (if (or (< width 1) (evenp width)) nil
    (let ((h 1))
      (while (/= width 1)
        (incf h)
        (decf width 2))
      h)))

(defun shimapan:make ()
  (interactive)
  (with-help-window shimapan:var-buffer-name
    (with-current-buffer standard-output
      (erase-buffer)
      (let ((width (shimapan:waist-width))
            (padding-left (/ shimapan:var-waist-padding 2)))
        (dotimes (h (shimapan:height width))
          (insert (make-string padding-left ? )
                  (make-string width ?a))
          (newline)
          (decf width 2)
          (incf padding-left))))))
