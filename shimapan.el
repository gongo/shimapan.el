(require 'cl-lib)

(defconst shimapan:var-buffer-name "*Shimapan*"
  "shimapan を表示させるバッファ名")

(defconst shimapan:var-line-row 2)

(defconst shimapan:var-waist-padding 12
  "shimapan の腰部分と、画面との隙間の長さ(left + right)

|---| <-here->  |---|

|   =============   |
|    ===========    |
|     =========     |
|      =======      |
|       =====       |
|        ===        |
|         =         |
")


(defvar shimapan:var-color-line '("cyan" "white"))
(defvar shimapan:var-color-bg "black")

(defun shimapan:waist-width ()
  "現在の window に適した shimapan の腰部分の長さを返す"
  (let ((w (- (window-width) shimapan:var-waist-padding)))
    (when (cl-evenp w) (decf w))
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
  (if (or (< width 1) (cl-evenp width)) nil
    (let ((h 1))
      (while (/= width 1)
        (incf h)
        (decf width 2))
      h)))


(defun shimapan:make ()
  (save-selected-window
    (with-current-buffer (get-buffer-create shimapan:var-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((width (shimapan:waist-width))
            (padding (/ shimapan:var-waist-padding 2)))
        (dotimes (h (shimapan:height width))
          (insert (make-string (+ (* 2 padding) width -1) ? ))
          (beginning-of-line)
          (shimapan:line-coloring (point) padding width h)
          (end-of-line)
          (newline)
          (decf width 2)
          (incf padding)))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (view-mode))))

(defun shimapan:line-coloring (start padding line row)
  (let (end (p start) (c (if (< (% row 4) 2) 0 1)))
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (setq p (+ p padding))
    (put-text-property start end
                       'face `(:background ,shimapan:var-color-bg))
    (put-text-property p (+ p (1- line))
                       'face `(:background ,(nth c shimapan:var-color-line)))))

(defun shimapan:mounting ()
  (interactive)
  (shimapan:make)
  (switch-to-buffer shimapan:var-buffer-name))
