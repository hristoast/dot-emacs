;;; functions.el --- Extra convenience functions.
;;; Commentary:
;; Extra convenience functions.
;;; Code:

(defun hristoast-build-project ()
  "Compile the current project."
  (interactive)
  (defvar compilation-read-command nil)
  (call-interactively 'compile))

(defun hristoast--do-func-to-marked-region (func)
  "Do FUNC on a region forward and in reverse."
  (let ((mark (mark))
        (point (point)))
    (if (> mark point)
        (funcall func point mark)
      (funcall func mark point))))

(defun hristoast-indent-appropriately ()
  "Appropriately indent the current line or region."
  (interactive)
  (if mark-active
      (hristoast--do-func-to-marked-region 'indent-region)
    (indent-according-to-mode)))

(defun hristoast-toggle-comment ()
  "Toggle comments on the current line or highlighted region."
  (interactive)
  (if mark-active
    (hristoast--do-func-to-marked-region 'comment-or-uncomment-region)
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position))))

(defun hristoast-toggle-fc-and-ws ()
  "Toggle displaying the fill column indicator and `whitespace-mode' in one handy function."
  (interactive)
  (display-fill-column-indicator-mode 'toggle)
  (whitespace-mode 'toggle))

(defun hristoast-run-async-shell-command-maybe-with-env (cmd env-key env-val)
  "Asyncronously run CMD with ENV-KEY=ENV-VAL set beforehand if need be."
  (when (and env-key env-val)
    (setenv env-key env-val))
  (async-shell-command cmd))

(defun hristoast-generate-openmw-compile-commands-json ()
  "A helper for generating a compile-commands.json file for using LSP with OpenMW."
  (interactive)
  (hristoast-run-async-shell-command-maybe-with-env
   (concat "cd ~/src/openmw && rm -rf build && mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=Debug .."
           (concat " && bear make -j" (number-to-string (+ 1 (string-to-number (shell-command-to-string "nproc")))))
           " && mv -fv compile_commands.json ..")
   "CMAKE_PREFIX_PATH" "/opt/build-openmw/osg-openmw:/opt/build-openmw/bullet"))

(defun hristoast-compile-openmw-debug ()
  "A helper for compiling a debug build of OpenMW."
  (interactive)
  (hristoast-run-async-shell-command-maybe-with-env
   (concat "cd ~/src/openmw && rm -rf build && mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=Debug .."
           (concat " && make -j" (number-to-string (+ 1 (string-to-number (shell-command-to-string "nproc"))))))
   nil nil))

(defun hristoast-regenerate-openmw-compile-commands-json ()
  "A helper for incrementally generating a compile-commands.json file for using LSP with OpenMW."
  (interactive)
  (hristoast-run-async-shell-command-maybe-with-env
   (concat "cd ~/src/openmw/build && cmake -DCMAKE_BUILD_TYPE=Debug .."
           (concat " && bear make -j" (number-to-string (+ 1 (string-to-number (shell-command-to-string "nproc")))))
           " && mv -fv compile_commands.json ..")
   "CMAKE_PREFIX_PATH" "/opt/build-openmw/osg-openmw:/opt/build-openmw/bullet"))

(defun hristoast-recompile-openmw-debug ()
  "A helper for incrementally compiling a debug build of OpenMW."
  (interactive)
  (hristoast-run-async-shell-command-maybe-with-env
   (concat "cd ~/src/openmw/build && cmake -DCMAKE_BUILD_TYPE=Debug .."
           (concat " && make -j" (number-to-string (+ 1 (string-to-number (shell-command-to-string "nproc"))))))
   nil nil))

;; (defun hristoast-launch-thing (thing)
;;   "Open 'THING', which should be some sort of X program."
;;   (start-process "" nil thing))

;;; functions.el ends here
