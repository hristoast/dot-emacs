;;; functions.el --- Extra convenience functions.
;;; Commentary:
;; Extra convenience functions.
;;; Code:

(defun build-project ()
  "Compile the current project."
  (interactive)
  (defvar compilation-read-command nil)
  (call-interactively 'compile))

(defun do-func-to-marked-region (func)
  "Do (FUNC) on a region forward and in reverse."
  (let ((mark (mark))
        (point (point)))
    (if (> mark point)
        (funcall func point mark)
      (funcall func mark point))))

(defun indent-appropriately ()
  "Appropriately indent the current line or region."
  (interactive)
  (if mark-active
      (do-func-to-marked-region 'indent-region)
    (indent-according-to-mode)))

(defun toggle-comment ()
  "Toggle comments on the current line or highlighted region."
  (interactive)
  (if mark-active
    (do-func-to-marked-region 'comment-or-uncomment-region)
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position))))

(defun export-compile-commands (project)
  "Generate a `compile_commands.json' file for a give project.
Select the appropriate cmake invocation via the `PROJECT' arg."
  ;; TODO: define project commands in a .dir-locals.el file.
  (interactive (list (read-from-minibuffer "project name: " nil nil nil nil)))
  (cond ((string= project "tes3mp")
         (let ((default-directory "~/src/openmw-tes3mp"))
           (shell-command
            "cmake -DDESIRED_QT_VERSION=5 -DRakNet_INCLUDES=/opt/morrowind/src/raknet/include -DLIBUNSHIELD_INCLUDE_DIR=/usr/include -DBullet_BulletCollision_LIBRARY=/usr/lib/libBulletCollision.so -DBullet_INCLUDE_DIR=/usr/include/bullet -DBullet_LinearMath_LIBRARY=/usr/lib/libLinearMath.so -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .")))
        ((string= project "openmw")
         (let ((default-directory "~/src/openmw"))
           (shell-command
            "cmake -DDESIRED_QT_VERSION=5 -DLIBUNSHIELD_INCLUDE_DIR=/usr/include -DBullet_BulletCollision_LIBRARY=/usr/lib/libBulletCollision.so -DBullet_INCLUDE_DIR=/usr/include/bullet -DBullet_LinearMath_LIBRARY=/usr/lib/libLinearMath.so -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .")))
        (t (shell-command "echo The given project name was not recognized."))))

(defun export-compile-commands-openmw ()
  "Shortcut for calling export-compile-commands for OpenMW."
  (interactive)
  (export-compile-commands "openmw"))

(defun export-compile-commands-tes3mp ()
  "Shortcut for calling export-compile-commands for TES3MP."
  (interactive)
  (export-compile-commands "tes3mp"))

(defun launch-thing (thing)
  "Open 'THING', which should be some sort of X program."
  (start-process "" nil thing))

;;; functions.el ends here
