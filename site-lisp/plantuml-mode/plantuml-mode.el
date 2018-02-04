;;; plantuml-mode.el --- A major mode for plantuml-language. -*- lexical-binding: t -*-

;; Author: ein
;; Maintainer: ein
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: https://github.com/nobiruwa/dot-emacs.git
;; Keywords: PlantUML, UML, Java

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use this mode for editing files in the plantuml-language (http://plantuml.com/).

;;; Code:

(defgroup plantuml nil
  "Major mode for editing PlantUML files"
  :group 'tools)

(defun plantuml-customize ()
  "Run \\[customize-group] for the `plantuml' gruop."
  (interactive)
  (customize-group 'plantuml))

(defcustom plantuml-mode-abbrev-table nil "Abbbrev table in use in PlantUML mode buffers.")
(define-abbrev-table 'plantuml-mode-abbrev-table ())

(defcustom plantuml-cygwin nil
  "*Emacs working on cygwin or not."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-cygwin-cygpath-program-win-to-unix "cygpath -w \"%s\" \"%s\""
  "*Cygwin's cygpath when getting windows path from unix path."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-cygwin-cygpath-program-unix-to-win "cygpath -u \"%s\" \"%s\""
  "*Cygwin's cygpath when getting unix path from windows path."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-java-program "java"
  "*Location of the java program. This is used by `compile'."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-plantuml-jar-path "~/.emacs.d/etc/plantuml/plantuml.jar"
  "*Location of the plantuml jar file. This is used by `compile'."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-auto-indent-on-newline t
  "*If not nil, `electric-plantuml-terminate-line' is executed in a line is terminated."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-indent-width tab-width
  "*Indentation width in PlantUML mode buffers."
  :type 'integer
  :group 'plantuml)

(defcustom plantuml-auto-indent-on-braces nil "*If not nil, `electric-plantuml-open-brace' and `electric-plantuml-close-brace' are executed when { or } are typed."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-common-keywords
  '("header" "endheader"
    "scale"
    "title" "skinparam" "end title"
    "caption"
    "legend" "endlegend")
  "*Common keywords for PlantUML files."
  :type '(repeat (string :tag "Keyword"))
  :group 'plantuml)

(defcustom plantuml-declare-keywords
  '("@startuml" "@enduml")
  "*Keywords for declaration of PlantUML files."
  :type '(repeat (string :tag "Keyword"))
  :group 'plantuml)

(defcustom plantuml-sequence-keywords
  '("actor" "boundary" "control" "database" "entity"
    "autonumber" "stop" "resume"
    "newpage"
    "alt" "else" "end" "loop" "par" "break" "critical" "group")
  "*Keywords for declaration of PlantUML sequence diagram."
  :type '(repeat (string :tag "Keyword"))
  :group 'plantuml)

(defcustom plantuml-arrow-keywords
  '("->" "->x" "->>" "-\\" "\\-" "\\\\-" "//--" "->o" "o\\--" "<->" "<->o" "o<->")
  "*Keywords for arrows."
  :type '(repeat (string :tag "Keyword"))
  :group 'plantuml)

(defvar plantuml-font-lock-keywords
  `(,(concat "\\<" (regexp-opt plantuml-sequence-keywords) "\\>"))
  "*Font locks for keywords")

(provide 'plantuml-mode)

;;; plantuml-mode.el ends here
