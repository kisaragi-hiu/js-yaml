;;; yaml.el --- YAML library for Emacs Lisp -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: parser yaml convenience


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

;; Eventuall a port of js-yaml.

;;; Code:

(defvar loader require('./lib/loader'))
(defvar dumper require('./lib/dumper'))

(defvar yaml-Type            require("./lib/type"))
(defvar yaml-Schema          require("./lib/schema"))
(defvar yaml-FAILSAFE_SCHEMA require("./lib/schema/failsafe"))
(defvar yaml-JSON_SCHEMA     require("./lib/schema/json"))
(defvar yaml-CORE_SCHEMA     require("./lib/schema/core"))
(defvar yaml-DEFAULT_SCHEMA  require("./lib/schema/default"))
(defvar yaml-load            loader.load)
(defvar yaml-loadAll         loader.loadAll)
(defvar yaml-dump            dumper.dump)
(defvar yaml-YAMLException   require("./lib/exception"))

(defvar yaml-types
  `(
     (binary    . "./lib/type/binary")
     (float     . "./lib/type/float")
     (map       . "./lib/type/map")
     (null      . "./lib/type/null")
     (pairs     . "./lib/type/pairs")
     (set       . "./lib/type/set")
     (timestamp . "./lib/type/timestamp")
     (bool      . "./lib/type/bool")
     (int       . "./lib/type/int")
     (merge     . "./lib/type/merge")
     (omap      . "./lib/type/omap")
     (seq       . "./lib/type/seq")
     (str       . "./lib/type/str")))

(provide 'yaml)

;;; yaml.el ends here
