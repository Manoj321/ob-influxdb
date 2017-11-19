;;; ob-influxdb.el --- InfluxDB queries in org-mode

;; Copyright (C) 2017 Manoj Kumar M

;; Author: Manoj Kumar M <manojm.321@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; InfluxDB queries in org-mode.
;; use :db :host :precision header args to override defaults

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:influxdb
  `((:results . "raw"))
  "Default arguments for evaluating an influxdb block.")

(defun org-babel-execute:influxdb (body input-params)
  "Execute a block containing an InfluxDB query with org-babel."
  (message (concat "Executing InfluxDB query: " body))

  (let* ((params (org-babel-process-params input-params))
         (database (cdr (assoc :db params)))
         (host (cdr (assoc :host params)))
         (format (cdr (assoc :format params)))
         (precision (cdr (assoc :precision params))))
      (with-temp-buffer
        (unless database
          (setq database "telegraf"))
        (unless host
          (setq host "localhost"))
        (unless precision
          (setq precision "rfc3339"))
        (insert body)
        (let ((cmd-status (shell-command-on-region
         (point-min)
         (point-max)
         ;; csv format adds the measurement name as first column, so skip it
         (format "influx -database %s -precision %s -host %s -format csv -execute \"%s\" | cut -d, -f2-"
                 database precision host body)
         (current-buffer)
         t
         "*InfluxDB Result Buffer*"
         t)))
          ;; influx client returns success (0 return code) when there
          ;; is no output (eg: bad measurement name)
          (when (and (equal cmd-status 0)
                      (not (equal (point-min) (point-max))))
            (org-table-convert-region (point-min) (point-max) '(4))
            (org-table-insert-hline)
            (buffer-substring (point-min) (point-max))
           )))))
             
(defcustom org-babel-influxdb-template-selector
  "I"
  "Character to enter after '<' to trigger template insertion."
  :group 'org-babel
  :safe t
  :type 'string)

(eval-after-load "org"
  '(progn
     (add-to-list 'org-src-lang-modes '("influxdb" . "ob-influxdb"))
     (add-to-list 'org-structure-template-alist
                  `(,org-babel-influxdb-template-selector
                    "#+BEGIN_SRC influxdb :host localhost :db telegraf :precision rfc3339 \n?\n#+END_SRC\n"))))


(provide 'ob-influxdb)
;;; ob-influxdb.el ends here
