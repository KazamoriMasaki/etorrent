					; etorrent.el -- Makes Emacs as a Bittorent Client and let you enjoy your favourites!

;; Copyright (C) 2020 Kazamori Masaki <KazamoriMasaki@hotmail.com>

;; Author: Kazamori Masaki <KazamoriMasaki@hotmail.com>
;; Keywords: Bittorent
;; Version: 20201101

;;; Commentary

;; This file implements a simple bittorrent client in elisp.
;; Hope you can enjoy your favourite movies and watch them added to  
;; your computer in Emacs :)!


;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(setq etorrent-table '())
(setq debug-on-error t)
(setq debug-on-quit t)
(setq etorrent-log-buffer nil)
(setq etorrent-download-proc-table (make-hash-table :test 'equal))


;; Ref
;; https://blog.jse.li/posts/torrent/
;; First, we shall parse the torrent file.

;; How to read file into str in elisp, thanks to Xah Lee's toturial.
;; Ref: http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun file-content-to-str (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun is-digit (char)
  (and (> char ?0)
       (< char ?9)))

;; We should parse the Bencode(pronounced as Bee-encode) to extract the file
;; metadata.
(defun parse-bencode ()
  (let ((c (char-after)))
    ;; Compare two chars in elisp
    ;; Ref
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Comparison.html
    (cond ;; method dispatch
     ((char-equal c ?d)
      (parse-bencode-dict))
     ((char-equal c ?i)
      (parse-bencode-integer))
     ((char-equal c ?l)
      (parse-bencode-list))
     ((is-digit c)
      (parse-bencode-string))
     ((char-equal c ?e)
      (parse-bencode-end))
     (t ;; default case
      (message (concat "ERROR: " (char-to-string c)))))))

(defun parse-bencode-dict ()
  "Parses bencode dict which encoded as d<contents>e, return as Emacs Lisp hash table."
  (forward-char) ; consume 'd'
  (let ((dict (make-hash-table :test 'equal)) ; :test equal specifies what function to use to test key exsitence.
	(key (parse-bencode))
	(value (parse-bencode)))
    (while (not (eq key 'end))
      (puthash key value dict)
      (setq key (parse-bencode))
      (setq value (parse-bencode)))
    (forward-char) ; consume 'e'
    dict)) ; return it back

(defun parse-bencode-integer ()
  "Parses bencode integer which encoded as i<decimal>e, return as an Emacs Lisp integer."
  (forward-char) ; consume 'i'
  (string-to-number (buffer-substring-no-properties ; return the characters of part of the buffer, without the text properties
		     (point) (- (search-forward "e") 1)))) ;no need to consume character 'e since search-forward already set point to the end of the occurrence found.

(defun parse-bencode-list ()
  "Parses bencode list which encoded as l<content>e, return as an Emacs Lisp list."
  (forward-char) ; consume 'l'
  (defun parse-bencode-list-content() 
    (let* ((value (parse-bencode))
	   (content (list value)))
      (if (eq value 'end)
	  nil
	(append content (parse-bencode-list-content))))) ; join lists recursively.
  (setq ls (parse-bencode-list-content))
  (forward-char) ; consume 'e'
  ls)

(defun parse-bencode-string ()
  "A bencode string is encoded as <length>:<contents>"
  (let* ((str-len
	  (string-to-number
	   (buffer-substring-no-properties (point) (- (search-forward ":") 1))))
	 (str-begin (point))
	 (str-end (progn (forward-char str-len) (point))))
    (buffer-substring-no-properties str-begin str-end)))

(defun parse-bencode-end ()
  'end)

(defun parse-torrent-file (file)
  "Parses a bittorrent file into a s-exp."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (parse-bencode)))

;; Error handling goes here.
(defun load-torrent-file (file)
  (interactive "storrent file: ")
  (parse-torrent-file file))

;; Talks to trackers. Gets peer responses.
(defun escape-url (str)
  "escape the url, copied from emac-wiki, which copied it from w3m-url-encode-string (w3m.el)"
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "") 'raw-text)
		  nil))))

;; Ref: http://ergoemacs.org/emacs/elisp_hash_table.html
(defun hash-table-to-list-of-pair (hash-table)
  "Return a list of (key value) pair"
  (let ((result nil))
    (maphash
     (lambda (k v)
       (push (list k v) result))
     hash-table)
    (nreverse result)))

(defun dump-value-to-bencode (value)
  "Encodes a lisp value into bencode"
  (cond
   ((integerp value)
    (concat "i" (number-to-string value) "e"))
   ((stringp value)
    (concat (number-to-string (length value)) ":" value))
   ((listp value)
    (concat "l"
	    (mapconcat 'dump-value-to-bencode value "") ; function sequence sepearator
	    "e"))
   ((hash-table-p value)
    (concat "d"
	    (mapconcat (lambda (x)
			 (concat (dump-value-to-bencode (car x))
				 (dump-value-to-bencode (cadr x))))
		       (hash-table-to-list-of-pair value)
		       "")
	    "e"))
   (t
    (progn 
      (message "dump-value-to-bencode-error"
	       (princ value))))))

(defun build-tracker-url (metainfo)
  "We talk to the tracker to announce our presense as a peer and to retrieve a list of other peers by making a GET request to the announce URL support."
  (let* ((info-hash (url-hexify-string (sha1
					(dump-value-to-bencode 
					 (gethash "info" metainfo))
					nil nil t)))
	 (base (gethash "announce" metainfo))
	 (peer-id "-UT2040-qnmlgbqnmlgb") ; peer-id 伪装成utorrent,代号为去年买了个表
	 (port "48738") 
	 (uploaded  "0")
	 (downloaded "0")
	 (compact "1")
	 (left "100")
	 (url (concat  base
		       (if (string-match-p "?" base) "&" "?")
		       "info_hash=" info-hash "&"
		       "peer_id=" peer-id "&"
		       "port=" port "&"
		       "uploaded=" uploaded "&"
		       "downloaded=" downloaded "&"
		       "left=" left))
	 (url-request-method "GET")
	 (url-user-agent "uTorrent/2040(22967)"))
    (print base)
    (print url)
    (save-excursion
      (set-buffer (url-retrieve-synchronously url))
      (progn (goto-char (point-min))
	     (delete-region (point-min) (search-forward "\n\n")) ; delete header
	     (parse-bencode)))))

;; Ref: https://allenkim67.github.io/programming/2016/05/04/how-to-make-your-own-bittorrent-client.html#downloading-from-peers
(defun build-handshake (info-hash peer-id)
  "building a bittorrent handshake."
  (concat "\x13"
	  "BitTorrent protocol"
	  "\x00\x00\x00\x00\x00\x00\x00\x00"
	  info-hash
	  peer-id))

(require 'bindat)

(defconst message-spec
  '((:length u32)
    (:message strz (:length))))

(defconst peer-message-spec
  '((:length  u32)
    (:id  u8)))

(defconst have-message-payload-spec
  '((:piece-index . u32)))

(defun build-choke ()
  (bindat-pack peer-message-spec
	       '((:length .  1)
		 (:id . 0))))

(defun build-unchoke ()
  (bindat-pack peer-message-spec
	       '((:length . 1)
		 (:id . 1))))

(defun build-interested ()
  (bindat-pack peer-message-spec
	       '((:length . 1)
		 (:id . 2))))

(defun build-uninterested ()
  (bindat-pack peer-message-spec
	       '((:length . 1)
		 (:id . 3))))


(defun build-have (payload)
  (concat (bindat-pack peer-message-spec
		       `((:length . 5)
			 (:id . 4)))
	  payload))

(defun build-bitfield (bitfield)
  (concat (bindat-pack peer-message-spec
		       `((:length . ,(+ 1 (length bitfield)))
			 (:id . 5)))
	  bitfield))

(defun build-request (index begin len)
  (let ((payload (bindat-pack 
		  '((:index u32)
		    (:begin u32)
		    (:length u32))
		  `((:index . ,index)
		    (:begin . ,begin)
		    (:length . ,len)))))
    (concat (bindat-pack peer-message-spec
			 `((:length . 13)
			   (:id . 6)))
	    payload)))

(defun etorrent-process-get (proc attribute)
  (gethash attribute (gethash (process-get proc :name) etorrent-download-proc-table)))

(defun etorrent-process-put (proc attribute value)
  (puthash attribute value (gethash (process-get proc :name) etorrent-download-proc-table)))

(defun needed (proc piece-block) 
  (let ((index (car piece-block))
	(begin (cadr piece-block)))
    (etorrent-log (format "needed index %d begin %d" index begin))
    (aref
     (aref (etorrent-process-get proc ":received") index)
     (/ begin (etorrent-process-get proc ":piece-length")))))

(defun request-piece (proc)
  "let us build the queue in the lisp way."
  (let* ((queue (etorrent-process-get proc ":queue"))
	 (requested (etorrent-process-get proc ":requested"))
	 (piece-block (car queue)))
    (etorrent-log (format "request-piece, queue %s, piece-block %s"
		   (prin1-to-string queue)
		   (prin1-to-string piece-block)))
    (if (needed proc piece-block)
	(progn
	  (etorrent-process-put proc ":queue" (cdr queue))
	  (request-piece proc))
      (progn
	(etorrent-process-put proc ":piece-blocks" (push piece-block piece-blocks))
	(let ((request-message (apply 'build-request piece-block)))
	  (etorrent-log (format "Request message %s" request-message))
	  (process-send-string proc request-message))))))

(defun choke-handler (proc)
  (delete-process proc))

(defun unchoke-handler (proc)
  (request-piece proc))

(defun enque-piece (proc piece-index)
  (let ((queue (etorrent-process-get proc ":queue"))
	(block (etorrent-process-get proc ":block")))
    (etorrent-log (format "enque-piece %s block %s" (prin1-to-string queue) (prin1-to-string block)))
    (setq queue
	  (nconc
	   (mapcar 
	    (lambda (v)
	      (push piece-index v))
	    block) 
	   queue))
    (etorrent-log (format "enque-piece %s" (prin1-to-string queue)))
    (etorrent-process-put proc ":queue" queue)))

(defun have-handler (proc payload)
  (etorrent-log (format "Have handler, payload %s" payload))
  (let ((piece-index (bindat-get-field
		      (bindat-unpack '((:piece-index)) 
				     payload)
		      :piece-index)))
    (enque-piece proc piece-index)
    (request-piece proc)))

(defun bitfield-handler (proc payload)
  (etorrent-log (format "Bitfield handler, payload %s" payload))
  (dotimes (i (length payload))
    (let ((byte (aref payload i)))
      (dotimes (j 8)
	(let ((mask (lsh 1 7)))
	  (if (logand (lsh byte j) mask)
	      (enque-piece proc (+ (* i 8) j)))))))
  (request-piece proc))

(defun add-received (proc index begin)
  (setf (aref
	 (aref (etorrent-process-get proc ":received") index)
	 (/ begin (etorrent-process-get proc ":piece-length")))
	t))

; Ref: https://emacs.stackexchange.com/questions/19018/what-emacs-lisp-boolean-logical-convenience-functions-exist
(defun bool-vector-all-p (bool-vector)
  (null (memq nil (bool-vector))))

(defun piece-done-p (proc index)
  (bool-vector-all-p (aref (etorrent-process-get proc ":received"))))

(defun download-done-p (proc)
  (seq-every-p 'bool-vector-all-p (etorrent-process-get proc ":received")))

(defun piece-handler (proc payload)
  (let *((piece-payload-struct (bindat-unpack '((:index u32)
						(:begin u32)
						(:data strz (- (length payload) 8)))
					      payload))
	 (index (bindat-get-field piece-payload-struct :index))
	 (begin (bindat-get-field piece-payload-struct :begin))
	 (data (bindat-get-field piece-payload-struct :data))
	 (file-buffer (etorrent-process-get proc ":file-buffer"))
	 (file-length (etorrent-process-get proc ":file-length"))
	 (piece-length (etorrent-process-get proc ":piece-length")))
       (with-current-buffer file-buffer
	 (setf (point) (+ (* index piece-length) begin))
	 (insert data))
       (add-received proc index begin)
       (if (download-done-p proc)
	   (progn 
	     (with-current-buffer file-buffer
	       (write-region (point-min) (point-max) (process-get proc :file-name)))
	     (delete-process proc))
	 (request-piece proc))))

(defun handshake-p (msg)
  "test whether message is a handshake message or not."
  (and (= (aref msg 0) ?\x13)
       (string= (substring msg 1 20) "BitTorrent protocol")))


;; About how to contorl network 
;; Ref: https://github.com/skeeto/emacs-web-server/blob/master/simple-httpd.el
(defun parse-peer-message-length ()
  "Parse peer message length, an u32."
  (if (= (buffer-size) 0)
      0
    (let ((msg-len-raw-bytes (string-to-unibyte (buffer-substring-no-properties (point-min) 5))))
      (bindat-get-field (bindat-unpack '((:length u32)) msg-len-raw-bytes) :length))))

;; Ref: https://allenkim67.github.io/programming/2016/05/04/how-to-make-your-own-bittorrent-client.html#handling-messages
(defun peer-message-handler (proc msg)
  "Handle network message."
  (etorrent-log (format "Got message. Length: %d .%s" (length msg) msg)) 
  (with-current-buffer (get-buffer-create "etorrent-network-log")
    (setf (point) (point-max))
    (insert msg)
    (when (handshake-p msg)
      (etorrent-log "Got handshake.")
      (delete-region (point-min) 69)
      (process-send-string proc (build-interested)))
    (etorrent-log "Start parsing")
    (let ((message-length (parse-peer-message-length)))
      (etorrent-log (format "Message-length %d buffer size %d" message-length (buffer-size)))
      (while (and (> (buffer-size) 0)
		  (>= (buffer-size) (+ message-length 4)))
	(etorrent-log "enter loop")
	(let ((buffer-content (buffer-substring-no-properties 5 (+ message-length 5))))
	  (delete-region (point-min) (+ message-length 5))
	  (etorrent-log (format "After delete buffer-size %d" (buffer-size)))
	  (if (not (string= "" buffer-content))
	      (let ((id (aref buffer-content 0))
		    (payload (string-to-unibyte (substring-no-properties buffer-content 1 nil))))
		(etorrent-log (format "mwssage id %d" id))
		(cond 
		 ((= id 0) (choke-handler proc))
		 ((= id 1) (unchoke-handler proc))
		 ((= id 4) (have-handler proc payload))
		 ((= id 5) (bitfield-handler proc payload))
		 ((= id 7) (piece-handler proc payload)))))
	    (setq message-length (parse-message-length)))))))

(defun etorrent-log (msg) 
  (let ((message-buffer (get-buffer-create "etorrent-log")))
    (with-current-buffer message-buffer
      (setf (point) (point-max))
      (insert (format "[%s] %s\n" (format-time-string "%D %-I:%M %p") msg)))))

(defun gen-range (start end step)
  (let ((length (- end start)))
    (if (<= length step)
	(list (list start length))
      (cons (list start step) (gen-range (+ start step) end step)))))

(defun split-pieces (piece-length block-length)
  (gen-range 0 (- piece-length 1) block-length))

(defun download-torrent (metainfo)
  (let ((tracker-rsp (build-tracker-url metainfo))
	(info-hash (sha1
		    (dump-value-to-bencode 
		     (gethash "info" metainfo))
		    nil nil t))
	(peer-id "-UT2040-qnmlgbqnmlgb") ; peer-id 伪装成utorrent,代号为去年买了个表
	(port "9527"))
    (let* ((peer-list (gethash "peers" tracker-rsp))
	   (handshake-message (build-handshake info-hash peer-id))
	   (file-length (gethash "length" (gethash "info" metainfo)))
	   (piece-length (gethash "piece length" (gethash "info" metainfo)))
	   (block-length (lsh 1 14))
	   (file-name (gethash "name" (gethash "info" metainfo)))
	   (n-pieces (ceiling file-length piece-length))
	   (n-blocks (ceiling piece-length block-length)))

      (defun try-make-network (peer-list)
	(if peer-list
	    (condition-case nil
		(let ((proc (make-network-process :name (concat info-hash "-" (gethash "ip" (car peer-list)))
						  :host (gethash "ip" (car peer-list))
						  :service (gethash "port" (car peer-list))
						  :filter 'peer-message-handler
						  :no-wait nil)))
		  proc)
	      ('error
	       (etorrent-log (format "Connected to %s failed." (car peer-list)))
	       (try-make-network (cdr peer-list))))))

      (let ((proc (try-make-network peer-list))
	    (download-attribute (make-hash-table :test 'equal)))

	(puthash ":file-name" file-name download-attribute)
	(puthash ":file-buffer" (get-buffer-create file-name) download-attribute)
	(puthash ":file-length" file-length download-attribute)
	(puthash ":piece-length" piece-length download-attribute)
	(puthash ":block-length" block-length download-attribute)
	(puthash ":received" (make-vector n-pieces (make-bool-vector n-blocks nil)) download-attribute)
	(puthash ":requested" (make-vector n-pieces (make-bool-vector n-blocks nil)) download-attribute)
	(puthash ":block" (split-pieces piece-length block-length) download-attribute)
	(puthash ":queue" '() download-attribute)

	(puthash (process-get proc :name) download-attribute etorrent-download-proc-table)
	(process-send-string (try-make-network peer-list) handshake-message)
	(switch-to-buffer (get-buffer-create "etorrent-log"))))))

;; How to save emacs lisp objects to file and reload them.
;; Emacs lisp simply saves objects as S-expressions, reloads them by evaluation.
;; Ref: https://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dump varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defun print-to-file (filename data)
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  (if (file-exists-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(cl-assert (eq (point) (point-min)))
	(read (current-buffer)))
    nil))

;; test if hashtable dump-able.
;; Ref: https://stackoverflow.com/questions/11745097/serializing-an-emacs-lisp-hash-to-file
(featurep 'hashtable-print-readable)

(defun print-torrent-row (torrent-row)
  ;; How to format with a list
  ;; Ref:: https://stackoverflow.com/questions/13041979/how-do-i-format-a-list-of-strings
  (insert-before-markers (apply 'format "%-24s %-8s %-8s %-24s %6s\n" torrent-row)))

;; Torrent table stores a table of torrent, including their file name, size and progress etc.
(defun load-torrent-table ()
  "Loads torrent table into a global variable `etorrent-table`."
  (if (file-exists-p ".etorrent-table")
      (setq etorrent-table (read-from-file ".etorrent-table"))
    (with-current-buffer (get-buffer-create "etorrent")
      (mapc 'print-torrent-row
	    etorrent-table))))

(defun save-torrent-table ()
  "Save torrent table."
  (print-to-file ".etorrent-table" etorrent-table))

;; User Interface
;; To download some torrent file. Run "M-x etorrent-download" and enter the path to the torrent file.
;; Then, this emacs bittorrent client will be invoked and add this new file to the buffer.
(defun etorrent-init ()
  "Set some parameters."
  (setq max-lisp-eval-depth 10000) ; gives a deeper stack upper limit.
  (setq debug-on-error t))

(defun etorrent-download (torrent-file)
  "Provides a command that will run bittorrent client in Emacs Lisp.
  Downloads the torrent file by file name."
  (interactive "sEnter torrent file name: ")
  (if (not (and (get-buffer "etorrent") (get-buffer "etorrent-log")))
      (etorrent))
  (let* ((torrent-info (load-torrent-file torrent-file))
	 (torrent-row `(,(file-name-nondirectory torrent-file)
			,(gethash "length" torrent-info)
			"0.0%" "" "0")))
    (append etorrent-table torrent-row)
    (with-current-buffer (get-buffer "etorrent")
      (print-torrent-row torrent-row))
    (download-torrent torrent-info)))

(defun etorrent ()
  (interactive)
  (etorrent-init)
  (let ((etorrent-buffer (get-buffer "etorrent"))
	(etorrent-log-buffer (get-buffer "etorrent-log")))
    (if (not etorrent-buffer)
	(progn
	  (setq etorrent-buffer (get-buffer-create "etorrent"))
	  (switch-to-buffer etorrent-buffer)
	  (font-lock-mode)
	  (erase-buffer)
	  (insert-before-markers (file-content-to-str "etorrent-welcome.txt"))
	  (insert-before-markers (propertize (format "%-24s %-8s %-8s %-24s %6s\n" "Name" "Size" "Done" "Status" "Seed")
					     'font-lock-face '(:foreground "cyan")))
	  (load-torrent-table)))
    (if (not etorrent-log-buffer)
	(setq etorrent-log-buffer (get-buffer-create "etorrent-log"))))
  (if (not (get-buffer-window "etorrent"))
      (split-window-horizontally))
  (switch-to-buffer (get-buffer-create "etorrent")))

(prin1 etorrent-download-proc-table)
