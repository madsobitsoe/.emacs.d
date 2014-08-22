;; helm-spotify-custom.el --- Control Spotify with Helm
;; Based heavily on the work by Kris Jenkins
;; Find the original on https://github.com/krisajenkins/helm-spotify

(require 'url)
(require 'json)
(require 'helm)

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defun spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
        "Spotify"
        href)))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (alist-get '(href) track)))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (spotify-play-href (alist-get '(album href) track)))


(defun spotify-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "http://ws.spotify.com/search/1/track.json?q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun spotify-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (alist-get '(name) track))
	(track-length (alist-get '(length) track))
	(album-name   (alist-get '(album name) track))
	(artist-names (mapcar (lambda (artist)
				(alist-get '(name) artist))
			      (alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (mapconcat 'identity artist-names "/")
	    album-name)))

(defun spotify-search-formatted (search-term)
  (mapcar (lambda (track)
	    (cons (spotify-format-track track) track))
	  (alist-get '(tracks) (spotify-search search-term))))


(defun helm-spotify-search ()
  (spotify-search-formatted helm-pattern))

(defun helm-spotify-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track))       . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

;;;###autoload
(defvar helm-source-spotify-track-search
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-spotify-search)
    (action-transformer . helm-spotify-actions-for-track)))

;;;###autoload
(defun helm-spotify ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spotify-track-search)
	:buffer "*helm-spotify*"))


(provide 'helm-spotify-custom)
;;; helm-spotify-custom.el ends here 
