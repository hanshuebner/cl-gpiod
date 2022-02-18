;; -*- Lisp -*-

(defpackage :gpiod-ffi
  (:nicknames :gpiod)
  (:use :cl :alexandria :cffi)
  (:export
   #:+ctxless-flag-open-drain+
   #:+ctxless-flag-open-source+
   #:+ctxless-flag-bias-disable+
   #:+ctxless-flag-bias-pull-down+
   #:ctxless-get-value
   #:ctxless-get-value-ext
   #:ctxless-get-value-multiple
   #:ctxless-get-value-multiple-ext
   #:ctxless-set-value
   #:ctxless-set-value-ext
   #:ctxless-set-value-multiple
   #:ctxless-set-value-multiple-ext
   #:+ctxless-event-rising-edge+
   #:+ctxless-event-falling-edge+
   #:+ctxless-event-both-edges+
   #:+ctxless-event-cb-timeout+
   #:+ctxless-event-cb-rising-edge+
   #:+ctxless-event-cb-falling-edge+
   #:+ctxless-event-cb-ret-err+
   #:+ctxless-event-cb-ret-ok+
   #:+ctxless-event-cb-ret-stop+
   #:+ctxless-event-poll-ret-stop+
   #:+ctxless-event-poll-ret-err+
   #:+ctxless-event-poll-ret-timeout+
   #:ctxless-event-poll-fd
   #:timespec
   #:ctxless-event-monitor
   #:ctxless-event-monitor-ext
   #:ctxless-event-monitor-multiple
   #:ctxless-event-monitor-multiple-ext
   #:ctxless-find-line
   #:chip-open
   #:chip-open-by-name
   #:chip-open-by-label
   #:chip-open-lookup
   #:chip-close
   #:chip-name
   #:chip-label
   #:chip-num-lines
   #:chip-get-line
   #:+line-bulk-max-lines+
   #:line-bulk-init
   #:line-bulk-add
   #:line-bulk-get-line
   #:line-bulk-num-lines
   #:chip-get-lines
   #:chip-get-all-lines
   #:chip-find-line
   #:chip-find-lines
   #:+line-direction-input+
   #:+line-direction-output+
   #:+line-active-state-high+
   #:+line-active-state-low+
   #:+line-bias-as-is+
   #:+line-bias-as-disable+
   #:+line-bias-as-pull-up+
   #:+line-bias-as-pull-down+
   #:line-offset
   #:line-name
   #:line-consumer
   #:line-direction
   #:line-active-state
   #:line-bias
   #:line-is-used
   #:line-is-open-drain
   #:line-is-open-source
   #:line-update
   #:+line-request-direction-as-is+
   #:+line-request-direction-input+
   #:+line-request-direction-output+
   #:+line-request-event-falling-edge+
   #:+line-request-event-rising-edge+
   #:+line-request-event-both-edges+
   #:+line-request-flag-open-drain+
   #:+line-request-flag-open-source+
   #:+line-request-flag-active-low+
   #:+line-request-flag-bias-disable+
   #:+line-request-flag-bias-pull-down+
   #:+line-request-flag-bias-pull-up+
   #:line-request
   #:line-request-input
   #:line-request-output
   #:line-request-rising-edge-events
   #:line-request-falling-edge-events
   #:line-request-both-edges-events
   #:line-request-input-flags
   #:line-request-output-flags
   #:line-request-rising-edge-events-flags
   #:line-request-falling-edge-events-flags
   #:line-request-both-edges-events-flags
   #:line-request-bulk
   #:line-request-bulk-input
   #:line-request-bulk-output
   #:line-request-bulk-rising-edge-events
   #:line-request-bulk-falling-edge-events
   #:line-request-bulk-both-edges-events
   #:line-request-bulk-input-flags
   #:line-request-bulk-output-flags
   #:line-request-bulk-rising-events-flags
   #:line-request-bulk-falling-events-flags
   #:line-request-bulk-both-edges-events-flags
   #:line-release
   #:line-release-bulk
   #:line-is-requested
   #:line-is-free
   #:line-get-value
   #:line-get-value-bulk
   #:line-set-value
   #:line-set-value-bulk
   #:line-set-config
   #:line-set-config-bulk
   #:line-set-flags
   #:line-set-flags-bulk
   #:line-set-direction-input
   #:line-set-direction-input-bulk
   #:line-set-direction-output
   #:line-set-direction-output-bulk
   #:+line-event-rising-edge+
   #:+line-event-falling-edge+
   #:line-event-wait
   #:line-event-wait-bulk
   #:line-event-read
   #:line-event-read-multiple
   #:line-event-get-fd
   #:line-event-read-fd
   #:line-event-read-fd-multiple
   #:line-get
   #:line-find
   #:line-close-chip
   #:line-get-chip
   #:chip-iter-new
   #:chip-iter-free
   #:chip-iter-free-noclose
   #:chip-iter-next
   #:chip-iter-next-noclose
   #:line-iter-new
   #:line-iter-free
   #:line-iter-next
   #:version-string
   #:line-request-config))

(in-package :gpiod-ffi)

(define-foreign-library libgpiod
  (:linux "libgpiod.so"))

(use-foreign-library libgpiod)

(defmacro gpiod-bit (nr)
  `(ash 1 (- ,nr)))

(defconstant +ctxless-flag-open-drain+ (gpiod-bit 0))
(defconstant +ctxless-flag-open-source+ (gpiod-bit 1))
(defconstant +ctxless-flag-bias-disable+ (gpiod-bit 2))
(defconstant +ctxless-flag-bias-pull-down+ (gpiod-bit 3))
(defconstant +ctxless-flag-bias-pull-down+ (gpiod-bit 4))

(defcfun ("gpiod_ctxless_get_value" ctxless-get-value) :int
  (device :string)
  (offset :unsigned-int)
  (active-low :bool)
  (consumer :string))

(defcfun ("gpiod_ctxless_get_value_ext" ctxless-get-value-ext) :int
  (device :string)
  (offset :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_ctxless_get_value_multiple" ctxless-get-value-multiple) :int
  (device :string)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string))

(defcfun ("gpiod_ctxless_get_value_multiple_ext" ctxless-get-value-multiple-ext) :int
  (device :string)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_ctxless_set_value" ctxless-set-value) :int
  (device :string)
  (offset :unsigned-int)
  (value :int)
  (active-low :bool)
  (consumer :string)
  (cb :pointer)
  (data (:pointer :void)))

(defcfun ("gpiod_ctxless_set_value_ext" ctxless-set-value-ext) :int
  (device :string)
  (offset :unsigned-int)
  (value :int)
  (active-low :bool)
  (consumer :string)
  (cb :pointer)
  (data (:pointer :void))
  (flags :int))

(defcfun ("gpiod_ctxless_set_value_multiple" ctxless-set-value-multiple) :int
  (device :string)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (cb :pointer)
  (data (:pointer :void)))

(defcfun ("gpiod_ctxless_set_value_multiple_ext" ctxless-set-value-multiple-ext) :int
  (device :string)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (cb :pointer)
  (data (:pointer :void))
  (flags :int))

(defconstant +ctxless-event-rising-edge+ 1)
(defconstant +ctxless-event-falling-edge+ 2)
(defconstant +ctxless-event-both-edges+ 3)

(defconstant +ctxless-event-cb-timeout+ 1)
(defconstant +ctxless-event-cb-rising-edge+ 2)
(defconstant +ctxless-event-cb-falling-edge+ 3)

(defconstant +ctxless-event-cb-ret-err+ -1)
(defconstant +ctxless-event-cb-ret-ok+ 0)
(defconstant +ctxless-event-cb-ret-stop+ 1)

(defconstant +ctxless-event-poll-ret-stop+ -2)
(defconstant +ctxless-event-poll-ret-err+ -1)
(defconstant +ctxless-event-poll-ret-timeout+ 0)

(defcstruct ctxless-event-poll-fd
  (fd :int)
  (event :bool))

(defcstruct timespec
  (tv-sec :long)
  (tv-usec :long))

(defcfun ("gpiod_ctxless_event_monitor" ctxless-event-monitor) :int
  (device :string)
  (event-type :int)
  (offset :int)
  (active-low :bool)
  (consumer :string)
  (timeout (:pointer (:struct timespec)))
  (poll-cb :pointer)
  (event-cb :pointer)
  (data (:pointer :void)))

(defcfun ("gpiod_ctxless_event_monitor_ext" ctxless-event-monitor-ext) :int
  (device :string)
  (event-type :int)
  (offset :int)
  (active-low :bool)
  (consumer :string)
  (timeout (:pointer (:struct timespec)))
  (poll-cb :pointer)
  (event-cb :pointer)
  (data (:pointer :void))
  (flags :int))

(defcfun ("gpiod_ctxless_event_monitor_multiple" ctxless-event-monitor-multiple) :int
  (device :string)
  (event-type :int)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (timeout (:pointer (:struct timespec)))
  (poll-cb :pointer)
  (event-cb :pointer)
  (data (:pointer :void)))

(defcfun ("gpiod_ctxless_event_monitor_multiple_ext" ctxless-event-monitor-multiple-ext) :int
  (device :string)
  (event-type :int)
  (offsets (:pointer :unsigned-int))
  (values (:pointer :int))
  (num-lines :unsigned-int)
  (active-low :bool)
  (consumer :string)
  (timeout (:pointer (:struct timespec)))
  (poll-cb :pointer)
  (event-cb :pointer)
  (data (:pointer :void))
  (flags :int))

(defcfun ("gpiod_ctxless_find_line" ctxless-find-line) :int
  (name :string)
  (chipname (:pointer :char))
  (chipname-size :long)
  (offset (:pointer :int)))

(defcfun ("gpiod_chip_open" chip-open) :pointer
  (path :string))

(defcfun ("gpiod_chip_open_by_name" chip-open-by-name) :pointer
  (name :string))

(defcfun ("gpiod_chip_open_by_number" chip-open-by-number) :pointer
  (num :unsigned-int))

(defcfun ("gpiod_chip_open_by_label" chip-open-by-label) :pointer
  (label :string))

(defcfun ("gpiod_chip_open_lookup" chip-open-lookup) :pointer
  (descr :string))

(defcfun ("gpiod_chip_close" chip-close) :void
  (chip :pointer))

(defcfun ("gpiod_chip_name" chip-name) :string
  (chip :pointer))

(defcfun ("gpiod_chip_label" chip-label) :string
  (chip :pointer))

(defcfun ("gpiod_chip_num_lines" chip-num-lines) :int
  (chip :pointer))

(defcfun ("gpiod_chip_get_line" chip-get-line) :pointer
  (chip :pointer)
  (offset :unsigned-int))

(defconstant +line-bulk-max-lines+ 64)

(defcstruct line-bulk
  (lines :pointer :count #.+line-bulk-max-lines+)
  (num-lines :unsigned-int))

(defun line-bulk-init (bulk)
  (with-foreign-slots ((num-lines) bulk (:struct line-bulk))
    (setf num-lines 0)))

(defun line-bulk-add (bulk line)
  (with-foreign-slots ((num-lines lines) bulk (:struct line-bulk))
    (setf (mem-aref lines :pointer num-lines) line)
    (incf num-lines)))

(defun line-bulk-get-line (bulk offset)
  (with-foreign-slots ((lines) bulk (:struct line-bulk))
    (mem-aref lines :pointer offset)))

(defun line-bulk-num-lines (bulk)
  (with-foreign-slots ((num-lines) bulk (:struct line-bulk))
    num-lines))

(defcfun ("gpiod_chip_get_lines" chip-get-lines) :int
  (chip :pointer)
  (offsets (:pointer :unsigned-int))
  (num-offsets :unsigned-int)
  (bulk (:pointer (:struct line-bulk))))

(defcfun ("gpiod_chip_get_all_lines" chip-get-all-lines) :int
  (chip :pointer)
  (bulk (:pointer (:struct line-bulk))))

(defcfun ("gpiod_chip_find_line" chip-find-line) :pointer
  (chip :pointer)
  (name :string))

(defcfun ("gpiod_chip_find_lines" chip-find-lines) :int
  (chip :pointer)
  (names :pointer)
  (bulk (:pointer (:struct line-bulk))))

(defconstant +line-direction-input+ 1)
(defconstant +line-direction-output+ 2)

(defconstant +line-active-state-high+ 1)
(defconstant +line-active-state-low+ 2)

(defconstant +line-bias-as-is+ 1)
(defconstant +line-bias-as-disable+ 2)
(defconstant +line-bias-as-pull-up+ 3)
(defconstant +line-bias-as-pull-down+ 4)

(defcfun ("gpiod_line_offset" line-offset) :unsigned-int
  (line :pointer))

(defcfun ("gpiod_line_name" line-name) :string
  (line :pointer))

(defcfun ("gpiod_line_consumer" line-consumer) :string
  (line :pointer))

(defcfun ("gpiod_line_direction" line-direction) :int
  (line :pointer))

(defcfun ("gpiod_line_active_state" line-active-state) :int
  (line :pointer))

(defcfun ("gpiod_line_bias" line-bias) :int
  (line :pointer))

(defcfun ("gpiod_line_is_used" line-is-used) :bool
  (line :pointer))

(defcfun ("gpiod_line_is_open_drain" line-is-open-drain) :bool
  (line :pointer))

(defcfun ("gpiod_line_is_open_source" line-is-open-source) :bool
  (line :pointer))

(defcfun ("gpiod_line_update" line-update) :int
  (line :pointer))

(defconstant +line-request-direction-as-is+ 1)
(defconstant +line-request-direction-input+ 2)
(defconstant +line-request-direction-output+ 3)
(defconstant +line-request-event-falling-edge+ 4)
(defconstant +line-request-event-rising-edge+ 5)
(defconstant +line-request-event-both-edges+ 6)

(defconstant +line-request-flag-open-drain+ (gpiod-bit 0))
(defconstant +line-request-flag-open-source+ (gpiod-bit 1))
(defconstant +line-request-flag-active-low+ (gpiod-bit 2))
(defconstant +line-request-flag-bias-disable+ (gpiod-bit 3))
(defconstant +line-request-flag-bias-pull-down+ (gpiod-bit 4))
(defconstant +line-request-flag-bias-pull-up+ (gpiod-bit 5))

(defcstruct line-request-config
  (consumer :string)
  (request-type :int)
  (flags :int))

(defcfun ("gpiod_line_request" line-request) :int
  (line :pointer)
  (config (:pointer (:struct line-request-config)))
  (default-val :int))

(defcfun ("gpiod_line_request_input" line-request-input) :int
  (line :pointer)
  (consumer :string))

(defcfun ("gpiod_line_request_output" line-request-output) :int
  (line :pointer)
  (consumer :string)
  (default-val :int))

(defcfun ("gpiod_line_request_rising_edge_events" line-request-rising-edge-events) :int
  (line :pointer)
  (consumer :string))

(defcfun ("gpiod_line_request_falling_edge_events" line-request-falling-edge-events) :int
  (line :pointer)
  (consumer :string))

(defcfun ("gpiod_line_request_both_edges_events" line-request-both-edges-events) :int
  (line :pointer)
  (consumer :string))

(defcfun ("gpiod_line_request_input_flags" line-request-input-flags) :int
  (line :pointer)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_output_flags" line-request-output-flags) :int
  (line :pointer)
  (consumer :string)
  (flags :int)
  (default-val :int))

(defcfun ("gpiod_line_request_rising_edge_events_flags" line-request-rising-edge-events-flags) :int
  (line :pointer)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_falling_edge_events_flags" line-request-falling-edge-events-flags) :int
  (line :pointer)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_both_edges_events_flags" line-request-both-edges-events-flags) :int
  (line :pointer)
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_bulk" line-request-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (config (:pointer (:struct line-request-config)))
  (default-vals (:pointer :int)))

(defcfun ("gpiod_line_request_bulk_input" line-request-bulk-input) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string))

(defcfun ("gpiod_line_request_bulk_output" line-request-bulk-output) :int
  (bulk (:pointer (:struct line-bulk)))
  (config (:pointer (:struct line-request-config)))
  (default-vals (:pointer :int)))

(defcfun ("gpiod_line_request_bulk_rising_edge_events" line-request-bulk-rising-edge-events) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string))

(defcfun ("gpiod_line_request_bulk_falling_edge_events" line-request-bulk-falling-edge-events) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string))

(defcfun ("gpiod_line_request_bulk_both_edges_events" line-request-bulk-both-edges-events) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string))

(defcfun ("gpiod_line_request_bulk_input_flags" line-request-bulk-input-flags) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_bulk_output_flags" line-request-bulk-output-flags) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string)
  (flags :int)
  (default-vals (:pointer :int)))

(defcfun ("gpiod_line_request_bulk_rising_events_flags" line-request-bulk-rising-events-flags) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_bulk_falling_events_flags" line-request-bulk-falling-events-flags) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_request_bulk_both_edges_events_flags" line-request-bulk-both-edges-events-flags) :int
  (bulk (:pointer (:struct line-bulk)))
  (consumer :string)
  (flags :int))

(defcfun ("gpiod_line_release" line-release) :void
  (line :pointer))

(defcfun ("gpiod_line_release_bulk" line-release-bulk) :void
  (bulk (:pointer (:struct line-bulk))))

(defcfun ("gpiod_line_is_requested" line-is-requested) :bool
  (line :pointer))

(defcfun ("gpiod_line_is_free" line-is-free) :bool
  (line :pointer))

(defcfun ("gpiod_line_get_value" line-get-value) :int
  (line :pointer))

(defcfun ("gpiod_line_get_value_bulk" line-get-value-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (values (:pointer :int)))

(defcfun ("gpiod_line_set_value" line-set-value) :int
  (line :pointer)
  (value :int))

(defcfun ("gpiod_line_set_value_bulk" line-set-value-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (values (:pointer :int)))

(defcfun ("gpiod_line_set_config" line-set-config) :int
  (line :pointer)
  (direction :int)
  (flags :int)
  (value :int))

(defcfun ("gpiod_line_set_config_bulk" line-set-config-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (direction :int)
  (flags :int)
  (values (:pointer :int)))

(defcfun ("gpiod_line_set_flags" line-set-flags) :int
  (line :pointer)
  (flags :int))

(defcfun ("gpiod_line_set_flags_bulk" line-set-flags-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (flags :int))

(defcfun ("gpiod_line_set_direction_input" line-set-direction-input) :int
  (line :pointer))

(defcfun ("gpiod_line_set_direction_input_bulk" line-set-direction-input-bulk) :int
  (bulk (:pointer (:struct line-bulk))))

(defcfun ("gpiod_line_set_direction_output" line-set-direction-output) :int
  (line :pointer)
  (value :int))

(defcfun ("gpiod_line_set_direction_output_bulk" line-set-direction-output-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (values (:pointer :int)))

(defconstant +line-event-rising-edge+ 1)
(defconstant +line-event-falling-edge+ 2)

(defcstruct line-event
  (ts (:struct timespec))
  (event-type :int))

(defcfun ("gpiod_line_event_wait" line-event-wait) :int
  (line :pointer)
  (timeout (:pointer (:struct timespec))))

(defcfun ("gpiod_line_event_wait_bulk" line-event-wait-bulk) :int
  (bulk (:pointer (:struct line-bulk)))
  (ts (:pointer (:struct timespec)))
  (event-bulk (:pointer (:struct line-bulk))))

(defcfun ("gpiod_line_event_read" line-event-read) :int
  (line :pointer)
  (event (:pointer (:struct line-event))))

(defcfun ("gpiod_line_event_read_multiple" line-event-read-multiple) :int
  (line :pointer)
  (events (:pointer (:struct line-event)))
  (num-events :unsigned-int))

(defcfun ("gpiod_line_event_get_fd" line-event-get-fd) :int
  (line :pointer))

(defcfun ("gpiod_line_event_read_fd" line-event-read-fd) :int
  (fd :int)
  (event (:pointer (:struct line-event))))

(defcfun ("gpiod_line_event_read_fd_multiple" line-event-read-fd-multiple) :int
  (fd :int)
  (events (:pointer (:struct line-event)))
  (num-events :unsigned-int))

(defcfun ("gpiod_line_get" line-get) :pointer
  (device :string)
  (offset :unsigned-int))

(defcfun ("gpiod_line_find" line-find) :pointer
  (name :string))

(defcfun ("gpiod_line_close_chip" line-close-chip) :void
  (line :pointer))

(defcfun ("gpiod_line_get_chip" line-get-chip) :pointer
  (line :pointer))

(defcfun ("gpiod_chip_iter_new" chip-iter-new) :pointer)

(defcfun ("gpiod_chip_iter_free" chip-iter-free) :void
  (iter :pointer))

(defcfun ("gpiod_chip_iter_free_noclose" chip-iter-free-noclose) :void
  (iter :pointer))

(defcfun ("gpiod_chip_iter_next" chip-iter-next) :pointer
  (iter :pointer))

(defcfun ("gpiod_chip_iter_next_noclose" chip-iter-next-noclose) :pointer
  (iter :pointer))

(defcfun ("gpiod_line_iter_new" line-iter-new) :pointer
  (chip :pointer))

(defcfun ("gpiod_line_iter_free" line-iter-free) :void
  (iter :pointer))

(defcfun ("gpiod_line_iter_next" line-iter-next) :pointer
  (iter :pointer))

(defcfun ("gpiod_version_string" version-string) :string)
