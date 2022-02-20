# cl-gpiod - A Common Lisp library to interact with Linux GPIO ports

This library interfaces to
[libgpiod](https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git/tree/include/gpiod.h)
which provides access to GPIO ports.

## libgpiod CFFI definitions

CFFI definitions for all of libgpiod's functionality are provided,
except for the deprecated functions.  All functions and macros are
provided in Common Lisp syntax, i.e. underscores are replaced by minus
signs, upper case macro definitions are turned to lower case constants
with leading and trailing plus signs, the `gpio_` prefix is removed to
be replaced by the `gpiod:` package alias.   Thus,
`gpiod_chip_open_by_name` becomes `gpio:chip-open-by-name`.

## cl-gpiod high-level interface

A `define-gpio` macro is provided, which is used to define all the
bits that an application wants to use on a single GPIO chip.  It
introduces the concept of "ports", which can either be single or
groups of arbitrary bits.  For each port, getter and setter functions
are defined.

Single-bit ports are accessed as booleans when reading or writing.
The logic for input bits can be reversed by setting the `:active-low`
flag.  Multi-bit ports read and write as integer values.

Here is a short example of the high-level interface:

```
(cl-gpiod:define-gpio demo-board
  :chip-name "gpiochip0"
  :ports ((data-out :lines (25 24 23 22)
                    :direction :output)
          (data-in :lines (21 20 19 16)
                   :direction :input
                   :flags (:bias-pull-up :active-low))
          (button-1 :line 17
                    :event :rising-edge
                    :flags (:bias-pull-up :active-low))
          (button-2 :line 18
                    :event :falling-edge
                    :flags (:bias-pull-up :active-low))))
```

Here, bits 23, 24 and 25 are grouped as multi-bit output port named
`data-out`.  It can be written like so:

    (setf (data-out) 3)

Bits 17 and 18 are defined as input ports with edge detection
capability.  They can be read like so:

    (when (button-1) ...)

Also, it is possible to wait for the configured level transition to
occur using the `cl-gpiod:wait-for-event` and
`cl-gpiod:wait-for-event-with-timeout` functions:

    (cl-gpiod:wait-for-event-with-timeout 'button-1 0.5)

Before the accessors can be used, the GPIO chip must be opened using
`cl-gpiod:open-chip`.  A "consumer name" needs to be provided which
will be shown in `gpioinfo` to help identify users of GPIO bits:

    (cl-gpiod:open-chip demo-board "demo")

```
apple2-dev 197_% gpioinfo                                                                                                               ±[●●][main]
gpiochip0 - 54 lines:
    line   0:      unnamed       unused   input  active-high
[...]
    line  17:      unnamed       "demo"   input   active-low [used pull-up]
    line  18:      unnamed       "demo"   input   active-low [used pull-up]
    line  19:      unnamed       unused   input  active-high
    line  20:      unnamed       unused   input  active-high
    line  21:      unnamed       unused   input  active-high
    line  22:      unnamed       unused  output  active-high
    line  23:      unnamed       "demo"  output  active-high [used]
    line  24:      unnamed       "demo"  output  active-high [used]
    line  25:      unnamed       "demo"  output  active-high [used]
    line  26:      unnamed       unused   input  active-high
```


# TODO

 - Better documentation
 - Better open/close experience

# Links

 - [Intro to libgpio](https://lloydrochester.com/post/hardware/libgpiod-intro-rpi/)

# License

Copyright 2022, Hans Hübner

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
